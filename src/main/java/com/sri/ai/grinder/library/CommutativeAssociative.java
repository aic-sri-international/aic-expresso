/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.grinder.library;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Equals;

/**
 * Implements the basics for commutative, associative operators with a neutral
 * element such as addition, multiplication, conjunction, etc. It relies on
 * actual implementations providing the functor of the operation, its neutral
 * element, a predicate identifying arguments that can be operated on (for
 * example, addition operates on constants, but not variables), and a method
 * applying the operation on these operable arguments. It works by either
 * returning the result of the operation on all arguments, if all arguments are
 * operable (for addition, e.g., 1 + 2 + 3 returns 6), or returning an
 * application of the operation on the result on operable arguments together
 * with the non-operable arguments (for addition, 1 + X + 3 becomes 4 + X).
 * 
 * @author braz
 */
@Beta
public abstract class CommutativeAssociative extends AbstractRewriter {
	
	public CommutativeAssociative() {
		this.setReifiedTests(new HasKind(getFunctor()));
	}

	public abstract Object getFunctor();
	protected abstract Expression getNeutralElement();
	protected abstract Expression getAbsorbingElement();
	protected abstract Predicate<Expression> getIsOperableArgumentSyntaxTreePredicate();
	protected abstract Expression operationOnOperables(LinkedList<Expression> operableArguments);
	protected abstract Expression operationOnExpressionOperables(LinkedList<Expression> operableArguments);

	public Expression getNeutralElementExpression() {
		return getNeutralElement();
	}

	public Predicate<Expression> getIsOperableArgumentExpressionPredicate() {
		Predicate<Expression> result = new Predicate<Expression>() {
			@Override
			public boolean apply(Expression expression) {
				boolean result = getIsOperableArgumentSyntaxTreePredicate().apply(expression);
				return result;
			}
		};
		return result;
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {

		if ( ! expression.hasFunctor(getFunctor())) {
			return expression;
		}
		
		List<Expression> arguments = expression.getArguments();
		
		if (arguments.isEmpty()) {
			return getNeutralElementExpression();
		}
		
		if (arguments.size() == 1) {
			return expression.get(0);
		}
		
		LinkedList<Expression> operableArguments = new LinkedList<Expression>();
		LinkedList<Expression> nonOperableArguments = new LinkedList<Expression>();
		int indexOfFirstOperable =
			Util.collect(
					arguments,
					operableArguments, getIsOperableArgumentExpressionPredicate(),
					nonOperableArguments);
	
		if (operableArguments.size() == 0) {
			return expression; // everything is an non-operable argument, nothing that can be done.
		}
		
		// get result on operable arguments
		Expression resultOnOperableArgumentsExpression = operationOnExpressionOperables(operableArguments);

		// this next if then else is both an optimization for the case in which there is a single operable,
		// and a way to make sure
		// we return the same expression instance when it doesn't change (like x + 2), lest we generate
		// a distinct but equal instance that would keep being re-evaluated by the same rewriter.
		if (operableArguments.size() == 1) {
			if (operableArguments.getFirst().equals(getNeutralElementExpression())) {
				// we don't need to include the neutral element, and are left with non-operable arguments only, done.
				Expression result = makeExpressionWithSameFunctorAsThis(nonOperableArguments);
				return result;
			}
			else {
				// the expression is formed by non-operable arguments and a single non-neutral operable argument, nothing else to do.
				return expression;
			}
		}

		// now we deal with the case of more than one operable argument
		
		// if there are no non-operable arguments, that's it.
		if (nonOperableArguments.size() == 0) {
			return resultOnOperableArgumentsExpression;
		}

		// if there are non-operable arguments, put them together with operables's result, unless this is the neutral element.
		LinkedList<Expression> argumentsOfResultingExpression = nonOperableArguments; // changing semantics, and therefore, for clarity, the name. It's ok to modify it since it's been created locally.
		if ( ! resultOnOperableArgumentsExpression.equals(getNeutralElement())) {
			argumentsOfResultingExpression.add(indexOfFirstOperable, resultOnOperableArgumentsExpression);
		}

		// return operation on result on operable and non-operable arguments.
		Expression result = makeExpressionWithSameFunctorAsThis(argumentsOfResultingExpression);
		return result;
	}
	
	/**
	 * An instance method version of {@link CommutativeAssociative#make(Object, List, Object)}
	 * that uses the current object for obtaining the functor and neutral element.
	 */
	public Expression makeWithSameFunctorAsThis(List<Expression> arguments) {
		return make(Expressions.wrap(getFunctor()), arguments, getNeutralElement());
	}
	
	/**
	 * An instance method version of {@link CommutativeAssociative#make(Object, List, Object)}
	 * that uses the current object for obtaining the functor and neutral element.
	 */
	public Expression makeExpressionWithSameFunctorAsThis(List<Expression> arguments) {
		return make(Expressions.wrap(getFunctor()), arguments, getNeutralElement());
	}
	
	/**
	 * Similar to {@link Expressions#apply(Object, Object...)},
	 * but makes a simplified function application of an associative commutative operator,
	 * that is, its application but for the cases in which there are no arguments, or a single argument.
	 * When there are no arguments, a given neutral element value is returned.
	 * When a single argument is provided, it is returned itself.
	 */
	public static Expression make(Object functor, List<Expression> arguments, Expression neutralElement) {
		Predicate<Expression> notEqual = Predicates.not(new Equals<Expression>(neutralElement));
		arguments = Util.collectToList(arguments, notEqual);
		if (arguments.isEmpty()) {
			return Expressions.wrap(neutralElement);
		}
		if (arguments.size() == 1) {
			return arguments.get(0);
		}
		Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(Expressions.wrap(functor), arguments);
		result = Associative.associateWhenSureOperatorIsAssociative(result);
		return result;
	}

	public static Expression make(Object functor, Collection<Expression> arguments, Expression absorbingElement, Expression neutralElement) {
		return make(functor, arguments.iterator(), absorbingElement, neutralElement);
	}
	
	/**
	 * Makes a commutative associative application from arguments in an iterator's range.
	 * This is potentially efficient in that it will stop as soon as the result is determined
	 * when it finds an absorbing element.
	 * If the arguments are being computed on the fly (for example, the iterator is a UnaryFunctionIterator),
	 * this can save a lot of time.
	 */
	public static Expression make(Object functor, Iterator<Expression> argumentsIterator, Expression absorbingElement, Expression neutralElement) {
		absorbingElement = Expressions.wrap(absorbingElement);
		List<Expression> arguments = new LinkedList<Expression>();
		while (argumentsIterator.hasNext()) {
			Expression argument = argumentsIterator.next();
			if (argument.equals(absorbingElement)) {
				return absorbingElement;
			}
			if ( ! argument.equals(neutralElement)) {
				arguments.add(argument);
			}
		}
		Expression result = make(functor, arguments, neutralElement);
		return result;
	}

	/**
	 * If expression is an application of functor, returns its arguments.
	 * If not, assumes that it is equivalent to <code>functor(expression)</code>
	 * (a property of commutative associative functions)
	 * and returns its arguments, that is, a singleton collection containing expression.
	 */
	public static List<Expression> getArgumentsOfNormalizedApplicationOf(Object functor, Expression expression) {
		if (expression.hasFunctor(functor)) {
			return expression.getArguments();
		}
		return Util.list(expression);
	}
}
