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

import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

/**
 * Implements equality.
 * 
 * @author braz
 */
@Beta
public class Equality extends AbstractRewriter {

	public static final Expression FUNCTOR = DefaultSymbol.createSymbol("=");

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (expression.hasFunctor(FUNCTOR) &&
				expression.numberOfArguments() > 1) {
			return equalityResultIfItIsKnown(expression, process);
		}
		return expression;
	}

	public static Expression equalityResultIfItIsKnown(Expression expression, RewritingProcess process) {
		Boolean equalityResult =
			equalityResultIfItIsKnownOrNull(expression, process.getIsConstantPredicate());
		if (equalityResult != null) {
			return DefaultSymbol.createSymbol(equalityResult);
		}
		return expression;
	}

	private static Boolean equalityResultIfItIsKnownOrNull(Expression expression, Predicate<Expression> isConstantPredicate) {
		int maxIndex = expression.numberOfArguments() - 1;
		for (int i = 0; i != maxIndex; i++) {
			Boolean equalityOfPair =
				equalityOfPairIfItIsKnownOrNull(
						expression.get(i), expression.get(i+1), isConstantPredicate);
			if (equalityOfPair == null) {
				return null;
			}
			if (!equalityOfPair) {
				return false;
			}
		}
		return true;
	}

	private static Boolean equalityOfPairIfItIsKnownOrNull(
			Expression expression1, Expression expression2, Predicate<Expression> isConstantPredicate) {
		if (isConstantPredicate.apply(expression1)) {
			if (isConstantPredicate.apply(expression2)) {
				return expression1.equals(expression2); // two constants; true if they are equal, false if they are distinct
			}
			else {
				return null;  // variable and constant; we don't know
			}
		}
		else if (isConstantPredicate.apply(expression2)) {
			return null; // variable and constant; we don't know
		}
		else if (expression1.equals(expression2)) {
			return true; // both are the same variable
		}
		else {
			return null; // distinct variables; we don't know
		}
	}

	public static Expression getFunctor() {
		return FUNCTOR;
	}
	
	public static Expression conditionForSubExpressionsEquality(Expression expression1, Expression expression2) {
		List<Expression> conditionsForSubExpressionsToBeEqual = listOfEqualitiesOfSubExpressions(
				expression1, expression2);
		Expression condition = And.make(conditionsForSubExpressionsToBeEqual);
		return condition;
	}

	public static List<Expression> listOfEqualitiesOfSubExpressions(
			Expression expression1, Expression expression2) {
		List<Expression> conditionsForSubExpressionsToBeEqual =
			Util.zipWith(
					MAKE_PAIR_EQUALITY,
					Util.listFrom(expression1.getImmediateSubExpressionsIterator()),
					Util.listFrom(expression2.getImmediateSubExpressionsIterator()));
		return conditionsForSubExpressionsToBeEqual;
	}

	/**
	 * Makes an equality of some expressions, returning Expressions. Returns TRUE if the expressions are identical.
	 */
	public static Expression make(Object... arguments) {
		List<SyntaxTree> expressions = Expressions.wrap(arguments);
		Set<Expression> uniqueExpressions = new HashSet<Expression>(expressions);
		if (uniqueExpressions.size() < 2) {
			return Expressions.TRUE;
		}
		return new DefaultCompoundSyntaxTree("=", expressions);
	}

	public static BinaryFunction<Expression, Expression, Expression> MAKE_PAIR_EQUALITY = new BinaryFunction<Expression, Expression, Expression>() {
		@Override
		public Expression apply(Expression e1, Expression e2) {
			return make(e1, e2);
		}
	};
	
	/**
	 * If given constraint is an equality binding a set of variables to a constant,
	 * substitutes them in expression by that constant.
	 * Can potentially throw {@link Substitute#NoException} if such substitutions are not supported
	 * (this can happen in certain types of complex expressions -- see {@link Substitute} for more details).
	 */
	public static Expression substituteVariablesByConstantIfTheyAreBoundToItByEquality(Expression expression, Expression constraint, RewritingProcess process) {
		if (isEquality(constraint)) {
			Pair<List<Expression>, Expression> variablesListAndConstant = getVariablesListAndConstantOrNullIfNoConstant(constraint, process);
			if (variablesListAndConstant != null) {
				for (Expression variable : variablesListAndConstant.first) {
					expression = Substitute.replace(expression, variable, variablesListAndConstant.second, process);
				}
			}
		}
		return expression;
	}

	/**
	 * Assumes that first argument is an equality (with possibly more than two arguments) and returns a pair,
	 * the first member of which is a collection of the variables in the equality,
	 * and the second member of which is the first constant present.
	 * Returns null if there are no constants in the equality arguments.
	 */
	public static Pair<List<Expression>, Expression> getVariablesListAndConstantOrNullIfNoConstant(Expression equality, RewritingProcess process) {
		Pair<List<Expression>, Expression> result;
		List<Expression> variables = new LinkedList<Expression>();
		List<Expression> constants = new LinkedList<Expression>();
		Predicate<Expression> notIsConstant = Predicates.not(process.getIsConstantPredicate());
		Util.collectOrReturnFalseIfElementDoesNotFitEither(
				equality.getArguments(),
				variables, notIsConstant,
				constants, process.getIsConstantPredicate());
		if (constants.isEmpty()) {
			result = null;
		}
		else {
			result = new Pair<List<Expression>, Expression>(variables, Util.getFirst(constants));
		}
		return result;
	}

	public static boolean isEquality(Expression expression) {
		return expression.hasFunctor(FunctorConstants.EQUAL);
	}
	
	public static Set<Expression> getSymbolsBoundToSomethingElse(Expression equality) {
		Set<Expression> result = new LinkedHashSet<Expression>();
		
		if (isEquality(equality)) {
			for (Expression term : equality.getArguments()) {
				result.add(term);
			}
			// i.e. A = A, A is not bound to something else. 
			if (result.size() == 1) {
				result.clear();
			}
		}
		
		return result;
	}

	/**
	 * Returns X = c, if 'expression' is c = X, for X a variable and c a constant, or 'expression' otherwise.
	 * It actually works for any other functor as well. 
	 */
	public static Expression normalize(Expression expression, RewritingProcess process) {
		if (expression.numberOfArguments() == 2
				&& process.isConstant(expression.get(0))) {
			if ( ! process.isConstant(expression.get(1))) {
				Expression result = Expressions.make(expression.getFunctor(), expression.get(1), expression.get(0));
				return result;
			}
		}
		return expression;
	}

	/**
	 * Returns a conjunction of equalities between the corresponding elements of two lists
	 * (two elements are corresponding if they have the indices).
	 * Returns false if the lists have different sizes.
	 */
	public static Expression makePairwiseEquality(List<Expression> list1, List<Expression> list2) {
		if (list1.size() == list2.size()) {
			Expression result = And.make(Expressions.makePairwiseApplications(FunctorConstants.EQUAL, list1, list2));
			return result;
		}
		return Expressions.FALSE;
	}
}
