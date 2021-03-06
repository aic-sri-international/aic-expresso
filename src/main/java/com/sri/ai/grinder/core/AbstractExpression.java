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
package com.sri.ai.grinder.core;

import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndSyntacticContext;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.ReplaceByIfEqualTo;
import com.sri.ai.util.base.TernaryProcedure;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.math.Rational;

/**
 * A basic, default implementation of some of the {@link Expression} methods.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractExpression implements Expression {
	//
	protected static final long serialVersionUID = 4L; // Note: Increment this when you want to ensure any parsing caches are invalidated 
	
	protected String  cachedToString = null;
	
	@Override
	public Expression replaceFirstOccurrence(Expression replaced, Expression replacement, Registry registry) {
		return replaceFirstOccurrence(new ReplaceByIfEqualTo<Expression>(replacement, replaced), null, null, registry);
	}

	@Override
	public Expression replaceAllOccurrences(Expression replaced, Expression replacement, Registry registry) {
		return replaceAllOccurrences(new ReplaceByIfEqualTo<Expression>(replacement, replaced), null, null, registry);
	}

	@Override
	public Expression replaceFirstOccurrence(Expression replaced, Expression replacement, PruningPredicate prunePredicate, Registry registry) {
		return replaceFirstOccurrence(new ReplaceByIfEqualTo<Expression>(replacement, replaced), prunePredicate, registry);
	}

	@Override
	public Expression replaceAllOccurrences(Expression replaced, Expression replacement, PruningPredicate prunePredicate, Registry registry) {
		return replaceAllOccurrences(new ReplaceByIfEqualTo<Expression>(replacement, replaced), prunePredicate, registry);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, Registry registry) {
		return replaceFirstOccurrence(replacementFunction, null, null, registry);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, Registry registry) {
		return replaceAllOccurrences(replacementFunction, null, null, registry);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, Registry registry) {
		return replaceFirstOccurrence(replacementFunction, prunePredicate, null, registry);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, Registry registry) {
		return replaceAllOccurrences(replacementFunction, prunePredicate, null, registry);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, ReplacementFunctionMaker makeSpecificSubExpressionAndSyntacticContextReplacementFunction, PruningPredicate prunePredicate, PruningPredicateMaker makeSpecificSubExpressionAndSyntacticContextPrunePredicate, Registry registry) {
		return replace(
				replacementFunction, makeSpecificSubExpressionAndSyntacticContextReplacementFunction,
				prunePredicate, makeSpecificSubExpressionAndSyntacticContextPrunePredicate,
				true, false, false, null, registry);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, ReplacementFunctionMaker makeSpecificSubExpressionAndSyntacticContextReplacementFunction, PruningPredicate prunePredicate, PruningPredicateMaker makeSpecificSubExpressionAndSyntacticContextPrunePredicate, Registry registry) {
		return replace(
				replacementFunction, makeSpecificSubExpressionAndSyntacticContextReplacementFunction,
				prunePredicate, makeSpecificSubExpressionAndSyntacticContextPrunePredicate,
				false, false, false, null, registry);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {
		return replaceFirstOccurrence(replacementFunction, null, listener, registry);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {
		return replaceAllOccurrences(replacementFunction, null, listener, registry);
	}

	@Override
	public Expression replaceFirstOccurrence(Expression replaced, Expression replacement, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {
		return replaceFirstOccurrence(new ReplaceByIfEqualTo<Expression>(replacement, replaced), null, listener, registry);
	}

	@Override
	public Expression replaceAllOccurrences(Expression replaced, Expression replacement, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {
		return replaceAllOccurrences(new ReplaceByIfEqualTo<Expression>(replacement, replaced), null, listener, registry);
	}

	@Override
	public Expression replaceFirstOccurrence(Expression replaced, Expression replacement, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {
		return replaceFirstOccurrence(new ReplaceByIfEqualTo<Expression>(replacement, replaced), prunePredicate, listener, registry);
	}

	@Override
	public Expression replaceAllOccurrences(Expression replaced, Expression replacement, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {
		return replaceAllOccurrences(new ReplaceByIfEqualTo<Expression>(replacement, replaced), prunePredicate, listener, registry);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {
		return replace(replacementFunction, true /* only the first one */, prunePredicate, false, listener, registry);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {
		return replace(replacementFunction, false /* not only the first one */, prunePredicate, false, listener, registry);
	}

	@Override
	public Expression replace(
			Function<Expression, Expression> replacementFunction, boolean onlyTheFirstOne,
			PruningPredicate prunePredicate, boolean ignoreTopExpression, TernaryProcedure<Expression, Expression, Registry> listener, Registry registry) {

		return replace(replacementFunction, null, prunePredicate, null, onlyTheFirstOne, ignoreTopExpression, false, listener, registry);
	}
	
	@Override
	public Expression replace(
			Function<Expression, Expression> replacementFunction,
			ReplacementFunctionMaker makeSpecificSubExpressionAndSyntacticContextReplacementFunction,
			PruningPredicate prunePredicate,
			PruningPredicateMaker makeSpecificSubExpressionAndSyntacticContextPrunePredicate,
			boolean onlyTheFirstOne, 
			boolean ignoreTopExpression, 
			boolean replaceOnChildrenBeforeTopExpression, 
			TernaryProcedure<Expression, Expression, Registry> listener,
			Registry registry) {
		
		if (prunePredicate != null && prunePredicate.apply(this, replacementFunction, registry)) {
			return this;
		}
		
		Expression result = this;
		
		if ( ! ignoreTopExpression && ! replaceOnChildrenBeforeTopExpression) { // if replaceOnChildrenBeforeTopExpression, this is done later in the function
			result = applyReplacementFunction(replacementFunction, result, registry);
		}

		if (result == this) { // if no change in top expression, or need to change children first:
			Iterator<ExpressionAndSyntacticContext> subExpressionsAndContextsIterator = getImmediateSubExpressionsAndContextsIterator();
			while (subExpressionsAndContextsIterator.hasNext()) {
				ExpressionAndSyntacticContext subExpressionAndSyntacticContext = subExpressionsAndContextsIterator.next();
				Expression           originalSubExpression   = subExpressionAndSyntacticContext.getExpression();

				if (originalSubExpression == null) {
					// no replacement of null
					continue;
				}

				PruningPredicate prunePredicateForThisSubExpressionAndSyntacticContext = prunePredicate;
				if (makeSpecificSubExpressionAndSyntacticContextPrunePredicate != null) {
					prunePredicateForThisSubExpressionAndSyntacticContext =
							makeSpecificSubExpressionAndSyntacticContextPrunePredicate.apply(
									this, prunePredicate, subExpressionAndSyntacticContext);
					if (prunePredicateForThisSubExpressionAndSyntacticContext == Expressions.TRUE_PRUNING_PREDICATE) {
						continue;
					}
				}

				Function<Expression, Expression> replacementFunctionForThisSubExpressionAndSyntacticContext = replacementFunction;
				if (makeSpecificSubExpressionAndSyntacticContextReplacementFunction != null) {
					replacementFunctionForThisSubExpressionAndSyntacticContext =
							makeSpecificSubExpressionAndSyntacticContextReplacementFunction
							.apply(this, replacementFunction, subExpressionAndSyntacticContext, registry);
				}

				Expression replacementSubExpression =
						originalSubExpression.replace(
								replacementFunctionForThisSubExpressionAndSyntacticContext,
								makeSpecificSubExpressionAndSyntacticContextReplacementFunction,
								prunePredicateForThisSubExpressionAndSyntacticContext,
								makeSpecificSubExpressionAndSyntacticContextPrunePredicate,
								onlyTheFirstOne,
								false /* do not ignore top expression */, replaceOnChildrenBeforeTopExpression, listener, registry);

				if (replacementSubExpression != originalSubExpression) {
					result = result.replace(subExpressionAndSyntacticContext.setExpression(replacementSubExpression));
					if (onlyTheFirstOne) {
						break;
					}
				}
			}
			
			if (! ignoreTopExpression && replaceOnChildrenBeforeTopExpression) {
				result = applyReplacementFunction(replacementFunction, result, registry);
				
			}
		}

		if (listener != null) {
			listener.apply(this, result, registry);
		}
		
		return result;
	}

	private static Expression applyReplacementFunction(Function<Expression, Expression> replacementFunction, Expression expression, Registry registry) {
		Expression result;
		if (replacementFunction instanceof ReplacementFunctionWithContextuallyUpdatedProcess) {
			result = ((ReplacementFunctionWithContextuallyUpdatedProcess) replacementFunction).apply(expression, registry);
		}
		else {
			result = replacementFunction.apply(expression);
		}
		return result;
	}
	
	@Override
	public List<Expression> getImmediateSubExpressions() {
		List<Expression> result = null;
		Iterator<ExpressionAndSyntacticContext> immediateSubExpressionsAndContextsIterator = getImmediateSubExpressionsAndContextsIterator();
	
		Iterator<Expression> resultIterator =
			new FunctionIterator<ExpressionAndSyntacticContext, Expression>(
					immediateSubExpressionsAndContextsIterator,
					ExpressionAndSyntacticContext.GET_EXPRESSION);
	
		result = Util.listFrom(resultIterator);
		return result;
	}

	@Override
	public Expression replace(ExpressionAndSyntacticContext replacementAndContext) {
		SubExpressionAddress address = replacementAndContext.getAddress();
		Expression result = address.replace(this, replacementAndContext.getExpression());
		Expression expressionReplacement = result;
		return expressionReplacement;
	}

	@Override
	public Iterator<Expression> getImmediateSubExpressionsIterator() {
		return
		new FunctionIterator<ExpressionAndSyntacticContext, Expression>(
				getImmediateSubExpressionsAndContextsIterator(),
				ExpressionAndSyntacticContext.GET_EXPRESSION);
	}

	///////////////////////// FUNCTION APPLICATION METHODS //////////////////////

	@Override
	public Expression getFunctorOrSymbol() {
		Expression result = getFunctor();
		if (result == null && getSyntacticFormType().equals(Symbol.SYNTACTIC_FORM_TYPE)) {
			result = this;
		}
		return result;
	}

	@Override
	public abstract Expression getFunctor();

	@Override
	public boolean hasFunctor(Object functor) {
		return Util.notNullAndEquals(getFunctor(), functor);
	}

	@Override
	public int numberOfArguments() {
		List<Expression> arguments = getArguments();
		return arguments.size();
	}

	@Override
	public abstract List<Expression> getArguments();

	@Override
	public Expression get(int index) {
		List<Expression> arguments = getArguments();
		return arguments.get(index);
	}

	////// OBJECT
	
	@Override
	/** Expressions are compared in the same way their respective syntax trees are. */
	public int compareTo(Object anotherObject) {
		
		if (this == anotherObject) {
			return 0;
		}
		
		SyntaxTree anotherSyntaxTree;
		
		if (anotherObject instanceof Expression) {
			anotherSyntaxTree = ((Expression) anotherObject).getSyntaxTree();
		}
		else {
			anotherSyntaxTree = SyntaxTrees.wrap(anotherObject);
		}
		
		int result = getSyntaxTree().compareTo(anotherSyntaxTree);
		return result;
	}
	
	@Override
	public int hashCode() {
		// NOTE: the implementations of SyntaxTree cache their hash codes so no need to do again here.
		int result = getSyntaxTree().hashCode();

		return result;
	}

	@Override
	public boolean equals(Object another) {
		if (this == another) {
			return true;
		}
	
		SyntaxTree anotherSyntaxTree;
		
		if (another instanceof SyntaxTree) {
			anotherSyntaxTree = (SyntaxTree) another;
		}
		else if (another instanceof Expression) {
			anotherSyntaxTree = ((Expression) another).getSyntaxTree();
		}
		else {
			anotherSyntaxTree = SyntaxTrees.makeSyntaxLeaf(another);
		}
		
		boolean result = getSyntaxTree().equals(anotherSyntaxTree);
		return result;
	}

	/////// DEFAULT SYMBOL-SPECIFIC METHODS (ONLY SYMBOLS NEED TO OVERRIDE THESE)
	
	@Override
	public Object getValue() {
		return null;
	}

	@Override
	public boolean isStringLiteral() {
		return false;
	}

	@Override
	public boolean booleanValue() {
		return false;
	}

	@Override
	public int intValue() {
		return 0;
	}

	@Override
	public long longValue() {
		return 0;
	}

	@Override
	public int intValueExact() throws ArithmeticException {
		return 0;
	}

	@Override
	public double doubleValue() {
		return 0;
	}

	@Override
	public Rational rationalValue() {
		// TODO Auto-generated method stub
		return null;
	}

	/**
	 * Computes the value of {@link Object#toString()},
	 * which caches it the first time it is invoked for this object.
	 */
	abstract public String makeToString();
	
	@Override
	public String toString() {
		if (cachedToString == null) {
			cachedToString = makeToString();
		}
		return cachedToString;
	}
}
