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
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.collect.ImmutableList;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
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
	protected static final long serialVersionUID = 3L; // Note: Increment this when you want to ensure any parsing caches are invalidated 
	
	protected static Cache<Thread, Function<Expression, String>> threadToString = newThreadToStringCache();
	protected String                             cachedToString                      = null;
	protected int                                cachedHashCode                      = -1;
	protected volatile Object                    cachedSyntacticFormType             = null;
	protected Lock                               lazyInitCachedSyntacticFormTypeLock = new ReentrantLock();
	protected volatile ImmutableList<Expression> cachedArguments                     = null;
	protected Lock                               lazyInitCachedArgumentsLock         = new ReentrantLock();
	protected Expression                         cachedFunctor                       = null;
	
	protected volatile transient ImmutableList<ExpressionAndContext> cachedImmediateSubExpressionsAndContexts;
	protected Lock               lazyInitCachedImmediateSubExpressionsAndContextsLock = new ReentrantLock();
	
	@Override
	public Expression replaceFirstOccurrence(Expression replaced, Expression replacement, RewritingProcess process) {
		return replaceFirstOccurrence(new ReplaceByIfEqualTo<Expression>(replacement, replaced), null, null, process);
	}

	@Override
	public Expression replaceAllOccurrences(Expression replaced, Expression replacement, RewritingProcess process) {
		return replaceAllOccurrences(new ReplaceByIfEqualTo<Expression>(replacement, replaced), null, null, process);
	}

	@Override
	public Expression replaceFirstOccurrence(Expression replaced, Expression replacement, PruningPredicate prunePredicate, RewritingProcess process) {
		return replaceFirstOccurrence(new ReplaceByIfEqualTo<Expression>(replacement, replaced), prunePredicate, process);
	}

	@Override
	public Expression replaceAllOccurrences(Expression replaced, Expression replacement, PruningPredicate prunePredicate, RewritingProcess process) {
		return replaceAllOccurrences(new ReplaceByIfEqualTo<Expression>(replacement, replaced), prunePredicate, process);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, RewritingProcess process) {
		return replaceFirstOccurrence(replacementFunction, null, null, process);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, RewritingProcess process) {
		return replaceAllOccurrences(replacementFunction, null, null, process);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, RewritingProcess process) {
		return replaceFirstOccurrence(replacementFunction, prunePredicate, null, process);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, RewritingProcess process) {
		return replaceAllOccurrences(replacementFunction, prunePredicate, null, process);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, ReplacementFunctionMaker makeSpecificSubExpressionAndContextReplacementFunction, PruningPredicate prunePredicate, PruningPredicateMaker makeSpecificSubExpressionAndContextPrunePredicate, RewritingProcess process) {
		return replace(
				replacementFunction, makeSpecificSubExpressionAndContextReplacementFunction,
				prunePredicate, makeSpecificSubExpressionAndContextPrunePredicate,
				true, false, false, null, process);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, ReplacementFunctionMaker makeSpecificSubExpressionAndContextReplacementFunction, PruningPredicate prunePredicate, PruningPredicateMaker makeSpecificSubExpressionAndContextPrunePredicate, RewritingProcess process) {
		return replace(
				replacementFunction, makeSpecificSubExpressionAndContextReplacementFunction,
				prunePredicate, makeSpecificSubExpressionAndContextPrunePredicate,
				false, false, false, null, process);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {
		return replaceFirstOccurrence(replacementFunction, null, listener, process);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {
		return replaceAllOccurrences(replacementFunction, null, listener, process);
	}

	@Override
	public Expression replaceFirstOccurrence(Expression replaced, Expression replacement, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {
		return replaceFirstOccurrence(new ReplaceByIfEqualTo<Expression>(replacement, replaced), null, listener, process);
	}

	@Override
	public Expression replaceAllOccurrences(Expression replaced, Expression replacement, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {
		return replaceAllOccurrences(new ReplaceByIfEqualTo<Expression>(replacement, replaced), null, listener, process);
	}

	@Override
	public Expression replaceFirstOccurrence(Expression replaced, Expression replacement, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {
		return replaceFirstOccurrence(new ReplaceByIfEqualTo<Expression>(replacement, replaced), prunePredicate, listener, process);
	}

	@Override
	public Expression replaceAllOccurrences(Expression replaced, Expression replacement, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {
		return replaceAllOccurrences(new ReplaceByIfEqualTo<Expression>(replacement, replaced), prunePredicate, listener, process);
	}

	@Override
	public Expression replaceFirstOccurrence(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {
		return replace(replacementFunction, true /* only the first one */, prunePredicate, false, listener, process);
	}

	@Override
	public Expression replaceAllOccurrences(Function<Expression, Expression> replacementFunction, PruningPredicate prunePredicate, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {
		return replace(replacementFunction, false /* not only the first one */, prunePredicate, false, listener, process);
	}

	@Override
	public Expression replace(
			Function<Expression, Expression> replacementFunction, boolean onlyTheFirstOne,
			PruningPredicate prunePredicate, boolean ignoreTopExpression, TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {

		return replace(replacementFunction, null, prunePredicate, null, onlyTheFirstOne, ignoreTopExpression, false, listener, process);
	}
	
	@Override
	public Expression replace(
			Function<Expression, Expression> replacementFunction,
			ReplacementFunctionMaker makeSpecificSubExpressionAndContextReplacementFunction,
			PruningPredicate prunePredicate,
			PruningPredicateMaker makeSpecificSubExpressionAndContextPrunePredicate,
			boolean onlyTheFirstOne, 
			boolean ignoreTopExpression, 
			boolean replaceOnChildrenBeforeTopExpression, 
			TernaryProcedure<Expression, Expression, RewritingProcess> listener, RewritingProcess process) {
		
		if (prunePredicate != null && prunePredicate.apply(this, replacementFunction, process)) {
			return this;
		}
		
		Expression result = this;
		
		if ( ! ignoreTopExpression && ! replaceOnChildrenBeforeTopExpression) { // if replaceOnChildrenBeforeTopExpression, this is done later in the function
			result = applyReplacementFunction(replacementFunction, result, process);
		}

		if (result == this) { // if no change in top expression, or need to change children first:
			Iterator<ExpressionAndContext> subExpressionsAndContextsIterator = getImmediateSubExpressionsAndContextsIterator();
			while (subExpressionsAndContextsIterator.hasNext()) {
				ExpressionAndContext subExpressionAndContext = subExpressionsAndContextsIterator.next();
				Expression           originalSubExpression   = subExpressionAndContext.getExpression();

				if (originalSubExpression == null) {
					// no replacement of null
					continue;
				}

				PruningPredicate prunePredicateForThisSubExpressionAndContext = prunePredicate;
				if (makeSpecificSubExpressionAndContextPrunePredicate != null) {
					prunePredicateForThisSubExpressionAndContext =
							makeSpecificSubExpressionAndContextPrunePredicate.apply(
									this, prunePredicate, subExpressionAndContext);
					if (prunePredicateForThisSubExpressionAndContext == Expressions.TRUE_PRUNING_PREDICATE) {
						continue;
					}
				}

				Function<Expression, Expression> replacementFunctionForThisSubExpressionAndContext = replacementFunction;
				if (makeSpecificSubExpressionAndContextReplacementFunction != null) {
					replacementFunctionForThisSubExpressionAndContext =
							makeSpecificSubExpressionAndContextReplacementFunction
							.apply(this, replacementFunction, subExpressionAndContext, process);
				}

				RewritingProcess subProcess;
				if (replacementFunction instanceof ReplacementFunctionWithContextuallyUpdatedProcess) {
					subProcess = GrinderUtil.extendContextualSymbolsAndConstraint(subExpressionAndContext, process);
				}
				else {
					subProcess = process;
				}

				Expression replacementSubExpression =
						originalSubExpression.replace(
								replacementFunctionForThisSubExpressionAndContext,
								makeSpecificSubExpressionAndContextReplacementFunction,
								prunePredicateForThisSubExpressionAndContext,
								makeSpecificSubExpressionAndContextPrunePredicate,
								onlyTheFirstOne,
								false /* do not ignore top expression */, replaceOnChildrenBeforeTopExpression, listener, subProcess);

				if (replacementSubExpression != originalSubExpression) {
					result = result.replace(subExpressionAndContext.setExpression(replacementSubExpression));
					if (onlyTheFirstOne) {
						break;
					}
				}
			}
			
			if (! ignoreTopExpression && replaceOnChildrenBeforeTopExpression) {
				result = applyReplacementFunction(replacementFunction, result, process);
				
			}
		}

		if (listener != null) {
			listener.apply(this, result, process);
		}
		
		return result;
	}

	private static Expression applyReplacementFunction(Function<Expression, Expression> replacementFunction, Expression expression, RewritingProcess process) {
		Expression result;
		if (replacementFunction instanceof ReplacementFunctionWithContextuallyUpdatedProcess) {
			result = ((ReplacementFunctionWithContextuallyUpdatedProcess) replacementFunction).apply(expression, process);
		}
		else {
			result = replacementFunction.apply(expression);
		}
		return result;
	}
	
	@Override
	public List<Expression> getSubExpressions() {
		List<Expression> result = null;
		Iterator<ExpressionAndContext> immediateSubExpressionsAndContextsIterator = getImmediateSubExpressionsAndContextsIterator();
	
		Iterator<Expression> resultIterator =
			new FunctionIterator<ExpressionAndContext, Expression>(
					immediateSubExpressionsAndContextsIterator,
					ExpressionAndContext.GET_EXPRESSION);
	
		result = Util.listFrom(resultIterator);
		return result;
	}

	@Override
	public Expression replace(ExpressionAndContext replacementAndContext) {
		SubExpressionAddress address = replacementAndContext.getAddress();
		Expression result = address.replace(this, replacementAndContext.getExpression());
		Expression expressionReplacement = result;
		return expressionReplacement;
	}

	@Override
	public Iterator<Expression> getImmediateSubExpressionsIterator() {
		return
		new FunctionIterator<ExpressionAndContext, Expression>(
				getImmediateSubExpressionsAndContextsIterator(),
				ExpressionAndContext.GET_EXPRESSION);
	}


	private static Cache<Thread, Function<Expression, String>> newThreadToStringCache() {
		Cache<Thread, Function<Expression, String>> result = CacheBuilder.newBuilder()
				.expireAfterAccess(ExpressoConfiguration.getSyntaxToStringThreadCacheTimeoutInSeconds(), TimeUnit.SECONDS)
				// Don't hold onto old threads unnecessarily
				.weakKeys()
				.build();
		
		return result;
	}

	///////////////////////// FUNCTION APPLICATION METHODS //////////////////////

	@Override
	public Expression getFunctorOrSymbol() {
		Expression result = getFunctor();
		if (result == null && getSyntacticFormType().equals("Symbol")) {
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
		if (cachedHashCode == -1) {
			cachedHashCode = getSyntaxTree().hashCode();
		}
		
		return cachedHashCode;
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
	abstract public Expression clone();

	@Override
	public Object getValue() {
		return null;
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
