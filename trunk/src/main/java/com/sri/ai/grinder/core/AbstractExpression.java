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

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.collect.ImmutableList;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.SyntaxTreeToStringFunction;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IsInstanceOf;
import com.sri.ai.util.base.ReplaceByIfEqualTo;
import com.sri.ai.util.base.TernaryProcedure;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A basic, default implementation of some of the {@link Expression} methods.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractExpression implements Expression {
	
	//
	private static final long serialVersionUID = 3L; // Note: Increment this when you want to ensure any parsing caches are invalidated 
	
	protected SyntaxTree syntaxTree;
	
	protected static final IsInstanceOf IS_EXPRESSION_MODULE = new IsInstanceOf<Object>(ExpressionKnowledgeModule.class);
	//
	private volatile transient ImmutableList<ExpressionAndContext> cachedImmediateSubExpressionsAndContexts;
	private Lock               lazyInitCachedImmediateSubExpressionsAndContextsLock = new ReentrantLock();
	
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
			Iterator<ExpressionAndContext> subExpressionsAndContextsIterator = getImmediateSubExpressionsAndContextsIterator(process);
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

				RewritingProcess subProcess = GrinderUtil.extendContextualVariablesAndConstraint(subExpressionAndContext, process);

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
		// System.out.println("Replacement function on " + expression + " ----> " + result);
		return result;
	}
	
	/**
	 * When arguments to constructor are Expressions, store them, indexing them by their path in the syntax tree.
	 * When a sub-expression is determined with the same path, it gets replaced by the original one.
	 * This is important because the original expression may be an instance of an Expression extension,
	 * whereas the normal sub-expression mechanism always produces DefaultCompoundSyntaxTree or DefaultSymbols.
	 */
	protected Map<List<Integer>, Expression> originalExpressionsByPath =
			new LinkedHashMap<List<Integer>, Expression>();
	
	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(RewritingProcess process) {
		if (cachedImmediateSubExpressionsAndContexts == null) {
			lazyInitCachedImmediateSubExpressionsAndContextsLock.lock();
			try {
				// Note: Ensure still null when acquire the lock
				if (cachedImmediateSubExpressionsAndContexts == null) { 
					Iterator<? extends ExpressionAndContext> iterator = getImmediateSubExpressionsAndContextsIteratorAfterBookkeeping(process);
					
					@SuppressWarnings("unchecked")
					List<ExpressionAndContext> expressionAndContexts = (List<ExpressionAndContext>) Util.listFrom(iterator);
					expressionAndContexts =
							Util.replaceElementsNonDestructively(
									expressionAndContexts,
									new ReplaceExpressionByOriginalOneIndexedByPathInGivenMap());
					// the above is a bit of a hack to ensure Expressions provided at construction are re-used as sub-expressions.
					
					// Ensure they cannot be mutated by accident.
					cachedImmediateSubExpressionsAndContexts = ImmutableList.copyOf(expressionAndContexts);
				}
			} finally {
				lazyInitCachedImmediateSubExpressionsAndContextsLock.unlock();
			}
		}
		return cachedImmediateSubExpressionsAndContexts.iterator();
	}

	
	private class ReplaceExpressionByOriginalOneIndexedByPathInGivenMap implements Function<ExpressionAndContext, ExpressionAndContext> {
		@Override
		public ExpressionAndContext apply(ExpressionAndContext input) {
			ExpressionAndContext result = input;
			List<Integer> path = input.getPath();
			Expression original = originalExpressionsByPath.get(path);
			if (original != null) {
				result = input.setExpression(original);
			}
			return result;
		}
	};
	
	private Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIteratorAfterBookkeeping(RewritingProcess process) {
		Iterator<ExpressionAndContext> result = null;
		
		ExpressionKnowledgeModule knowledgeBasedExpressionModule = Expressions.getKnowledgeBasedExpressionModule();
		
		if (knowledgeBasedExpressionModule != null) {
			result = knowledgeBasedExpressionModule.getImmediateSubExpressionsIterator(this, process);
		}
		
		if (result == null) { // because no provider knows about this expression, we use the default method:
			if (this.getSyntaxTree() instanceof CompoundSyntaxTree) {
				result = FunctionApplicationProvider.getImmediateSubExpressionsAndContextsIteratorFromFunctionApplication(
						this, process);
			}
			else {
				List<ExpressionAndContext> emptyList = Collections.emptyList();
				result = emptyList.iterator();
			}
		}
		
		return result;
	}

	public static final Function<Object, SyntaxTree> wrapper = new Function<Object, SyntaxTree>() {
		@Override
		public SyntaxTree apply(Object object) {
			return SyntaxTrees.wrap(object);
		}
	};

	private static Cache<Thread, Function<Expression, String>> threadToString = newThreadToStringCache();
	//
	private String                             cachedToString                      = null;
	private volatile Object                    cachedSyntacticFormType             = null;
	private Lock                               lazyInitCachedSyntacticFormTypeLock = new ReentrantLock();
	private volatile ImmutableList<Expression> cachedArguments                     = null;
	private Lock                               lazyInitCachedArgumentsLock         = new ReentrantLock();

	public static Map<SyntaxTree, SyntaxTree> wrapAsMap(Object... pairs) {
		return Util.map(Expressions.wrap(pairs).toArray());
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

	abstract public Expression clone();
	
	/**
	 * Indicates what syntactic form type the expression is.
	 * Syntactic forms are the primitive types of expressions in a logic.
	 * For example, in FOL we have the forms: term, predicate, simple formula, quantified formula, etc.
	 * HOL typically has function applications and lambda expressions as its basic syntactic forms.
	 * Many types can be introduced: intensional and extensional sets, for example, and even application-specific types.
	 */
	@Override
	public Object getSyntacticFormType() {
		if (cachedSyntacticFormType == null) {
			lazyInitCachedSyntacticFormTypeLock.lock();
			try {
				// Note: Ensure still null when acquire the lock
				if (cachedSyntacticFormType == null) {
					ExpressionKnowledgeModule knowledgeBasedExpressionModule = Expressions.getKnowledgeBasedExpressionModule();
		
					if (knowledgeBasedExpressionModule != null) {
						cachedSyntacticFormType = knowledgeBasedExpressionModule.getSyntacticFormType(this, Expressions.getProcess());
					}
		
					if (cachedSyntacticFormType == null) { // no one knows about this expression, we use the default.
						if (getSyntaxTree() instanceof CompoundSyntaxTree) {
							cachedSyntacticFormType = "Function application";
						}
						else if (getSyntaxTree() instanceof Symbol) {
							cachedSyntacticFormType = "Symbol";
						}
					}
				}
			} finally {
				lazyInitCachedSyntacticFormTypeLock.unlock();
			}
		}
		
		return cachedSyntacticFormType;
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return syntaxTree;
	}

	@Override
	public Expression replace(ExpressionAndContext replacementAndContext) {
		List<Integer> path = replacementAndContext.getPath();
		Expression expressionReplacement = Expressions.replaceAtPath(this, path, replacementAndContext.getExpression());
		return expressionReplacement;
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator() {
		return getImmediateSubExpressionsAndContextsIterator(Expressions.getProcess());
	}

	@Override
	public Iterator<Expression> getImmediateSubExpressionsIterator() {
		return
		new FunctionIterator<ExpressionAndContext, Expression>(
				getImmediateSubExpressionsAndContextsIterator(),
				ExpressionAndContext.GET_EXPRESSION);
	}

	///////////////////////// FUNCTION APPLICATION METHODS //////////////////////

	@Override
	public Expression getFunctor() {
		return FunctionApplicationProvider.getFunctor(this);
	}

	@Override
	public Expression getFunctorOrSymbol() {
		return FunctionApplicationProvider.getFunctorOrSymbol(this);
	}

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
	public List<Expression> getArguments() {
		if (cachedArguments == null) {
			lazyInitCachedArgumentsLock.lock();
			try {
				// Note: Ensure still null when acquire the lock
				if (cachedArguments == null) {
					if (getSyntacticFormType().equals("Symbol")) {
						return Collections.emptyList();
					}
		
					Iterator<ExpressionAndContext> immediateSubExpressionsAndContextsIterator = getImmediateSubExpressionsAndContextsIterator();

					Iterator<Expression> resultIterator =
						new FunctionIterator<ExpressionAndContext, Expression>(
								immediateSubExpressionsAndContextsIterator,
								ExpressionAndContext.GET_EXPRESSION);
		
					// eventually we want this condition to be moved out right below the Symbol test
					if (getSyntacticFormType().equals("Function application")) {
						resultIterator = Util.removeFirst(resultIterator); // functor does not count as an argument for function applications
					}
					
					// Ensure they cannot be mutated by accident.
					cachedArguments = ImmutableList.copyOf(resultIterator);
				}
			} finally {
				lazyInitCachedArgumentsLock.unlock();
			}
		}

		return cachedArguments;
	}

	@Override
	public Expression get(int index) {
		List<Expression> arguments = getArguments();
		return arguments.get(index);
	}

	@Override
	public Expression set(int index, Expression newIthArgument) {
		if (getSyntacticFormType().equals("Function application")) {
			Expression oldArgument = get(index);
			if (newIthArgument == oldArgument) {
				return this;
			}
			// Used to be the following line, but that creates a symbol around newIthArgument.
			// Expression result = Expressions.makeFromSyntaxTree(getSyntaxTree().setImmediateSubTree(index, newIthArgument));
			Object root = getSyntaxTree().getRootTree();
			Object[] subTrees = getSyntaxTree().getImmediateSubTrees().toArray();
			subTrees[index] = newIthArgument;
			Expression result = Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(root, subTrees);
			return result;
		}
		Util.fatalError("set can only be invoked for Expressions of function application syntactic form, but was invoked for " + this);
		return null;
	}
	
	@Override
	public String toString() {
		if (cachedToString == null) {
			Function<Expression, String> toString = getToString();
			if (toString != null) {
				cachedToString = toString.apply(this);
			}
			else 
			{
				cachedToString = defaultToString();
			}
		}
		return cachedToString;
	}
	
	private static Cache<Thread, Function<Expression, String>> newThreadToStringCache() {
		Cache<Thread, Function<Expression, String>> result = CacheBuilder.newBuilder()
				.expireAfterAccess(ExpressoConfiguration.getSyntaxToStringThreadCacheTimeoutInSeconds(), TimeUnit.SECONDS)
				// Don't hold onto old threads unnecessarily
				.weakKeys()
				.build();
		
		return result;
	}
	
	/**
	 * 
	 * @return the default configured to string unary function for the current
	 *         thread. The instance to use is determined by the configuration
	 *         settings for
	 *         {@link ExpressoConfiguration#KEY_DEFAULT_SYNTAX_TO_STRING_UNARY_FUNCTION_CLASS}
	 *         .
	 */
	private static Function<Expression, String> getToString() {
		Function<Expression, String> result = threadToString.getIfPresent(Thread.currentThread());
		if (result == null) {
			// Initialize with the defaultToString() caller, as other methods can
			// rely on Grammar instances that can cause recursive calls, this
			// prevents such recursion from occurring.
			threadToString.put(Thread.currentThread(), new SyntaxTreeToStringFunction());
			
			result = ExpressoConfiguration.newConfiguredInstance(ExpressoConfiguration.getDefaultSyntaxToStringUnaryFunctionClass());

			// Now assign the configured object.
			threadToString.put(Thread.currentThread(), result);
		}
		
		return result;
	}
}
