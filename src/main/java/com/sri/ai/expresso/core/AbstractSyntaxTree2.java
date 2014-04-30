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
package com.sri.ai.expresso.core;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.collect.ImmutableList;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractExpression2;
import com.sri.ai.grinder.core.FunctionApplicationProvider;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryProcedure;
import com.sri.ai.util.base.ReplaceByIfEqualTo;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A basic, default implementation of some SyntaxTree and helper methods.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractSyntaxTree2 extends AbstractExpression2 implements SyntaxTree {
	
	public static final Function<Object, SyntaxTree> wrapper = new Function<Object, SyntaxTree>() {
		@Override
		public SyntaxTree apply(Object object) {
			return SyntaxTrees.wrap(object);
		}
	};

	/**
	 * An arbitrary Java object associated with tree node.
	 * How it is used is up to users; a typical use is representing the name or value of a symbol, in the case of leaf nodes,
	 * and a type of expression in the case of non-leaf nodes.
	 */
	protected Object valueOrRootSyntaxTree;
	// Note: Should only be assigned immutable lists.
	protected List<SyntaxTree> subTrees = Collections.emptyList();
	//
	//
	private String                             cachedToString                      = null;

	public static Map<SyntaxTree, SyntaxTree> wrapAsMap(Object... pairs) {
		return Util.map(Expressions.wrap(pairs).toArray());
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), null, null);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), null, null);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), prunePredicate);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), prunePredicate);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction) {
		return replaceSubTreesFirstOccurrence(replacementFunction, null, null);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction) {
		return replaceSubTreesAllOccurrences(replacementFunction, null, null);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate) {
		return replaceSubTreesFirstOccurrence(replacementFunction, prunePredicate, null);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate) {
		return replaceSubTreesAllOccurrences(replacementFunction, prunePredicate, null);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesFirstOccurrence(replacementFunction, null, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesAllOccurrences(replacementFunction, null, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), null, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), null, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), prunePredicate, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), prunePredicate, listener);
	}

	abstract public SyntaxTree clone();
	
	@Override
	public int numberOfImmediateSubTrees() {
		return subTrees.size();
	}

	@Override
	public SyntaxTree getSubTree(Object fieldKey) {
		if (fieldKey.equals("functor")) {
			return getRootTree();
		}
		else if (fieldKey instanceof Number) {
			int index = ((Number)fieldKey).intValue();
			if (index == -1) {
				return (SyntaxTree) valueOrRootSyntaxTree;
			}
			if (index < subTrees.size()) {
				return subTrees.get(index);
			}
			else {
				return null;
			}
		}
		return null;
	}

	@Override
	public List<SyntaxTree> getImmediateSubTrees() {
		return subTrees;
	}

	@Override
	public Iterator<SyntaxTree> getImmediateSubTreesIterator() {
		return getImmediateSubTrees().iterator();
	}

	@Override
	public String toString() {
		if (cachedToString == null) {
//			Function<Expression, String> toString = getToString();
//			if (toString != null) {
//				cachedToString = toString.apply(this);
//			}
//			else 
//			{
				cachedToString = defaultToString();
//			}
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

	private static final long serialVersionUID = 1L;
	private volatile Object                    cachedSyntacticFormType             = null;
	private Lock                               lazyInitCachedSyntacticFormTypeLock = new ReentrantLock();
	private volatile ImmutableList<Expression> cachedArguments                     = null;
	private Lock                               lazyInitCachedArgumentsLock         = new ReentrantLock();
	private static Cache<Thread, Function<Expression, String>> threadToString = newThreadToStringCache();

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
					ExpressionKnowledgeModule knowledgeBasedExpressionModule = getKnowledgeBasedExpressionModule();
		
					if (knowledgeBasedExpressionModule != null) {
						cachedSyntacticFormType = knowledgeBasedExpressionModule.getSyntacticFormType(this, getProcess());
					}
		
					if (cachedSyntacticFormType == null) { // no one knows about this expression, we use the default.
						if (this instanceof CompoundSyntaxTree) {
							cachedSyntacticFormType = "Function application";
						}
						else if (this instanceof Symbol) {
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
		return this;
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIteratorAfterBookkeeping(
			RewritingProcess process) {
		return ExpressionKnowledgeModule.getKnowledgeBasedImmediateSubExpressionsAndContextIteratorAfterBookkeeping(this, process);
	}

	@Override
	public Expression replace(ExpressionAndContext replacementAndContext) {
		List<Integer> path = replacementAndContext.getPath();
		Expression expressionReplacement = Expressions.replaceAtPath(this, path, replacementAndContext.getExpression());
		return expressionReplacement;
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator() {
		return getImmediateSubExpressionsAndContextsIterator(getProcess());
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
			Expression result = Expressions.makeFromSyntaxTree(getSyntaxTree().setImmediateSubTree(index, newIthArgument));
			return result;
		}
		Util.fatalError("set can only be invoked for Expressions of function application syntactic form, but was invoked for " + this);
		return null;
	}
	
}
