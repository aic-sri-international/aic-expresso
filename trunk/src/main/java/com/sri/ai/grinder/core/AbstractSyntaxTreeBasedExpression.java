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

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.collect.ImmutableList;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.SyntaxTreeBasedSubExpressionAddress;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.Util;

/**
 * A basic, default implementation of some of the {@link Expression} methods.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractSyntaxTreeBasedExpression extends AbstractExpression {

	private static final long serialVersionUID = 1L;

	protected SyntaxTree syntaxTree;
	
	/**
	 * When arguments to constructor are Expressions, store them, indexing them by their address in the syntax tree.
	 * When a sub-expression is determined with the same address, it gets replaced by the original one.
	 * This is important because code may expect to find sub-expressions being the same instances used for construction.
	 */
	protected Map<SubExpressionAddress, Expression> originalExpressionsByPath = new LinkedHashMap<SubExpressionAddress, Expression>();
	
	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(RewritingProcess process) {
		if (cachedImmediateSubExpressionsAndContexts == null) {
			lazyInitCachedImmediateSubExpressionsAndContextsLock.lock();
			try {
				// Note: Ensure still null when acquire the lock
				if (cachedImmediateSubExpressionsAndContexts == null) { 
					Iterator<? extends ExpressionAndContext> iterator = getImmediateSubExpressionsAndContextsIteratorFromExpressionKnowledgeModule(process);
					
					@SuppressWarnings("unchecked")
					List<ExpressionAndContext> expressionAndContexts = (List<ExpressionAndContext>) Util.listFrom(iterator);
					expressionAndContexts =
							Util.replaceElementsNonDestructively(
									expressionAndContexts,
									new ReplaceExpressionByOriginalOneIndexedByPath());
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

	private class ReplaceExpressionByOriginalOneIndexedByPath implements Function<ExpressionAndContext, ExpressionAndContext> {
		@Override
		public ExpressionAndContext apply(ExpressionAndContext input) {
			ExpressionAndContext result = input;
			SubExpressionAddress path = input.getAddress();
			Expression original = originalExpressionsByPath.get(path);
			if (original != null) {
				result = input.setExpression(original);
			}
			return result;
		}
	};
	
	private Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIteratorFromExpressionKnowledgeModule(RewritingProcess process) {
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
	
	///////////////////////// FUNCTION APPLICATION METHODS //////////////////////

	private static final SubExpressionAddress FUNCTOR_PATH = SyntaxTreeBasedSubExpressionAddress.get(Util.list(-1));

	@Override
	public Expression getFunctor() {
		if (cachedFunctor == null) {
			Expression possibleOriginalFunctorExpression = originalExpressionsByPath.get(FUNCTOR_PATH);
			if (possibleOriginalFunctorExpression != null) {
				cachedFunctor = possibleOriginalFunctorExpression;
			}
			else {
				cachedFunctor = FunctionApplicationProvider.getFunctor(this);
			}
		}
		return cachedFunctor;
	}

	@Override
	public Expression set(int index, Expression newIthArgument) {
		if (getSyntacticFormType().equals("Function application")) {
			Expression oldArgument = get(index);
			if (newIthArgument == oldArgument) {
				return this;
			}
			Object root = getSyntaxTree().getRootTree();
			Object[] subTrees = getSyntaxTree().getImmediateSubTrees().toArray();
			subTrees[index] = newIthArgument;
			Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(root, subTrees);
			return result;
		}
		Util.fatalError("set can only be invoked for Expressions of function application syntactic form, but was invoked for " + this);
		return null;
	}
}
