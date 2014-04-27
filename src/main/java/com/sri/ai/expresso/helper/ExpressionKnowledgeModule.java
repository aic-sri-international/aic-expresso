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
package com.sri.ai.expresso.helper;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.core.AbstractModuleNoOpRewriter;
import com.sri.ai.grinder.api.Module;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractExpression;
import com.sri.ai.grinder.core.FunctionApplicationProvider;

/**
 * This module concentrates the functionality for registering and using
 * sub-expression providers.
 * 
 * @author braz
 */
@Beta
public class ExpressionKnowledgeModule extends AbstractModuleNoOpRewriter {

	/**
	 * An interface for objects that know how to determine the sub-expressions
	 * of certain types of expressions.
	 * Providers must notify {@link ExpressionKnowledgeModule} of their existence
	 * with the method {@link ExpressionKnowledgeModule#register(Provider)} so it can invoke them.
	 * as necessary.
	 * {@link ExpressionKnowledgeModule#register(Provider, RewritingProcess)} is provided as a convenience for finding the module in the rewriting process.
	 */
	public static interface Provider extends Module.Provider {
		/**
		 * Provides a value for {@link KnowledgeBasedExpression.getSyntacticFormType(RewritingProcess).
		 */
		Object getSyntacticFormType(Expression expression, RewritingProcess process);
		
		/**
		 * Returns an iterator to sub-expressions of the given expression,
		 * if the provider knows about that type of expression, or <code>null</code>.
		 */
		Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(Expression expression, RewritingProcess process);
	}

	/**
	 * Registers a {@link Provider} in the {@link ExpressionKnowledgeModule} module of the given process,
	 * or throw an error if there is not one.
	 */
	public static void register(Provider provider, RewritingProcess process) throws Error {
		register(ExpressionKnowledgeModule.class, provider, process);
	}

	public Object getSyntacticFormType(Expression expression, RewritingProcess process) {
		Object result;
		for (Module.Provider provider : providers.keySet()) {
			Provider expressionKnowledgeModuleProvider = (Provider) provider;
			result = expressionKnowledgeModuleProvider.getSyntacticFormType(expression, process);
			if (result != null) {
				return result;
			}
		}
		// default syntactic form type
		if (expression.getSyntaxTree() instanceof CompoundSyntaxTree) {
			return "Function application";
		}
		return "Symbol";
	}

	public Iterator<ExpressionAndContext> getImmediateSubExpressionsIterator(Expression expression, RewritingProcess process) {
		Iterator<ExpressionAndContext> result;
		for (Module.Provider provider : providers.keySet()) {
			Provider expressionKnowledgeModuleProvider = (Provider) provider;
			result = expressionKnowledgeModuleProvider.getImmediateSubExpressionsAndContextsIterator(expression, process);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Method defining the extraction of knowledge-based sub-expressions from a given expression and process.
	 */
	public static Iterator<ExpressionAndContext> getKnowledgeBasedImmediateSubExpressionsAndContextIteratorAfterBookkeeping(
			Expression expression, RewritingProcess process) {
	
		Iterator<ExpressionAndContext> result = null;
	
		ExpressionKnowledgeModule knowledgeBasedExpressionModule = AbstractExpression.getKnowledgeBasedExpressionModule();
	
		if (knowledgeBasedExpressionModule != null) {
			result = knowledgeBasedExpressionModule.getImmediateSubExpressionsIterator(expression, process);
		}
	
		if (result == null) { // because no provider knows about this expression, we use the default method:
			if (expression.getSyntaxTree() instanceof CompoundSyntaxTree) {
				result = FunctionApplicationProvider.getImmediateSubExpressionsAndContextsIteratorFromFunctionApplication(
						expression, process);
			}
			else {
				List<ExpressionAndContext> emptyList = Collections.emptyList();
				result = emptyList.iterator();
			}
		}
	
		return result;
	}
}
