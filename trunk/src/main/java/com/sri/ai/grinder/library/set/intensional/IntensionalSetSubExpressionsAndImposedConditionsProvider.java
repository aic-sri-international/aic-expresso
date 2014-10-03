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
package com.sri.ai.grinder.library.set.intensional;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.core.DefaultExpressionAndContext;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.controlflow.ImposedConditionsModule;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;

/**
 * 
 * @author braz
 *
 */
@Beta
public class IntensionalSetSubExpressionsAndImposedConditionsProvider extends AbstractRewriter
implements
NoOpRewriter,
ExpressionKnowledgeModule.Provider,
ImposedConditionsModule.Provider 
{
	//
	// START-ExpressionKnowledgeModule.Provider
	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(Expression expression, final RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			Collection<ExpressionAndContext> subExpressionsAndContexts = new LinkedList<ExpressionAndContext>();

			Expression condition = IntensionalSet.getCondition(expression);
			ExpressionAndContext headAndContext = new DefaultExpressionAndContext(IntensionalSet.getHead(expression), IntensionalSet.getPathToHead(), IntensionalSet.getIndexExpressions(expression), condition);
			subExpressionsAndContexts.add(headAndContext);
			
			if ( ! condition.equals(true)) {
				ExpressionAndContext conditionAndContext =
					new DefaultExpressionAndContext(condition, IntensionalSet.getPathToCondition(), IntensionalSet.getIndexExpressions(expression), Expressions.TRUE);
				subExpressionsAndContexts.add(conditionAndContext);
			}

			Iterator<ExpressionAndContext> subExpressionsAndContextsFromIndexExpressionsIterator =
				getExpressionsAndContextsFromIndexExpressionsIterator(expression);
			
			List<ExpressionAndContext> fromIndexExpressions = Util.listFrom(subExpressionsAndContextsFromIndexExpressionsIterator);

			Iterator<ExpressionAndContext> result = new NestedIterator<ExpressionAndContext>(subExpressionsAndContexts, fromIndexExpressions);

			return result;
		}
		return null;
	}

	public FunctionIterator<Pair<Expression, List<Integer>>, ExpressionAndContext> getExpressionsAndContextsFromIndexExpressionsIterator(Expression expression) {
		// SUB_EXPRESSION_ADDRESS
		return new FunctionIterator<Pair<Expression, List<Integer>>, ExpressionAndContext>(
				IntensionalSet.getSubExpressionsAndPathsFromIndexExpressionsIterator(expression),
				new DefaultExpressionAndContext.MakerFromExpressionAndPathPair(new ArrayList<Expression>()));
	}

	@Override
	public Object getSyntacticFormType(Expression expression, RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			return "Intensional set";
		}
		return null;
	}
	// END-ExpressionKnowledgeModule.Provider
	//
	
	//
	// START-ImposedConditionsModule.Provider 
	@Override
	public List<Pair<Expression, SubExpressionAddress>> getConditionsExpressionImposesOnSubExpressions(Expression expression, RewritingProcess process) {
		List<Pair<Expression, SubExpressionAddress>> result = null;
		
		if (knowledgeApplies(expression)) {
			result = new ArrayList<Pair<Expression, SubExpressionAddress>>();
			result.add(Pair.make(IntensionalSet.getCondition(expression), IntensionalSet.getPathToHead()));
		}
		
		return result;
	}
	// END-ImposedConditionsModule.Provider 
	//

	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
		ExpressionKnowledgeModule.register(this, process);
		ImposedConditionsModule.register(this, process);
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression; // will be removed eventually, not a real rewriter, just a module.
	}
	
	//
	// PRIVATE METHODS
	//
	// the methods below seem to be pretty much boilerplate, much shared with, say, BracketedExpression.
	// We should abstract this.
	private boolean knowledgeApplies(Expression expression) {
		return expression != null && IntensionalSet.isIntensionalSet(expression);
	}
}
