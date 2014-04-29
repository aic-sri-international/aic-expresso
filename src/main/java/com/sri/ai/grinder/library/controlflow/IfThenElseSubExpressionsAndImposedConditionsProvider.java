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
package com.sri.ai.grinder.library.controlflow;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultExpressionAndContext;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.util.base.Pair;

/**
 * An {@link ExpressionKnowledgeModule.Provider} and
 * {@link ImposedConditionsModule.Provider} provider for conditional
 * expressions.
 * 
 * @author braz
 * 
 */
@Beta
public class IfThenElseSubExpressionsAndImposedConditionsProvider extends AbstractRewriter
implements
NoOpRewriter,
ExpressionKnowledgeModule.Provider,
ImposedConditionsModule.Provider
{
	//
	// START-ExpressionKnowledgeModule.Provider
	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(Expression expression, final RewritingProcess process) {
		Iterator<ExpressionAndContext> result = null;
		if (knowledgeApplies(expression)) {
			Iterator<SyntaxTree> syntaxTreeIterator = expression.getSyntaxTree().getImmediateSubTreesIncludingRootOneIterator();
			Expression functor    = Expressions.makeFromSyntaxTree(syntaxTreeIterator.next());
			Expression condition  = Expressions.makeFromSyntaxTree(syntaxTreeIterator.next());
			Expression thenBranch = Expressions.makeFromSyntaxTree(syntaxTreeIterator.next());
			Expression elseBranch = Expressions.makeFromSyntaxTree(syntaxTreeIterator.next());
			List<ExpressionAndContext> expressionAndContexts = new ArrayList<ExpressionAndContext>();
			Expression thenCondition = condition;
			Expression elseCondition = Not.make(thenCondition);
			
			List<Expression> emptyList = Collections.emptyList();
			expressionAndContexts.add(new DefaultExpressionAndContext(functor, 
					IfThenElse.getPathToFunctor()));
			expressionAndContexts.add(new DefaultExpressionAndContext(condition, 
					IfThenElse.getPathToCondition()));
			expressionAndContexts.add(new DefaultExpressionAndContext(thenBranch, 
					IfThenElse.getPathToThen(), emptyList, thenCondition));
			expressionAndContexts.add(new DefaultExpressionAndContext(elseBranch, 
					IfThenElse.getPathToElse(), emptyList, elseCondition));
			
			result = expressionAndContexts.iterator();
		}
		return result;
	}

	@Override
	public Object getSyntacticFormType(Expression expression, RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			return "Function application";
		}
		return null;
	}
	
	// END-ExpressionKnowledgeModule.Provider
	//
	
	//
	// START-ImposedConditionsModule.Provider 
	@Override
	public List<Pair<Expression, List<Integer>>> getConditionsExpressionImposesOnSubExpressions(Expression expression, RewritingProcess process) {
		List<Pair<Expression, List<Integer>>> result = null;
		
		if (IfThenElse.isIfThenElse(expression)) {
			result = new ArrayList<Pair<Expression, List<Integer>>>();
			result.add(new Pair<Expression, List<Integer>>(IfThenElse.getCondition(expression), 
					IfThenElse.getPathToThen()));
			result.add(new Pair<Expression, List<Integer>>(Not.make(IfThenElse.getCondition(expression)), 
					IfThenElse.getPathToElse()));
		}
		
		return result;
	}
	// END-ImposedConditionsModule.Provider 
	//
	
	// 
	// START-Rewriter
	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
		ExpressionKnowledgeModule.register(this, process);
		ImposedConditionsModule.register(this, process);
	}
	
	// END-Rewriter
	//

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression; // will be removed eventually, not a real rewriter, just a module.
	}
	
	//
	// PRIVATE METHODS
	//
	private boolean knowledgeApplies(Expression expression) {
		return expression != null && IfThenElse.isIfThenElse(expression);
	}
}
