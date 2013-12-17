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

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.util.Util;

/**
 * An atomic rewriter for conditional expressions.
 * 
 * @author braz
 *
 */
@Beta
public class IfThenElseExternalization extends AbstractRewriter {

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		ExpressionAndContext ifThenElseSubExpressionAndContext = findIfThenElseSubExpressionAndContext(expression, process);
		
		if (ifThenElseSubExpressionAndContext == null) {
			return expression;
		}

		if (IfThenElse.isIfThenElse(expression)) {
			Expression condition = expression.get(0);
			if (ifThenElseSubExpressionAndContext.getExpression() != condition) { // the if then else expression found is in an if then else but is not the condition
				return expression; // externalizing an if then or else branch out of an if would just revert things and probably lead to an infinite loop.
			}
		}

		Expression ifThenElse = ifThenElseSubExpressionAndContext.getExpression();
		Expression condition = ifThenElse.get(0);
		boolean conditionIsSurelyIndependentOnScopedVariables =
			(
					ScopedVariables.scopingVariablesAreDefined(expression, process)
					&&
					(ScopedVariables.isKnownToBeIndependentOfScopeIn(condition, expression, process))
			);
		
		if ( ! conditionIsSurelyIndependentOnScopedVariables) {
			return expression;
		}
		
		Expression thenBranch = ifThenElse.get(1);
		Expression elseBranch = ifThenElse.get(2);
		
		// Create two expressions, one in which the "then branch" replaces the "if then else", and another in which the "else branch" does that.
		Expression newThenBranch = Expressions.replaceAtPath(expression, ifThenElseSubExpressionAndContext.getPath(), thenBranch);
		Expression newElseBranch = Expressions.replaceAtPath(expression, ifThenElseSubExpressionAndContext.getPath(), elseBranch);
		
		Expression result = IfThenElse.make(condition, newThenBranch, newElseBranch);
		return result;
	}
	
	private static ExpressionAndContext findIfThenElseSubExpressionAndContext(Expression expression, RewritingProcess process) {
		ExpressionAndContext result = 
		Util.getFirstSatisfyingPredicateOrNull(
				ExpressionKnowledgeModule.getKnowledgeBasedImmediateSubExpressionsAndContextIteratorAfterBookkeeping(expression, process),
				new IsIfThenElseSubExpression());
		return result;
	}

	private final static class IsIfThenElseSubExpression implements Predicate<ExpressionAndContext> {
		public boolean apply(ExpressionAndContext subExpressionAndContext) {
			Expression expression = subExpressionAndContext.getExpression();
			boolean result = expression != null && IfThenElse.isIfThenElse(expression);
			return result;
		}
	}
}
