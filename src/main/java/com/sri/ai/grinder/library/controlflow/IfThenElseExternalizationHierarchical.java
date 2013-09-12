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
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.RewriterFunction;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.util.Util;

/**
 * Receives a basic expression (basic operators plus products of intensional sets with basic expressions in the head)
 * and returns an equivalent expression in which all conditional expressions are on the top of the expression,
 * but for the ones constrained to be inside sets because they use their indices.
 */
@Beta
public class IfThenElseExternalizationHierarchical extends AbstractHierarchicalRewriter {

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		return normalize(expression, false, process);
	}

	private Expression normalize(Expression expression, boolean subExpressionsAreNormalized, RewritingProcess process) {

		// Make sure sub-expressions are normalized (externalized) first.
		if ( ! subExpressionsAreNormalized) {
			expression = expression.replace(new RewriterFunction(this, process), false /* all occurrences */, null, true /* ignore top expression, expression */, null, process);
		}

		ExpressionAndContext conditionalSubExpressionAndContext = findConditionalSubExpressionAndContext(expression, process);
		if (conditionalSubExpressionAndContext != null) {
			if (IfThenElse.isIfThenElse(expression) && conditionalSubExpressionAndContext.getExpression() != IfThenElse.getCondition(expression)) {
				// the conditional sub-expression is not the condition of 'expression'
				// therefore expression is already normalized; this is the recursion base case.
			}
			else { // expression is not a conditional expression itself
				Expression conditionalSubExpression = conditionalSubExpressionAndContext.getExpression();
				boolean noScopingRestrictions = decideWhetherThereAreNoScopingRestrictions(expression, conditionalSubExpression, process);
				if (noScopingRestrictions) {
					Expression condition  = IfThenElse.getCondition(conditionalSubExpression);
					Expression thenBranch = IfThenElse.getThenBranch(conditionalSubExpression);
					Expression elseBranch = IfThenElse.getElseBranch(conditionalSubExpression);

					// Create two expressions, one in which the "then branch" replaces the conditional sub-expression, and another in which the "else branch" does that.
					Expression newThenBranch = Expressions.replaceAtPath(expression, conditionalSubExpressionAndContext.getPath(), thenBranch);
					Expression newElseBranch = Expressions.replaceAtPath(expression, conditionalSubExpressionAndContext.getPath(), elseBranch);

					// Make sure the *new* subexpressions are normalized themselves, even though the original ones already were.
					// If they are not normalized, the unnormalized part must be on their top expression only,
					// since their sub-expressions were the original expression's sub-expressions, which we know to be normalized.
					// For example, suppose expression was f(if C1 then A1 else B1, if C2 then A2 else B2).
					// At this point, we have if C1 then f(A1, if C2 then A2 else B2) else f(B1, if C2 then A2 else B2)
					// f(A1, if C2 then A2 else B2) and f(B1, if C2 then A2 else B2) are not normalized, but their sub-expressions are (they are some of expression's sub-expressions).
					// So we make a recursive call on these new then and else branches, with the information that their own sub-expressions are normalized. 
					newThenBranch = normalize(newThenBranch, true, process);
					newElseBranch = normalize(newElseBranch, true, process);
					expression = normalize(IfThenElse.make(condition, newThenBranch, newElseBranch), true, process);
				}
			}
		}
		return expression;
	}

	private static ExpressionAndContext findConditionalSubExpressionAndContext(Expression expression, RewritingProcess process) {
		ExpressionAndContext result = 
		Util.getFirstSatisfyingPredicateOrNull(
				ExpressionKnowledgeModule.getKnowledgeBasedImmediateSubExpressionsAndContextIteratorAfterBookkeeping(expression, process),
				new IsConditionalSubExpression());
		return result;
	}

	private final static class IsConditionalSubExpression implements Predicate<ExpressionAndContext> {
		public boolean apply(ExpressionAndContext subExpressionAndContext) {
			Expression expression = subExpressionAndContext.getExpression();
			boolean result = expression != null && IfThenElse.isIfThenElse(expression);
			return result;
		}
	}

	private boolean decideWhetherThereAreNoScopingRestrictions(Expression expression, Expression conditionalSubExpression, RewritingProcess process) {
		Expression condition  = IfThenElse.getCondition(conditionalSubExpression);
		boolean conditionIsSurelyIndependentOnScopedVariables =
				ScopedVariables.scopingVariablesAreDefined(expression, process)
				&&
				(ScopedVariables.isKnownToBeIndependentOfScopeIn(condition, expression, process));
		return conditionIsSurelyIndependentOnScopedVariables;
	}
}
