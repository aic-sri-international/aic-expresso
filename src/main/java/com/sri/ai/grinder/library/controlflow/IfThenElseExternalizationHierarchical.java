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
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.RewriterReplacementFunction;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.FormulaSimplify;
import com.sri.ai.util.Util;

/**
 * Receives a basic expression (basic operators plus products of intensional sets with basic expressions in the head)
 * and returns an equivalent expression in which all conditional expressions are on the top of the expression,
 * but for the ones constrained to be inside sets because they use their supportedIndices.
*/
@Beta
public class IfThenElseExternalizationHierarchical extends AbstractHierarchicalRewriter {

	private Rewriter formulaSimplify;
	
	public IfThenElseExternalizationHierarchical() {
		super();
		formulaSimplify = new FormulaSimplify();
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = normalize(expression, false, process);
		return result;
	}

	private Expression normalize(Expression expression, boolean subExpressionsAreNormalized, RewritingProcess process) {

		expression = formulaSimplify.rewrite(expression, process);

		// Make sure sub-expressions are normalized (externalized) first, if not already
		// Note that this assumes formula simplifications above do not disturb if then else externalization
		if ( ! subExpressionsAreNormalized) {
			expression = expression.replace(
					new RewriterReplacementFunction(this),
					false /* don't do first occurrence only, but all occurrences */,
					null  /* no pruning */,
					true  /* ignore top expression */,
					null,
					process);
		}

		ExpressionAndContext conditionalSubExpressionAndContext = findConditionalSubExpressionAndContext(expression, process);
		if (conditionalSubExpressionAndContext != null) {
			if (IfThenElse.isIfThenElse(expression) && conditionalSubExpressionAndContext.getExpression() != IfThenElse.getCondition(expression)) {
				// The conditional sub-expression is not the condition of 'expression'.
				// This, together with the fact that all sub-expressions are normalized (externalized),
				// means expression is already normalized; this is the recursion base case.
			}
			else {
				// expression is not a conditional expression itself and has a conditional sub-expression,
				// or it is and its own condition is also a conditional -- both cases require externalization
				Expression conditionalSubExpression = conditionalSubExpressionAndContext.getExpression();
				boolean noScopingRestrictions = decideWhetherThereAreNoScopingRestrictions(expression, conditionalSubExpression, process);
				if (noScopingRestrictions) {
					Expression condition  = IfThenElse.getCondition(conditionalSubExpression);
					Expression thenBranch = IfThenElse.getThenBranch(conditionalSubExpression);
					Expression elseBranch = IfThenElse.getElseBranch(conditionalSubExpression);
					Expression result = conditionalSubExpressionAndContext.getAddress().replace(expression, thenBranch);

					// Create two expressions, one in which the "then branch" replaces the conditional sub-expression, and another in which the "else branch" does that.
					Expression newThenBranch = result;
					Expression newElseBranch = conditionalSubExpressionAndContext.getAddress().replace(expression, elseBranch);

					// Make sure the *new* subexpressions are normalized themselves, even though the original ones already were.
					// If they are not normalized, the unnormalized part must be on their top expression only,
					// since their sub-expressions were original expression's sub-expressions, which we know to be normalized.
					// For example, suppose expression was f(if C1 then A1 else B1, if C2 then A2 else B2).
					// At this point, we have if C1 then f(A1, if C2 then A2 else B2) else f(B1, if C2 then A2 else B2)
					// f(A1, if C2 then A2 else B2) and f(B1, if C2 then A2 else B2) are not normalized, but their sub-expressions are (they are some of expression's sub-expressions).
					// So we make a recursive call on these new then and else branches, with the information that their own sub-expressions are normalized.
					RewritingProcess thenProcess = GrinderUtil.extendContextualConstraint(condition, process);
					newThenBranch = normalize(newThenBranch, true, thenProcess);
					
					RewritingProcess elseProcess = GrinderUtil.extendContextualConstraint(Not.make(condition), process);
					newElseBranch = normalize(newElseBranch, true, elseProcess);
					
					expression = IfThenElse.make(condition, newThenBranch, newElseBranch);
				}
			}
		}

		expression = process.rewrite(formulaSimplify, expression);
		
		return expression;
	}

	private static ExpressionAndContext findConditionalSubExpressionAndContext(Expression expression, RewritingProcess process) {
		ExpressionAndContext result = 
		Util.getFirstSatisfyingPredicateOrNull(
				expression.getImmediateSubExpressionsAndContextsIterator(),
				new IsConditionalSubExpression());
		return result;
	}

	private final static class IsConditionalSubExpression implements Predicate<ExpressionAndContext> {
		@Override
		public boolean apply(ExpressionAndContext subExpressionAndContext) {
			Expression expression = subExpressionAndContext.getExpression();
			boolean result = expression != null && IfThenElse.isIfThenElse(expression);
			return result;
		}
	}

	private boolean decideWhetherThereAreNoScopingRestrictions(Expression expression, Expression conditionalSubExpression, RewritingProcess process) {
		Expression condition  = IfThenElse.getCondition(conditionalSubExpression);
		boolean result = GrinderUtil.isKnownToBeIndependentOfScopeIn(condition, expression, process);
		return result;
	}
}
