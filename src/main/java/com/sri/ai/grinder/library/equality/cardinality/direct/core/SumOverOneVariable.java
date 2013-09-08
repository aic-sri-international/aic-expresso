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
package com.sri.ai.grinder.library.equality.cardinality.direct.core;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.helper.concurrent.BranchRewriteTask;
import com.sri.ai.grinder.helper.concurrent.RewriteOnBranch;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;

/**
 * Default implementation of R_sum_over_one_variable(sum_{x: Cx}  S).
 * 
 * @author oreilly
 *
 */
@Beta
public class SumOverOneVariable extends AbstractHierarchicalRewriter implements CardinalityRewriter {
	
	private RewriteOnBranch rewriteSumOverOneVariableOnBranch = new RewriteOnBranch() {
				@Override
				public Expression rewrite(Expression[] expressions, RewritingProcess process) {
					Expression result = process.rewrite(R_sum_over_one_variable, expressions[0]);
					return result;
				}
			};
	
	public SumOverOneVariable() {
	}
	
	@Override
	public String getName() {
		return R_sum_over_one_variable;
	}
	
	/**
	 * @see CardinalityRewriter#R_sum_over_one_variable
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		// Assert input arguments, sum_{x: Cx} S
		CardinalityUtil.assertSummationExpression(expression);
		
		Expression x  = CardinalityUtil.getIndexXFromSummation(expression);
		Expression cX = CardinalityUtil.getConstraintsOnXFromSummation(expression);
		Expression s  = CardinalityUtil.getCountingSolutionSFromSummation(expression);
		if ( !assertEpressionEasilySummable(s, x, process) ) {
			throw new IllegalArgumentException("S is not easily summable with respect to x");
		}
		
		if (IfThenElse.isIfThenElse(s)) {
			Trace.log("S is \"if F then S1 else S2\".");
			Expression f  = IfThenElse.getCondition(s);
			Expression s1 = IfThenElse.getThenBranch(s);
			Expression s2 = IfThenElse.getElseBranch(s);
			Trace.log("    return R_normalize(R_sum_over_one_variable(sum_{x:Cx and F} S1) + R_sum_over_one_variable(sum_{x:Cx and not F} S2)))");
			Expression sumS1 = CardinalityUtil.makeSummationExpression(x, CardinalityUtil.makeAnd(cX, f), s1);
			Expression sumS2 = CardinalityUtil.makeSummationExpression(x, CardinalityUtil.makeAnd(cX, CardinalityUtil.makeNot(f)), s2);
			
			BranchRewriteTask[] taskRewriters = new BranchRewriteTask[] {
					new BranchRewriteTask(rewriteSumOverOneVariableOnBranch, new Expression[] {sumS1}),
					new BranchRewriteTask(rewriteSumOverOneVariableOnBranch, new Expression[] {sumS2})
			};	
			
			List<Expression> plusTerms    = GrinderUtil.branchAndMergeTasks(taskRewriters, process);			
			Expression       plusSum1And2 = Plus.make(plusTerms);
			
			result = process.rewrite(R_normalize, plusSum1And2);
		} 
		else  {
			Trace.log("S is a numeric constant expression.");
			if (s.equals(Expressions.ZERO)) {
				Trace.log("    if S is 0");
				Trace.log("        return 0");
				
				result = Expressions.ZERO;
			} 
			else {
				Trace.log("    return R_normalize(R_card(|Cx|_{x}) * S)");
				Expression cardCxIndexedByX       = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(cX, x);
				Expression resultCardCxIndexedByX = process.rewrite(R_card, cardCxIndexedByX);
				Expression timesS                 = Times.make(Arrays.asList(resultCardCxIndexedByX, s));
				
				result = process.rewrite(R_normalize, timesS);
			}
		}
		
		return result;
	}
	
	private boolean assertEpressionEasilySummable(Expression expression, Expression var, RewritingProcess process) {
		boolean result = false;
		
		if ( IfThenElse.isIfThenElse(expression) ) {
			Expression thenBranch = IfThenElse.getThenBranch(expression);
			Expression elseBranch = IfThenElse.getElseBranch(expression);
			
			result = assertEpressionEasilySummable(thenBranch, var, process) && assertEpressionEasilySummable(elseBranch, var, process);
		} 
		else {
			Set<Expression> freeVariables = Expressions.freeVariables(expression, process);
			
			result = !freeVariables.contains(var);
		}
		
		return result;
	}
}
