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

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.util.Util;

/**
 * Rewriter simplifying one conjunct by using the fact that the others must be true.
 * For example, X = a and (X != a or Y = a) must simplify to X = a and Y = a.
 * 
 * This corresponds to the theorem
 * <pre>
 * (p => (q <=> q'))  =>  (p and q) <=> (p and q')
 * </pre>
 * 
 * @author braz
 */
@Beta
public class ConjunctsHoldTrueForEachOther extends AbstractRewriter {
	
//	@Override
//	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
//		if (And.isConjunction(expression)) {
//			boolean thereWasSimplification = false;
//			List<Expression> newConjuncts = new LinkedList<Expression>();
//			for (int i = 0; i != expression.numberOfArguments(); i++) {
//				Expression conjunct = expression.get(i);
//				Expression conjunctionMinusIthConjunct = Expressions.removeIthArgument(expression, i);
//				RewritingProcess iThProcess = GrinderUtil.extendContextualConstraint(conjunctionMinusIthConjunct, process);
//				Expression newConjunct = iThProcess.rewrite(CardinalityRewriter.R_incomplete_implied_certainty, conjunct);
//				newConjuncts.add(newConjunct);
//				thereWasSimplification = thereWasSimplification || newConjunct != conjunct;
//			}
//			if (thereWasSimplification) {
//				Expression result = And.make(newConjuncts);
//				return result;
//			}
//		}
//		return expression;
//	}
	
//	@Override
//	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
//		if (And.isConjunction(expression) && expression.numberOfArguments() > 1) {
//			Expression firstConjunct = expression.get(0);
//			Expression remainingConjuncts = Expressions.removeIthArgument(expression, 0);
//			remainingConjuncts = rewriteAfterBookkeeping(remainingConjuncts, process);
//			
//			RewritingProcess processAssumingFirstConjunct = GrinderUtil.extendContextualConstraint(firstConjunct, process);
//			Expression newRemainingConjuncts = processAssumingFirstConjunct.rewrite(CardinalityRewriter.R_incomplete_implied_certainty, remainingConjuncts);
//			if (newRemainingConjuncts != remainingConjuncts) { // if first conjunct simplifies remaining, keeps it and simplification
//				List<Expression> newConjuncts = Util.list(firstConjunct);
//				newConjuncts.addAll(newRemainingConjuncts.getArguments());
//				Expression result = And.make(newConjuncts);
//				System.out.println("Simplified under contextual constraint: " + process.getContextualConstraint());
//				System.out.println("expression: " + expression);
//				System.out.println("to        : " + result);
//				return result;
//			}
//			
//			RewritingProcess processAssumingRemainingConjuncts = GrinderUtil.extendContextualConstraint(remainingConjuncts, process);
//			Expression newFirstConjunct = processAssumingRemainingConjuncts.rewrite(CardinalityRewriter.R_incomplete_implied_certainty, firstConjunct);
//			if (newFirstConjunct != firstConjunct) { // if remaining conjuncts simplify first one, keeps the simplification and remaining conjuncts
//				List<Expression> newConjuncts = Util.list(newFirstConjunct);
//				newConjuncts.addAll(remainingConjuncts.getArguments());
//				Expression result = And.make(newConjuncts);
//				System.out.println("Simplified under contextual constraint: " + process.getContextualConstraint());
//				System.out.println("expression: " + expression);
//				System.out.println("to        : " + result);
//				return result;
//			}
//		}
//		return expression;
//	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (And.isConjunction(expression) && expression.numberOfArguments() > 1) {
			for (int i = 0; i != expression.numberOfArguments(); i++) {
				Expression iThConjunct = expression.get(i);
				Expression remainingOfConjunction = Expressions.removeIthArgument(expression, i);
				RewritingProcess processAssumingRemainingOfConjunction = GrinderUtil.extendContextualConstraint(remainingOfConjunction, process);
				Expression newIThConjunct = processAssumingRemainingOfConjunction.rewrite(CardinalityRewriter.R_incomplete_implied_certainty, iThConjunct);
				if (newIThConjunct != iThConjunct) {
					List<Expression> newConjuncts = new ArrayList<Expression>(expression.getArguments());
					newConjuncts.set(i, newIThConjunct);
					Expression result = And.make(newConjuncts);
					return result;
				}
			}
		}
		return expression;
	}
}
