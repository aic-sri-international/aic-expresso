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

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.core.HasFormula;
import com.sri.ai.grinder.expression.ExpressionCacheKey;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.equality.sat.SATSolver;
import com.sri.ai.grinder.library.equality.sat.SATSolverFactory;

/**
 * Rewriter replacing expression by true or false if it or its negation is implied by the context.
 * 
 * @author braz
 *
 */
@Beta
public class TopImpliedCertainty extends AbstractHierarchicalRewriter implements CardinalityRewriter {
	
	private Set<ExpressionCacheKey> testing      = new LinkedHashSet<ExpressionCacheKey>();
	private boolean                 useSATSolver = GrinderConfiguration.isCompleteSimplifyUseSATSolver();
	private SATSolver               satSolver    = SATSolverFactory.newInstance();
	
	public TopImpliedCertainty() {
		this.setReifiedTests(new HasFormula());
	}
	
	@Override
	public String getName() {
		return R_top_implied_certainty;
	}
	
	/**
	 * @see CardinalityRewriter#R_top_implied_certainty
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression;
		
		if (!(expression instanceof Symbol)) {			
			// Note: as is_tautology relies on R_complete_normalize,
			// which this rewriter is a part of, ensure we do not
			// recurse indefinitely in the case where a similar formula
			// is generated by the is_tautology call.
			boolean test = false;
			ExpressionCacheKey key = new ExpressionCacheKey(expression, process.getContextualConstraint());
			synchronized (testing) {
				if (!testing.contains(key)) {
					testing.add(key);
					test = true;
				}
			}
			if (test) {
				Expression c         = process.getContextualConstraint();
				Expression cImpliesF = Implication.make(c, expression);
				
				boolean useSAT = useSATSolver && FormulaUtil.isQuantifierFreeFormula(cImpliesF, process);
				
				RewritingProcess tautologyProcess = process; 
				Trace.log("if F is a formula");
				Trace.log("    if is_tautology(C => F)");
				if (useSAT) {
					// You can use SAT for tautology of a formula F by checking 
					// if not F is satisfiable. If it is, F is not a tautology.
					Expression notCImpliesF = Not.make(cImpliesF);
					if (!satSolver.isSatisfiable(notCImpliesF, process)) {
						Trace.log("        return true");
						result = Expressions.TRUE;
					}
				}
				else {
					// Need to perform the is_tautology tests with an empty context.
					tautologyProcess = process.newSubProcessWithContext(new LinkedHashMap<Expression, Expression>(), Expressions.TRUE);
					if (IsTautology.isTautology(cImpliesF, tautologyProcess)) {
						Trace.log("        return true");
						result = Expressions.TRUE;
					} 
				}
				
				if (result == expression) {
					Trace.log("    if is_tautology(C => not F)");
					if (useSAT) {
						// (c => not F) <=> True (tautology)
						// <=>
						// not (c => not F) <=> False
						// <=>
						// not (not c or not F) <=> False
						// <=>
						// c and F <=> False (unsatisfiable)
						Expression constraint = And.make(process.getContextualConstraint(), expression);
						if (!satSolver.isSatisfiable(constraint, process)) {
							Trace.log("        return false");
							result = Expressions.FALSE;
						}
					}
					else {
						Expression cImpliesNotF = Implication.make(c, CardinalityUtil.makeNot(expression));
						if (IsTautology.isTautology(cImpliesNotF, tautologyProcess)) {
							Trace.log("        return false");
							result = Expressions.FALSE;
						}
					}
				}
				
				synchronized (testing) {
					testing.remove(key);
				}
			}
		}

		return result;
	}
}
