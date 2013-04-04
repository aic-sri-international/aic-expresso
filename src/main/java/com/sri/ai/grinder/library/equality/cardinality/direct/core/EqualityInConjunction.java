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
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.Substitute;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.tuple.Tuple;

/**
 * Default implementation of R_equality_in_conjunction(| x_i = t and Phi |_{x1, ..., xn}).
 * 
 * @author oreilly
 *
 */
@Beta
public class EqualityInConjunction extends AbstractHierarchicalRewriter implements CardinalityRewriter {
	
	public EqualityInConjunction() {
	}
	
	@Override
	public String getName() {
		return R_equality_in_conjunction;
	}
	

	public static boolean isOptimizable(Expression expression, RewritingProcess process) {
		boolean result = false;
		if (getFirstOptimizableInformation(expression, process) != null) {
			result = true;
		}
		
		return result;
	}
	
	/**
	 * @see CardinalityRewriter#R_equality_in_conjunction
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		// Assert input arguments, (| F |_x, quantification)
		if (!(Tuple.isTuple(expression) &&
			  Tuple.size(expression) == 2)
			  ) {
			throw new IllegalArgumentException("Invalid input argument expression, expect (| F |_X, quantification):"+expression);
		}
		
		Expression cardinalityOfIndexedFormulaExpression  = Tuple.get(expression, 0);
		Expression quantificationSymbol                   = Tuple.get(expression, 1);
		CardinalityRewriter.Quantification quantification = CardinalityRewriter.Quantification.getQuantificationForSymbol(quantificationSymbol);
		
		OptimizableInformation oi = getFirstOptimizableInformation(cardinalityOfIndexedFormulaExpression, process);
		if (oi == null) {
			throw new IllegalArgumentException("Invalid input argument expression, cannot be optimized:"+cardinalityOfIndexedFormulaExpression);
		} 
		else {			
			if (oi.ts.size() == 0) {
				Trace.log("if t is the same expression as x_i");
				Trace.log("    return R_card(| Phi |_X, quantification)");
				Expression phiIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(oi.phi, oi.x.toArray(new Expression[oi.x.size()]));
				
				result = process.rewrite(R_card,
							CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(phiIndexedByX, quantification));
			} 
			else {
				Expression t = oi.ts.iterator().next();
				// i.e {x \ xi}
				oi.x.remove(oi.xi);
				Trace.log("return R_card(| R_simplify(Phi[x_i / t]) |_X\\{xi}, quantification) // Phi={}, x_i={}, t={}", oi.phi, oi.xi, t);
				Expression phiXiReplacedWithT           = Substitute.replace(oi.phi, oi.xi, t, process);
				Expression simplifiedPhiXiReplacedWithT = process.rewrite(R_simplify, phiXiReplacedWithT);
				
				Expression cardPhiXiReplacedWithTIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(simplifiedPhiXiReplacedWithT, oi.x.toArray(new Expression[oi.x.size()]));
				result = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardPhiXiReplacedWithTIndexedByX, quantification));
			}
		}
		
		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	private static class OptimizableInformation {
		public Expression xi;
		public Set<Expression> ts = new HashSet<Expression>();
		public Expression phi;
		public Set<Expression> x;
	}
	
	private static OptimizableInformation getFirstOptimizableInformation(Expression expression, RewritingProcess process) {
		OptimizableInformation result = null;
		
		if (CardinalityUtil.isCardinalityOfIndexedFormulaExpression(expression)) {
			// | {(on x1,..., xn) (x1, ..., xn) | F} |
			Expression intensionalSet = expression.get(0);
			Expression f              = IntensionalSet.getCondition(intensionalSet);
			if (And.isConjunction(f)) {
				// Note: want to use a set for efficiency but keep the order by using a linked hash set.
				Set<Expression> indexExpressions = new LinkedHashSet<Expression>(IntensionalSet.getIndexExpressions(intensionalSet));
				int idx = 0;
				for (Expression conjunct : f.getArguments()) {
					// x_i = t and Phi
					if (conjunct.hasFunctor(FunctorConstants.EQUAL)) {
						result = getOptimizableInformation(f, conjunct, indexExpressions, idx, process);
						if (result != null) {
							break;
						}
					}
					idx++;
				}
			} 
			else if (f.hasFunctor(FunctorConstants.EQUAL)) {
				// Note: want to use a set for efficiency but keep the order by using a linked hash set.
				Set<Expression> indexExpressions = new LinkedHashSet<Expression>(IntensionalSet.getIndexExpressions(intensionalSet));
				result = getOptimizableInformation(f, f, indexExpressions, 0, process);
			}
		}
		
		return result;
	}
	
	private static OptimizableInformation getOptimizableInformation(Expression f, Expression conjunct, Set<Expression> indexExpressions, int idx, RewritingProcess process) {
		OptimizableInformation result = null;
		for (Expression xi : conjunct.getArguments()) {
			if (indexExpressions.contains(xi)) {
				result = new OptimizableInformation();
				result.xi = xi;
				result.ts.addAll(conjunct.getArguments());
				List<Expression> phiConjuncts = new ArrayList<Expression>();
				if (f != conjunct) {
					phiConjuncts.addAll(f.getArguments().subList(0, idx));
					phiConjuncts.addAll(f.getArguments().subList(idx+1, f.getArguments().size()));
				} 
				// Using a Set ensures this case is handled:
				// X = X = Y
				result.ts.remove(xi);
				// Ensure this case is handled:
				// X = Y = Z
				// by returning X = Y and pushing X = Z into Phi to be handled later on
				while (result.ts.size() > 1) {
					Expression t = result.ts.iterator().next();
					phiConjuncts.add(Equality.make(result.xi, t));
					result.ts.remove(t);
				}
				
				result.phi = And.make(phiConjuncts);
				result.x = indexExpressions;
				break;
			}
		}
		return result;
	}
}
