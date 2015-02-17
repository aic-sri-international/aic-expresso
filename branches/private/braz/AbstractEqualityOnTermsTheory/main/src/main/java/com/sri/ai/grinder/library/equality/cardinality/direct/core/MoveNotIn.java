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
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.AbstractCardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;

/**
 * Default implementation of R_move_not_in(F).
 * 
 * @author oreilly
 *
 */
@Beta
public class MoveNotIn extends AbstractCardinalityRewriter {
	
	public MoveNotIn() {
	}
	
	@Override
	public String getName() {
		return R_move_not_in;
	}
	
	/**
	 * @see CardinalityRewriter#R_move_not_in
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		return staticRewrite(expression, process);
	}

	public static Expression staticRewrite(Expression expression, RewritingProcess process) {
		Expression result = expression;
	
		// Assert input arguments
		if (!(expression.hasFunctor(FunctorConstants.NOT) &&
		      expression.numberOfArguments() == 1)) {
			throw new IllegalArgumentException("Invalid input argument expression, not expected:"+expression);
		}
		
		Expression notF = expression.get(0);
		if (notF.equals(Expressions.FALSE)) {
			Trace.log("F is \"not FALSE\"");
			Trace.log("    return TRUE");
			result = Expressions.TRUE;
		} 
		else if (notF.equals(Expressions.TRUE)) {
			Trace.log("F is \"not TRUE\"");
			Trace.log("    return FALSE");
			result = Expressions.FALSE;
		} 
		else if (notF.hasFunctor(FunctorConstants.EQUAL)) {
			Trace.log("F is \"not (t1 = t2 = ... = tn)\"");
			Trace.log("   return t1 != t2 or ... or t_{n-1} != tn");
			List<Expression> disequalities = new ArrayList<Expression>();
			for (int i = 0; i != notF.numberOfArguments() - 1; i++) {
				Expression first  = notF.get(i);
				Expression second = notF.get(i + 1);
				if ( ! first.equals(second)) { // not the same expression (which would be trivially false)
					disequalities.add(Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", first, second));
				}
			}
			result = Or.make(disequalities);
		} 
		else if (notF.hasFunctor(FunctorConstants.INEQUALITY)) {
			Trace.log("F is \"not x != t\"");
			Trace.log("   return x = t");
			result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(FunctorConstants.EQUAL, notF.getArguments());
			// Handle normalization edge case.
			if (CardinalityUtil.isEqualityOnSameTerms(result)) {
				result = Expressions.TRUE;
			}
		} 
		else if (notF.hasFunctor(FunctorConstants.NOT) && notF.numberOfArguments() == 1) {
			Trace.log("F is \"not not G\"");
			Trace.log("    G <- R_top_simplify(G)");
			Trace.log("    return G");
			Expression g = notF.get(0);
			g = process.rewrite(R_top_simplify, g);
			result = g;
		} 
		else if (And.isConjunction(notF)) {
			Trace.log("F is \"not (G1 and ... and Gn)\"");
			Trace.log("    (G1 and ... and Gn) <- R_top_simplify_conjunction(G1 and ... and Gn)");
			Expression gConjunction = process.rewrite(R_top_simplify_conjunction, notF);
			Trace.log("    return not G1 or ... or not Gn");
			List<Expression> notGis = new ArrayList<Expression>();
			if (And.isConjunction(gConjunction)) {
				for (Expression gi : gConjunction.getArguments()) {
					notGis.add(CardinalityUtil.makeNot(gi));
				}
			} 
			else {
				// Was simplified to a single term
				notGis.add(CardinalityUtil.makeNot(gConjunction));
			}
			result = Or.make(notGis);
		}
		else if (Or.isDisjunction(notF)) {
			Trace.log("F is \"not (G1 or ... or Gn)\"");
			Trace.log("    (G1 or ... or Gn) <- R_top_simplify_disjunction(G1 or ... or Gn)");
			Expression gDisjunction = process.rewrite(R_top_simplify_disjunction, notF);
			Trace.log("    return not G1 and ... and not Gn");
			List<Expression> notGis = new ArrayList<Expression>();
			if (Or.isDisjunction(gDisjunction)) {
				for (Expression gi : gDisjunction.getArguments()) {
					notGis.add(CardinalityUtil.makeNot(gi));
				}
			} 
			else {
				// Was simplified to a single term
				notGis.add(CardinalityUtil.makeNot(gDisjunction));
			}
			result = And.make(notGis);
		} 
		else if (notF.hasFunctor(FunctorConstants.IMPLICATION) &&
				   notF.numberOfArguments() == 2) {
			Trace.log("F is \"not (G => H)\"");
			Trace.log("    return R_top_simplify_conjunction(G and not H)");
			Expression gAndNotH = CardinalityUtil.makeAnd(notF.get(0), CardinalityUtil.makeNot(notF.get(1)));
		
			result = process.rewrite(R_top_simplify_conjunction, gAndNotH);
		} 
		else if (notF.hasFunctor(FunctorConstants.EQUIVALENCE) &&
				   notF.numberOfArguments() == 2) {
			Trace.log("F is \"not (G <=> H)\"");
			Trace.log("    return R_top_simplify_disjunction(R_top_simplify_conjunction(G and not H) or R_top_simplify_conjunction(not G and H))");
			Expression g        = notF.get(0);
			Expression h        = notF.get(1);
			Expression notG     = CardinalityUtil.makeNot(g);
			Expression notH     = CardinalityUtil.makeNot(h);
			Expression gAndNotH = CardinalityUtil.makeAnd(g, notH);
			Expression notGAndH = CardinalityUtil.makeAnd(notG, h);
			
			Expression simplifiedGAndNotH = process.rewrite(R_top_simplify_conjunction, gAndNotH);
			Expression simplifiednotGAndH = process.rewrite(R_top_simplify_conjunction, notGAndH);
			
			Expression disjunction = CardinalityUtil.makeOr(simplifiedGAndNotH, simplifiednotGAndH);
			
			result = process.rewrite(R_top_simplify_disjunction, disjunction);
		} 
		else if (ThereExists.isThereExists(notF)) {
			Trace.log("F is \"not (there exists x G)\"");
			Trace.log("    return for all x not G");
			Expression indexExpression = ThereExists.getIndexExpression(notF);
			Expression g               = ThereExists.getBody(notF);
			Expression notG            = CardinalityUtil.makeNot(g);
			
			result = ForAll.make(indexExpression, notG);
		} 
		else if (ForAll.isForAll(notF)) {
			Trace.log("F is \"not (for all x G)\"");
			Trace.log("    return there exists x not G");
			Expression indexExpression = ForAll.getIndexExpression(notF);
			Expression g               = ForAll.getBody(notF);
			Expression notG            = CardinalityUtil.makeNot(g);
			
			result = ThereExists.make(indexExpression, notG);
		}
		
		return result;
	}
}
