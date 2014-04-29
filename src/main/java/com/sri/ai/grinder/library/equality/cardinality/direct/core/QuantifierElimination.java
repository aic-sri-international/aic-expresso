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

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.util.Util;

/**
 * Default implementation of R_quantifier_elimination(F).
 * 
 * @author oreilly
 *
 */
@Beta
public class QuantifierElimination extends AbstractHierarchicalRewriter implements CardinalityRewriter {

	public QuantifierElimination() {
	}
	
	@Override
	public String getName() {
		return R_quantifier_elimination;
	}
	
	/**
	 * @see CardinalityRewriter#R_quantifier_elimination
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expressionF, RewritingProcess process) {
		Expression result = null;
		
		// Assert input argument
		if (!FormulaUtil.isFormula(expressionF, process)) {
			throw new IllegalArgumentException("F is not a formula:"+expressionF);
		}
				
		expressionF = CardinalityUtil.removeIfThenElsesFromFormula(expressionF, process);

		if (expressionF.hasFunctor(FunctorConstants.FOR_ALL)) {
			Trace.log("if F is \"for all x: Y\"");
			Trace.log("    return R_normalize(R_card(|R_top_simplify(Y)|_x, \"for all\") = |type(x)| )");
			
			Expression indexExpression = ForAll.getIndexExpression(expressionF);
			Expression body = ForAll.getBody(expressionF);
			
			RewritingProcess subProcess = GrinderUtil.extendContextualVariablesWithIndexExpression(indexExpression, process);
			body = subProcess.rewrite(R_top_simplify, body);
			
			Expression numberOfSolutionsOfBodyInIndexProblem     = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(body, indexExpression);
			Expression numberOfSolutionsOfBodyInIndexSolution    = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(numberOfSolutionsOfBodyInIndexProblem, CardinalityRewriter.Quantification.FOR_ALL));
			Expression indexDomainSize                           = CardinalityUtil.makeCardinalityOfIndexExpressions(Util.list(indexExpression));
			Expression numberOfSolutionsAndDomainSizeMustBeEqual = Equality.make(numberOfSolutionsOfBodyInIndexSolution, indexDomainSize);
			
			result = process.rewrite(R_normalize, numberOfSolutionsAndDomainSizeMustBeEqual);
		} 
		else if (expressionF.hasFunctor(FunctorConstants.THERE_EXISTS)) {
			Trace.log("if F is \"there exists x: Y\"");
			Trace.log("    return R_normalize(R_card(|R_top_simplify(Y)|_x, \"there exists\") > 0)");
			
			Expression indexExpression = ThereExists.getIndexExpression(expressionF);
			Expression body = ThereExists.getBody(expressionF);
			
			RewritingProcess subProcess = GrinderUtil.extendContextualVariablesWithIndexExpression(indexExpression, process);
			body = subProcess.rewrite(R_top_simplify, body);
			
			Expression numberOfSolutionsOfBodyInIndexProblem  = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(body, indexExpression);
			Expression numberOfSolutionsOfBodyInIndexSolution = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(numberOfSolutionsOfBodyInIndexProblem, CardinalityRewriter.Quantification.THERE_EXISTS));
			Expression numberOfSolutionsMustBeGreaterThanZero = Expressions.makeFunctionApplication(FunctorConstants.GREATER_THAN, numberOfSolutionsOfBodyInIndexSolution, Expressions.ZERO);
			
			result = process.rewrite(R_normalize, numberOfSolutionsMustBeGreaterThanZero);
		} 
		else if (expressionF.hasFunctor(FunctorConstants.NOT) && expressionF.numberOfArguments() == 1) {
			Trace.log("if F is \"not G\"");
			Trace.log("    return R_quantifier_elimination(R_move_not_in(not G))");
			
			Expression movedNotIn = process.rewrite(R_move_not_in, expressionF);

			result = process.rewrite(R_quantifier_elimination, movedNotIn);
		} 
		else if (expressionF.hasFunctor(FunctorConstants.IMPLICATION) && expressionF.numberOfArguments() == 2) {
			Trace.log("if F is G => H");
			Trace.log("    return R_quantifier_elimination(R_top_simplify_disjunction(not G or H))");
			Expression g                        = expressionF.get(0);
			Expression h                        = expressionF.get(1);
			Expression notGorH                  = CardinalityUtil.makeOr(CardinalityUtil.makeNot(g), h);
			Expression topSimplifiedDisjunction = process.rewrite(R_top_simplify_disjunction, notGorH);
			
			result = process.rewrite(R_quantifier_elimination, topSimplifiedDisjunction);
		} 
		else if (expressionF.hasFunctor(FunctorConstants.EQUIVALENCE) && expressionF.numberOfArguments() == 2) {
			Trace.log("if F is G <=> H");
			Trace.log("    return R_quantifier_elimination(R_top_simplify_conjunction((not G or H) and (G or not H)))");
			Expression g                        = expressionF.get(0);
			Expression h                        = expressionF.get(1);
			Expression notGorHAndGorNotH        = CardinalityUtil.makeAnd(CardinalityUtil.makeOr(CardinalityUtil.makeNot(g), h), CardinalityUtil.makeOr(g, CardinalityUtil.makeNot(h)));
			Expression topSimplifiedConjunction = process.rewrite(R_top_simplify_conjunction, notGorHAndGorNotH);
			
			result = process.rewrite(R_quantifier_elimination, topSimplifiedConjunction);
		} 
		else if (And.isConjunction(expressionF)) {
			Trace.log("if F is a conjunction F1 and ... and Fn");
			Trace.log("    F' <- True");
			Trace.log("    i  <- 1");
			int        n      = expressionF.numberOfArguments();
			Expression fPrime = Expressions.TRUE;
			int        i      = 0;
			Expression fi, gi = null;
			Trace.log("    while F' is not \"False\" and i <= n");
			while (!fPrime.equals(Expressions.FALSE) && i < n) {
				Trace.log("         Gi <- R_quantifier_elimination(Fi)");
				fi = expressionF.get(i);
				gi = process.rewrite(R_quantifier_elimination, fi);
				
				Trace.log("         F' <- add_conjunct_and_top_simplify(Gi, F')");
				fPrime = AddConjunctAndTopSimplify.addConjunctAndTopSimplify(gi, fPrime, process);
				
				Trace.log("         i <- i + 1");
				i++;
			}
			
			Trace.log("    return F'");
			// Ensure we don't return an equivalent expression in place of the input
			if (fPrime.equals(expressionF)) {
				fPrime = expressionF;
			}
			result = fPrime;
		} 
		else if (Or.isDisjunction(expressionF)) {
			Trace.log("if F is a disjunction F1 or ... or Fn");
			Trace.log("    F' <- False");
			Trace.log("    i  <- 1");
			int        n      = expressionF.numberOfArguments();
			Expression fPrime = Expressions.FALSE;
			int        i      = 0;
			Expression fi, gi = null;
			Trace.log("    while F' is not \"True\" and i <= n");
			while (!fPrime.equals(Expressions.TRUE) && i < n) {
				Trace.log("        Gi <- R_quantifier_elimination(Fi)");
				fi = expressionF.get(i);
				gi = process.rewrite(R_quantifier_elimination, fi);
				
				Trace.log("        F' <- add_disjunct_and_top_simplify(Gi, F')");
				fPrime = AddDisjunctAndTopSimplify.addDisjunctAndTopSimplify(gi, fPrime, process);
				
				Trace.log("        i  <- i + 1");
				i++;
			}
			Trace.log("    return F'");
			// Ensure we don't return an equivalent expression in place of the input
			if (fPrime.equals(expressionF)) {
				fPrime = expressionF;
			}
			result = fPrime;
		} 
		else {
			Trace.log("return F");
			result = expressionF;
		}
		
		return result;
	}
}
