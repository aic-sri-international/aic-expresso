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
package com.sri.ai.grinder.library.equality.formula;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.equality.formula.helper.DistributeAndOverAnd;
import com.sri.ai.grinder.library.equality.formula.helper.DistributeAndOverOr;
import com.sri.ai.grinder.library.equality.formula.helper.DistributeOrOverOr;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeAnd;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeLiteral;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeOr;

/**
 * Convert a formula into an inferentially equivalent Disjunctive Normal
 * Form Expression (DNF). A formula is in DNF if it is a disjunction of
 * conjunction of literals.
 * 
 * @author oreilly
 */
@Beta
public class FormulaToDNF {

	/**
	 * Convert a formula into an inferentially equivalent Disjunctive Normal
	 * Form Expression (DNF).
	 * 
	 * @param formula
	 *            a formula.
	 * @param process
	 *            the rewriting process
	 * @return false, true, or a disjunction of conjunctions of literals.
	 * @throws IllegalArgumentException
	 *             if the input formula expression is not actually a formula.
	 * @see FormulaUtil#isFormula(Expression, RewritingProcess)
	 */
	public static Expression convertToDNF(Expression formula, RewritingProcess process) {
		Expression result = formula;

		// (INSEA)DO - NNF transformation handles the first part. 
		result = FormulaToNNF.convertToNNF(formula, process);

		// INSEAD)O - dsitribute
		result = distribution(result, process);
		
		// INSEADO) - Operators Out
		result = operatorsOut(result, process);
			
		if (FormulaUtil.isLiteral(result, process)) {
			result = Expressions.make(Or.FUNCTOR, Expressions.make(And.FUNCTOR, result));
		}
		else if (And.isConjunction(result)) {
			result = Expressions.make(Or.FUNCTOR, result);
		}
		
		if (!(result.equals(Expressions.TRUE) || result.equals(Expressions.FALSE))) {
			if (!FormulaUtil.isDNF(result, process)) {
				throw new IllegalStateException("Failed to convert to DNF: "+result);
			}
		}

		return result;
	}

	//
	// PRIVATE
	//	
	private static Expression distribution(Expression formula, RewritingProcess process) {
		TotalRewriter dnfRewriter = new TotalRewriter(FormulaToDNF.class.getName()+ " distribution Total Rewriter",
			Arrays.asList((Rewriter)
				// Want to ensure the following normalizations
				// are applied to ensure the final DNF form is easier
				// to work with.
				new NormalizeOr(),
				new NormalizeAnd(),
				new NormalizeLiteral(),
				// INSEAD)O
				new DistributeAndOverOr(),
				new DistributeAndOverAnd(),
				new DistributeOrOverOr()
			));
		Expression result = dnfRewriter.rewrite(formula, process);	
		return result;
	}
	
	private static Expression operatorsOut(Expression formula, RewritingProcess process) {
		// We don't really do as we want to keep as a disjuction of conjunctions of literals
		// However, we need to ensure literal conjuncts are represented explicitly
		// as singleton clauses.
		TotalRewriter dnfRewriter = new TotalRewriter(FormulaToDNF.class.getName()+ " operatorsOut Total Rewriter",
			Arrays.asList((Rewriter)
				new DisjunctionSingletonLiteralToConjunctionOfLiteralsRewriter()
			));
		Expression result = dnfRewriter.rewrite(formula, process);	
		return result;
	}
	
	/**
	 * Performs the singleton literal to conjunction of literals in DNF:
	 * 
	 * L1 or L2 -> or(and(L1), and(L2))
	 */
	private static class DisjunctionSingletonLiteralToConjunctionOfLiteralsRewriter extends AbstractRewriter {
		
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (Or.isDisjunction(expression) && expression.numberOfArguments() > 0) {
				// L1 or L2 -> or(and(L1), and(L2))
				boolean newDisjunct = false;
				List<Expression> disjuncts = new ArrayList<Expression>();
				for (Expression disjunct : expression.getArguments()) {
					if (FormulaUtil.isLiteral(disjunct, process)) {
						newDisjunct = true;
						disjuncts.add(Expressions.make(And.FUNCTOR, disjunct));
					}
					else {
						disjuncts.add(disjunct);
					}
				}
				if (newDisjunct) {
					result = Or.make(disjuncts);
				}
			}
			
			return result;
		}
 	}
}
