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
import com.sri.ai.grinder.library.equality.formula.helper.DistributeOrOverAnd;
import com.sri.ai.grinder.library.equality.formula.helper.DistributeOrOverOr;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeAnd;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeLiteral;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeOr;

/**
 * Convert a formula into an inferentially equivalent Conjunctive Normal
 * Form Expression (CNF). A formula is in CNF if it is a conjunction of
 * disjunction of literals.
 * 
 * Transformation rules are based on the INSEADO method outlined in:
 * 
 * <a href="http://logic.stanford.edu/classes/cs157/2012/lectures/lecture09.pdf">INSEADO Rules (slide 6 to 9)</a>
 * 
 * @author oreilly
 */
@Beta
public class FormulaToCNF {

	/**
	 * Convert a formula into an inferentially equivalent Conjunctive Normal
	 * Form Expression (CNF).
	 * 
	 * @param formula
	 *            a formula.
	 * @param process
	 *            the rewriting process
	 * @return false, true, or a conjunction of clauses.
	 * @throws IllegalArgumentException
	 *             if the input formula expression is not actually a formula.
	 * @see FormulaUtil#isFormula(Expression, RewritingProcess)
	 */
	public static Expression convertToCNF(Expression formula, RewritingProcess process) {
		Expression result = formula;

		// (INSEA)DO - NNF transformation handles the first part. 
		result = FormulaToNNF.convertToNNF(formula, process);
		
// TODO - implement a more efficient (i.e. linear) version based on Tseitin-Transformation
// http://en.wikipedia.org/wiki/Tseitin-Transformation
		// INSEAD)O - dsitribute
		result = distribution(result, process);
		
		// INSEADO) - Operators Out
		result = operatorsOut(result, process);
			
		if (FormulaUtil.isLiteral(result, process)) {
			result = Expressions.make(And.FUNCTOR, Expressions.make(Or.FUNCTOR, result));
		}
		else if (Or.isDisjunction(result)) {
			result = Expressions.make(And.FUNCTOR, result);
		}
		
		if (!(result.equals(Expressions.TRUE) || result.equals(Expressions.FALSE))) {
			if (!FormulaUtil.isCNF(result, process)) {
				throw new IllegalStateException("Failed to convert to CNF: "+result);
			}
		}

		return result;
	}

	//
	// PRIVATE
	//	
	private static Expression distribution(Expression formula, RewritingProcess process) {
		TotalRewriter cnfRewriter = new TotalRewriter(Arrays.asList((Rewriter)
				// Want to ensure the following normalizations
				// are applied to ensure the final CNF form is easier
				// to work with.
				new NormalizeOr(),
				new NormalizeAnd(),
				new NormalizeLiteral(),
				// INSEAD)O
				new DistributeOrOverAnd(),
				new DistributeOrOverOr(),
				new DistributeAndOverAnd()
				
			));
		Expression result = cnfRewriter.rewrite(formula, process);	
		return result;
	}
	
	private static Expression operatorsOut(Expression formula, RewritingProcess process) {
		// We don't really do as we want to keep as a conjunction of disjunctions
		// However, we need to ensure literal conjuncts are represented explicitly
		// as singleton clauses.
		TotalRewriter cnfRewriter = new TotalRewriter(Arrays.asList((Rewriter)
				new ConjunctionSingletonLiteralToClausesRewriter()
			));
		Expression result = cnfRewriter.rewrite(formula, process);	
		return result;
	}
	
	/**
	 * Performs the singleton literal to clause of the formula conversion to CNF:
	 * 
	 * L1 and L2 -> and(or(L1), or(L2))
	 */
	private static class ConjunctionSingletonLiteralToClausesRewriter extends AbstractRewriter {
		
		@Override
		public Expression rewriteAfterBookkeeping(Expression expression,
				RewritingProcess process) {
			Expression result = expression;
			
			if (And.isConjunction(expression) && expression.numberOfArguments() > 0) {
				// L1 and L2 -> and(or(L1), or(L2))
				boolean newConjunct = false;
				List<Expression> conjuncts = new ArrayList<Expression>();
				for (Expression conjunct : expression.getArguments()) {
					if (FormulaUtil.isLiteral(conjunct, process)) {
						newConjunct = true;
						conjuncts.add(Expressions.make(Or.FUNCTOR, conjunct));
					}
					else {
						conjuncts.add(conjunct);
					}
				}
				if (newConjunct) {
					result = And.make(conjuncts);
				}
			}
			
			return result;
		}
 	}
}
