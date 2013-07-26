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

import java.util.Arrays;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.library.equality.formula.helper.EquivalenceOut;
import com.sri.ai.grinder.library.equality.formula.helper.ExistentialOut;
import com.sri.ai.grinder.library.equality.formula.helper.ImplicationOut;
import com.sri.ai.grinder.library.equality.formula.helper.NegationIn;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeAnd;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeDisequalityLiteral;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeEqualityLiteral;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeLiteral;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeOr;
import com.sri.ai.grinder.library.equality.formula.helper.StandardizeVariables;
import com.sri.ai.grinder.library.equality.formula.helper.UniversalOut;

/**
 * Convert a formula into an inferentially equivalent Negation Normal
 * Form Expression (NNF). A formula is in NNF if negation is allowed
 * only over atoms, and conjunction, disjunction, and negation are the
 * only allowed boolean connectives (note negation is represented by 
 * 'X != Y' as opposed to 'not(X = Y)' as we only have equality and
 * want to normalize to a single negated form). 
 * 
 * @author oreilly
 *
 */
@Beta
public class FormulaToNNF {

	/**
	 * Convert a formula into an inferentially equivalent Negation Normal
	 * Form Expression (NNF).
	 * 
	 * @param formula
	 *            a formula.
	 * @param process
	 *            the rewriting process
	 * @return the formula in NNF form.
	 * @throws IllegalArgumentException
	 *             if the input formula expression is not actually a formula.
	 * @see FormulaUtil#isFormula(Expression, RewritingProcess)
	 */
	public static Expression convertToNNF(Expression formula, RewritingProcess process) {
		Expression result = formula;

		if (!FormulaUtil.isFormula(formula, process)) {
			throw new IllegalArgumentException(
					"Expression to be converted is not a formula: " + formula);
		}
		
		// Ensure equalities of the form:
		// X = ... = Z
		// are normalized up front.
		result = NormalizeLiteral.normalizeLiterals(formula, process);
		
		// I)NSEA - implications out
		result = ImplicationOut.implicationsOut(result, process);
		result = EquivalenceOut.equivalencesOut(result, process);
		
		// IN)SEA - negtaions in
		result = NegationIn.negationsIn(result, process);
		
		// INS)EA - standardize
		result = StandardizeVariables.standardize(result, process);
		
		// INSE)A- existentials out
		result = ExistentialOut.existentialsOut(result, process);
		
		// INSEA)- alls out
		result = UniversalOut.universalsOut(result, process);
		
		// Normalize the result
		TotalRewriter normalizeRewriter = new TotalRewriter(FormulaToNNF.class.getName()+ " Normalize Total Rewriter",
			Arrays.asList((Rewriter)
				// Want to ensure the following normalizations
				// are applied to ensure the final NNF form is easier
				// to work with.
				new NormalizeOr(),
				new NormalizeAnd(),
				new NormalizeEqualityLiteral(),
				new NormalizeDisequalityLiteral()
			));
		result = normalizeRewriter.rewrite(result, process);
		
		if (!FormulaUtil.isNNF(result, process)) {
			throw new IllegalStateException("Failed to convert to NNF: "+result);
		}

		return result;
	}
}
