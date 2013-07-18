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
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;

/**
 * Default implementation of is_contradiction(F).
 * 
 * @author oreilly
 *
 */
@Beta
public class IsContradiction {
	
	/**
	 * <pre>
	 * is_contradiction(F)
	 * F is a formula
	 * Returns whether F is a contradiction or not
	 * 
	 * let x1, ..., xn be the free variables in F
	 * return whether R_complete_simplify( there exists x1 : ... there exists xn : F ) is "False"
	 * </pre>
	 * 
	 * @param expressionF
	 *            a formula.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return true expression is a contradiction, false otherwise.
	 */
	public static boolean isContradiction(Expression expressionF, RewritingProcess process) {
		boolean result = false;
		
		// Assert input argument
		if (!FormulaUtil.isFormula(expressionF, process)) {
			throw new IllegalArgumentException("F is not a formula:"+expressionF);
		}
		
		Set<Expression> freeVariablesInF = Expressions.freeVariables(expressionF, process);
		
		// let x1, ..., xn be the free variables in F
		// return whether R_complete_simplify( there exists x1 : ... there exists xn : F ) is "False"
		Expression thereExistsX1ToXn = ThereExists.make(new ArrayList<Expression>(freeVariablesInF), expressionF);
		Expression simplifiedResult  = process.rewrite(CardinalityRewriter.R_complete_simplify, thereExistsX1ToXn);
		if (simplifiedResult.equals(Expressions.FALSE)) {
			result = true;
		} 
		
		return result;
	}
}
