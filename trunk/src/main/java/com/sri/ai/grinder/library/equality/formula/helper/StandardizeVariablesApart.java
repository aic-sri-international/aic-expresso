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
package com.sri.ai.grinder.library.equality.formula.helper;

import java.util.HashMap;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;

/**
 * Standardizes Variables apart in a formula:
 * 
 * for all X: (there exists Y: X = Y) or (there exists Y: X != Y)
 * ->
 * for all X: (there exists Y: X = Y) or (there exists Y': X != Y')
 * 
 * Basically quantifiers in the formula using the same index name are made unique.
 */
@Beta
public class StandardizeVariablesApart {

	/**
	 * Standardize the variables in a formula.
	 * 
	 * @param formula
	 *        the formula to have its variables standardized.
	 * @param process
	 * @return a formula with standardized variable names (i.e. unique) across quantifiers in the formula.
	 */
	public static Expression standardizeApart(Expression formula, RewritingProcess process) {
		Expression input  = formula;
		Expression result = formula;
		do {
			input  = result;
			result = input.replaceFirstOccurrence(new StandardizeVariablesApartReplacementFunction(process), process);
		} while (result != input);
		
		return result;
	}

	private static class StandardizeVariablesApartReplacementFunction implements Function<Expression, Expression> {
		
		private RewritingProcess process     = null;
		private Map<Expression, Expression>  seenIndices = new HashMap<Expression, Expression>();
		
		public StandardizeVariablesApartReplacementFunction(RewritingProcess process) {
			this.process = process;
		}
		
		@Override
		public Expression apply(Expression expression) {
			Expression result = expression;
		
			Expression index = null;
			if (ForAll.isForAll(expression)) {
				index = ForAll.getIndex(expression);
			}
			else if (ThereExists.isThereExists(expression)) {
				index = ThereExists.getIndex(expression);
			}
			
			if (index != null) {
				if (seenIndices.containsKey(index)) {
					RewritingProcess saProcess = GrinderUtil.extendContextualVariables(seenIndices, process);
					result = StandardizedApartFrom.standardizedApartFrom(expression, expression, saProcess);
				}
				else {
					seenIndices.put(index, null); // we do not need the domains for standardization apart purposes
				}
			}
			
			return result;
		}
	}
}
