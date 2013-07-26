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
package com.sri.ai.grinder.core;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewriterTestAttribute;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;

/**
 * A RewriterTestAttribute used to represent an expression is a formula.
 * Note: This is distinct from KindAttribute as different 'kind' attribute
 * functors can also end up being formulas but not necessarily. This ensures,
 * the attributes and values remain mutually exclusive.
 * 
 * @author oreilly
 *
 */
@Beta
public class IsFormulaAttribute implements RewriterTestAttribute {

	public static final IsFormulaAttribute INSTANCE = new IsFormulaAttribute();
	
	//
	// ALLOWED KNOWN KIND VALUES IN ADVANCE (i.e. functors are determined at runtime).
	
	// i.e. used when the given expression is not a formula
	public static final Object VALUE_IS_NOT_FORMULA = new Object() {
		@Override
		public boolean equals(Object o) {
			return this == o; 
		}
		
		@Override
		public String toString() {
			return "not a formula";
		}
	};
	
	public static final Object VALUE_IS_FORMULA = new Object() {
		@Override
		public boolean equals(Object o) {
			return this == o; 
		}
		
		@Override
		public String toString() {
			return "is formula";
		}
	};
	
	//
	// START-RewriterTestAttribute
	@Override
	public Object getValue(Expression expression, RewritingProcess process) {
		Object result = VALUE_IS_NOT_FORMULA;
		
		if (FormulaUtil.isFormula(expression, process)) {
			result = VALUE_IS_FORMULA;
		}		
		
		return result;
	}
	// END-ReriterTestAttribute
	//
	
	@Override
	public String toString() {
		return "formula";
	}
	
	//
	// PRIVATE
	//
	/**
	 * Private constructor so that only a singleton may be created.
	 */
	private IsFormulaAttribute() {
		
	}
}
