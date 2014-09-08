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
package com.sri.ai.grinder.library.equality.cardinality.core;

import java.util.LinkedHashMap;
import java.util.Map;

import com.sri.ai.brewer.api.Parser;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;

public class CountsDeclaration implements CardinalityTypeOfLogicalVariable.TypeSizeOfLogicalVariable {
	private Integer  allCounts          = null;
	private String[] variableCountPairs = null;
	private Map<Expression, Integer> variableTypeSizes = new LinkedHashMap<Expression, Integer>();
	private Parser parser;
	
	public CountsDeclaration(int allCounts) {
		this.allCounts = allCounts;
	}
	
	public void setParser(Parser parser) {
		this.parser = parser;
	}
	
	public CountsDeclaration(String... variableCountPairs) {
		this.variableCountPairs = variableCountPairs;
		if (variableCountPairs.length % 2 != 0) {
			throw new IllegalArgumentException("not pairs!");
		}
	}
	
	//
	// START-TypeSizeOfLogicalVariable
	@Override
	public Integer size(Expression logicalVariable, RewritingProcess process) {
		Integer result = null;
		if (allCounts != null) {
			result = allCounts;
		} 
		else {
			result = variableTypeSizes.get(logicalVariable);
		}
		return result;
	}
	// END-TypeSizeOfLogicalVariable
	//
	
	public void setup(RewritingProcess process) {			
		if (variableCountPairs != null) {
			for (int i = 0; i < variableCountPairs.length; i += 2) {
				Expression var  = parser.parse(variableCountPairs[i]);
				Integer    size = new Integer(variableCountPairs[i+1]);
				variableTypeSizes.put(var, size);
			}
		}
		CardinalityTypeOfLogicalVariable.registerTypeSizeOfLogicalVariableWithProcess(this, process);
	} 
}

