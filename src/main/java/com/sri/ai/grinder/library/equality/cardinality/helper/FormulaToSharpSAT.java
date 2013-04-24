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
package com.sri.ai.grinder.library.equality.cardinality.helper;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.Variables;
import com.sri.ai.grinder.library.equality.formula.FormulaToCNF;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;

@Beta
public class FormulaToSharpSAT {
	
	public interface ConversionListener {
		void start(int numberVariables);
		void clause(int[] clause);
		void end();
	}
	
	public static void converToSharpSAT(Expression formula, int domainSize, RewritingProcess process, ConversionListener conversionListener) {

		Expression cnfFormula = FormulaToCNF.convertToCNF(formula, process);
		
		Map<Expression, Integer> constIds = getConstants(cnfFormula, process);
		Map<Expression, Integer> varIds   = getVariables(cnfFormula, process);
		
		// Converting the problem to a propositional problem: Assume we are
		// X1=X2 or X1!=a1 where |type(X1)|=|type(X2)|=3. 
		// So, F is "X1=X2 or X1!=a1". First, we name the 
		// other two elements in the domain as a2 and a3. 
		// Then, we define the following propositional variables: 
		//	v1: X1 = a1
		//	v2: X1 = a2
		//	v3: X1 = a3
		//	v4: X2 = a1
		//	v5: X2 = a2
		//	v6: X2 = a3
		
		if (constIds.size() > domainSize) {
			throw new IllegalArgumentException("Domain size too small to represent constants : "+constIds.keySet());
		}
		else if (constIds.size() < domainSize) {
			// Extend with additional constants to represent the full domain size
			int id = 1;
			while (constIds.size() < domainSize) {
				Symbol newConst = DefaultSymbol.createSymbol("a"+id);
				if (!constIds.containsKey(newConst)) {
					constIds.put(newConst, constIds.size()+1);
				}
				id++;
			}
		}
		
		conversionListener.start(varIds.size() * domainSize);

		//
		// Describe the domain
		describeDomain(conversionListener, varIds.size(), domainSize);
		
		//
		// Describe the formula
		for (Expression fClause : cnfFormula.getArguments()) {
			
// TODO - do the rewriter to introduce the appropriate formulas for hard literals
			Expression propFormula = fClause;
			
			Expression cnfPropFormula = FormulaToCNF.convertToCNF(propFormula, process);
			
			for (Expression pClause : cnfPropFormula.getArguments()) {
				int[] clause = new int[pClause.numberOfArguments()];
				int current = 0;
				for (Expression literal : pClause.getArguments()) {
					int multiplier = Equality.isEquality(literal) ? 1 : -1;
					int varId      = varIds.get(literal.get(0));
					int constId    = constIds.get(literal.get(1));
					
					
					clause[current] = (((varId-1) * domainSize) + constId) * multiplier;
					current++;
				}
				conversionListener.clause(clause);
			}
		}
		
		conversionListener.end();
	}
	
	//
	// PRIVATE
	//
	private static Map<Expression, Integer> getConstants(Expression formula, final RewritingProcess process) {
		Set<Expression> consts = FormulaUtil.getConstants(formula, process);
		
		Map<Expression, Integer> constIds = new LinkedHashMap<Expression, Integer>();
		int id = 0;
		for (Expression cons : consts) {
			constIds.put(cons, ++id);
		}
		
		return constIds;
	}
	
	private static Map<Expression, Integer> getVariables(Expression formula, final RewritingProcess process) {
		Set<Expression> vars   = new LinkedHashSet<Expression>();
		
		vars.addAll(Variables.get(formula, process));
		
		Map<Expression, Integer> varIds = new LinkedHashMap<Expression, Integer>();
		int id = 0;
		for (Expression var : vars) {
			varIds.put(var, ++id);
		}
		
		return varIds;
	}
	
	private static void describeDomain(ConversionListener conversionListener, int numVars, int domainSize) {
		// The first series of clauses should determine that 
		// "X1 equals to a1 or a2 or a3". Similarly, we have to specify that 
		// "X2 equals to a1 or a2 or a3". The following clauses describe these:
		// v1 or v2 or v3	
		// v4 or v5 or v6
		for (int v = 0; v < numVars; v++) {
			int[] clause = new int[domainSize];
			for (int i = 0; i < domainSize; i++) {
				clause[i] = (v*domainSize) + i + 1;
			}
			conversionListener.clause(clause);
		}
		
		// Then we have to enforce that X1 and X2 can be at most one of a1, a2, a3. 
		// So if "X1=a1 => X1!=a2" and "X1=a1 => X1!=a3". We then have the following 
		// clauses:
		// -v1 or -v2
		// -v1 or -v3	
		// -v2 or -v3	
		// -v4 or -v5	
		// -v4 or -v6	
		// -v5 or -v6
		for (int b = 0; b < (numVars*domainSize); b += domainSize) {
			for (int d = 0; d < domainSize; d++) {
				int svidx = b + d + 1;
				for (int i = d+1; i < domainSize; i++) {
					int[] clause = new int[2];
					clause[0] = 0 - svidx;
					clause[1] = 0 - svidx - (i-d);
					conversionListener.clause(clause);
				}
			}
		}
	}
}
