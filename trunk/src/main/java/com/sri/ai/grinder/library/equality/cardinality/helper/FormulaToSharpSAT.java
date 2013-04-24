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

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.SubExpressionSelection;
import com.sri.ai.grinder.library.equality.formula.FormulaToCNF;

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
		
		if (constIds.size() > domainSize) {
			throw new IllegalArgumentException("Domain size too small to represent constants : "+constIds.keySet());
		}
		else if (constIds.size() < domainSize) {
			// Extend with additional constants
// TODO			
		}
		
		conversionListener.start(varIds.size() * domainSize);

// TODO
		//
		// Describe the domain
		
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
					
					
					clause[current] = ((varId * domainSize) + constId) * multiplier;
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
		Set<Expression> consts = new LinkedHashSet<Expression>();
		
		Iterator<Expression> subExpressionsIterator =  new SubExpressionsDepthFirstIterator(formula);
		while (subExpressionsIterator.hasNext()) {
			Expression expression = subExpressionsIterator.next();
			if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
				for (Expression term : expression.getArguments()) {
					if (process.isConstant(term)) {
						consts.add(term);
					}
				}
			}
		}
		
		Map<Expression, Integer> constIds = new LinkedHashMap<Expression, Integer>();
		int id = 0;
		for (Expression cons : consts) {
			constIds.put(cons, id++);
		}
		
		return constIds;
	}
	
	private static Map<Expression, Integer> getVariables(Expression formula, final RewritingProcess process) {
		Set<Expression> vars   = new LinkedHashSet<Expression>();
		
		vars.addAll(SubExpressionSelection.get(formula, new Predicate<Expression>() {
			@Override
			public boolean apply(Expression arg) {
				return process.isVariable(arg);
			}
		}));
		
		Map<Expression, Integer> varIds = new LinkedHashMap<Expression, Integer>();
		int id = 0;
		for (Expression var : vars) {
			varIds.put(var, id++);
		}
		
		return varIds;
	}
}
