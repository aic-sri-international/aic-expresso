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
package com.sri.ai.grinder.library.equality.sat;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.equality.formula.FormulaToDNF;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.util.collect.DisjointSets;

@Beta
public class EqualityLogicSATSolver implements SATSolver {

	//
	// START-SATSolver
	@Override
	public String getName() {
		return "Equality";
	}
	
	@Override
	public boolean isSatisfiable(Expression formula, RewritingProcess process) {
		boolean result = false;
		
		Expression dnf = FormulaToDNF.convertToDNF(formula, process);
		if (dnf.equals(Expressions.TRUE)) {
			result = true;
		}
		else if (!dnf.equals(Expressions.FALSE)) {
			for (Expression conjunct : dnf.getArguments()) {
				Set<Expression> consts = FormulaUtil.getConstants(conjunct, process);
				Set<Expression> vars   = Expressions.getVariables(conjunct, process);
				Set<Expression> all    = new LinkedHashSet<Expression>();
				all.addAll(consts);
				all.addAll(vars);
				
				//
				result = true;
				DisjointSets<Expression> disjointSets = new DisjointSets<Expression>(all);
				for (Expression positiveLiteral : FormulaUtil.getPositiveLiterals(conjunct, process)) {
					disjointSets.union(positiveLiteral.get(0), positiveLiteral.get(1));
				}
				Map<Expression, Set<Expression>> elementToDisjointSet = disjointSets.getElementToDisjointSet();
				// Check if inequalities are in the same set
				for (Expression negativeLiteral : FormulaUtil.getNegativeLiterals(conjunct, process)) {
					Set<Expression> setL = elementToDisjointSet.get(negativeLiteral.get(0));
					Set<Expression> setR = elementToDisjointSet.get(negativeLiteral.get(1));
					if (setL.equals(setR)) {
						result = false;
						break;
					}
				}
				// Check if more than 1 constant in any of the sets
				if (result) {
					for (Set<Expression> s : elementToDisjointSet.values()) {
						int bSize = s.size();
						if (s.removeAll(consts)) {
							// More than 1 constant
							if ((bSize - s.size()) > 1) {
								result = false;
								break;
							}
						}
					}
				}
				// Note: cannot be used further as the above logic removes
				// all the constants as part of its validation logic.
				elementToDisjointSet = null;
				
				if (result) {
					break;
				}
			}
		}
		
		return result;
	}
	
	// END-SATSolver
	//
}
