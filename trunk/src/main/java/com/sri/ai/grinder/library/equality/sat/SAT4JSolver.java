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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IProblem;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;

import com.google.common.annotations.Beta;
import com.google.common.base.Throwables;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Variables;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.equality.cardinality.helper.FormulaToSharpSAT;
import com.sri.ai.grinder.library.equality.cardinality.helper.FormulaToSharpSAT.EndState;
import com.sri.ai.grinder.library.equality.formula.FormulaToNNF;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;

@Beta
public class SAT4JSolver implements SATSolver {

	private boolean useSharpSAT = true;
	
	//
	// START - SATSolver
	@Override
	public String getName() {
		return "SAT4J";
	}
	
	@Override
	public boolean isSatisfiable(Expression formula, RewritingProcess process) {		
		boolean result;
		
		if (useSharpSAT) {
// TODO - the SharpSAT conversion is overkill for satisfiablity testing
// as it generates a propositional grounding intended to be count equivalent
// as opposed to juse equisatisfiable. Replace with more efficient grounding
// as detailed in Chapter 4 of Decision Procedures book.
			SAT4JCall sat4jCall = new SAT4JCall();
			FormulaToSharpSAT.convertToSharpSAT(formula, process, sat4jCall);
			result = sat4jCall.getResult();
		}
		else {
			result = equalityLogicToPropositionalLogicCall(formula, process);
		}
		
		return result;
	}
	// END - SATSolver
	//
	
	//
	// PRIVATE
	//
	private boolean equalityLogicToPropositionalLogicCall(Expression formula, RewritingProcess process) {
		boolean result = false;
		
		// Remove Constants
		Expression formulaNoConstants = removeConstants(formula, process);
		// Convert to NNF
		Expression formulaNNF         = FormulaToNNF.convertToNNF(formulaNoConstants, process);
		
		if (formulaNNF.equals(Expressions.TRUE)) {
			result = true;
		}
		else if (formulaNNF.equals(Expressions.FALSE)) {
			result = false;
		}
		else {	
// TODO			
			// Equality Logic to Propositional Logic
//			Expression propositionalFormula = equalityLogicToPropositional(formulaNNF, process);
		
			// Convert to linear CNF and call SAT4J
//			result                          = convertToLinearCNFAndCallSAT4J(propositionalFormula, process);
		}
		
		return result;
	}
	
	private Expression removeConstants(Expression formula, RewritingProcess process) {
		Expression result = formula;
		
		// Replace each constant in formula with a new variable C_i
		Set<Expression>  consts       = FormulaUtil.getConstants(formula, process);
		Set<Expression>  vars         = Variables.get(formula, process);
		List<Expression> newConstVars = new ArrayList<Expression>(); 		
		for (Expression c : consts) {
			Expression newConstVar = newVariable(vars, newConstVars.size()+1);
			
			newConstVars.add(newConstVar);
			
			result = result.replaceAllOccurrences(c, newConstVar, process);
		}
		
		// For each pair of constants ci, cj such that 1 <= i < j <=n, add the constraint
		// C_i != C_j to result
		List<Expression> conjuncts = new ArrayList<Expression>();
		conjuncts.add(result);
		for (int i = 0; i < newConstVars.size(); i++) {
			for (int j = i+1; j < newConstVars.size(); j++) {
				conjuncts.add(Disequality.make(newConstVars.get(i), newConstVars.get(j)));
			}
		}
		
		result = And.make(conjuncts);
		
		return result;
	}
	
//	private Expression equalityLogicToPropositional(Expression formula, RewritingProcess process) { 
//		TotalRewriter propositionalRewriter = new TotalRewriter(Arrays.asList((Rewriter)
//				new BooleanVariableRewriter()
//			));
//		Expression result = propositionalRewriter.rewrite(formula, process);	
//		return result;
//		
//		return result;
//	}
	
	private Expression newVariable(Set<Expression> existingVariables, int suffixIdx) {
		
		Expression result = null;
		
		do {
			result = DefaultSymbol.createSymbol("C"+suffixIdx);
			suffixIdx++;
		} while (existingVariables.contains(result));
		
		existingVariables.add(result);
		
		return result;
	}
	
	private class SAT4JCall implements FormulaToSharpSAT.ConversionListener {
		private Boolean result      = null;
		private ISolver sat4jSolver = null;
		
		public boolean getResult() {
			return result;
		}
		
		@Override
		public void start(int numberVariables) {
			result      = null;
			sat4jSolver = SolverFactory.newDefault();
			
			sat4jSolver.newVar(numberVariables);
		}
		
		@Override
		public boolean processClauseAndContinue(int[] clause) {
			boolean processMore = true;
			if (result == null) {
				try {				
					sat4jSolver.addClause(new VecInt(clause));
				} catch (ContradictionException cex) {				
					result     = false;
					processMore = false;
				}
			}
			return processMore;
		}
		
		@Override
		public void end(FormulaToSharpSAT.EndState state) {
			if (result == null) {
				if (state == EndState.TRIVIAL_TAUTOLOGY) {
					result = true;
				}
				else  if (state == EndState.TRIVIAL_CONTRADICTION) {
					result = false;
				}
				else {
					// Needs solving.
					try {
						IProblem problem = sat4jSolver;
						result = problem.isSatisfiable();
					} catch (TimeoutException toe) {
						Throwables.propagate(toe);
					}
				}
			}
		}
	}
}
