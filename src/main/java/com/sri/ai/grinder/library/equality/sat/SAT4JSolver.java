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
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.sat4j.core.VecInt;
import org.sat4j.minisat.SolverFactory;
import org.sat4j.specs.ContradictionException;
import org.sat4j.specs.IProblem;
import org.sat4j.specs.ISolver;
import org.sat4j.specs.TimeoutException;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Throwables;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.equality.formula.FormulaToNNF;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.library.equality.formula.PropositionalCNFListener;
import com.sri.ai.grinder.library.equality.formula.helper.NormalizeLiteral;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.Triple;

@Beta
public class SAT4JSolver implements SATSolver {
	
	//
	// START - SATSolver
	@Override
	public String getName() {
		return "SAT4J";
	}
	
	@Override
	public boolean isSatisfiable(Expression formula, RewritingProcess process) {		
		
		boolean result = equalityLogicToPropositionalLogicCall(formula, process);
		
		return result;
	}
	// END - SATSolver
	//
	
	//
	// PRIVATE
	//
	private boolean equalityLogicToPropositionalLogicCall(Expression formula, RewritingProcess process) {
		boolean result = false;
		
		// Convert to NNF
		Expression formulaInNNF            = FormulaToNNF.convertToNNF(formula, process);
		
		// Remove Constants
		Expression formulaInNNFNoConstants = removeConstants(formulaInNNF, process);

		if (formulaInNNFNoConstants.equals(Expressions.TRUE)) {
			result = true;
		}
		else if (formulaInNNFNoConstants.equals(Expressions.FALSE)) {
			result = false;
		}
		else {			
			// Equality Logic to Propositional Logic
			Map<Pair<Expression, Expression>, Integer> atomToPropVar = new LinkedHashMap<Pair<Expression, Expression>, Integer>();
			Expression   propositionalFormula = equalityLogicToPropositional(formulaInNNFNoConstants, process, atomToPropVar);
			
			// Convert to linear CNF and call SAT4J
			result                          = convertToLinearCNFAndCallSAT4J(propositionalFormula, atomToPropVar.size(), process);
		}
		
		return result;
	}
	
	private Expression removeConstants(Expression formula, RewritingProcess process) {
		Expression result = formula;
		
		// Replace each constant in formula with a new variable C_i
		Set<Expression>  consts       = FormulaUtil.getConstants(formula, process);
		Set<Expression>  vars         = Expressions.getVariables(formula, process);
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
		
		// Ensure the literals are still ordered correctly (i.e. the newly introduced variables
		// are ordered lexically).
		result = NormalizeLiteral.normalizeLiterals(result, process);
		
		return result;
	}
	
	private Expression equalityLogicToPropositional(Expression formula, RewritingProcess process, Map<Pair<Expression, Expression>, Integer> atomToPropVar) {				
		// Construct propositional skeleton
		Expression propositionalSkeleton = formula.replaceAllOccurrences(new BooleanVariableReplacementFunction(atomToPropVar),  process); 
		
		// Construct the non-polar equality graph G
		NPGraph g = makeNPGraph(formula, process, atomToPropVar);
		
		// Make G chordal
		g = makeChordal(g, atomToPropVar);
		
		// For each triangle in G
		List<Expression> transitivityConstraints = new ArrayList<Expression>();
		for (Triple<Expression, Expression, Expression> triangle : trianglesInG(g, atomToPropVar)) {
			transitivityConstraints.add(Or.make(Not.make(triangle.first), Not.make(triangle.second), triangle.third));
			transitivityConstraints.add(Or.make(Not.make(triangle.first), Not.make(triangle.third),  triangle.second));
			transitivityConstraints.add(Or.make(Not.make(triangle.third), Not.make(triangle.second), triangle.first));
		}
		
		// Return: propositionalSkeleton and transitivityConstraints
		List<Expression> conjuncts = new ArrayList<Expression>();
		conjuncts.add(propositionalSkeleton);
		conjuncts.addAll(transitivityConstraints);		
		Expression result = And.make(conjuncts);
		
		return result;
	}
	
	private boolean convertToLinearCNFAndCallSAT4J(Expression propositionalFormula, int numberVars, RewritingProcess process) {
		boolean result = false;
		
		// Determine the number of auxillary variables required in the translation (i.e. the # of logical gates).
		int numberAuxVars = 0;
		SubExpressionsDepthFirstIterator subEIterator = new SubExpressionsDepthFirstIterator(propositionalFormula);
		while (subEIterator.hasNext()) {
			Expression subE = subEIterator.next();
			if (And.isConjunction(subE) || Or.isDisjunction(subE) || Expressions.hasFunctor(subE, FunctorConstants.NOT)) {
				numberAuxVars++;
			}
		}
		
		int totNumberVars = numberVars + numberAuxVars;
		SAT4JCall sat4jCall = new SAT4JCall();
		sat4jCall.start(totNumberVars);
		
		sat4jCall.processClauseAndContinue(new int[] {totNumberVars});
		
		// We are in NNF so can generate implication as opposed to equivalence constraints.
		OneSidedTseitinEncodingReplacementFunction tseitinEncoding = new OneSidedTseitinEncodingReplacementFunction(sat4jCall, numberVars+1);
		
		Expression before = propositionalFormula;
		Expression after  = propositionalFormula;	
	
		do {
			before = after;
			after  = before.replaceAllOccurrences(tseitinEncoding, process);		
		} while (after != before);
		
		sat4jCall.end(PropositionalCNFListener.EndState.NEEDS_SOLVING);
		
		result = sat4jCall.getResult();
		
		return result;
	}
	
	private NPGraph makeNPGraph(Expression formula, RewritingProcess process, Map<Pair<Expression, Expression>, Integer> atomToPropVar) {
		NPGraph result = new NPGraph();
		
		for (Expression var : Expressions.getVariables(formula, process)) {
			result.addVertex(var);
		}
		
		for (Pair<Expression, Expression> atom : atomToPropVar.keySet()) {
			result.addEdge(atom.first, atom.second);
		}
		
		return result;
	}
	
	private NPGraph makeChordal(NPGraph g, Map<Pair<Expression, Expression>, Integer> atomToPropVar) {
		NPGraph result = new NPGraph(g);
		
		Set<Expression> vertices = new LinkedHashSet<Expression>(g.getVertices());
		for (Expression v : vertices) {
			List<Expression> neighbours = new ArrayList<Expression>(g.getNeighbours(v));
			for (int i = 0; i < neighbours.size(); i++) {
				Expression ni = neighbours.get(i);
				for (int j = i+1; j < neighbours.size(); j++) {
					Expression nj = neighbours.get(j);
					if (!g.getNeighbours(ni).contains(nj)) {						
						Pair<Expression, Expression> key = null;
						if (ni.toString().compareTo(nj.toString()) < 0) {
							key = new Pair<Expression, Expression>(ni, nj);
						}
						else {
							key = new Pair<Expression, Expression>(nj, ni);
						}
						
						if (!atomToPropVar.containsKey(key)) {
							result.addEdge(ni, nj);
							int propId = atomToPropVar.size()+1;
							atomToPropVar.put(key, propId);
						}
					}
				}
			}
			g.removeVertex(v);
		}
		
		return result;
	}
	
	private List<Triple<Expression, Expression, Expression>> trianglesInG(NPGraph g, Map<Pair<Expression, Expression>, Integer> atomToPropVar) {
		List<Triple<Expression, Expression, Expression>> result = new ArrayList<Triple<Expression, Expression, Expression>>();
		
		List<Pair<Expression, Expression>> atoms = sortAtoms(atomToPropVar.keySet());
		
		for (int i = 0; i < atoms.size(); i++) {
			Pair<Expression, Expression> atomI = atoms.get(i);
			for (int j = i+1; j < atoms.size(); j++) {
				Pair<Expression, Expression> atomJ = atoms.get(j);
				if (atomI.second.equals(atomJ.first)) {
					for (int k = i+1; k < atoms.size(); k++) {
						if (k != j) {
							Pair<Expression, Expression> atomK = atoms.get(k);
							if (atomI.first.equals(atomK.first)  &&
							    atomJ.second.equals(atomK.second)   ) {
								result.add(new Triple<Expression, Expression, Expression>(
											DefaultSymbol.createSymbol(atomToPropVar.get(atomI)),
											DefaultSymbol.createSymbol(atomToPropVar.get(atomJ)),
											DefaultSymbol.createSymbol(atomToPropVar.get(atomK))
										));
							}
						}
					}
				}
			}
		}
		
		return result;
	}
	
	private List<Pair<Expression, Expression>> sortAtoms(Collection<Pair<Expression, Expression>> atoms) {
		List<Pair<Expression, Expression>> sortedAtoms = new ArrayList<Pair<Expression, Expression>>(atoms);
		
		Collections.sort(sortedAtoms, new Comparator<Pair<Expression, Expression>>() {
			@Override
			public int compare(Pair<Expression, Expression> c1, Pair<Expression, Expression> c2) {
				int result = c1.first.toString().compareTo(c2.first.toString());
				if (result == 0) {
					result = c1.second.toString().compareTo(c2.second.toString());
				}
				return result;
			}
		});
		
		return sortedAtoms;
	}
	
	private Expression newVariable(Set<Expression> existingVariables, int suffixIdx) {
		
		Expression result = null;
		
		do {
			result = DefaultSymbol.createSymbol("C"+suffixIdx);
			suffixIdx++;
		} while (existingVariables.contains(result));
		
		existingVariables.add(result);
		
		return result;
	}
	
	private class NPGraph { 
		private Map<Expression, Set<Expression>> verticesAndNeighbours = new LinkedHashMap<Expression, Set<Expression>>();
		
		public NPGraph() {
			
		}
		
		public NPGraph(NPGraph g) {
			for (Map.Entry<Expression, Set<Expression>> entry : g.verticesAndNeighbours.entrySet()) {
				verticesAndNeighbours.put(entry.getKey(), new LinkedHashSet<Expression>(entry.getValue()));
			}
		}
		
		public Set<Expression> getVertices() {
			return verticesAndNeighbours.keySet();
		}
		
		public Set<Expression> getNeighbours(Expression vertex) {
			return verticesAndNeighbours.get(vertex);
		}
		
		public void addVertex(Expression vertex) {
			if (!verticesAndNeighbours.containsKey(vertex)) {
				verticesAndNeighbours.put(vertex, new LinkedHashSet<Expression>());
			}
		}
		
		public void removeVertex(Expression vertex) {
			verticesAndNeighbours.remove(vertex);
			for (Set<Expression> neighbours : verticesAndNeighbours.values()) {
				neighbours.remove(vertex);
			}
		}
		
		public void addEdge(Expression v1, Expression v2) {
			verticesAndNeighbours.get(v1).add(v2);
			verticesAndNeighbours.get(v2).add(v1);
		}
	} 
	
	private class OneSidedTseitinEncodingReplacementFunction implements Function<Expression, Expression> {
		private String GATE_FUNCTOR = "g";
		
		private boolean   done         = false;
		private SAT4JCall sat4jCall;
		private int       nextAuxVarid;
		
		public OneSidedTseitinEncodingReplacementFunction(SAT4JCall sat4jCall, int auxVarStartId) {
			this.sat4jCall    = sat4jCall;
			this.nextAuxVarid = auxVarStartId;
		}
		
		@Override
		public Expression apply(Expression expression) {
			Expression result = expression;
			
			if (!done) {
				if (convertToGate(expression)) {
					if (And.isConjunction(expression)) {
						for (Expression arg : expression.getArguments()) {
							int[] clause = new int[2];
							clause[0] = this.nextAuxVarid*-1;
							if (Expressions.hasFunctor(arg, GATE_FUNCTOR)) {
								clause[1] = ((Symbol)arg.get(0)).intValue();
							}
							else {
								clause[1] = ((Symbol)arg).intValue();
							}
							
							if (!sat4jCall.processClauseAndContinue(clause)) {								
								done = true;
								break;
							}
						}
					}
					else if (Or.isDisjunction(expression)) {
						// Or
						int[] clause = new int[expression.numberOfArguments()+1];
						clause[0] =  this.nextAuxVarid*-1;
						int cIdx = 1;
						for (Expression arg : expression.getArguments()) {
							clause[0] = this.nextAuxVarid*-1;
							if (Expressions.hasFunctor(arg, GATE_FUNCTOR)) {
								clause[cIdx] = ((Symbol)arg.get(0)).intValue();
							}
							else {
								clause[cIdx] = ((Symbol)arg).intValue();
							}
							cIdx++;
						}
						if (!sat4jCall.processClauseAndContinue(clause)) {							
							done = true;
						}
					}
					else {
						// Not
						int[] clause = new int[2];
						clause[0] =  this.nextAuxVarid*-1;
						Expression arg = expression.get(0);
						clause[1] = ((Symbol)arg).intValue()*-1;
						if (!sat4jCall.processClauseAndContinue(clause)) {							
							done = true;
						}
					}
					result = Expressions.make(GATE_FUNCTOR, DefaultSymbol.createSymbol(this.nextAuxVarid));
					this.nextAuxVarid++;
				}
			}
			
			return result;
		}
		
		private boolean convertToGate(Expression expression) {
			boolean result = false;
			
			if (And.isConjunction(expression) || Or.isDisjunction(expression) || Expressions.hasFunctor(expression, FunctorConstants.NOT)) {
				result = true;
				for (Expression arg : expression.getArguments()) {
					// Only convert to a gate if all arguments are variables are gates only
					if (And.isConjunction(arg) || Or.isDisjunction(arg) || Expressions.hasFunctor(arg, FunctorConstants.NOT)) {
						result = false;
						break;
					}
				}
			}
			
			return result;
		}
	}
	
	private class BooleanVariableReplacementFunction implements Function<Expression, Expression> {
		private Map<Pair<Expression, Expression>, Integer> atomToPropVar = new LinkedHashMap<Pair<Expression, Expression>, Integer>();
		
		public BooleanVariableReplacementFunction(Map<Pair<Expression, Expression>, Integer> atomToPropVar) {
			this.atomToPropVar = atomToPropVar;
		}
		
		@Override
		public Expression apply(Expression expression) {
			Expression result = expression;
			
			if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
				Pair<Expression, Expression> key = new Pair<Expression, Expression>(expression.get(0), expression.get(1));
				Integer propVarId = atomToPropVar.get(key);
				if (propVarId == null) {
					propVarId = atomToPropVar.size()+1;
					atomToPropVar.put(key, propVarId);
				}
				if (Equality.isEquality(expression)) {
					result = DefaultSymbol.createSymbol(propVarId);
				}
				else {
					result = Not.make(DefaultSymbol.createSymbol(propVarId));
				}
			}
			
			return result;
		}
 	}
	
	private class SAT4JCall implements PropositionalCNFListener {
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
			boolean processMore = false;
			if (result == null) {
				try {	
					VecInt vClause = new VecInt(clause);				
					sat4jSolver.addClause(vClause);
					processMore = true;
				} catch (ContradictionException cex) {
					result     = false;
				}
			}
			return processMore;
		}
		
		@Override
		public void end(PropositionalCNFListener.EndState state) {
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
