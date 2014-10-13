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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.Collection;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSetInterface;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Simplify;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Equals;
import com.sri.ai.util.base.Not;
import com.sri.ai.util.base.Pair;

@Beta
/** 
 * A DPLL specialization for model counting.
 */
public class PlainCardinalityDPLLWithFreeVariables extends PlainGenericDPLLWithFreeVariables {
	
	protected CountsDeclaration countsDeclaration;

	/**
	 * Builds a rewriter for cardinality computation.
	 */
	public PlainCardinalityDPLLWithFreeVariables(CountsDeclaration countsDeclaration) {
		this.countsDeclaration = countsDeclaration;
	}

	@Override
	protected Pair<Expression, List<Expression>> getFormulaAndIndicesFromRewriterProblemArgument(Expression set, RewritingProcess process) {
		List<Expression> indices = IndexExpressions.getIndices(((IntensionalSetInterface) set).getIndexExpressions());
		Expression formula = SimplifyFormula.simplify(((IntensionalSetInterface) set).getCondition(), process);
		Pair<Expression, List<Expression>> formulaAndIndices = Pair.make(formula, indices);
		return formulaAndIndices;
	}

	@Override
	public RewritingProcess makeRewritingProcess(Expression expression) {
		Rewriter rewriterWithModules = new Simplify();
		RewritingProcess result = new DefaultRewritingProcess(expression, rewriterWithModules);
		result.notifyReadinessOfRewritingProcess();
		if (countsDeclaration != null) {
			countsDeclaration.setup(result);
		}
		return result;
	}

	@Override
	protected Expression bottomSolution() {
		return Expressions.ZERO;
	}

	@Override
	protected boolean isTopSolution(Expression solutionForSubProblem) {
		return false;
	}

	@Override
	protected Expression combineUnconditionalSolutions(Expression solution1, Expression solution2) {
		return Expressions.makeSymbol(solution1.rationalValue().add(solution2.rationalValue()));
	}

	@Override
	protected TheoryConstraint makeConstraint(Expression atomsConjunction, Collection<Expression> indices, RewritingProcess process) {
		SymbolEqualityModelCountingConstraint result = new SymbolEqualityModelCountingConstraint(atomsConjunction, indices, process);
		return result;
	}

	@Override
	protected Expression expressionIsSplitterCandidate(Expression subExpression, Collection<Expression> indices, RewritingProcess process) {
		Expression result = null;
		if (subExpression.hasFunctor(FunctorConstants.EQUALITY) || subExpression.hasFunctor(FunctorConstants.DISEQUALITY)) {
	
			Expression variable = Util.getFirstSatisfyingPredicateOrNull(subExpression.getArguments(), new IsVariable(process));
	
			Expression otherTerm = Util.getFirstSatisfyingPredicateOrNull(
					subExpression.getArguments(), Not.make(Equals.make(variable)));
	
			result = makeSplitterWithIndexIfAnyComingFirst(variable, otherTerm, indices);
		}
		return result;
	}
	
	@Override
	protected boolean splitterIsOnFreeVariablesOnly(Expression splitter, Collection<Expression> indices) {
		boolean result = ! indices.contains(splitter.get(0));
		return result;
	}

	@Override
	protected Expression applySplitterTo(Expression formula, Expression splitter, RewritingProcess process) {
		Expression result = SimplifyFormula.simplifyGivenEquality(formula, splitter, process);
		return result;
	}

	@Override
	protected Expression applySplitterNegationTo(Expression formula, Expression splitter, RewritingProcess process) {
		Expression result = SimplifyFormula.simplifyGivenDisequality(formula, splitter, process);
		return result;
	}

	@Override
	protected Expression completeSimplifySolutionGivenSplitter(Expression solution, Expression splitter, RewritingProcess process) {
		Expression result = SimplifyFormula.completeSimplifySolutionGivenEquality(solution, splitter, process);
		return result;
	}

	@Override
	protected Expression completeSimplifySolutionGivenSplitterNegation(Expression solution, Expression splitter, RewritingProcess process) {
		Expression result = SimplifyFormula.completeSimplifySolutionGivenEqualityNegation(solution, splitter, process);
		return result;
	}

	@Override
	protected Collection<Expression> getIndicesUnderSplitter(Expression splitter, Collection<Expression> indices) {
		Expression variable  = splitter.get(0);
		Collection<Expression> result = ! indices.contains(variable)? indices : Util.makeSetWithoutExcludedElement(indices, variable);
		return result;
	}

	@Override
	protected Collection<Expression> getIndicesUnderSplitterNegation(Expression splitter, Collection<Expression> indices) {
		return indices;
	}

	protected static Expression makeSplitterWithIndexIfAnyComingFirst(Expression variable, Expression otherTerm, Collection<Expression> indices) {
		Expression result;
		// if variable is a free variable or constant and other term is an index, we invert them because
		// the algorithm requires the first term to be an index if there are any indices in the atom.
		if ( ! indices.contains(variable) && indices.contains(otherTerm) ) {
			result = Equality.make(otherTerm, variable);
		}
		else {
			result = Equality.make(variable, otherTerm);
		}
		return result;
	}
	
//	protected static Expression applySplitterToCompleteSolution(Expression solution, Expression splitter, RewritingProcess process) {
//		Expression result = null;
//		
//		if (Equality.isEquality(solution) || Disequality.isDisequality(solution)) {
//			result = SimplifyFormula.applyEqualityTo(solution, splitter, process);
//		}
//		else if (Expressions.isNumber(solution)) {
//			result = solution;
//		}
//		else if (IfThenElse.isIfThenElse(solution)) {
//			Expression variable  = splitter.get(0);
//			Expression otherTerm = splitter.get(1);
//
//			Expression solutionCondition = IfThenElse.getCondition(solution);
//			Expression solutionVariable  = solutionCondition.get(0);
//			Expression solutionOtherTerm = solutionCondition.get(1);
//			
//			if (solutionVariable.equals(variable)) {
//				// splitter: X = Y solution condition: X = W, with Y and W possibly the same term
//				// In this case, applying X = Y to the solution then branch does not do anything because it does not contain X.
//				// We need instead to apply W = Y since W stands for X in the then branch of solution.
//				
//				Expression conditionAppliedToThenBranch = Equality.makeWithConstantSimplification(solutionOtherTerm /* W */, otherTerm /* Y */, process);
//				if (conditionAppliedToThenBranch.equals(Expressions.TRUE)) { // splitter is irrelevant
//					result = solution;
//				}
//				else if (conditionAppliedToThenBranch.equals(Expressions.FALSE)) { // applied condition renders solution condition false
//					Expression solutionElseBranch = IfThenElse.getElseBranch(solution);
//					result = applySplitterToCompleteSolution(solutionElseBranch, splitter, process);
//				}
//				else {
//					// At this point, either Y or W or both is a variable (two constants would have fallen into the cases above).
//					// In the then branch, in which X = W,
//					// if W is a constant, Y must be a variable, so the splitter is saying variable Y is equal to constant W.
//					// If Y is a constant, the splitter is saying variable W is equal to constant Y.
//					conditionAppliedToThenBranch = Equality.normalize(conditionAppliedToThenBranch, process);
//					Expression solutionThenBranch = IfThenElse.getThenBranch(solution);
//					Expression newThenBranch = applySplitterToCompleteSolution(solutionThenBranch, conditionAppliedToThenBranch, process);
//
//					Expression solutionElseBranch = IfThenElse.getElseBranch(solution);
//					SymbolEqualityModelCountingConstraint constraint = new SymbolEqualityModelCountingConstraint(And.make(splitter, solutionCondition), null, process);
//					Expression newElseBranch = applySplitterToCompleteSolution(solutionElseBranch, constraint, process);
//					
//					result = IfThenElse.make(conditionAppliedToThenBranch, newThenBranch, newElseBranch);
//				}
//			}
//		}
//		
//		return result;
//	}
}
