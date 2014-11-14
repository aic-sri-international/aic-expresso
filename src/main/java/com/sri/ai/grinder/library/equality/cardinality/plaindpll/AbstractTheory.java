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

import static com.sri.ai.expresso.helper.Expressions.freeVariablesAndTypes;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.getIndexExpressionsFromSymbolsAndTypes;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

@Beta
/** 
 * Basic implementation of some methods of {@link Theory}.
 */
abstract public class AbstractTheory implements Theory {

	/**
	 * Simplifies an expression by exhaustively simplifying its top expression with basic boolean operators in equality logic (including quantifier elimination),
	 * then simplifying its sub-expressions,
	 * and again exhaustively simplifying its top expression.
	 * @param expression
	 * @param topSimplifier
	 * @param process
	 * @return
	 */
	@Override
	public Expression simplify(Expression expression, RewritingProcess process) {
		Expression result = simplify(expression, (e, p) -> topSimplifyExhaustively(e, p), process);
		return result;
	}


	/**
	 * Simplifies an expression by exhaustively simplifying its top expression with given top simplifier, then simplifying its sub-expressions,
	 * and again exhaustively simplifying its top expression.
	 * @param expression
	 * @param topSimplifier
	 * @param process
	 * @return
	 */
	protected static Expression simplify(
			Expression expression,
			BinaryFunction<Expression, RewritingProcess, Expression> topSimplifier,
			RewritingProcess process) {
		
		Expression result = expression;
		result = topSimplifier.apply(result, process);
		if (result.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			List<Expression> originalArguments = result.getArguments();
			ArrayList<Expression> simplifiedArguments =
					Util.mapIntoArrayList(originalArguments, e -> simplify(e, topSimplifier, process));
			if ( ! Util.sameInstancesInSameIterableOrder(originalArguments, simplifiedArguments)) { // this check speeds cardinality algorithm by about 25%; it is also required for correctness wrt not returning a new instance that is equal to the input.
				result = Expressions.apply(result.getFunctor(), simplifiedArguments);
			}
			result = topSimplifier.apply(result, process);
		}
	
		return result;
	}

	/**
	 * Simplifies the top expression of an equality-logic-with-quantifiers formula until it cannot be simplified anymore.
	 * Always returns either a symbol or a function application (quantified formulas have their top quantifiers eliminated).
	 * @param formula
	 * @param process
	 * @return
	 */
	protected Expression topSimplifyExhaustively(Expression formula, RewritingProcess process) {
		
		Expression previous;
		do {
			formula = topSimplifyOnce(previous = formula, process);
		} while (formula != previous);
		
		return formula;
	}

	private Expression topSimplifyOnce(Expression formula, RewritingProcess process) {
		BinaryFunction<Expression, RewritingProcess, Expression> simplifier;
		if (formula.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
			simplifier = getFunctionApplicationSimplifiers().get(formula.getFunctor().getValue());
		}
		else {
			simplifier = getSyntacticFormTypeSimplifiers().get(formula.getSyntacticFormType());
		}
		
		if (simplifier != null) {
			formula = simplifier.apply(formula, process);
		}
		
		return formula;
	}

	public static Expression simplifySolutionUnderConstraint(Expression solution, Expression constraint, RewritingProcess process) {
		Expression result = null;
		
		if (constraint.equals(Expressions.TRUE)) {
			result = solution;
		}
		else {
			result = simplifySolutionUnderNonTrivialConstraint(solution, constraint, process);
		}
	
		return result;
	}

	public static Expression simplifySolutionUnderNonTrivialConstraint(Expression solution, Expression constraint, RewritingProcess process) {
		Expression result = null;
		
		if (IfThenElse.isIfThenElse(solution)) {
			Expression newCondition = impliesExpressionOrItsNegationOrNeither(IfThenElse.getCondition(solution), constraint, process);
			if (newCondition.equals(Expressions.TRUE)) {
				result = simplifySolutionUnderNonTrivialConstraint(IfThenElse.getThenBranch(solution), constraint, process);
			}
			else if (newCondition.equals(Expressions.FALSE)) {
				result = simplifySolutionUnderNonTrivialConstraint(IfThenElse.getElseBranch(solution), constraint, process);
			}
			else {
				Expression newThenBranch = simplifySolutionUnderNonTrivialConstraint(IfThenElse.getThenBranch(solution), constraint, process);
				Expression newElseBranch = simplifySolutionUnderNonTrivialConstraint(IfThenElse.getElseBranch(solution), constraint, process);
				result = IfThenElse.makeIfDistinctFrom(solution, newCondition, newThenBranch, newElseBranch, false);
			}
		}
		else {
			result = solution;
		}
		return result;
	}

	/**
	 * Provides a map from functors's getValue() values (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified formula
	 * according to this theory.
	 * DPLL will use these simplifiers when a new decision is made and literals are replaced by boolean constants. 
	 * @return
	 */
	abstract protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers();

	/**
	 * Provides a map from syntactic form types (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified formula
	 * according to this theory.
	 * DPLL will use these simplifiers when a new decision is made and literals are replaced by boolean constants. 
	 * @return
	 */
	abstract protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers();
	/**
	 * Returns 'true' if expression is tautologically implied by constraint,
	 * 'false' if its negation is tautologically implied by constraint,
	 * and expression itself otherwise.
	 * @param expression
	 * @param constraint
	 * @param process
	 * @return
	 */
	public static Expression impliesExpressionOrItsNegationOrNeither(Expression expression, Expression constraint, RewritingProcess process) {
		Expression result = null;
	
		Expression constraintImpliesExpression = Implication.make(constraint, expression);
		List<Expression> freeVariablesIndexExpressions = getIndexExpressionsFromSymbolsAndTypes(freeVariablesAndTypes(constraintImpliesExpression, process));
	
		Expression closedConstraintImpliedExpression = new DefaultUniversallyQuantifiedFormula(freeVariablesIndexExpressions, constraintImpliesExpression);
		Expression alwaysImpliesExpression = (new PlainTautologicalityDPLL()).rewrite(closedConstraintImpliedExpression, process);
		if (alwaysImpliesExpression.equals(Expressions.TRUE)) {
			result = Expressions.TRUE;
		}
		else {
			Expression constraintImpliesNegationOfExpression = Implication.make(constraint, Not.make(expression));
			Expression closedConstraintImpliesNegationOfExpression = new DefaultUniversallyQuantifiedFormula(freeVariablesIndexExpressions, constraintImpliesNegationOfExpression);
			Expression alwaysImpliesNegationOfExpression = (new PlainTautologicalityDPLL()).rewrite(closedConstraintImpliesNegationOfExpression, process);
			if (alwaysImpliesNegationOfExpression.equals(Expressions.TRUE)) {
				result = Expressions.FALSE;
			}
			else {
				result = expression;
			}
		}
	
		return result;
	}
	
	@Override
	public Expression applySplitterToSolution(Expression splitter, Expression solution, RewritingProcess process) {
		Expression result;
		if (IfThenElse.isIfThenElse(solution)) {
			TheoryConstraint constraint = makeConstraint(Util.list(), process);
			constraint = constraint.applySplitter(splitter, Util.list(), process);
			result = applyConstraintToSolution(constraint, solution, process);
		}
		else {
			result = solution;
		}
		return result;
	}
	
	@Override
	public Expression applySplitterNegationToSolution(Expression splitter, Expression solution, RewritingProcess process) {
		Expression result;
		if (IfThenElse.isIfThenElse(solution)) {
			TheoryConstraint constraint = makeConstraint(Util.list(), process);
			constraint = constraint.applySplitterNegation(splitter, Util.list(), process);
			result = applyConstraintToSolution(constraint, solution, process);
		}
		else {
			result = solution;
		}
		return result;
	}
	
	private Expression applyConstraintToSolution(TheoryConstraint constraint, Expression solution, RewritingProcess process) {
		Expression result;
		
		if (IfThenElse.isIfThenElse(solution)) {
			Expression solutionSplitter = IfThenElse.getCondition(solution);
			TheoryConstraint constraintUnderSolutionSplitter = constraint.applySplitter(solutionSplitter, Util.list(), process);
			if (constraintUnderSolutionSplitter != null) {
				TheoryConstraint constraintUnderSolutionSplitterNegation = constraint.applySplitterNegation(solutionSplitter, Util.list(), process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression thenBranch = IfThenElse.getThenBranch(solution);
					Expression elseBranch = IfThenElse.getElseBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter,         thenBranch, process);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					result = IfThenElse.makeIfDistinctFrom(solution, solutionSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
				}
				else {
					Expression thenBranch = IfThenElse.getThenBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter, thenBranch, process);
					result = newThenBranch;
				}
			}
			else {
				TheoryConstraint constraintUnderSolutionSplitterNegation = constraint.applySplitterNegation(solutionSplitter, Util.list(), process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression elseBranch = IfThenElse.getElseBranch(solution);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					result = newElseBranch;
				}
				else {
					throw new Error("Constraint applied to solution should be compatible with the splitter or its negation (otherwise either the constraint is unsatisfiable, or the sub-solution is, and in this case we should not have gotten here).");
				}
			}
		}
		else {
			result = solution;
		}
		
		return result;
	}
}