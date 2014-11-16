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

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Simplify;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.base.Pair;

/**
 * A "plain" implementation of symbolic generalized DPLL (without using Grinder-style contexts and simplifications)
 * that is more ad hoc than rewriter-based approaches but also much faster.
 * 
 * This generic skeleton allows the algorithm to be used for different tasks.
 * 
 * @author braz
 *
 */
public abstract class AbstractPlainDPLL extends AbstractHierarchicalRewriter {

	///////////////////////////// BEGINNING OF ABSTRACT METHODS //////////////////////////////////////

	/**
	 * Derives formula and indices to be used from intensional set passed to rewriter as argument.
	 */
	abstract protected Pair<Expression, List<Expression>> getFormulaAndIndexExpressionsFromRewriterProblemArgument(Expression expression, RewritingProcess process);

	/**
	 * Indicates whether given solution for a sub-problem makes the other sub-problem for an index conditioning irrelevant
	 * (for example, the solution 'true' in satisfiability).
	 * Eventually this will be provided by a semiring parameter.
	 */
	protected abstract boolean isTopSolution(Expression solutionForSubProblem);
	
	/**
	 * Combines two unconditional solutions for split sub-problems
	 * (for example, disjunction of two boolean constants in satisfiability, or addition in model counting).
	 * Eventually this will be provided by a semiring parameter.
	 */
	protected abstract Expression additiveOperationOnUnconditionalSolutions(Expression solution1, Expression solution2, RewritingProcess process);

	/**
	 * Multiplies the (possibly symbolic) value of an expression to the number of times an expression evaluates to it
	 * (for example, in summation of numeric expressions this will be the product of two numbers,
	 * in model counting this will be 0 if value is 'false' and numberOfOccurrences if value is 'true',
	 * and in satisfiability it will 'false' if value is 'false' and 'true' if value is 'true' and numberOfOccurrences is greater than 0).
	 * Eventually this will be provided by a semiring parameter.
	 */
	protected abstract Expression additiveOperationAppliedAnIntegerNumberOfTimes(Expression value, Expression numberOfOccurrences, RewritingProcess process);

	

	///////////////////////////// END OF ABSTRACT METHODS //////////////////////////////////////

	///////////////////////////// BEGINNING OF FIXED PART OF GENERIC DPLL //////////////////////////////////////
	
	protected Theory theory;
	
	/** A {@link CountsDeclaration} encapsulating sort size information. */
	protected CountsDeclaration countsDeclaration;
	
	public AbstractPlainDPLL(Theory theory) {
		this.theory = theory;
	}

	public AbstractPlainDPLL(Theory theory, CountsDeclaration countsDeclaration) {
		this.theory = theory;
		this.countsDeclaration = countsDeclaration;
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Pair<Expression, List<Expression>> formulaAndIndexExpressions = getFormulaAndIndexExpressionsFromRewriterProblemArgument(expression, process);
		Expression       formula          = formulaAndIndexExpressions.first;
		List<Expression> indexExpressions = formulaAndIndexExpressions.second;
		Expression       simplifiedFormula = theory.simplify(formula, process); // eventually this will should not be needed as simplification should be lazy 
		RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIndexExpressions(indexExpressions, process);
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		Expression result = solve(simplifiedFormula, indices, subProcess);
		return result;
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

	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices.
	 */
	protected Expression solve(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		TheoryConstraint constraint = theory.makeConstraint(indices, process);
		Expression result = solve(expression, constraint, indices, process);
		return result;
	}

	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices under given constraint,
	 * which must not be <code>null</code> (that is, a contradiction).
	 */
	protected Expression solve(Expression expression, TheoryConstraint constraint, Collection<Expression> indices, RewritingProcess process) {
		Expression result;
		if (constraint != null) {
			Expression splitter = pickSplitter(expression, indices, constraint, process);

			if (splitter != null) {
				result = computeSolutionBasedOnSplittedProblems(splitter, expression, indices, constraint, process);
			}
			else {
				Expression unconditionalValue = expression;
				Expression numberOfOccurrences = constraint.modelCount(indices, process);
				result = additiveOperationAppliedAnIntegerNumberOfTimes(unconditionalValue, numberOfOccurrences, process);
			}
		}
		else {
			throw new Error(this.getClass() + ".solve must not receive a contradiction constraint (a null pointer)");
		}
		return result;
	}

	/** Picks splitter from either expression or constraint; assumes constraint is not <code>null</code>. */
	protected Expression pickSplitter(Expression expression, Collection<Expression> indices, TheoryConstraint constraint, RewritingProcess process) {
		Expression splitter;
		splitter = theory.pickSplitterInExpression(expression, indices, constraint, process);
		if (splitter == null) { // expression is constant value, so it does not have any splitters
			splitter = constraint.pickSplitter(indices, process);
		}
		return splitter;
	}

	protected Expression computeSolutionBasedOnSplittedProblems(Expression splitter, Expression expression, Collection<Expression> indices, TheoryConstraint constraint, RewritingProcess process) {
		// Keep in mind that splitter may already be implied as true or false by theory constraint.
		// This should not happen if the theory application of splitters to expressions only replaced them by true or false,
		// but if it does more than that, those manipulations may create new literals that happen to be implied true or false by constraint.
		// This is verified at this point by applying the splitter to the constraint and checking if it is does not render it unsatisfiable.
		// If it does, only the solution under the splitter's negation is considered.
		// Otherwise, it computes the solution under the splitter and does the same check to the splitter's negation;
		// if the splitter's negation turns the constraint unsatisfiable, we know it is implied false by it
		// and then only the solution under the splitter is taken.
		// This means that the application of the splitter must be done to the constraint first,
		// and only if this constraint is not contradicted do we apply it to the expression and indices.
		// This prevents a more elegant formalization in which the splitter is applied to the three of them,
		// as if conceptually applied to the whole problem at once.
		
		Expression result;

		TheoryConstraint constraintUnderSplitter = constraint.applySplitter(splitter, indices, process);
		if (constraintUnderSplitter != null) {
			Expression solutionUnderSplitter = solveUnderSplitter(splitter, expression, constraintUnderSplitter, indices, process);

			boolean solutionIsConditionalOnSplitterOverFreeVariable = theory.splitterDoesNotInvolveIndex(splitter, indices);
			boolean solutionIsAdditionOfSubSolutions = ! solutionIsConditionalOnSplitterOverFreeVariable;
			if ( solutionIsAdditionOfSubSolutions && isTopSolution(solutionUnderSplitter)) {
				result = solutionUnderSplitter; // solution is already maximum, no need to consider the other side of splitter
			}
			else {
				TheoryConstraint constraintUnderSplitterNegation = constraint.applySplitterNegation(splitter, indices, process);

				if (constraintUnderSplitterNegation != null) {
					Expression solutionUnderSplitterNegation = solveUnderSplitterNegation(splitter, expression, constraintUnderSplitterNegation, indices, process);
					if (solutionIsConditionalOnSplitterOverFreeVariable) {
						// solution is <if splitter then solutionUnderSplitter else solutionUnderSplitterNegation>
						result = IfThenElse.make(splitter, solutionUnderSplitter, solutionUnderSplitterNegation, false /* no simplification to condition */);
					}
					else {
						// splitter is over an index or more,
						// so solution is solutionUnderSplitter + solutionUnderSplitterNegation
						// If unconditional, these solutions need to be combined
						// If conditional, these solutions need to be merged into a single conditional
						result = addSymbolicResults(solutionUnderSplitter, solutionUnderSplitterNegation, process);
					}
				}
				else { // splitter cannot be false under constraint, so it is true under constraint, so final solution is one under splitter
					result = solutionUnderSplitter;
				}
			}
		}
		else {
            // splitter is false under constraint, so its negation is true, so final solution is one under splitter negation
			TheoryConstraint constraintUnderSplitterNegation = constraint.applySplitterNegation(splitter, indices, process);
			Expression solutionUnderSplitterNegation = solveUnderSplitterNegation(splitter, expression, constraintUnderSplitterNegation, indices, process);
			result = solutionUnderSplitterNegation;
		}
		
		return result;
	}

	/**
	 * @param splitter
	 * @param expression
	 * @param constraintUnderSplitter must not be <code>null</code>
	 * @param indices
	 * @param process
	 * @return
	 */
	protected Expression solveUnderSplitter(Expression splitter, Expression expression, TheoryConstraint constraintUnderSplitter, Collection<Expression> indices, RewritingProcess process) {
		Expression expressionUnderSplitter = theory.applySplitterToExpression(splitter, expression, process);
		Collection<Expression> indicesUnderSplitter = theory.applyIndicesUnderSplitter(splitter, indices);
		Expression result = solve(expressionUnderSplitter, constraintUnderSplitter, indicesUnderSplitter, process);
		return result;
	}

	/**
	 * 
	 * @param splitter
	 * @param expression
	 * @param constraintUnderSplitterNegation must not be <code>null</code>
	 * @param indices
	 * @param process
	 * @return
	 */
	protected Expression solveUnderSplitterNegation(Expression splitter, Expression expression, TheoryConstraint constraintUnderSplitterNegation, Collection<Expression> indices, RewritingProcess process) {
		Expression expressionUnderSplitterNegation = theory.applySplitterNegationToExpression(splitter, expression, process);
		Collection<Expression> indicesUnderSplitterNegation = theory.applyIndicesUnderSplitterNegation(splitter, indices);
		Expression result = solve(expressionUnderSplitterNegation, constraintUnderSplitterNegation, indicesUnderSplitterNegation, process);
		return result;
	}

	/**
	 * If solutions are unconditional expressions, simply add them.
	 * If they are conditional (symbolic), perform distributive on conditions.
	 */
	protected Expression addSymbolicResults(Expression solution1, Expression solution2, RewritingProcess process) {

		Expression result = null;

		if (IfThenElse.isIfThenElse(solution1)) {
			Expression condition  = IfThenElse.getCondition(solution1);
			Expression thenBranch = IfThenElse.getThenBranch(solution1);
			Expression elseBranch = IfThenElse.getElseBranch(solution1);
			Expression solution2UnderCondition    = theory.applySplitterToSolution(condition, solution2, process);
			Expression solution2UnderNotCondition = theory.applySplitterNegationToSolution(condition, solution2, process);
			Expression newThenBranch = addSymbolicResults(thenBranch, solution2UnderCondition,    process);
			Expression newElseBranch = addSymbolicResults(elseBranch, solution2UnderNotCondition, process);
			result = IfThenElse.make(condition, newThenBranch, newElseBranch, false /* no simplification to condition */);
		}
		else if (IfThenElse.isIfThenElse(solution2)) {
			Expression condition  = IfThenElse.getCondition(solution2);
			Expression thenBranch = IfThenElse.getThenBranch(solution2);
			Expression elseBranch = IfThenElse.getElseBranch(solution2);
			Expression solution1UnderCondition    = theory.applySplitterToSolution(condition, solution1, process);
			Expression solution1UnderNotCondition = theory.applySplitterNegationToSolution(condition, solution1, process);
			Expression newThenBranch = addSymbolicResults(solution1UnderCondition,    thenBranch, process);
			Expression newElseBranch = addSymbolicResults(solution1UnderNotCondition, elseBranch, process);
			result = IfThenElse.make(condition, newThenBranch, newElseBranch, false /* no simplification to condition */);
		}
		else {
			result = additiveOperationOnUnconditionalSolutions(solution1, solution2, process);
		}

		return result;
	}

	///////////////////////////// END OF FIXED PART OF GENERIC DPLL //////////////////////////////////////
}