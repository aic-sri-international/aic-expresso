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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;

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
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.Theory.Constraint;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.QuarternaryFunction;

/**
 * A "plain" implementation of symbolic generalized DPLL (without using Grinder-style contexts and simplifications)
 * that is more ad hoc than rewriter-based approaches but also much faster.
 * 
 * This generic skeleton allows the algorithm to be used for different tasks.
 * 
 * @author braz
 *
 */
public class DPLLGeneralizedAndSymbolic extends AbstractHierarchicalRewriter {
	
	/**
	 * A standard version of the algorithm picks a splitter,
	 * splits the problem according to it into two sub-problems,
	 * and adds their sub-solutions.
	 * When splitting on non-index (free) variables,
	 * this introduces, in the sub-problems, conditions on free variables
	 * that will be part of the eventual model count in the leaf (which will be conditional on them).
	 * After that, those conditions will need to be externalized, which can be expensive.
	 * The following flag optimizes this a bit.
	 * When splitting on free variables, it combines the sub-solutions not by adding them,
	 * but by making then the then- and else-branches in an if-then-else expression.
	 * In other words, it externalizes the condition early, instead of leaving it to be
	 * externalized later all the way from the leaf solution.
	 * It is important to notice, however, that the sub-problems still contain this condition
	 * (or its negation) and the final leaf model count will still be conditional on them,
	 * in spite of being in a branch that has that conditional already determined.
	 * If left alone, these leaf-level conditions will need to be externalized all the same
	 * (even though they will eventually be cancelled out by the fact of being already
	 * trivialized in that branch), and there will be no efficiency gain.
	 * Therefore, in order to truly benefit from this early externalization,
	 * it is also important give the sub-problems information about which conditions
	 * are already guaranteed by the path so far, so that it can be used to "nip conditions in the bud",
	 * right at the leaf level, before needing to externalize them.
	 */
	public final static boolean earlyExternalizationOfFreeVariableSplittersOptimization = true; // IMPORTANT: unit tests will break if set to false. However DPLL stress tests can still be used. As of this writing (12/4/2014) the false setting was slightly slower.
	
	/**
	 * The following flag is only used if {@link #earlyExternalizationOfFreeVariableSplittersOptimization} is true.
	 * It chooses between keeping guaranteed splitters to simplify conditional model counts,
	 * or to run a complete simplification of the solution after it's computed.
	 */
	public final static boolean keepGuaranteedSplittersInsteadOfPostSimplifyingSolutions = true;

	/** The background theory for the algorithm. */
	protected Theory theory;
	
	/** The problem type being solved. */
	protected ProblemType problemType;

	/** A {@link CountsDeclaration} encapsulating sort size information. */
	protected CountsDeclaration countsDeclaration;
	
	public DPLLGeneralizedAndSymbolic(Theory theory, ProblemType problemType) {
		this(theory, problemType, null);
	}

	public DPLLGeneralizedAndSymbolic(Theory theory, ProblemType problemType, CountsDeclaration countsDeclaration) {
		this.theory = theory;
		this.problemType = problemType;
		this.countsDeclaration = countsDeclaration;
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Pair<Expression, List<Expression>> formulaAndIndexExpressions = problemType.getExpressionAndIndexExpressionsFromRewriterProblemArgument(expression, process);
		Expression       formula          = formulaAndIndexExpressions.first;
		List<Expression> indexExpressions = formulaAndIndexExpressions.second;
		Expression       simplifiedFormula = theory.simplify(formula, process); // eventually this will should not be needed as simplification should be lazy 
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIndexExpressions(indexExpressions, process);
		subProcess.initializeDPLLContextualConstraint(theory, indices);
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
		Constraint constraint = theory.makeConstraint(indices);
		Expression result = solve(expression, constraint, process);
		if ( earlyExternalizationOfFreeVariableSplittersOptimization && ! keepGuaranteedSplittersInsteadOfPostSimplifyingSolutions) {
			result = theory.applyConstraintToSolution(process.getDPLLContextualConstraint(), result, process);
		}
		return result;
	}

	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices under given constraint,
	 * which is considered a contradiction if it has the value <code>null</code>.
	 */
	protected Expression solve(Expression expression, Constraint constraint, RewritingProcess process) {
		Expression result;
		
		assert constraint != null : this.getClass() + ".solve must not receive a contradiction constraint (a null pointer)";

		Expression splitter = pickSplitter(expression, constraint, process);

		if (splitter != null) {
			result = computeSolutionBasedOnSplitterProblems(splitter, expression, constraint, process);
		}
		else {
			Expression unconditionalValue = expression;
			Expression numberOfOccurrences = constraint.modelCount(process);
			Expression valueToBeSummed = problemType.fromExpressionValueWithoutLiteralsToValueToBeSummed(unconditionalValue);
			result = problemType.addNTimes(valueToBeSummed, numberOfOccurrences, process);
		}

		return result;
	}

	/** Picks splitter from either expression or constraint; assumes constraint is not <code>null</code>. */
	protected Expression pickSplitter(Expression expression, Constraint constraint, RewritingProcess process) {
		Expression splitter;
		splitter = theory.pickSplitterInExpression(expression, constraint, process);
		if (splitter == null) { // expression is constant value, so it does not have any splitters
			splitter = constraint.pickSplitter(process);
		}
		return splitter;
	}

	private Expression computeSolutionBasedOnSplitterProblems(Expression splitter, Expression expression, Constraint constraint, RewritingProcess process) {
		
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
		
		QuarternaryFunction<Expression, Expression, Expression, RewritingProcess, Expression> conditionalCombiner
		= (s, solution1, solution2, p) -> IfThenElse.make(s, solution1, solution2, false /* no simplification to condition */);

		QuarternaryFunction<Expression, Expression, Expression, RewritingProcess, Expression> additionCombiner
		= (s, solution1, solution2, p) -> addSymbolicResults(solution1, solution2, p);

		boolean splitterDependsOnFreeVariablesOnly = ! theory.splitterDependsOnIndex(splitter, constraint.getIndices());
		
		QuarternaryFunction<Expression, Expression, Expression, RewritingProcess, Expression> combiner;
		boolean guaranteedSplitter;
		if (earlyExternalizationOfFreeVariableSplittersOptimization && splitterDependsOnFreeVariablesOnly) {
			combiner = conditionalCombiner;
			guaranteedSplitter = true;
		}
		else { // default, generic procedure
			combiner = additionCombiner;
			guaranteedSplitter = false;
		}

		Expression result;
		Constraint constraintUnderSplitter = constraint.applySplitter(true, splitter, guaranteedSplitter, process);
		RewritingProcess processUnderSplitter = combiner == conditionalCombiner? process.extendDPLLContextualConstraint(true, splitter) : process;
		
		if (constraintUnderSplitter != null) {
			Expression solutionUnderSplitter = solveUnderSplitter(true, splitter, expression, constraintUnderSplitter, processUnderSplitter);

			if (combiner == additionCombiner && problemType.isMaximum(solutionUnderSplitter)) {
				result = solutionUnderSplitter; // solution is already maximum, no need to consider the sub-problem under the splitter negation
			}
			else {
				Constraint constraintUnderSplitterNegation = constraint.applySplitter(false, splitter, guaranteedSplitter, process);
				RewritingProcess processUnderSplitterNegation = combiner == conditionalCombiner? process.extendDPLLContextualConstraint(false, splitter) : process;

				if (constraintUnderSplitterNegation != null) {
					Expression solutionUnderSplitterNegation = solveUnderSplitter(false, splitter, expression, constraintUnderSplitterNegation, processUnderSplitterNegation);
					result = combiner.apply(splitter, solutionUnderSplitter, solutionUnderSplitterNegation, process);
				}
				else { // splitter cannot be false under constraint, so it is true under constraint, so final solution is one under splitter
					result = solutionUnderSplitter;
				}
			}
		}
		else {
            // splitter is false under constraint, so its negation is true, so final solution is one under splitter negation
			Constraint constraintUnderSplitterNegation = constraint.applySplitter(false, splitter, guaranteedSplitter, process);
			RewritingProcess processUnderSplitterNegation = combiner == conditionalCombiner? process.extendDPLLContextualConstraint(false, splitter) : process;
			Expression solutionUnderSplitterNegation = solveUnderSplitter(false, splitter, expression, constraintUnderSplitterNegation, processUnderSplitterNegation);
			result = solutionUnderSplitterNegation;
		}
		
		return result;
	}

	/**
	 * @param splitterSign
	 * @param splitter
	 * @param expression
	 * @param constraintUnderSplitter must not be <code>null</code>
	 * @param process
	 * @return
	 */
	protected Expression solveUnderSplitter(boolean splitterSign, Expression splitter, Expression expression, Constraint constraintUnderSplitter, RewritingProcess process) {
		Expression expressionUnderSplitter = theory.applySplitterToExpression(splitterSign, splitter, expression, process);
		Expression result = solve(expressionUnderSplitter, constraintUnderSplitter, process);
		return result;
	}

	/**
	 * If solutions are unconditional expressions, simply add them.
	 * If they are conditional (symbolic), perform distributive on conditions.
	 */
	protected Expression addSymbolicResults(Expression solution1, Expression solution2, RewritingProcess process) {

		Expression result = null;

		if (DPLLUtil.isConditionalSolution(solution1, theory, process)) {
			Expression splitter   = IfThenElse.getCondition (solution1);
			Expression thenBranch = IfThenElse.getThenBranch(solution1);
			Expression elseBranch = IfThenElse.getElseBranch(solution1);
			
			Constraint constraint = process.getDPLLContextualConstraint();
			Expression normalizedSplitter = DPLLUtil.normalizeOrTrivializedSplitter(splitter, constraint, theory, process);
			
			if (normalizedSplitter.equals(TRUE)) {
				result = addSymbolicResults(thenBranch, solution2, process);
			}
			else if (normalizedSplitter.equals(FALSE)) {
				result = addSymbolicResults(elseBranch, solution2, process);
			}
			else {
				RewritingProcess processUnderSplitterAssertion = process.extendDPLLContextualConstraint(true,  normalizedSplitter);
				RewritingProcess processUnderSplitterNegation  = process.extendDPLLContextualConstraint(false, normalizedSplitter);
				Expression newThenBranch = addSymbolicResults(thenBranch, solution2, processUnderSplitterAssertion);
				Expression newElseBranch = addSymbolicResults(elseBranch, solution2, processUnderSplitterNegation);
				result = IfThenElse.make(normalizedSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
				result = theory.applyConstraintToSolution(theory.makeConstraint(Util.list()), result, process);
			}
		}
		else if (DPLLUtil.isConditionalSolution(solution2, theory, process)) {
			Expression splitter   = IfThenElse.getCondition (solution2);
			Expression thenBranch = IfThenElse.getThenBranch(solution2);
			Expression elseBranch = IfThenElse.getElseBranch(solution2);
			
			Constraint constraint = process.getDPLLContextualConstraint();
			Expression normalizedSplitter = DPLLUtil.normalizeOrTrivializedSplitter(splitter, constraint, theory, process);
			
			if (normalizedSplitter.equals(TRUE)) {
				result = addSymbolicResults(solution1, thenBranch, process);
			}
			else if (normalizedSplitter.equals(FALSE)) {
				result = addSymbolicResults(solution1, elseBranch, process);
			}
			else {
				RewritingProcess processUnderSplitterAssertion = process.extendDPLLContextualConstraint(true,  splitter);
				RewritingProcess processUnderSplitterNegation  = process.extendDPLLContextualConstraint(false, splitter);
				Expression newThenBranch = addSymbolicResults(solution1, thenBranch, processUnderSplitterAssertion);
				Expression newElseBranch = addSymbolicResults(solution1, elseBranch, processUnderSplitterNegation);
				result = IfThenElse.make(splitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
				result = theory.applyConstraintToSolution(theory.makeConstraint(Util.list()), result, process);
			}
		}
		else {
			result = problemType.add(solution1, solution2, process);
		}

		return result;
	}
}