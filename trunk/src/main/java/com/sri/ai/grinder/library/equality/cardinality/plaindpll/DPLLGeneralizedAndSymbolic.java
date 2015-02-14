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
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.helper.Expressions;
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
	 * and combines their sub-solutions.
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
	 * are already guaranteed by the path so far as a contextual constraint, so that it can be used to "nip conditions in the bud",
	 * right at the leaf level, before needing to externalize them.
	 */
	public final static boolean earlyExternalizationOfFreeVariableSplittersOptimization = true; // IMPORTANT: unit tests will break if set to false. However DPLL stress tests can still be used. As of this writing (12/4/2014) the false setting was slightly slower.
	
	/** The background equalityTheory for the algorithm. */
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
		Pair<Expression, IndexExpressionsSet> formulaAndIndexExpressions = problemType.getExpressionAndIndexExpressionsFromRewriterProblemArgument(expression, process);
		Expression       formula          = formulaAndIndexExpressions.first;
		IndexExpressionsSet indexExpressions = formulaAndIndexExpressions.second;
		Expression       simplifiedFormula = theory.simplify(formula, process); // eventually this will should not be needed as simplification should be lazy 
		List<Expression> indices = IndexExpressions.getIndices(indexExpressions);
		RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIndexExpressions(indexExpressions, process);
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
	 * Convenience substitute for {@link #solve(Expression, Collection, RewritingProcess)} that takes care of constructing the RewritingProcess.
	 */
	public Expression solve(Expression expression, Collection<Expression> indices, Map<String, String> mapFromVariableNameToTypeName, Map<String, String> mapFromTypeNameToSizeString) {
		RewritingProcess process = new DefaultRewritingProcess(this);
		for (Map.Entry<String, String> variableNameAndTypeName : mapFromVariableNameToTypeName.entrySet()) {
			String variableName = variableNameAndTypeName.getKey();
			String typeName     = variableNameAndTypeName.getValue();
			process = GrinderUtil.extendContextualSymbolsWithIndexExpression(Expressions.parse(variableName + " in " + typeName), process);
		}
		for (Map.Entry<String, String> typeNameAndSizeString : mapFromTypeNameToSizeString.entrySet()) {
			String typeName   = typeNameAndSizeString.getKey();
			String sizeString = typeNameAndSizeString.getValue();
			process.putGlobalObject(Expressions.parse("|" + typeName + "|"), Expressions.parse(sizeString));
		}
		Expression result = solve(expression, indices, process);
		return result;
	}

	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices.
	 */
	public Expression solve(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		// TODO: should replace this oldConstraint by a copy constructor creating a sub-process, but surprisingly there is no complete copy constructor available in DefaultRewritingProcess.
		Theory.Constraint oldConstraint = process.getDPLLContextualConstraint();
		Constraint contextualConstraint = theory.makeConstraint(Util.list()); // contextual constraint does not involve any indices -- defined on free variables only
		process.initializeDPLLContextualConstraint(contextualConstraint);

		Constraint constraint = theory.makeConstraint(indices);
		Expression result = solve(expression, constraint, process);
		
		process.initializeDPLLContextualConstraint(oldConstraint);
		return result;
	}

	/**
	 * Returns the summation (or the provided semiring additive operation) of an expression over the provided set of indices under given constraint,
	 * which is considered a contradiction if it has the value <code>null</code>.
	 */
	protected Expression solve(Expression expression, Constraint constraint, RewritingProcess process) {
		
//		System.out.println("Solving");
//		System.out.println("expression           : " + expression);
//		System.out.println("constraint           : " + constraint);
//		System.out.println("contextual constraint: " + process.getDPLLContextualConstraint());
//		System.out.println("\n");
		
		Expression result;
		
		assert constraint != null : this.getClass() + ".solve must not receive a contradiction constraint (a null pointer)";

		Expression splitter = pickSplitter(expression, constraint, process);

		if (splitter != null) {
			result = solveBasedOnSplitting(splitter, expression, constraint, process);
		}
		else {
			Expression unconditionalValue = normalizeUnconditionalExpression(expression, process);
			Expression numberOfOccurrences = constraint.modelCount(process);
			Expression valueToBeSummed = problemType.fromExpressionValueWithoutLiteralsToValueToBeAdded(unconditionalValue);
			result = problemType.addNTimes(valueToBeSummed, numberOfOccurrences, process);
		}

//		System.out.println("Solved");
//		System.out.println("expression           : " + expression);
//		System.out.println("constraint           : " + constraint);
//		System.out.println("contextual constraint: " + process.getDPLLContextualConstraint());
//		System.out.println("result               : " + result);
//		System.out.println("\n");
		
		return result;
	}

	/**
	 * Method used to normalize unconditional expressions to some normal form chosen by extending classes
	 * (default is identity).
	 * @param expression
	 * @param process
	 * @return
	 */
	public Expression normalizeUnconditionalExpression(Expression expression, RewritingProcess process) {
		return expression;
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

	/**
	 * Interface for functions combining two sub-solutions from a single splitting.
	 */
	private static interface Combiner extends QuarternaryFunction<Expression, Expression, Expression, RewritingProcess, Expression> {};

	private Expression solveBasedOnSplitting(Expression splitter, Expression expression, Constraint constraint, RewritingProcess process) {
		
		// Keep in mind that splitter may already be implied as true or false by equalityTheory constraint.
		// This should not happen if the equalityTheory application of splitters to expressions only replaced them by true or false,
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
		
		Combiner conditionalCombiner = (s, solution1, solution2, p) -> IfThenElse.make(s, solution1, solution2, false /* no simplification to condition */);
		Combiner additionCombiner    = (s, solution1, solution2, p) -> addSymbolicResults(solution1, solution2, p);
		
		Combiner combiner;
		boolean splitterMustBeInContextualConstraint;

		boolean splitterDependsOnFreeVariablesOnly = ! theory.splitterDependsOnIndex(splitter, constraint.getIndices());
		if (earlyExternalizationOfFreeVariableSplittersOptimization && splitterDependsOnFreeVariablesOnly) {
			combiner = conditionalCombiner;
			splitterMustBeInContextualConstraint = true;
		}
		else { // default, generic procedure
			combiner = additionCombiner;
			splitterMustBeInContextualConstraint = false;
		}

		Expression solutionUnderSplitter = solveUnderSplitter(true, splitter, expression, constraint, splitterMustBeInContextualConstraint, process);
		boolean noNeedToComputeNegation  = solutionUnderSplitter != null && combiner == additionCombiner && problemType.isAbsorbingElement(solutionUnderSplitter);
		Expression solutionUnderSplitterNegation = 
				noNeedToComputeNegation? null : solveUnderSplitter(false, splitter, expression, constraint, splitterMustBeInContextualConstraint, process);
		Expression result = combine(combiner, splitter, solutionUnderSplitter, solutionUnderSplitterNegation, process);
		
		return result;
	}
	
	/**
	 * Combines two sub-solutions under a splitter and its negation, where a null sub-solution means the respective splitter or negation cannot be true,
	 * in which case the combination is simply the other sub-solution.
	 * @param combiner
	 * @param splitter
	 * @param solutionUnderSplitter
	 * @param solutionUnderSplitterNegation
	 * @param process
	 * @return
	 */
	private Expression combine(Combiner combiner, Expression splitter, Expression solutionUnderSplitter, Expression solutionUnderSplitterNegation, RewritingProcess process) {
		Expression result;
		if (solutionUnderSplitter == null) {
			result = solutionUnderSplitterNegation;
		}
		else if (solutionUnderSplitterNegation == null) {
			result = solutionUnderSplitter;
		}
		else {
			result = combiner.apply(splitter, solutionUnderSplitter, solutionUnderSplitterNegation, process);
		}
		return result;
	}

	/**
	 * Solves under splitter, returning null if the splitter is contradictory with the constraint.
	 * @param splitterSign
	 * @param splitter
	 * @param expression
	 * @param constraint
	 * @param splitterInContextualConstraint
	 * @param process
	 * @return
	 */
	private Expression solveUnderSplitter(boolean splitterSign, Expression splitter, Expression expression, Constraint constraint, boolean splitterInContextualConstraint, RewritingProcess process) {
		Expression result;
		Constraint constraintUnderSplitter = constraint.applySplitter(splitterSign, splitter, process);
		if (constraintUnderSplitter == null) {
			result = null;
		}
		else {
			Expression expressionUnderSplitter = theory.applySplitterToExpression(splitterSign, splitter, expression, process);
			boolean rendersAdditiveIdentitySolution = expressionUnderSplitter.equals(problemType.expressionValueLeadingToAdditiveIdentityElement());
			if (rendersAdditiveIdentitySolution) {
				result = problemType.additiveIdentityElement();
			}
			else {
				RewritingProcess processUnderSplitter = splitterInContextualConstraint? process.extendDPLLContextualConstraint(splitterSign, splitter) : process;
				result = solve(expressionUnderSplitter, constraintUnderSplitter, processUnderSplitter);
			}
		}
		return result;
	}

	/**
	 * If solutions are unconditional expressions, simply add them.
	 * If they are conditional (symbolic), perform distributive on conditions.
	 */
	protected Expression addSymbolicResults(Expression solution1, Expression solution2, RewritingProcess process) {

		Expression result = null;

		if (solution1.equals(problemType.additiveIdentityElement())) {
			result = theory.applyConstraintToSolution(process.getDPLLContextualConstraint(), solution2, process);
		}
		else if (solution2.equals(problemType.additiveIdentityElement())) {
			result = theory.applyConstraintToSolution(process.getDPLLContextualConstraint(), solution1, process);
		}
		else if (DPLLUtil.isConditionalSolution(solution1, theory, process)) {
			Expression splitter   = IfThenElse.getCondition (solution1);
			Expression thenBranch = IfThenElse.getThenBranch(solution1);
			Expression elseBranch = IfThenElse.getElseBranch(solution1);

			Constraint constraint = process.getDPLLContextualConstraint();
			Expression normalizedSplitter = DPLLUtil.normalizeOrTrivializeSplitter(splitter, constraint, theory, process);

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
			}
		}
		else if (DPLLUtil.isConditionalSolution(solution2, theory, process)) {
			Expression splitter   = IfThenElse.getCondition (solution2);
			Expression thenBranch = IfThenElse.getThenBranch(solution2);
			Expression elseBranch = IfThenElse.getElseBranch(solution2);

			Constraint constraint = process.getDPLLContextualConstraint();
			Expression normalizedSplitter = DPLLUtil.normalizeOrTrivializeSplitter(splitter, constraint, theory, process);

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
				result = IfThenElse.make(normalizedSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
			}
		}
		else {
			result = problemType.add(solution1, solution2, process);
		}
	
		return result;
	}
}