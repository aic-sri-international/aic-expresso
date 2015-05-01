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
package com.sri.ai.grinder.plaindpll.core;

import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.GroupProblemType;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.util.Util;
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
public class SGDPLLT extends AbstractSolver {
	
	public int debugLevel = 3;
	
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
	
	public SGDPLLT(ConstraintTheory theory, GroupProblemType problemType) {
		this(theory, problemType, null);
	}

	public SGDPLLT(ConstraintTheory theory, GroupProblemType problemType, CountsDeclaration countsDeclaration) {
		super(theory, problemType, countsDeclaration);
	}

	@Override
	protected Expression solveAfterBookkeeping(Expression expression, Collection<Expression> indices, Constraint constraint, RewritingProcess process) {
		
		long startTime = 0;
		if (debug(process)) {
			startTime = System.currentTimeMillis();
			System.out.println("Solving");
			System.out.println("level                : " + getLevel(process));	
			System.out.println("expression           : " + shortString(expression));
			System.out.println("constraint           : " + constraint);
			System.out.println("contextual constraint: " + process.getDPLLContextualConstraint());
			System.out.println("\n");
		}
		
		Expression result;
		
		myAssert(() -> constraint != null, () -> "solve(Expression, Constraint, RewritingProcess) must only be given non-null expressions");
		
		Expression splitter = pickSplitter(expression, indices, constraint, process);

		if (splitter != null) {
			result = solveBasedOnSplitting(splitter, expression, indices, constraint, process);
		}
		else {
			Expression unconditionalValue = normalizeUnconditionalExpression(expression, process);
			Expression numberOfOccurrences = constraint.modelCount(indices, process);
			result = problemType.addNTimes(unconditionalValue, numberOfOccurrences, process);
		}

		if (debug(process)) {
			long endTime = System.currentTimeMillis();
			System.out.println("Solved in " + (endTime - startTime) + " ms");
			System.out.println("level                : " + getLevel(process));	
			System.out.println("expression           : " + shortString(expression));
			System.out.println("constraint           : " + constraint);
			System.out.println("contextual constraint: " + process.getDPLLContextualConstraint());
			System.out.println("result               : " + shortString(result));
			System.out.println("\n");
		}		

		return result;
		
		/**
		 * (*) This is a little subtle. If numberOfOccurrences is 0, it means the constraint was inconsistent but
		 * that has not been detected before.
		 * Solvers are allowed to be incomplete with their consistency checks when a splitter is applied,
		 * but the cost of that is that sometimes things will be checked when they did not have to be;
		 * the more complete the solver can be without being expensive, the better.
		 * They are *not*, however, allowed to be incomplete when counting models (it is not even possible, since they *have* to return a number,
		 * so it is either completeness or just plain unsoundness) so, if a contradiction went unnoticed,
		 * it is sure to be detected here.
		 * The reason we have this check return null instead of the addNTimes(valueToBeSummed, 0), which would be correct anyway,
		 * is that by returning null we are telling the invoker that the constraint was inconsistent even though it had not been notified by the solver.
		 * Returning addNTimes(valueToBeSummed, 0) would not achieve this, since the invoker would not be told that this was a result of inconsistency
		 * (it could be that valueToBeSummed is the identity element).
		 * This gives it a change to retract its latest condition (in case it is a conditional combiner instead of additional combiner
		 * -- see {@link #solveBasedOnSplitting(Expression splitter, Expression expression, Constraint constraint, RewritingProcess process)} 
		 */
	}

	/**
	 * Hook method used to normalize unconditional expressions to some normal form chosen by extending classes
	 * (default is identity).
	 * @param expression
	 * @param process
	 * @return
	 */
	public Expression normalizeUnconditionalExpression(Expression expression, RewritingProcess process) {
		return expression;
	}

	/** Picks splitter from either expression or constraint; assumes constraint is not <code>null</code>. */
	protected Expression pickSplitter(Expression expression, Collection<Expression> indices, Constraint constraint, RewritingProcess process) {
		Expression splitter;
		splitter = constraintTheory.pickSplitterInExpression(expression, constraint, process);
		if (splitter == null) { // expression is constant value, so it does not have any splitters
			splitter = constraint.pickSplitter(indices, process);
		}
		return splitter;
	}

	/**
	 * Interface for functions combining two sub-solutions from a single splitting.
	 */
	private static interface Combiner extends QuarternaryFunction<Expression, Expression, Expression, RewritingProcess, Expression> {};

	private Expression solveBasedOnSplitting(Expression splitter, Expression expression, Collection<Expression> indices, Constraint constraint, RewritingProcess process) {
		
		// Keep in mind that splitter may already be implied as true or false by theoryWithEquality constraint.
		// This should not happen if the theoryWithEquality application of splitters to expressions only replaced them by true or false,
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

		boolean splitterDependsOnFreeVariablesOnly = ! constraintTheory.splitterDependsOnIndex(splitter, indices);
		if (earlyExternalizationOfFreeVariableSplittersOptimization && splitterDependsOnFreeVariablesOnly) {
			combiner = conditionalCombiner;
			splitterMustBeInContextualConstraint = true;
		}
		else { // default, generic procedure
			combiner = additionCombiner;
			splitterMustBeInContextualConstraint = false;
		}

		Expression solutionUnderSplitter = solveUnderSplitter(true, splitter, expression, indices, constraint, splitterMustBeInContextualConstraint, process);
		boolean noNeedToComputeNegation  = solutionUnderSplitter != null && combiner == additionCombiner && problemType.isAdditiveAbsorbingElement(solutionUnderSplitter);
		Expression solutionUnderSplitterNegation = 
				noNeedToComputeNegation? null : solveUnderSplitter(false, splitter, expression, indices, constraint, splitterMustBeInContextualConstraint, process);
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
	 * Solves under splitter, returning null if the splitter is contradictory with the contextual constraint (the one representing the free variable splitters so far).
	 * Note that, if the splitter is contradictory with the contextual constraint, it will also invalidate the constraint and the sub-solution will be the additive identity element,
	 * so returning this identity element would be correct for the purpose of this method.
	 * So, why is it important to return null instead?
	 * To see why, assume the + case with a splitter not containing an index. Then the combination function is the conditional one.
	 * In this case, returning the identity element (0) would lead to the solution "if splitter then 0 else <splitter negation sub-solution>"
	 * which, while not incorrect, is redundant because at this point we know splitter must be false,
	 * and that therefore "<splitter negation sub-solution>" is a better solution.
	 * @param splitterSign
	 * @param splitter
	 * @param expression
	 * @param constraint
	 * @param splitterInContextualConstraint
	 * @param process
	 * @return
	 */
	private Expression solveUnderSplitter(boolean splitterSign, Expression splitter, Expression expression, Collection<Expression> indices, Constraint constraint, boolean splitterInContextualConstraint, RewritingProcess process) {
		Expression result;
		myAssert(() -> process.getDPLLContextualConstraint() != null, () -> "SGDPLL(T) should not operate under a contradictory contextual constraint");
		RewritingProcess processUnderSplitter = splitterInContextualConstraint? process.extendDPLLContextualConstraint(splitterSign, splitter) : process;
		if (processUnderSplitter.getDPLLContextualConstraint() == null) {
			result = null;
			// subtle note about past bug: until February 2015 this check for returning null was done on constraint, not contextual constraint. That is incorrect, however, because the fact that a splitter turns the constraint inconsistent does not mean that the splitter is false. That would be the case only if the constraint was always required to hold, which is not the case. However, that check was indirectly covering the cases in which the splitter was contradictory with the *contextual* constraint, which *is* required to always hold, so it was being useful. To make things harder to detect, it seems that with the theories we then had it was hard to produce an example that exposed the bug, that is, an example in which the splitter was inconsistent with the constraint but *not* with the contextual constraint. The problem only surfaced when I started returning null for model counts equal to 0 (also inconsistent): sum_X X != a and Y != X for |X| = 2 creates a constraint with 0 models after application of splitter Y != a, but that does *not* mean that Y != a is necessarily false! Yet, using model count 0 inconsistency for the constraint was leading the algorithm to believe that. It is only when we check against the contextual constraint (which at that point is just 'true' and not inconsistent with Y != a) that things work again.
		}
		else {
			Constraint constraintUnderSplitter = constraint.incorporate(splitterSign, splitter, process);
			if (constraintUnderSplitter == null) { // it would be more elegant to place this check this inside 'solve' (which as of now assumes the given constraint is never null), but placing the check here avoids unnecessary applications of the splitter to expression.
				result = problemType.additiveIdentityElement();
			}
			else {
				incrementLevel(processUnderSplitter, process);
				Expression expressionUnderSplitter = constraintTheory.applySplitterToExpression(splitterSign, splitter, expression, process);
				result = solve(expressionUnderSplitter, indices, constraintUnderSplitter, processUnderSplitter);
				decrementLevel(processUnderSplitter);
			}
		}
		return result;
	}
	
	private int getLevel(RewritingProcess process) {
		Integer level = (Integer) process.getGlobalObject("DPLL level");
		if (level == null) {
			level = 0;
		}
		return level;
	}

	private void incrementLevel(RewritingProcess subProcess, RewritingProcess process) {
		Integer level = getLevel(process);
		subProcess.putGlobalObject("DPLL level", level + 1);
	}

	private void decrementLevel(RewritingProcess process) {
		Integer level = getLevel(process);
		process.putGlobalObject("DPLL level", level - 1);
	}

	private boolean debug(RewritingProcess process) {
		return debug && getLevel(process) < debugLevel;
	}

	protected Expression addSymbolicResults(Expression solution1, Expression solution2, RewritingProcess process) {
		long start = 0;
		if (debug(process)) {
			System.out.println("Adding solutions");	
			System.out.println(solution1);
			System.out.println(solution2);
			start = System.currentTimeMillis();
		}
		
		Expression result = super.addSymbolicResults(solution1, solution2, process);
		
		if (debug(process)) {
			System.out.println("Finished adding solutions");	
			System.out.println(solution1);
			System.out.println(solution2);
			long end = System.currentTimeMillis();
			System.out.println("Took " + (end - start) + " ms.");	
		}
		
		return result;
	}
	
	private String shortString(Object object) {
		String string = object.toString();
		if (string.length() > 100) {
			return string.substring(0, 100);
		}
		return string;
	}
	
	@Override
	public String toString() {
		return "SGDPLL(T)";
	}
}