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
package com.sri.ai.grinder.sgdpll2.api;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;
import com.sri.ai.grinder.sgdpll2.core.solver.ContextDependentExpressionProblemSolver;

/**
 * An interface for step-solvers for problems involving free variables constrained by a contextual {@link Constraint2}.
 * The problem may either have the same solution for all free variable assignments under the context, or not.
 * Method {@link #step(Constraint2, RewritingProcess)} returns a {@link SolutionStep},
 * which is either a {@link Solution} with {@link Solution#getValue()} returning the solution,
 * or a {@link ItDependsOn} with {@link ItDependsOn#getLiteral()} returning a literal
 * that, if used to split the contextual constraint
 * (by conjoining the contextual constraint with the literal and with its negation, successively),
 * will help disambiguate the problem.
 * 
 * @author braz
 *
 */
@Beta
public interface ContextDependentExpressionProblemStepSolver extends ContextDependentProblemStepSolver<Expression>, Cloneable {

	/**
	 * Convenience method invoking
	 * {@link ContextDependentExpressionProblemSolver#solve(ContextDependentExpressionProblemStepSolver, Constraint2, RewritingProcess)}
	 * on this step solver.
	 * @param contextualConstraint
	 * @param process
	 * @return
	 */
	default Expression solve(Constraint2 contextualConstraint, RewritingProcess process) {
		Expression result = ContextDependentExpressionProblemSolver.solve(this, contextualConstraint, process);
		return result;
	}

	@Override
	ContextDependentExpressionProblemStepSolver clone();
	
	/**
	 * A specialization of {@link ContextDependentProblemStepSolver#SolutionStep} for Expressions.
	 * @author braz
	 *
	 */
	public static interface SolutionStep extends ContextDependentProblemStepSolver.SolutionStep<Expression> {
		/**
		 * Returns a {@link ContextDependentExpressionProblemStepSolver} to be used for finding the final solution
		 * in case the literal is defined as true by the contextual constraint.
		 * This is merely an optimization, and using the original step solver should still work,
		 * but will perform wasted working re-discovering that expressions is already true.
		 * @return
		 */
		ContextDependentExpressionProblemStepSolver getStepSolverForWhenLiteralIsTrue();
		
		/**
		 * Same as {@link #getStepSolverForWhenLiteralIsTrue()} but for when literal is false.
		 * @return
		 */
		ContextDependentExpressionProblemStepSolver getStepSolverForWhenLiteralIsFalse();
	}
	
	/**
	 * A specialization of {@link ContextDependentProblemStepSolver#ItDependsOn} for Expressions.
	 * @author braz
	 *
	 */
	public static class ItDependsOn extends ContextDependentProblemStepSolver.ItDependsOn<Expression> implements SolutionStep {

		public ItDependsOn(
				Expression literal,
				ConstraintSplitting constraintSplitting,
				ContextDependentExpressionProblemStepSolver stepSolverIfExpressionIsTrue,
				ContextDependentExpressionProblemStepSolver stepSolverIfExpressionIsFalse) {
			super(literal, constraintSplitting, stepSolverIfExpressionIsTrue, stepSolverIfExpressionIsFalse);
		}
		
		@Override
		public ContextDependentExpressionProblemStepSolver getStepSolverForWhenLiteralIsTrue() {
			return (ContextDependentExpressionProblemStepSolver) super.getStepSolverForWhenLiteralIsTrue();
		}
		
		@Override
		public ContextDependentExpressionProblemStepSolver getStepSolverForWhenLiteralIsFalse() {
			return (ContextDependentExpressionProblemStepSolver) super.getStepSolverForWhenLiteralIsFalse();
		}
	}
	
	/**
	 * A specialization of {@link ContextDependentProblemStepSolver#Solution} for Expressions.
	 * @author braz
	 *
	 */
	public static class Solution extends ContextDependentProblemStepSolver.Solution<Expression> implements SolutionStep {

		public Solution(Expression value) {
			super(value);
		}
		
		@Override
		public ContextDependentExpressionProblemStepSolver getStepSolverForWhenLiteralIsTrue() {
			throw new Error("Solution has no sub-step solvers since it does not depend on any expression");
		}

		@Override
		public ContextDependentExpressionProblemStepSolver getStepSolverForWhenLiteralIsFalse() {
			throw new Error("Solution has no sub-step solvers since it does not depend on any expression");
		}
	}

	/**
	 * Returns a solution step for the problem: either the solution itself, if independent
	 * on the values for free variables, or a literal that, if used to split the contextual constraint,
	 * will bring the problem closer to a solution.
	 * @param contextualConstraint
	 * @param process
	 * @return
	 */
	SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process);
}