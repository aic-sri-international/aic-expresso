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
package com.sri.ai.grinder.sgdpll.api;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll.core.constraint.ConstraintSplitting;

/**
 * An interface for step-solvers for problems involving free variables constrained by a contextual {@link Constraint}.
 * The problem may either have the same solution for all free variable assignments under the context, or not.
 * Method {@link #step(Constraint, RewritingProcess)} returns a {@link SolutionStep},
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
public interface ContextDependentProblemStepSolver<T> extends Cloneable {

	/**
	 * Cloning is important for this interface, because when a problem depends on an expression to be solved
	 * the {@link ItDependsOn} solution step will carry within it two sub-step solvers
	 * to be used on the two branches of the search (one for when the expression is enforced to be true,
	 * and another for false).
	 * <p>
	 * While it is correct to just re-use the step solver returning the {@link ItDependsOn} object,
	 * it would then wastefully check again for all the expressions that had already been enforced true or false
	 * by the contextual constraint.
	 * By allowing a step solver to clone itself, we are able to provide sub-step solvers
	 * that already know when to continue the search from.
	 * <p>
	 * Note also that, instead of clone, we could have a copy constructor or just regular constructors
	 * receiving state information as parameters.
	 * However, clone provides more flexibility if one is writing code that manipulates
	 * {@link ContextDependentProblemStepSolver}s in general, and therefore needs
	 * a way to create copies without knowing its actual class.
	 * <p>
	 * Finally, it is recommended that the implementation of clone be the code below
	 * unless there is a good reason for doing otherwise.
	 * It may be more elegant to simply create a clone with one of the class's constructors,
	 * but this will require extensions of that class to override this method,
	 * whereas the version below will be reusable by extensions.
	 * If a different choice is made, the new cloning method should check
	 * if the instance is not that of an extension, or it should be very clearly documented,
	 * or the class should be made final.
	 * <code>
	 * 	@Override
	 * 	public ContextDependentProblemStepSolver clone() {
	 * 		try {
	 * 			return (ContextDependentProblemStepSolver) super.clone();
	 * 		} catch (CloneNotSupportedException e) {
	 * 			throw new Error(e);
	 * 		}
	 * 	}
	 * <code>
	 * 
	 * @return a clone of this step solver.
	 */
	ContextDependentProblemStepSolver clone();
	
	/**
	 * A solution step of a {@link ContextDependentProblemStepSolver}.
	 * If {@link #itDepends()} returns <code>true</code>, the solution cannot be determined
	 * unless the contextual constraint be restricted according to the literal returned by {@link #getLiteral()}.
	 * Otherwise, the expression returned by {@link #getValue()} is the solution.
	 * @author braz
	 *
	 */
	public static interface SolutionStep<T> {
		boolean itDepends();
		
		/**
		 * If {@link #itDepends()} is true, returns the literal the solution depends on.
		 * @return
		 */
		Expression getLiteral();
		
		/**
		 * If {@link #itDepends()} is false, returns the solution value.
		 * @return
		 */
		T getValue();
		
		/**
		 * Returns a {@link ContextDependentProblemStepSolver} to be used for finding the final solution
		 * in case the literal is defined as true by the contextual constraint.
		 * This is merely an optimization, and using the original step solver should still work,
		 * but will perform wasted working re-discovering that expressions is already true.
		 * @return
		 */
		ContextDependentProblemStepSolver<T> getStepSolverForWhenLiteralIsTrue();
		
		/**
		 * Same as {@link #getStepSolverForWhenLiteralIsTrue()} but for when literal is false.
		 * @return
		 */
		ContextDependentProblemStepSolver<T> getStepSolverForWhenLiteralIsFalse();
		
		/**
		 * For solutions depending on a split, provides the constraint splitting
		 * for the contextual constraint and literal used, if available,
		 * or null otherwise.
		 * @return
		 */
		ConstraintSplitting getConstraintSplitting();
	}
	
	public static class ItDependsOn<T> implements SolutionStep<T> {

		private Expression literal;
		private ConstraintSplitting constraintSplitting;
		private ContextDependentProblemStepSolver<T> stepSolverIfExpressionIsTrue;
		private ContextDependentProblemStepSolver<T> stepSolverIfExpressionIsFalse;
		
		/**
		 * Represents a solution step in which the final solution depends on the definition of a given expression
		 * by the contextual constraint.
		 * Step solvers specialized for whether expression is true or false can be provided
		 * that already know about the definition of expression either way, for efficiency;
		 * however, if this step solver is provided instead, things still work because 
		 * the step solver will end up determining anyway that expression is now defined and move on.
		 * @param literal
		 * @param stepSolverIfExpressionIsTrue
		 * @param stepSolverIfExpressionIsFalse
		 */
		public ItDependsOn(
				Expression literal,
				ConstraintSplitting constraintSplitting,
				ContextDependentProblemStepSolver<T> stepSolverIfExpressionIsTrue,
				ContextDependentProblemStepSolver<T> stepSolverIfExpressionIsFalse) {
			super();
			this.literal = literal;
			this.constraintSplitting = constraintSplitting;
			this.stepSolverIfExpressionIsTrue  = stepSolverIfExpressionIsTrue;
			this.stepSolverIfExpressionIsFalse = stepSolverIfExpressionIsFalse;
		}
		
		@Override
		public Expression getLiteral() {
			return literal;
		}
		
		@Override
		public T getValue() {
			throw new Error("ItDependsOn does not define getValue().");
		}
		
		@Override
		public boolean itDepends() {
			return true;
		}

		@Override
		public ConstraintSplitting getConstraintSplitting() {
			return constraintSplitting;
		}

		@Override
		public ContextDependentProblemStepSolver<T> getStepSolverForWhenLiteralIsTrue() {
			return stepSolverIfExpressionIsTrue;
		}
		
		@Override
		public ContextDependentProblemStepSolver<T> getStepSolverForWhenLiteralIsFalse() {
			return stepSolverIfExpressionIsFalse;
		}
		
		@Override
		public String toString() {
			return "It depends on " + getLiteral();
		}
	}
	
	
	public static class Solution<T> implements SolutionStep<T> {

		private T value;
		
		public Solution(T value) {
			this.value = value;
		}
		
		@Override
		public boolean itDepends() {
			return false;
		}

		@Override
		public Expression getLiteral() {
			throw new Error("Solution does not define getLiteral().");
		}
		
		@Override
		public T getValue() {
			return value;
		}
		
		@Override
		public String toString() {
			return getValue().toString();
		}

		@Override
		public ContextDependentProblemStepSolver<T> getStepSolverForWhenLiteralIsTrue() {
			throw new Error("Solution has no sub-step solvers since it does not depend on any expression");
		}

		@Override
		public ContextDependentProblemStepSolver<T> getStepSolverForWhenLiteralIsFalse() {
			throw new Error("Solution has no sub-step solvers since it does not depend on any expression");
		}

		@Override
		public ConstraintSplitting getConstraintSplitting() {
			return null;
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
	SolutionStep<T> step(Constraint contextualConstraint, RewritingProcess process);
}