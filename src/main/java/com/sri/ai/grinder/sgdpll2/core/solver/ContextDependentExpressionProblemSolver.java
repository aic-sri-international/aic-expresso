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
package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.MultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.SatisfiabilityOfSingleVariableEqualityConstraintStepSolver;
import com.sri.ai.grinder.sgdpll2.theory.equality.SingleVariableEqualityConstraint;

/**
 * Solves a {@link ContextDependentExpressionProblemStepSolver} by successively conditioning the context on provided splitters.
 * 
 * @author braz
 *
 */
@Beta
public class ContextDependentExpressionProblemSolver {

	/**
	 * Returns the solution for a problem using a step solver, or null if the contextual constraint is found to be inconsistent.
	 * @param stepSolver
	 * @param contextualConstraint
	 * @param process
	 * @return
	 */
	public static Expression solve(ContextDependentProblemStepSolver<Expression> stepSolver, Constraint2 contextualConstraint, RewritingProcess process) {
		ContextDependentProblemStepSolver.SolutionStep<Expression> step = stepSolver.step(contextualConstraint, process);
//		System.out.println("Step: " + step);
//		System.out.println("Contextual constraint: " + contextualConstraint);	
		if (step == null) {
			// contextual constraint is found to be inconsistent
			return null;
		}
		else if (step.itDepends()) {
			Expression splitter = step.getLiteral();
			ConstraintSplitting split;
			if (step.getConstraintSplitting() != null) {
				split = step.getConstraintSplitting();
			}
			else {
				split = new ConstraintSplitting(contextualConstraint, splitter, process);
			}
			switch (split.getResult()) {
			case CONSTRAINT_IS_CONTRADICTORY:
				return null;
			case LITERAL_IS_UNDEFINED:
//				System.out.println("Step solver constraint: " + ((SatisfiabilityOfSingleVariableEqualityConstraintStepSolver)stepSolver).getConstraint());
//				System.out.println("Step: " + step);
//				System.out.println("Contextual constraint: " + contextualConstraint);	
//				System.out.println("Constraint and literal: " + split.getConstraintAndLiteral());	
//				System.out.println("Constraint and literal negation: " + split.getConstraintAndLiteralNegation());	
				Expression subSolution1 = solve(step.getStepSolverForWhenLiteralIsTrue (), split.getConstraintAndLiteral(), process);
				Expression subSolution2 = solve(step.getStepSolverForWhenLiteralIsFalse(), split.getConstraintAndLiteralNegation(), process);
				if (subSolution1 == null || subSolution2 == null) {
					return null;
				}
				else {
					return IfThenElse.make(splitter, subSolution1, subSolution2, true);
				}
			case LITERAL_IS_TRUE:
			case LITERAL_IS_FALSE:
				return solve(stepSolver, split.getConstraintConjoinedWithDefinedValueOfLiteral(), process);
			default:
				throw new Error("Undefined value");
			}
		}
		else {
			return step.getValue();
		}
	}
	
	public static void main(String[] args) {
		
		DefaultRewritingProcess process = new DefaultRewritingProcess(null);

		SingleVariableEqualityConstraint constraint = new SingleVariableEqualityConstraint(parse("X"), false, new EqualityConstraintTheory(true, true));
		constraint = constraint.conjoin(parse("X = Y"), process);
		constraint = constraint.conjoin(parse("X = Z"), process);
		constraint = constraint.conjoin(parse("X != W"), process);
		constraint = constraint.conjoin(parse("X != U"), process);
		
		ContextDependentExpressionProblemStepSolver problem = new SatisfiabilityOfSingleVariableEqualityConstraintStepSolver(constraint);

		MultiVariableConstraint contextualConstraint = new CompleteMultiVariableConstraint(new EqualityConstraintTheory(true, true));
		
		Expression result = solve(problem, contextualConstraint, process);
		
		System.out.println("result: " + result);	
	}
}