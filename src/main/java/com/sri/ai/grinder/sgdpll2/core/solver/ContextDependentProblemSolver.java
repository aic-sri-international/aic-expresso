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
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.MultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.SatisfiabilityOfSingleVariableEqualityConstraintStepSolver;
import com.sri.ai.grinder.sgdpll2.theory.equality.SingleVariableEqualityConstraint;

/**
 * Solves a {@link ContextDependentProblemStepSolver} by successively conditioning the context on provided splitters.
 * 
 * @author braz
 *
 */
@Beta
public class ContextDependentProblemSolver {

	public static Expression solve(ContextDependentProblemStepSolver stepSolver, Constraint contextualConstraint, RewritingProcess process) {
		ContextDependentProblemStepSolver.SolutionStep step = stepSolver.step(contextualConstraint, process);
		if (step.itDepends()) {
			Expression splitter = step.getExpression();
			ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, splitter, process);
			switch (split.getResult()) {
			case CONSTRAINT_IS_CONTRADICTORY:
				return null;
			case LITERAL_IS_UNDEFINED:
				Expression subSolution1 = solve(stepSolver, split.getConstraintAndLiteral(), process);
				Expression subSolution2 = solve(stepSolver, split.getConstraintAndLiteralNegation(), process);
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
			return step.getExpression();
		}
	}
	
	public static void main(String[] args) {
		
		DefaultRewritingProcess process = new DefaultRewritingProcess(null);

		SingleVariableEqualityConstraint constraint = new SingleVariableEqualityConstraint(parse("X"), new EqualityConstraintTheory());
		constraint = constraint.conjoin(parse("X = Y"), process);
		constraint = constraint.conjoin(parse("X = Z"), process);
		constraint = constraint.conjoin(parse("X != W"), process);
		constraint = constraint.conjoin(parse("X != U"), process);
		
		ContextDependentProblemStepSolver problem = new SatisfiabilityOfSingleVariableEqualityConstraintStepSolver(constraint);

		MultiVariableConstraint contextualConstraint = new CompleteMultiVariableConstraint(new EqualityConstraintTheory());
		
		Expression result = solve(problem, contextualConstraint, process);
		
		System.out.println("result: " + result);	
	}
}