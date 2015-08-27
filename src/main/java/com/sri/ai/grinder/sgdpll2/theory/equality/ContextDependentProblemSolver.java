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
package com.sri.ai.grinder.sgdpll2.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.parse;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblem;
import com.sri.ai.grinder.sgdpll2.core.DefaultMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.MultiVariableConstraint;

/**
 * Solves a {@link ContextDependentProblem} by successively conditioning the context on provided splitters.
 * 
 * @author braz
 *
 */
@Beta
public class ContextDependentProblemSolver {

	public static Expression solve(ContextDependentProblem problem, MultiVariableConstraint contextualConstraint, RewritingProcess process) {
		ContextDependentProblem.SolutionStep step = problem.step(contextualConstraint, process);
		if (step.itDepends()) {
			Expression splitter = step.getExpression();
			MultiVariableConstraint subContextualConstraint1 = contextualConstraint.conjoin(splitter, process);
			Expression subSolution1 = solve(problem, subContextualConstraint1, process);
			
			Expression splitterNegation = contextualConstraint.getConstraintTheory().getLiteralNegation(splitter);
			MultiVariableConstraint subContextualConstraint2 = contextualConstraint.conjoin(splitterNegation, process);
			Expression subSolution2 = solve(problem, subContextualConstraint2, process);
			
			return IfThenElse.make(splitter, subSolution1, subSolution2);
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
		
		ContextDependentProblem problem = new SatisfiabilityOfSingleVariableEqualityConstraint(constraint);

		MultiVariableConstraint contextualConstraint = new DefaultMultiVariableConstraint(new EqualityConstraintTheory());
		
		Expression result = solve(problem, contextualConstraint, process);
		
		System.out.println("result: " + result);	
	}
}