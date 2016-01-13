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
package com.sri.ai.test.grinder.sgdpll2.theory.base;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.simplifier.SeriallyMergedMapBasedSimplifier;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.inequality.InequalitySimplifier;
import com.sri.ai.grinder.library.number.NumericSimplifier;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.sgdpll2.core.solver.EvaluatorStepSolver;
import com.sri.ai.grinder.sgdpll2.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.propositional.PropositionalConstraintTheory;

@Beta
public class EvaluatorStepSolverTest {

	@Test
	public void test() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		ConstraintTheory constraintTheory
		= new CompoundConstraintTheory(
				new EqualityConstraintTheory(false, true),
				new InequalityConstraintTheory(false, true),
				new PropositionalConstraintTheory());
		RewritingProcess process = new DefaultRewritingProcess(null);
		process = constraintTheory.extendWithTestingInformation(process);
		Constraint2 contextualConstraint = new CompleteMultiVariableConstraint(constraintTheory);
		Simplifier topSimplifier
		= new SeriallyMergedMapBasedSimplifier(
				new InequalitySimplifier(),
				new NumericSimplifier(),
				new BooleanSimplifier());
		
		String expressionString;
		Expression expected;
		
		expressionString = "0";
		expected = parse("0");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, process);	
		
		expressionString = "(if I > J then 1 else 2) + (if I <= J then 30 else 40)";
		expected = parse("if I > J then 41 else 32");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, process);	
		
		expressionString = "(if I > J then 1 else 2) + (if I <= J then 3 else 4)";
		expected = parse("5");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, process);	
		
		expressionString = "(if I > J then if P or Q then 1 else 2 else 5) + (if I <= J then 3 else if not Q then 4 else -3)";
		expected = parse("if I > J then if P then if not Q then 5 else -2 else if Q then -2 else 6 else 8");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, process);	
	}

	private void runTest(String expressionString, Expression expected, Constraint2 contextualConstraint, Simplifier topSimplifier, RewritingProcess process) {
		Expression expression = parse(expressionString);
		EvaluatorStepSolver stepSolver = new EvaluatorStepSolver(expression, topSimplifier);
		Expression solution = ContextDependentExpressionProblemSolver.solve(stepSolver, contextualConstraint, process);
		System.out.println(expression + " -----> " + solution + "\n");
		assertEquals(expected, solution);
	}
}