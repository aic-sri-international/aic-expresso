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
package com.sri.ai.test.grinder.sgdpll.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.LinearRealArithmeticConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.SingleVariableLinearRealArithmeticConstraint;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.MeasureEquivalentIntervalOfSingleVariableLinearRealArithmeticConstraintStepSolver;

@Beta
public class LinearRealArithmeticConstraintTheoryTest {

	@Test
	public void testSatisfyingValues() {
		ConstraintTheory constraintTheory = new LinearRealArithmeticConstraintTheory(true, true);
		Context context = constraintTheory.makeContextWithTestingInformation();

		Expression variable;
		String constraintString;
		Expression expected;
		
		variable = parse("X");
		constraintString = "true";
		expected = parse("[0;4]");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X < 3";
		expected = parse("[0;3[");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3 and X <= 3";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4";
		expected = parse("]3.1; 3.4]");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4 and X != 3.2";
		expected = parse("]3.1; 3.4]");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2";
		expected = parse("]3.1; 3.4]");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2 and X = 3.2";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.2";
		expected = parse("{ 3.2 }");
		runSatisfyingValuesTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7";
		expected = parse("]3.1; 3.4]");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 8";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7 and X = 8";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7 and X = 8";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);

	
	
		variable = parse("X");
		constraintString = "X < Y";
		expected = parse("[0;Y["); // initial 'expected' was "if Y <= 4 then [0;Y[ else [0;4]" but Y's type is also [0;4] so condition is always true -- see next test for a situation in which condition is not always the same
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		// keep these tests together
		variable = parse("X");
		constraintString = "X < Y + 1";
		expected = parse("if Y > 3 then [0;4] else [0 ; Y + 1["); // see previous test for situation in which condition is always true
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3 and X <= 3";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4";
		expected = parse("]3.1; 3.4]");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4 and X != 3.2";
		expected = parse("]3.1; 3.4]");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2";
		expected = parse("]3.1; 3.4]");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2 and X = 3.2";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.2";
		expected = parse("{ 3.2 }");
		runSatisfyingValuesTest(variable, constraintString, expected, context);

		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.6";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7";
		expected = parse("]3.1; 3.4]");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 8";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7 and X = 8";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7 and X = 8";
		expected = parse("{}");
		runSatisfyingValuesTest(variable, constraintString, expected, context);
	}

	private void runSatisfyingValuesTest(Expression variable, String constraintString, Expression expected, Context context) {
		System.out.println("Solving for " + variable + " in " + constraintString);

		Constraint constraint
		= new SingleVariableLinearRealArithmeticConstraint(
				variable, true, context.getConstraintTheory());

		constraint = constraint.conjoin(parse(constraintString), context);
		
		ContextDependentExpressionProblemStepSolver stepSolver =
				new MeasureEquivalentIntervalOfSingleVariableLinearRealArithmeticConstraintStepSolver((SingleVariableLinearRealArithmeticConstraint) constraint);
		
		Expression actual = stepSolver.solve(context);

		System.out.println(
				"Variable " + variable + "\nhas possible values:\n" + actual
				+ "\nsatisfying constraint:\n" + constraintString + "\n");

		assertEquals(expected, actual);
	}
}