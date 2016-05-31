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
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.LinearRealArithmeticConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.MeasureEquivalentIntervalOfSingleVariableLinearRealArithmeticConstraintStepSolver;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.MeasureOfSingleVariableLinearRealArithmeticConstraintStepSolver;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.SingleVariableLinearRealArithmeticConstraint;

@Beta
public class LinearRealArithmeticConstraintTheoryTest {

	@Test
	public void testMeasureEquivalentInterval() {
		ConstraintTheory constraintTheory = new LinearRealArithmeticConstraintTheory(true, true);
		Context context = constraintTheory.makeContextWithTestingInformation();

		Expression variable;
		String constraintString;
		Expression expected;
		
		variable = parse("X");
		constraintString = "true";
		expected = parse("[0;4]");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X < 3";
		expected = parse("[0;3[");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3 and X <= 3";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4";
		expected = parse("]3.1; 3.4]");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4 and X != 3.2";
		expected = parse("]3.1; 3.4]");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2";
		expected = parse("]3.1; 3.4]");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2 and X = 3.2";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.2";
		expected = parse("{ 3.2 }");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7";
		expected = parse("]3.1; 3.4]");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 8";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7 and X = 8";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7 and X = 8";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);

	
	
		variable = parse("X");
		constraintString = "X < Y";
		expected = parse("if Y > 0 then [0;Y[ else {}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		// keep these tests together
		variable = parse("X");
		constraintString = "X < Y + 1";
		expected = parse("if Y > 3 then [0;4] else [0 ; Y + 1["); // see previous test for situation in which condition is always true
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3 and X <= 3";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4";
		expected = parse("]3.1; 3.4]");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4 and X != 3.2";
		expected = parse("]3.1; 3.4]");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2";
		expected = parse("]3.1; 3.4]");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2 and X = 3.2";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.2";
		expected = parse("{ 3.2 }");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);

		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.6";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7";
		expected = parse("]3.1; 3.4]");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 8";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7 and X = 8";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7 and X = 8";
		expected = parse("{}");
		runMeasureEquivalentIntervalTest(variable, constraintString, expected, context);
	}

	private void runMeasureEquivalentIntervalTest(Expression variable, String constraintString, Expression expected, Context context) {
		runTest(
				variable, 
				constraintString, 
				expected, 
				"measure-equivalent interval", 
				c -> new MeasureEquivalentIntervalOfSingleVariableLinearRealArithmeticConstraintStepSolver(
						(SingleVariableLinearRealArithmeticConstraint) c),
				context);
	}

	@Test
	public void testSatisfiability() {
		ConstraintTheory constraintTheory = new LinearRealArithmeticConstraintTheory(true, true);
		Context context = constraintTheory.makeContextWithTestingInformation();

		Expression variable;
		String constraintString;
		Expression expected;
		
		variable = parse("X");
		constraintString = "true";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X < 3";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3 and X <= 3";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4 and X != 3.2";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2 and X = 3.2";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.2";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 8";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7 and X = 8";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7 and X = 8";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);

	
	
		variable = parse("X");
		constraintString = "X < Y";
		expected = parse("Y > 0");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		// keep these tests together
		variable = parse("X");
		constraintString = "X < Y + 1";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X < Y - 4";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);

		variable = parse("X");
		constraintString = "X < 2 - Y";
		expected = parse("Y < 2");
		runSatisfiabilityTest(variable, constraintString, expected, context);


		variable = parse("X");
		constraintString = "X > Y";
		expected = parse("Y < 4");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		// keep these tests together
		variable = parse("X");
		constraintString = "X > Y + 1";
		expected = parse("Y < 3");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X >= Y + 4";
		expected = parse("Y <= 0");
		runSatisfiabilityTest(variable, constraintString, expected, context);

		variable = parse("X");
		constraintString = "X >= Y + 2";
		expected = parse("Y <= 2");
		runSatisfiabilityTest(variable, constraintString, expected, context);


		variable = parse("X");
		constraintString = "X >= Y";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		// keep these tests together
		variable = parse("X");
		constraintString = "X >= Y + 1";
		expected = parse("Y <= 3");
		runSatisfiabilityTest(variable, constraintString, expected, context);


		variable = parse("X");
		constraintString = "X > 3 and X <= 3";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4 and X != 3.2";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2 and X = 3.2";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.2";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);

		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.6";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7";
		expected = parse("true");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);

		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 8";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7 and X = 8";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7 and X = 8";
		expected = parse("false");
		runSatisfiabilityTest(variable, constraintString, expected, context);
	}

	private void runSatisfiabilityTest(Expression variable, String constraintString, Expression expected, Context context) {
		runTest(
				variable, 
				constraintString, 
				expected, 
				"satisfiability", 
				c -> new SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver(
						(SingleVariableLinearRealArithmeticConstraint) c),
				context);
	}

	@Test
	public void testMeasure() {
		ConstraintTheory constraintTheory = new LinearRealArithmeticConstraintTheory(true, true);
		Context context = constraintTheory.makeContextWithTestingInformation();
	
		Expression variable;
		String constraintString;
		Expression expected;
		
		variable = parse("X");
		constraintString = "true";
		expected = parse("4");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X < 3";
		expected = parse("3");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3 and X <= 3";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4";
		expected = parse("0.3");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4 and X != 3.2";
		expected = parse("0.3");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2";
		expected = parse("0.3");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2 and X = 3.2";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.2";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
	
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7";
		expected = parse("0.3");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
	
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 8";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7 and X = 8";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7 and X = 8";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
	
	
	
		variable = parse("X");
		constraintString = "X < Y";
		expected = parse("if Y > 0 then Y else 0");
		runMeasureTest(variable, constraintString, expected, context);
		// keep these tests together
		variable = parse("X");
		constraintString = "X < Y + 1";
		expected = parse("if Y > 3 then 4 else Y + 1");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X < Y - 4";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
	
		variable = parse("X");
		constraintString = "X < 2 - Y";
		expected = parse("if Y < 2 then -1*Y + 2 else 0");
		runMeasureTest(variable, constraintString, expected, context);
	
	
		variable = parse("X");
		constraintString = "X > Y";
		expected = parse("if Y < 4 then 4 - Y else 0");
		runMeasureTest(variable, constraintString, expected, context);
		// keep these tests together
		variable = parse("X");
		constraintString = "X > Y + 1";
		expected = parse("if Y < 3 then 4 - (Y + 1) else 0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X >= Y + 4";
		expected = parse("if Y <= 0 then 4 - (Y + 4) else 0");
		runMeasureTest(variable, constraintString, expected, context);
	
		variable = parse("X");
		constraintString = "X >= Y + 2";
		expected = parse("if Y <= 2 then 4 - (Y + 2) else 0");
		runMeasureTest(variable, constraintString, expected, context);
	
	
		variable = parse("X");
		constraintString = "X >= Y";
		expected = parse("4 - Y");
		runMeasureTest(variable, constraintString, expected, context);
		// keep these tests together
		variable = parse("X");
		constraintString = "X >= Y + 1";
		expected = parse("if Y <= 3 then 4 - (Y + 1) else 0");
		runMeasureTest(variable, constraintString, expected, context);
	
	
		variable = parse("X");
		constraintString = "X > 3 and X <= 3";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4";
		expected = parse("0.3");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X > 3.1 and X <= 3.4 and X != 3.2";
		expected = parse("0.3");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2";
		expected = parse("0.3");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 3.2 and X = 3.2";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.2";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
	
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 3.6";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
	
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7";
		expected = parse("0.3");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
	
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 8";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X != 7 and X = 7 and X = 8";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
		
		variable = parse("X");
		constraintString = "X <= 3.4 and X > 3.1 and X = 7 and X = 8";
		expected = parse("0");
		runMeasureTest(variable, constraintString, expected, context);
	}

	private void runMeasureTest(Expression variable, String constraintString, Expression expected, Context context) {
		runTest(
				variable, 
				constraintString, 
				expected, 
				"measure", 
				c -> new MeasureOfSingleVariableLinearRealArithmeticConstraintStepSolver(
						(SingleVariableLinearRealArithmeticConstraint) c),
				context);
	}

	/**
	 * @param variable
	 * @param constraintString
	 * @param expected
	 * @param computedFunction
	 * @param stepSolverMaker
	 * @param context
	 */
	private void runTest(Expression variable, String constraintString, Expression expected, String computedFunction, Function<Constraint, ContextDependentExpressionProblemStepSolver> stepSolverMaker, Context context) {
		System.out.println("Solving " + computedFunction + " for " + variable + " in " + constraintString);
	
		Constraint constraint
		= new SingleVariableLinearRealArithmeticConstraint(
				variable, true, context.getConstraintTheory());
	
		constraint = constraint.conjoin(parse(constraintString), context);
	
		ContextDependentExpressionProblemStepSolver stepSolver = stepSolverMaker.apply(constraint);
		
		Expression actual = stepSolver.solve(context);
	
		System.out.println(
				"Variable " + variable + "\nhas " + computedFunction + ":\n" + actual
				+ "\nfor constraint:\n" + constraintString + "\n");
	
		assertEquals(expected, actual);
	}
}