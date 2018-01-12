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
package com.sri.ai.test.grinder.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static org.junit.Assert.fail;

import java.util.Random;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionStepSolver;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.solver.DefaultSingleQuantifierEliminationProblem;
import com.sri.ai.grinder.group.Sum;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.tester.TheoryTestingSupport;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.differencearithmetic.SingleVariableDifferenceArithmeticConstraint;
import com.sri.ai.grinder.theory.differencearithmetic.SummationOnDifferenceArithmeticAndPolynomialStepSolver;

@Beta
public class SummationOnDifferenceArithmeticAndPolynomialStepSolverTest {

	public Random makeRandom() {
		return new Random();
	}
	
	@Test
	public void simpleBodyTest() {
		TheoryTestingSupport theoryTestingSupport = 
				TheoryTestingSupport.make(
						makeRandom(), 
						new DifferenceArithmeticTheory(true, true));
		Context context = theoryTestingSupport.makeContextWithTestingInformation();

		Expression variable;
		String constraintString;
		Expression body;
		Expression expected;
		
		variable = parse("I");
		body = parse("I");

		constraintString = "true";
		expected = parse("10");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I != 3";
		expected = parse("7");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I < 3";
		expected = parse("3");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "false";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I > 3 and I < 3";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I > 1 and I < 3 and I != 2";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
	}

	@Test
	public void polynomialBodyTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new DifferenceArithmeticTheory(true, true));
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
	
		Expression variable;
		String constraintString;
		Expression body;
		Expression expected;
		
		variable = parse("I");
		body = parse("I^2 - I + 1");

		constraintString = "true";
		expected = parse("25");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I != 3";
		expected = parse("18");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I < 3";
		expected = parse("5");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "false";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I > 3 and I < 3";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I > 1 and I < 3 and I != 2";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
	}
	
	@Test
	public void polynomialBodyWithADifferentVariableTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new DifferenceArithmeticTheory(true, true));
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
	
		Expression variable;
		String constraintString;
		Expression body;
		Expression expected;
		
		variable = parse("I");
		body = parse("I^2 - J + 1");

		constraintString = "true";
		expected = parse("-(5*J) + 35");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I != 3";
		expected = parse("-(4*J) + 25");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I < 3";
		expected = parse("-(3*J) + 8");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "false";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I > 3 and I < 3";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I > 1 and I < 3 and I != 2";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
	}

	@Test
	public void polynomialBodyAndConstraintWithADifferentVariableTest() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new DifferenceArithmeticTheory(true, true));
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
	
		Expression variable;
		String constraintString;
		Expression body;
		Expression expected;
		
		variable = parse("I");
		body = parse("I^2 - J + 1");
	
		constraintString = "I != J";
		expected = parse("-(J^2) -(4*J) + 34");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I <= J and I != J";
		expected = parse("(1/3 * J ^ 3 - 1.5 * J ^ 2) + 7/6 * J");
		runTest(variable, constraintString, body, expected, context);
		
		constraintString = "I < J and I > J";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, context);
	}

	private void runTest(Expression variable, String constraintString, Expression body, Expression expected, Context context) {
		Theory theory = context.getTheory();
		SingleVariableDifferenceArithmeticConstraint constraint
		= new SingleVariableDifferenceArithmeticConstraint(
				variable, true, theory);
		constraint = (SingleVariableDifferenceArithmeticConstraint) constraint.conjoin(parse(constraintString), context);
		
		Expression typeExpression = context.getTypeExpressionOfRegisteredSymbol(variable);
		
		SingleQuantifierEliminationProblem problem = new DefaultSingleQuantifierEliminationProblem(new Sum(), variable, typeExpression, constraint, body);
		
		ExpressionStepSolver stepSolver = new SummationOnDifferenceArithmeticAndPolynomialStepSolver(problem);
		
		Expression actual = stepSolver.solve(context);

		expected = simplify(expected, context);
		
		System.out.println(
				"sum({{ (on " + variable + " in " + GrinderUtil.getTypeExpressionOfExpression(variable, context) + ") " + 
						body + " : " + constraintString + " }} = " + actual + "\n");
		
		if (!expected.equals(actual)) {
			Expression difference = apply(MINUS, expected, actual);
			Expression differenceResult = simplify(difference, context);
			if (!differenceResult.equals(ZERO)) {
				System.err.println("Expressions are not equal and even difference is not zero");	
				System.err.println("Expected: " + expected);
				System.err.println("Actual: "   + actual);
				System.err.println("Difference: " + differenceResult);
				fail("Expressions are not equal and even difference is not zero. Expected: " + expected + ", actual: " + actual);
			}
		}
		// TODO: correctness test against grounding
	}

	/**
	 * @param expression
	 * @param context
	 * @return
	 */
	private Expression simplify(Expression expression, Context context) {
		return context.getTheory().simplify(expression, context);
	}
}