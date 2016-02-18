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
package com.sri.ai.test.grinder.sgdpll.theory.inequality;

import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static org.junit.Assert.fail;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TypeContext;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.core.constraint.CompleteMultiVariableContext;
import com.sri.ai.grinder.sgdpll.core.solver.SummationOnIntegerInequalityAndPolynomialStepSolver;
import com.sri.ai.grinder.sgdpll.interpreter.SymbolicCommonInterpreter;
import com.sri.ai.grinder.sgdpll.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.inequality.SingleVariableInequalityConstraint;

@Beta
public class SummationOnIntegerInequalityAndPolynomialStepSolverTest {

	@Test
	public void simpleBodyTest() {
		ConstraintTheory constraintTheory = new InequalityConstraintTheory(true, true);
		Context contextualConstraint = constraintTheory.makeContextualConstraintWithTestingInformation();

		Expression variable;
		String constraintString;
		Expression body;
		Expression expected;
		
		variable = parse("I");
		body = parse("I");

		constraintString = "true";
		expected = parse("10");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I != 3";
		expected = parse("7");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I < 3";
		expected = parse("3");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "false";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I > 3 and I < 3";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I > 1 and I < 3 and I != 2";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
	}

	@Test
	public void polynomialBodyTest() {
		ConstraintTheory constraintTheory = new InequalityConstraintTheory(true, true);
		Context context = new TypeContext(constraintTheory);
		context = constraintTheory.extendWithTestingInformation(context);
		Context contextualConstraint = new CompleteMultiVariableContext(constraintTheory, context);
	
		context = contextualConstraint;

		Expression variable;
		String constraintString;
		Expression body;
		Expression expected;
		
		variable = parse("I");
		body = parse("I^2 - I + 1");

		constraintString = "true";
		expected = parse("25");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I != 3";
		expected = parse("18");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I < 3";
		expected = parse("5");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "false";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I > 3 and I < 3";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I > 1 and I < 3 and I != 2";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
	}
	
	@Test
	public void polynomialBodyWithADifferentVariableTest() {
		ConstraintTheory constraintTheory = new InequalityConstraintTheory(true, true);
		Context context = new TypeContext(constraintTheory);
		context = constraintTheory.extendWithTestingInformation(context);
		Context contextualConstraint = new CompleteMultiVariableContext(constraintTheory, context);
	
		context = contextualConstraint;

		Expression variable;
		String constraintString;
		Expression body;
		Expression expected;
		
		variable = parse("I");
		body = parse("I^2 - J + 1");

		constraintString = "true";
		expected = parse("-5*J + 35");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I != 3";
		expected = parse("-4*J + 25");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I < 3";
		expected = parse("-3*J + 8");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "false";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I > 3 and I < 3";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I > 1 and I < 3 and I != 2";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
	}

	@Test
	public void polynomialBodyAndConstraintWithADifferentVariableTest() {
		ConstraintTheory constraintTheory = new InequalityConstraintTheory(true, true);
		Context context = new TypeContext(constraintTheory);
		context = constraintTheory.extendWithTestingInformation(context);
		Context contextualConstraint = new CompleteMultiVariableContext(constraintTheory, context);
	
		context = contextualConstraint;

		Expression variable;
		String constraintString;
		Expression body;
		Expression expected;
		
		variable = parse("I");
		body = parse("I^2 - J + 1");
	
		constraintString = "I != J";
		expected = parse("-1*J^2 + -4*J + 34");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I <= J and I != J";
		expected = parse("1/3 * J ^ 3 + -1.5 * J ^ 2 + 7/6 * J");
		runTest(variable, constraintString, body, expected, contextualConstraint);
		
		constraintString = "I < J and I > J";
		expected = parse("0");
		runTest(variable, constraintString, body, expected, contextualConstraint);
	}

	private void runTest(Expression variable, String constraintString, Expression body, Expression expected, Context contextualConstraint) {
		ConstraintTheory constraintTheory = contextualConstraint.getConstraintTheory();
		Constraint constraint
		= new SingleVariableInequalityConstraint(
				variable, true, constraintTheory);
		constraint = constraint.conjoin(parse(constraintString), contextualConstraint);
		
		ContextDependentExpressionProblemStepSolver stepSolver =
				new SummationOnIntegerInequalityAndPolynomialStepSolver(
						(SingleVariableInequalityConstraint) constraint,
						body, new SymbolicCommonInterpreter(constraintTheory));
		
		Expression actual = stepSolver.solve(contextualConstraint);

		expected = simplify(expected, contextualConstraint);
		
		System.out.println(
				"sum({{ (on " + variable + " in " + GrinderUtil.getType(variable, contextualConstraint) + ") " + 
						body + " : " + constraintString + " }} = " + actual + "\n");
		
		if (!expected.equals(actual)) {
			Expression difference = apply(MINUS, expected, actual);
			Expression differenceResult = simplify(difference, contextualConstraint);
			if (!differenceResult.equals(ZERO)) {
				System.err.println("Expressions are not equal and even difference is not zero");	
				System.err.println("Expected: " + expected);
				System.err.println("Actual: "   + actual);
				System.err.println("Difference: " + differenceResult);
				fail("Expressions are not equal and even difference is not zero");
			}
		}
		// TODO: correctness test against grounding
	}

	/**
	 * @param expression
	 * @param contextualConstraint
	 * @return
	 */
	private Expression simplify(Expression expression, Context contextualConstraint) {
		return contextualConstraint.getConstraintTheory().simplify(expression, contextualConstraint);
	}
}