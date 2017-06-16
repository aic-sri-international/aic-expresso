/*
t * Copyright (c) 2013, SRI International
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
package com.sri.ai.test.grinder.sgdpllt.theory.base;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Random;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.function.BruteForceFunctionTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;

@Beta
public class EvaluationTest {

	public Random makeRandom() {
		return new Random();
	}
	
	@Test
	public void testEvaluationOfFunctionApplications() {
		TheoryTestingSupport theoryTestingSupport
		= TheoryTestingSupport.make(
				makeRandom(), 
				new CompoundTheory(
						new EqualityTheory(false, true),
						new DifferenceArithmeticTheory(false, true),
						new PropositionalTheory()));

		Map<String, Type> variablesAndTypes = new LinkedHashMap<>(theoryTestingSupport.getVariableNamesAndTypesForTesting());
		Type booleanType = variablesAndTypes.get("P");
		variablesAndTypes.put("S", booleanType);
		variablesAndTypes.put("T", booleanType);
		variablesAndTypes.put("U", booleanType);
		theoryTestingSupport.setVariableNamesAndTypesForTesting(variablesAndTypes);
		
		Context context = theoryTestingSupport.makeContextWithTestingInformation();

		String expressionString;
		Expression expected;
		
		expressionString = "0";
		expected = parse("0");
		runTest(expressionString, expected, context);	
		
		expressionString = "I > J";
		expected = parse("I > J");
		runTest(expressionString, expected, context);	
		
		expressionString = "I > J and I < J";
		expected = parse("false");
		runTest(expressionString, expected, context);	
		
		expressionString = "(if I > J then 1 else 2) + (if I <= J then 30 else 40)";
		expected = parse("if I > J then 41 else 32");
		runTest(expressionString, expected, context);	
		
		expressionString = "(if I > J then 1 else 2) + (if I <= J then 3 else 4)";
		expected = parse("5");
		runTest(expressionString, expected, context);	
		
		expressionString = "(if I > J then if P or Q then 1 else 2 else 5) + (if I <= J then 3 else if not Q then 4 else -3)";
		expected = parse("if I > J then if P then if not Q then 5 else -2 else if Q then -2 else 6 else 8");
		runTest(expressionString, expected, context);	
		
		expressionString = "(if I > J then if P or X = a or Y != b then 1 else 2 else 5) + (if I <= J then 3 else if not (X != a or Y = c and Q) then 4 else -3)";
		expected = parse("if I > J then if P then if X != a then -2 else if Y = c then if Q then -2 else 5 else 5 else if X = a then if Y = c then if Q then -2 else 5 else 5 else if Y != b then -2 else -1 else 8");
		runTest(expressionString, expected, context);	
		
		expressionString = "if P and Q and R then 1 else 0";
		expected = parse("if P then if Q then if R then 1 else 0 else 0 else 0");
		runTest(expressionString, expected, context);	
		
		expressionString = "if P and Q and R and S and T and U then 1 else 0";
		expected = parse("if P then if Q then if R then if S then if T then if U then 1 else 0 else 0 else 0 else 0 else 0 else 0");
		runTest(expressionString, expected, context);	
	}

	@Test
	public void testEvaluationOfGroupOperationsOnSets() {
		TheoryTestingSupport theoryTestingSupport
		= TheoryTestingSupport.make(makeRandom(), new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, true),
				new PropositionalTheory()));

		Map<String, Type> variablesAndTypes = new LinkedHashMap<>(theoryTestingSupport.getVariableNamesAndTypesForTesting());
		Type booleanType = variablesAndTypes.get("P");
		variablesAndTypes.put("S", booleanType);
		variablesAndTypes.put("T", booleanType);
		variablesAndTypes.put("U", booleanType);
		theoryTestingSupport.setVariableNamesAndTypesForTesting(variablesAndTypes);
		
		Context context = theoryTestingSupport.makeContextWithTestingInformation();

		String expressionString;
		Expression expected;
		
		expressionString = "sum( {{ (on I in 1..10) 3 : I != 4 and P }} )";
		expected = parse("if P then 27 else 0");
		runTest(expressionString, expected, context);	

		expressionString = "sum( {{ (on ) 3 : I != 4 and P }} )";
		expected = parse("if I != 4 then if P then 3 else 0 else 0");
		runTest(expressionString, expected, context);	

		expressionString = "sum( {{ (on ) 3 : P and not P }} )";
		expected = parse("0");
		runTest(expressionString, expected, context);	

		expressionString = "sum( {{ (on I in 1..10, J in 1..2) 3 : I != 4 }} )";
		expected = parse("54");
		runTest(expressionString, expected, context);	

		expressionString = "sum( {{ (on I in 1..10, P in Boolean) 3 : I != 4 }} )";
		expected = parse("54");
		runTest(expressionString, expected, context);	

		
		expressionString = "max( {{ (on I in 1..10) 3 : I != 4 and P }} )";
		expected = parse("if P then 3 else -infinity");
		runTest(expressionString, expected, context);	

		expressionString = "max( {{ (on ) 3 : I != 4 and P }} )";
		expected = parse("if I != 4 then if P then 3 else -infinity else -infinity");
		runTest(expressionString, expected, context);	

		expressionString = "max( {{ (on ) 3 : P and not P }} )";
		expected = parse("-infinity");
		runTest(expressionString, expected, context);	

		expressionString = "max( {{ (on I in 1..10, J in 1..2) 3 : I != 4 }} )";
		expected = parse("3");
		runTest(expressionString, expected, context);	

		expressionString = "max( {{ (on I in 1..10, P in Boolean) 3 : I != 4 }} )";
		expected = parse("3");
		runTest(expressionString, expected, context);	
	}

	@Test
	public void testEvaluationOfCardinalityExpressions() {
		TheoryTestingSupport theoryTestingSupport
		= TheoryTestingSupport.make(makeRandom(), new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, true),
				new PropositionalTheory()));
	
		Map<String, Type> variablesAndTypes = new LinkedHashMap<>(theoryTestingSupport.getVariableNamesAndTypesForTesting());
		Type booleanType = variablesAndTypes.get("P");
		variablesAndTypes.put("S", booleanType);
		variablesAndTypes.put("T", booleanType);
		variablesAndTypes.put("U", booleanType);
		theoryTestingSupport.setVariableNamesAndTypesForTesting(variablesAndTypes);
		
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
	
		String expressionString;
		Expression expected;
		
		expressionString = "| {{ (on I in 1..10) 3 : I != 4 and P }} |";
		expected = parse("if P then 9 else 0");
		runTest(expressionString, expected, context);	
		
		expressionString = "| I in 1..10 : I != 4 and P |";
		expected = parse("if P then 9 else 0");
		runTest(expressionString, expected, context);
	
		expressionString = "| {{ (on ) 3 : I != 4 and P }} |";
		expected = parse("if I != 4 then if P then 1 else 0 else 0");
		runTest(expressionString, expected, context);
		
		expressionString = "| : I != 4 and P |";
		expected = parse("if I != 4 then if P then 1 else 0 else 0");
		runTest(expressionString, expected, context);	
	
		expressionString = "| {{ (on ) 3 : P and not P }} |";
		expected = parse("0");
		runTest(expressionString, expected, context);	
		
		expressionString = "| : P and not P |";
		expected = parse("0");
		runTest(expressionString, expected, context);
	
		expressionString = "| {{ (on I in 1..10, J in 1..2) 3 : I != 4 }} |";
		expected = parse("18");
		runTest(expressionString, expected, context);	
		
		expressionString = "| I in 1..10, J in 1..2 : I != 4 |";
		expected = parse("18");
		runTest(expressionString, expected, context);
	
		expressionString = "| {{ (on I in 1..10, P in Boolean) 3 : I != 4 }} |";
		expected = parse("18");
		runTest(expressionString, expected, context);	
		
		expressionString = "| I in 1..10, P in Boolean: I != 4 |";
		expected = parse("18");
		runTest(expressionString, expected, context);
	}

	@Test
	public void testEvaluationOfQuantifiedExpressions() {
		TheoryTestingSupport theoryTestingSupport
		= TheoryTestingSupport.make(makeRandom(), new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, true),
				new PropositionalTheory()));
	
		Map<String, Type> variablesAndTypes = new LinkedHashMap<>(theoryTestingSupport.getVariableNamesAndTypesForTesting());
		Type booleanType = variablesAndTypes.get("P");
		variablesAndTypes.put("S", booleanType);
		variablesAndTypes.put("T", booleanType);
		variablesAndTypes.put("U", booleanType);
		theoryTestingSupport.setVariableNamesAndTypesForTesting(variablesAndTypes);
		
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
	
		String expressionString;
		Expression expected;
		
		expressionString = "for all I in 1..10 : (I != 4 or I = 4) and P";
		expected = parse("P");
		runTest(expressionString, expected, context);	

		// the following example tests that quantified expressions that are function arguments get evaluated properly, as there once was a bug preventing it
		expressionString = "not(for all I in 1..10 : (I != 4 or I = 4))";
		expected = parse("false");
		runTest(expressionString, expected, context);	
	
		expressionString = "for all I in 1..10 : for all J in 1..2 : I != 4";
		expected = parse("false");
		runTest(expressionString, expected, context);	
	
		expressionString = "for all I in 1..10 : for all P in Boolean : I != 4 or I = 4 and (P or not P)";
		expected = parse("true");
		runTest(expressionString, expected, context);	

		
		expressionString = "there exists I in 1..10 : I != 4 and P";
		expected = parse("P");
		runTest(expressionString, expected, context);	
	
		expressionString = "there exists I in 1..10 : there exists J in 1..2 : I != 4 and J != 1";
		expected = parse("true");
		runTest(expressionString, expected, context);	
	
		expressionString = "there exists I in 1..10 : there exists P in Boolean : I != 4 and P";
		expected = parse("true");
		runTest(expressionString, expected, context);	
	}
	

	@Test
	public void testEvaluationOfQuantifiersOverFunctions() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport
				.make(
						makeRandom(), new CompoundTheory(
								new EqualityTheory(false, true), 
								new DifferenceArithmeticTheory(false, true),
								new PropositionalTheory(),
								new BruteForceFunctionTheory()
								));

		Map<String, Type> variablesAndTypes = new LinkedHashMap<>(
				theoryTestingSupport.getVariableNamesAndTypesForTesting());
		Type booleanType = variablesAndTypes.get("P");
		variablesAndTypes.put("S", booleanType);
		variablesAndTypes.put("T", booleanType);
		variablesAndTypes.put("U", booleanType);
		theoryTestingSupport.setVariableNamesAndTypesForTesting(variablesAndTypes);

		Context context = theoryTestingSupport.makeContextWithTestingInformation();

		String expressionString;
		Expression expected;

		expressionString = "sum( {{ (on f in 0..2 -> Boolean)  if f(0) and f(1) then 2 else 3  :  f(2) }} )";
		expected = parse("11"); // 2+3+3+3
		runTest(expressionString, expected, context);
		
		expressionString = "sum({{ (on f in 1..2 -> Boolean, g in 1..2 -> Boolean)  if f(1) and g(2) then 2 else 3  :  f(2) }} )";
		expected = parse("22"); 
		runTest(expressionString, expected, context);
		
		expressionString = "sum({{ (on f in 1..2 -> Boolean, g in 1..2 -> Boolean)  if f(1) and g(2) then 2 else 3 }} )";
		expected = parse("44"); 
		runTest(expressionString, expected, context);
		
		expressionString = "sum({{ (on f in 1..2 -> Boolean, g in 1..2 -> Boolean)  if f(1) then 2 else 3 }} )";
		expected = parse("40"); 
		runTest(expressionString, expected, context);
		
		expressionString = "sum({{ (on f in 1..2 -> Boolean)  if f(1) then 2 else 3 }} )";
		expected = parse("10"); 
		runTest(expressionString, expected, context);
		
		expressionString = "sum({{ (on f in 1..2 -> 1..2)  if f(1) = 1 then 2 else 3 }} )";
		expected = parse("10"); 
		runTest(expressionString, expected, context);
		
		expressionString = "sum({{ (on f in 1..2 -> 1..2)  f(1) }} )";
		expected = parse("6"); 
		runTest(expressionString, expected, context);
		
		expressionString = "sum({{ (on f in '->'(1..2))  f() }} )";
		expected = parse("3"); 
		runTest(expressionString, expected, context);
		
		expressionString = "sum({{ (on f in '->'(1..2))  f }} )";
		expected = parse("(lambda : 1) + (lambda : 2)"); 
		runTest(expressionString, expected, context);

		expressionString = "product( {{ (on f in 0..2 -> Boolean)  if f(0) and f(1) then 2 else 3  :  f(2) }} )";
		expected = parse("54"); // 2*3*3*3
		runTest(expressionString, expected, context);

		expressionString = "max( {{ (on f in 0..2 -> Boolean)  if f(0) and f(1) then 2 else 3  :  f(2) }} )";
		expected = parse("3");
		runTest(expressionString, expected, context);
		
		expressionString = "| f in 0..2 -> Boolean : f(0) |";
		expected = parse("4");
		runTest(expressionString, expected, context);
		
		expressionString = "| f in 0..2 -> Boolean : f(0) |";
		expected = parse("4");
		runTest(expressionString, expected, context);
		
		expressionString = "| f in 0..2 -> Boolean : f(0) |";
		expected = parse("4");
		runTest(expressionString, expected, context);
		
		expressionString = "| f in x(0..2, 0..2) -> Boolean : f(0, 0) |";
		expected = parse("256");
		runTest(expressionString, expected, context);
		
		expressionString = "| f in 0..2 x 0..2 -> Boolean : f(0, 0) |";
		expected = parse("256");
		runTest(expressionString, expected, context);
		
		expressionString = "for all f in 0..2 -> Boolean : f(0)";
		expected = parse("false");
		runTest(expressionString, expected, context);
		
		expressionString = "for all f in x(0..2) -> Boolean : (f(0) or not f(0)) and P";
		expected = parse("P");
		runTest(expressionString, expected, context);	
		
		expressionString = "there exists f in '->'(x(0..2), Boolean) : f(0)";
		expected = parse("true");
		runTest(expressionString, expected, context);
	}

	private void runTest(String expressionString, Expression expected, Context context) {
		Expression expression = parse(expressionString);
		ExpressionLiteralSplitterStepSolver stepSolver = context.getTheory().makeEvaluatorStepSolver(expression);
		System.out.println("Evaluating " + expression);
		Expression solution = ContextDependentExpressionProblemSolver.staticSolve(stepSolver, context);
		System.out.println(expression + " -----> " + solution + "\n");
		assertEquals(expected, solution);
	}
}