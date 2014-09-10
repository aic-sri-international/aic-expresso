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
package com.sri.ai.test.grinder.helper;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GetConditionsFor;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.util.Util;

public class GetConditionsForTest {

	private static final int NUMBER_OF_RUNS = 1; // for time-measurement
	
	private Expression expression;
	private Expression variable;
	private Expression expected;
	private Expression expected2;
	private Expression actual;
	RewritingProcess process;
	
	@Before
	public void setUp() {
		process = DirectCardinalityComputationFactory.newCardinalityProcess();
	}
	
	@Test
	public void testGetConditionsForVariable() {
		// How to read these tests:
		// the function derives an equivalent decision tree that has formulas on the variable of interest on the leaves.
		// A test equal to "true" means that *any* value of the variable of interest will do,
		// whereas a test equal to "false" means that the original formula is false in that context.
		// Any other test is going to select a certain set of values for the variable of interest accordingly.
		
		variable   = parse("X");
	
		expression = parse("true");
		expected   = parse("true");
		runConditionsForVariableTest();
	
		expression = parse("false");
		expected   = parse("false");
		runConditionsForVariableTest();
	
		expression = parse("if X = a then true else X = c");
		expected   = parse("X = a or X = c");
		runConditionsForVariableTest();
	
		expression = parse("(Y = a => (X = a or X != a)) and (Y != a => (X = a or X != a))");
		expected   = parse("true");
		runConditionsForVariableTest();
	
		expression = parse("(Y = a => (X = a or X = b or X = c and X = d)) and (Y = b => (X = a or X = b))");
		expected   = parse("if Y = a then X = a or X = b else (if Y = b then X = a or X = b else true)");
		runConditionsForVariableTest();
	
		expression = parse("X = a");
		expected   = parse("X = a");
		runConditionsForVariableTest();
	
		expression = parse("Y = a");
		expected   = parse("if Y = a then true else false");
		runConditionsForVariableTest();
	
		expression = parse("if X = a then true else false");
		expected   = parse("X = a");
		runConditionsForVariableTest();
	
		expression = parse("(if C != paris then true else false) or X = france");
		expected   = parse("if C = paris then X = france else true");
		runConditionsForVariableTest();
	
		expression = parse("C != paris or X = france");
		expected   = parse("if C = paris then X = france else true");
		runConditionsForVariableTest();
	
		expression = parse("(C = paris => X = france) and (C != paris => X != france)");
		expected   = parse("if C = paris then X = france else X != france");
		runConditionsForVariableTest();
	
		expression = parse("if C = paris then true else false");
		expected   = parse("if C = paris then true else false");
		runConditionsForVariableTest();
	
		expression = parse("(C = paris => X = france) and (C = berlin => X != france)");
		expected   = parse("if C = paris then X = france else if C != berlin then true else X != france");
		expected2  = parse("if C = paris then X = france else if C = berlin then X != france else true");
		runConditionsForVariableTest();
	
		expression = parse("(C = paris => X = france) and (C = berlin => X != france) and (C = paris or C = berlin)");
		expected   = parse("if C = paris then X = france else if C = berlin then X != france else false");
		runConditionsForVariableTest();
	
		expression = parse("X = france <=> (C = paris or C = lyon)");
		expected   = parse("if C = paris then X = france else (if C = lyon then X = france else X != france)");
		runConditionsForVariableTest();
	
		expression = parse("if X = france then (C = lyon or C = paris) and C != berlin else if X = germany then C = berlin else C = somewhereElse");
		expected   = parse("if C = lyon then X = france else (if C = paris then X = france else (if C = berlin then X = germany else (if C = somewhereElse then X != france and X != germany else false)))");
		runConditionsForVariableTest();
	
		expression = parse("X = male => C = human");
		expected   = parse("if C = human then true else X != male");
		runConditionsForVariableTest();
	}

	public void runConditionsForVariableTest() {
		for (int i = 0; i != NUMBER_OF_RUNS; i++) {
			process = DirectCardinalityComputationFactory.newCardinalityProcess();
			RewritingProcess subProcess =
					GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(
							expression, process);
			actual = GetConditionsFor.getConditionsForVariable(variable, expression, subProcess);
		}
		
		boolean success = expected.equals(actual) || expected2 != null && expected2.equals(actual);
		if (!success) {
			System.err.println("Test failed for getConditionsForVariable");
			System.err.println(expression);
			System.err.println("should have been transformed to");
			System.err.println(expected);
			if (expected2 != null) {
				System.err.println("or alternatively to");
				System.err.println(expected2);
			}
			System.err.println("but was instead transformed to");
			System.err.println(actual);
		}
		Assert.assertTrue(success);
		System.out.println(expression + "\n   ->   " + actual + "\n");
	}

	@Test
	public void testGetBDD() {
		expression = parse("true");
		expected   = parse("true");
		runGetBDDTest();
	
		expression = parse("false");
		expected   = parse("false");
		runGetBDDTest();
	
		expression = parse("if X = a then true else X = c");
		expected   = parse("if X = a then true else X = c");
		runGetBDDTest();
	
		expression = parse("(Y = a => (X = a or X != a)) and (Y != a => (X = a or X != a))");
		expected   = parse("true");
		runGetBDDTest();
	
		expression = parse("(Y = a => (X = a or X = b or X = c and X = d)) and (Y = b => (X = a or X = b))");
		expected   = parse("if Y = a then if X = a then true else X = b else (if Y = b then if X = a then true else X = b else true)");
		runGetBDDTest();
	
		expression = parse("X = a");
		expected   = parse("X = a");
		runGetBDDTest();
	
		expression = parse("if X = a then true else false");
		expected   = parse("X = a");
		runGetBDDTest();

		expression = parse("if X != a then false else true");
		expected   = parse("X = a");
		runGetBDDTest();

		expression = parse("X != a");
		expected   = parse("if X = a then false else true");
		runGetBDDTest();

		expression = parse("if X = france then (C = lyon or C = paris) and C != berlin else if X = germany then C = berlin else C = somewhereElse");
		expected   = parse("if X = france then if C = lyon then true else C = paris else (if X = germany then C = berlin else C = somewhereElse)");
		runGetBDDTest();
	}

	public void runGetBDDTest() {
		for (int i = 0; i != NUMBER_OF_RUNS; i++) {
			process = DirectCardinalityComputationFactory.newCardinalityProcess();
			RewritingProcess subProcess =
					GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(
							expression, process);
			actual = GetConditionsFor.getBDDOfQuantifierFreeFormula(expression, subProcess);
		}
		
		boolean success = expected.equals(actual) || expected2 != null && expected2.equals(actual);
		if (!success) {
			System.err.println("Test failed for getConditionsForVariable");
			System.err.println(expression);
			System.err.println("should have been transformed to");
			System.err.println(expected);
			if (expected2 != null) {
				System.err.println("or alternatively to");
				System.err.println(expected2);
			}
			System.err.println("but was instead transformed to");
			System.err.println(actual);
		}
		Assert.assertTrue(success);
		System.out.println(expression + "\n   ->   " + actual + "\n");
	}


	//@Test // algorithm is incorrect
	public void testGetImpliedValueFromBDD() {
		variable   = parse("X");
		
		expression = parse("true");
		expected   = null;
		runGetImpliedValueFromBDDTest();
		
		expression = parse("Z = b");
		expected   = null;
		runGetImpliedValueFromBDDTest();
		
		expression = parse("X = a");
		expected   = parse("a");
		runGetImpliedValueFromBDDTest();
		
		expression = parse("X = a or X = b");
		expected   = null;
		runGetImpliedValueFromBDDTest();
		
		expression = parse("X = a and X = b");
		expected   = null;
		runGetImpliedValueFromBDDTest();
		
		expression = parse("X = a and X = a");
		expected   = parse("a");
		runGetImpliedValueFromBDDTest();
		
		expression = parse("if Y = b then X = a else X = b");
		expected   = parse("if Y = b then a else b");
		runGetImpliedValueFromBDDTest();
		
		expression = parse("if Z = b then X = a and X = Y else X = b and X = Y");
		expected   = parse("if Z = b then a else b");
		// expected   = parse("Y"); that would be nicer
		runGetImpliedValueFromBDDTest();
		
		expression = parse("X = Y and X = a");
		expected   = parse("Y");
		runGetImpliedValueFromBDDTest();
		
		expression = parse("X = a and X = Y");
		expected   = parse("a");
		runGetImpliedValueFromBDDTest();
		
		expression = parse("X = Y and X = a  or  X = Y and X = b");
		expected   = parse("Y");
		runGetImpliedValueFromBDDTest();
		
		// The following is the test that made me realize the function is incorrect
		// In the Z = b branch, X is known to be a and the branch becomes Y = a and X = a
		// Then the BDD conditions on Y = a and find that if that is true, X = a, but if that is false,
		// then there is no solution.
		// Therefore it rejects the entire branch because under certain conditions it cannot find a value for X.
		// This is odd because X = Y is indeed implied by the formula, and Y has a fixed, albeit unknown, value.
		// It is true that if Y = c, then the formula is unsatisfiable, but in that case F => X = Y is still true!
		// And here lies the problem, the algorithm does not that into account that we are seeking the implication,
		// which is a license for considering only the possible worlds in which the formula holds.
		// One may propose to ignore 'false' leaf nodes when we find them, since we know the formula will always be satisfiable.
		// The problem with that is that some 'false' leaf nodes are legitimate.
		// For example, consider the formula 'X = a and X != b'
		// If we condition on X = b, we get false, but we do not want to dismiss that,
		// because it is what tells us that X cannot be b.
		// Perhaps there is a way to address this, but since I now realize the original pick single element works
		// when we know that there is a single element, I will leave this algorithm sitting here for a while.
//		expression = parse("if Z = b then X = Y and X = a else X = b and X = Y");
//		expected   = parse("Y"); // fails! Returns 'null'
//		runGetImpliedValueFromBDDTest();
		
		expression = parse("if Z = b then X = a and X = Y else X = b and X = Y");
		expected   = parse("if Z = b then a else b");
		// expected   = parse("Y"); that would be nicer
		runGetImpliedValueFromBDDTest();
		
		expression = parse("Y = b => X = a");
		expected   = null;
		runGetImpliedValueFromBDDTest();
	}

	@SuppressWarnings("deprecation")
	public void runGetImpliedValueFromBDDTest() {
		Expression bdd = null;
		for (int i = 0; i != NUMBER_OF_RUNS; i++) {
			process = DirectCardinalityComputationFactory.newCardinalityProcess();
			RewritingProcess subProcess =
					GrinderUtil.extendContextualSymbolsWithFreeSymbolsInExpressionwithUnknownTypeForSetUpPurposesOnly(
							expression, process);
			bdd = GetConditionsFor.getBDDOfQuantifierFreeFormula(expression, subProcess);
			actual = GetConditionsFor.getImpliedValueFromBDD(variable, bdd, subProcess);
		}
		
		System.out.println(expression + "\n   (using BDD) " + bdd + "\n   -> " + actual + "\n");
		boolean success = Util.equals(expected, actual);
		if (!success) {
			System.err.println("Test failed for getImpliedValueFromBDD");
			System.err.println(expression);
			System.err.println("should have resulted in");
			System.err.println(expected);
			if (expected2 != null) {
				System.err.println("or alternatively to");
				System.err.println(expected2);
			}
			System.err.println("but has instead resulted in");
			System.err.println(actual);
		}
		Assert.assertTrue(success);
	}
}
