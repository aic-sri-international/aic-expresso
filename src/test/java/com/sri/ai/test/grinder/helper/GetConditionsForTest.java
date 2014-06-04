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
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;

public class GetConditionsForTest {

	private static final int NUMBER_OF_RUNS = 1; // for time-measurement
	private static final boolean initialVersion = true; 
	private static final boolean dpllVersion    = true;    
	
//	private static final int NUMBER_OF_RUNS = 1000; // for time-measurement
//	private static final boolean initialVersion = true; 
//	private static final boolean dpllVersion    = false;    
	
//	private static final int NUMBER_OF_RUNS = 1000; // for time-measurement
//	private static final boolean initialVersion = false; 
//	private static final boolean dpllVersion    = true;    
	
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
		runTests();
	
		expression = parse("false");
		expected   = parse("false");
		runTests();
	
		expression = parse("X = a");
		expected   = parse("X = a");
		runTests();
	
		expression = parse("Y = a");
		expected   = parse("if Y = a then true else false");
		runTests();
	
		expression = parse("if X = a then true else false");
		expected   = parse("X = a");
		runTests();
	
		expression = parse("(if C != paris then true else false) or X = france");
		expected   = parse("if C != paris then true else X = france");
		runTests();
	
		expression = parse("C != paris or X = france");
		expected   = parse("if C != paris then true else X = france");
		runTests();
	
		expression = parse("(C = paris => X = france) and (C != paris => X != france)");
		expected   = parse("if C = paris then X = france else X != france");
		runTests();
	
		expression = parse("if C = paris then true else false");
		expected   = parse("if C = paris then true else false");
		runTests();
	
		expression = parse("(C = paris => X = france) and (C = berlin => X != france)");
		expected   = parse("if C = paris then X = france else if C != berlin then true else X != france");
		expected2  = parse("if C = paris then X = france else if C = berlin then X != france else true");
		runTests();
	
		expression = parse("(C = paris => X = france) and (C = berlin => X != france) and (C = paris or C = berlin)");
		expected   = parse("if C = paris then X = france else if C = berlin then X != france else false");
		runTests();
	
		expression = parse("X = france <=> (C = paris or C = lyon)");
		expected   = parse("if C = paris then X = france else (if C = lyon then X = france else X != france)");
		runTests();
	
		expression = parse("X = male => C = human");
		expected   = parse("if C = human then true else X != male");
		runTests();
	}

	public void runTests() {
		if (initialVersion) {
			runConditionsForVariableTest();
		}
		if (dpllVersion) {
		runConditionsForVariableDPLLTest();
		}
	}

	public void runConditionsForVariableDPLLTest() {
		for (int i = 0; i != NUMBER_OF_RUNS; i++) {
			process = DirectCardinalityComputationFactory.newCardinalityProcess();
			RewritingProcess subProcess =
					GrinderUtil.extendContextualVariablesWithFreeVariablesInExpressionWithUnknownDomainForSetUpPurposesOnly(
							expression, process);
			
			// run DPLL-style version
			actual = GetConditionsFor.getConditionsForVariableDPLL(variable, expression, subProcess);
			
		}
		String name = "getConditionsForVariableDPLL";
		outputTestResult(name);
	}

	public void runConditionsForVariableTest() {
		for (int i = 0; i != NUMBER_OF_RUNS; i++) {
			process = DirectCardinalityComputationFactory.newCardinalityProcess();
			RewritingProcess subProcess =
					GrinderUtil.extendContextualVariablesWithFreeVariablesInExpressionWithUnknownDomainForSetUpPurposesOnly(
							expression, process);

			// run non-DPLL-style version
			actual = GetConditionsFor.getConditionsForVariable(variable, expression, subProcess);
			// because this version does not return a completely simplified structure
			// we run complete_normalize on it for time-measurement purposes
			if (NUMBER_OF_RUNS > 1) {
				subProcess.rewrite(CardinalityRewriter.R_complete_normalize, actual);
			}
		}
		String name = "getConditionsForVariable";
		outputTestResult(name);
	}

	public void outputTestResult(String name) {
		boolean success = expected.equals(actual) || expected2 != null && expected2.equals(actual);
		if (!success) {
			System.err.println("Test failed for " + name + " version");
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
}
