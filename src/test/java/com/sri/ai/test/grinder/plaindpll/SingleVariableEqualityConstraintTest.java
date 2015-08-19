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
package com.sri.ai.test.grinder.plaindpll;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.plaindpll.tester.ConstraintTheoryTester;
import com.sri.ai.grinder.plaindpll.theory.EqualityConstraintTheory;

@Beta
public class SingleVariableEqualityConstraintTest {

	@Test
	public void test() {
		ConstraintTheoryTester.test(new EqualityConstraintTheory(null), 1000, 30);
	}
	
//	@Test
//	public void test() {
//		
//		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
//		
//		Expression expectedConstraint;
//		SingleVariableConstraint actual;
//		RewritingProcess process = DPLLUtil.extendProcessWith(map("X", "MyType", "p", "'->'(MyType,MyType)"), map("MyType", "3"), new DefaultRewritingProcess(null));
//		
//		actual = make("X", "X = 10", process);
//		expectedConstraint = parse("X = 10");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X != 10", process);
//		expectedConstraint = parse("X != 10");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X != 10 and X = 5", process);
//		expectedConstraint = parse("X = 5");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X = 10 and X = Y", process);
//		expectedConstraint = parse("X = 10 and 10 = Y");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X = 10 and X = 8", process);
//		expectedConstraint = null;
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X != 10 and X = 10", process);
//		expectedConstraint = null;
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X != 10 and X = 8", process);
//		expectedConstraint = parse("X = 8");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X != 10 and X = Y", process);
//		expectedConstraint = parse("X = Y and Y != 10");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X != 10 and X != 11 and X = 8", process);
//		expectedConstraint = parse("X = 8");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X != 10 and X != 11 and X = Y", process);
//		expectedConstraint = parse("X = Y and Y != 10 and Y != 11");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X != 10 and X != 11 and X != 12 and X = Y", process);
//		expectedConstraint = null;
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X = Y and X != 10 and X != 11 and X != 12", process);
//		expectedConstraint = parse("X = Y and Y != 10 and Y != 11 and Y != 12");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("p(X)", "p(X) = Y and p(X) != 10 and p(X) != 11 and p(X) != 12", process);
//		expectedConstraint = parse("p(X) = Y and Y != 10 and Y != 11 and Y != 12");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X = Y and a != a", process);
//		expectedConstraint = parse("X = Y and a != a");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X = Y and true", process);
//		expectedConstraint = parse("X = Y");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X = Y and false", process);
//		expectedConstraint = null;
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "a != a and X = Y", process);
//		expectedConstraint = parse("X = Y");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "true and X = Y", process);
//		expectedConstraint = parse("X = Y");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "false and X = Y", process);
//		expectedConstraint = null;
//		assertEquals(expectedConstraint, actual);
//		
////		// the following test depends on the solver replacing a variable bound value (Y) by a constant (a)
////		// when X = a is received. This allows it to detect the inconsistency when X = b is received.
////		// If such step were not taken, the bound value would remain Y and the external literals
////		// would be Y = a and Y = b. It would still be correct, but the inconsistency would
////		// be detected only when Y is dealt with.
////		actual = make("X", "X = Y and X = a and X = b", process);
////		expectedConstraint = null;
////		assertEquals(expectedConstraint, actual);
////
////		// tests whether reaching the number of unique values in domain (here, 3) triggers inconsistency,
////		// even though X is bound to Z, which for our solver means Z inherits the disequalities,
////		// making them "invisible" at the X level.
////		// Solver has been made complete by adding a separate Collection keeping track of terms disequals to X ever applied,
////		// even if they are "inherited" by another variable.
////		actual = make("X", "(X = Z) and X != a and X != b and X != c", process);
////		expectedConstraint = null;
////		assertEquals(expectedConstraint, actual);
//	}
//
//	@Test
//	public void testSimplifyGiven() {
//		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
//		
//		Expression expectedConstraint;
//		SingleVariableConstraint actual;
//		RewritingProcess process = DPLLUtil.extendProcessWith(map("X", "MyType"), map("MyType", "3"), new DefaultRewritingProcess(null));
//		
//		actual = make("X", "X = 10 and Y != 3", process);
//		actual = actual.simplifyGiven(parse("Y != 3"), process);
//		expectedConstraint = parse("X = 10");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X = 10", process);
//		actual = actual.simplifyGiven(parse("Y != 3"), process);
//		expectedConstraint = parse("X = 10");
//		assertEquals(expectedConstraint, actual);
//		
//		actual = make("X", "X = 10 and Y != 3", process);
//		actual = actual.simplifyGiven(parse("Z != 3"), process);
//		expectedConstraint = parse("X = 10 and Y != 3");
//		assertEquals(expectedConstraint, actual);
//	}
//	
//	/**
//	 * @param variableString
//	 * @param process the rewriting process
//	 * @return
//	 */
//	protected SingleVariableConstraint make(String variableString, String expressionString, RewritingProcess process) {
//		Expression variable = parse(variableString);
//		Expression expression = parse(expressionString);
//		Collection<Expression> conjuncts = And.getConjuncts(expression);
//
//		SingleVariableConstraint result = new SingleVariableEqualityConstraint(variable);
//		for (Expression conjunct : conjuncts) {
//			result = result.conjoin(conjunct, process);
//			if (result == null) {
//				break;
//			}
//		}
//		
//		return result;
//	}
}
