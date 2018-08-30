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
package com.sri.ai.test.grinder.theory.compound;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static org.junit.Assert.assertEquals;

import java.util.Map;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.constraint.AbstractTheoryTestingSupport;
import com.sri.ai.grinder.group.Max;
import com.sri.ai.grinder.group.Sum;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.tester.SGDPLLTTester;
import com.sri.ai.grinder.tester.TheoryTestingSupport;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.test.grinder.theory.base.AbstractTheoryTest;
import com.sri.ai.util.explanation.logging.api.ExplanationConfiguration;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

/**
 * Test of compound theory of equalities, difference arithmetic and propositional theories.
 * Still quite slow (as of January 2016), hence the small number of test problems.
 * @author braz
 *
 */
@Beta
public class CompoundTheoryWithDifferenceArithmeticTest extends AbstractTheoryTest {

	@Override
	protected TheoryTestingSupport makeTheoryTestingSupport() {
		TheoryTestingSupport result = TheoryTestingSupport.make(makeRandom(), new CompoundTheory(
				new EqualityTheory(false, true),
				new DifferenceArithmeticTheory(false, true),
				new PropositionalTheory()));
		
		// using different testing variables and types to test distribution of testing information
		// to sub constraint theories.
		
		Categorical booleanType = BOOLEAN_TYPE;
		Categorical dogsType    = new Categorical("Dogs", 4, arrayList(parse("fido"), parse("rex")));
		IntegerInterval oneTwoThree = new IntegerInterval(1, 3);
		
		Map<String, Type> variablesAndTypes =
				map(
						"F", booleanType,
						"G", booleanType,
						"R", dogsType,
						"S", dogsType,
						"T", oneTwoThree,
						"U", oneTwoThree
						);
		
		result.setVariableNamesAndTypesForTesting(variablesAndTypes);
		
		return result;
	}
	
	/**
	 * Indicates whether correctness should be checked against brute-force methods when possible.
	 * @return
	 */
	@Override
	protected boolean getTestAgainstBruteForce() {
		return true;
	}

	@Test
	public void basicTests() {
		
		ExplanationConfiguration.WHETHER_EXPLANATION_LOGGERS_ARE_ACTIVE_BY_DEFAULT = false;
		
		ThreadExplanationLogger.explanationBlockToFile("explanation.txt", code( () -> {
		
		CompoundTheory theory = 
				new CompoundTheory(
						new EqualityTheory(false, true),
						new DifferenceArithmeticTheory(false, true),
						new PropositionalTheory());
		
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), theory);
		
		Expression condition = parse("X = Y and Y = X and P and not Q and P and X = a and X != b");
		
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
		Constraint contextAndCondition = context.conjoin(condition, context);
		Expression expected = parse("(Y = a) and not Q and P and (X = Y)");
		assertEquals(expected, contextAndCondition);
		
		}));
	}
	
	// @Test double-nested indices not properly dealt with in the presence of context variables with the same names
	public void nestedIndicesTest() {
		
		ExplanationConfiguration.WHETHER_EXPLANATION_LOGGERS_ARE_ACTIVE_BY_DEFAULT = false;
		
		ThreadExplanationLogger.explanationBlockToFile("explanation.txt", code( () -> {
		
		CompoundTheory theory = 
				new CompoundTheory(
						new EqualityTheory(false, true),
						new DifferenceArithmeticTheory(false, true),
						new PropositionalTheory());
		
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), theory);
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
		Expression expression = parse("sum({{(on I in 1..2, J in 2..3) sum({{ (on I in 1..10, J in 1..2) I + J : I != J }}) }})");
		Expression expected = parse("536");
		Expression actual = context.evaluate(expression);
		println(actual);
		assertEquals(expected, actual);

		}));
	}

	@Test
	public void testSingleVariableConstraints() {
		SGDPLLTTester.testSingleVariableConstraints(
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				100 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		SGDPLLTTester.testIncompleteMultiVariableConstraints(
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				1 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testCompleteMultiVariableConstraints() {
		SGDPLLTTester.testCompleteMultiVariableConstraints(
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				20 /* number of tests */,
				50 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testModelCountingForSingleVariableConstraints() {
		SGDPLLTTester.testModelCountingForSingleVariableConstraints(
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				200 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		SGDPLLTTester.testGroupProblemSolvingForSingleVariableConstraints(
				getTestAgainstBruteForce(),
				new Sum(),
				makeTheoryTestingSupport(),
				10 /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMaxForSingleVariableConstraints() {
		SGDPLLTTester.testGroupProblemSolvingForSingleVariableConstraints(
				getTestAgainstBruteForce(),
				new Max(),
				makeTheoryTestingSupport(),
				10 /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testCompleteSatisfiabilitySpecialCases() {
		// This test is to make sure that some more tricky cases are indeed tested,
		// even though hopefully the large amount of generated random problems include them.
		
		// These are copied from the equality theory test,
		// so it is really just to check whether things hold up
		// if equality theory is embedded in a compound theory.

		String conjunction;
		Expression expected;
		Categorical someType = AbstractTheoryTestingSupport.getDefaultTestingType();

		Map<String, Type> variableNamesAndTypesForTesting = // need W besides the other defaults -- somehow not doing this in equality theory alone does not cause a problem, probably because the type for W is never needed when we have only equality theory
				map("X", someType, "Y", someType, "Z", someType, "W", someType);

		
		conjunction = "X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = FALSE;
		runCompleteSatisfiabilityTest(conjunction, expected, variableNamesAndTypesForTesting);
		
		conjunction = "X = Y and X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = FALSE;
		runCompleteSatisfiabilityTest(conjunction, expected, variableNamesAndTypesForTesting);
		
		conjunction = "X = a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = parse("(W = d) and (Z = c) and (X = a)");
		runCompleteSatisfiabilityTest(conjunction, expected, variableNamesAndTypesForTesting);
	}

	/**
	 * @param conjunction
	 * @param expected
	 */
	private void runCompleteSatisfiabilityTest(String conjunction, Expression expected, Map<String, Type> variableNamesAndTypesForTesting) {
		TheoryTestingSupport equalityTheoryTestingSupport = TheoryTestingSupport.make(makeRandom(), new EqualityTheory(true, true));
		equalityTheoryTestingSupport.setVariableNamesAndTypesForTesting(variableNamesAndTypesForTesting);
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(makeRandom(), equalityTheoryTestingSupport, TheoryTestingSupport.make(makeRandom(), new PropositionalTheory()));
		Constraint context = theoryTestingSupport.makeContextWithTestingInformation();
		for (Expression literal : And.getConjuncts(parse(conjunction))) {
			context = context.conjoin(literal, theoryTestingSupport.makeContextWithTestingInformation());
		}
		assertEquals(expected, context);
	}
}
