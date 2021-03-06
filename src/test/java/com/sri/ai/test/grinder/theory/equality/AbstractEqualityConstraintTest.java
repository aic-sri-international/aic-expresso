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
package com.sri.ai.test.grinder.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.universallyQuantifyFreeVariables;
import static com.sri.ai.grinder.library.FunctorConstants.EQUIVALENCE;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.map;

import java.util.HashMap;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.group.Max;
import com.sri.ai.grinder.group.Sum;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.rewriter.api.Simplifier;
import com.sri.ai.grinder.tester.SGDPLLTTester;
import com.sri.ai.grinder.tester.TheoryTestingSupport;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.equality.SingleVariableEqualityConstraint;
import com.sri.ai.test.grinder.theory.base.AbstractTheoryIncludingEqualityTest;

@Beta
public abstract class AbstractEqualityConstraintTest extends AbstractTheoryIncludingEqualityTest {

	@Override
	protected TheoryTestingSupport makeTheoryTestingSupport() {
		return TheoryTestingSupport.make(makeRandom(), new EqualityTheory(true, getPropagateAllLiteralsWhenVariableIsBound()));
	}

	// DO NOT CHANGE TEST PARAMETERS! IMPLEMENTATIONS HAVE RUN-TIME HISTORY WRITTEN DOWN
	// AND CHANGING THE TEST WILL ERASE THE TRACKING OF PROGRESS SO FAR
	
	@Test
	public void testSingleVariableConstraints() {
		SGDPLLTTester.testSingleVariableConstraints(
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		SGDPLLTTester.testIncompleteMultiVariableConstraints(
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				scale(500) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testCompleteMultiVariableConstraints() {
		SGDPLLTTester.testCompleteMultiVariableConstraints(
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				scale(200) /* number of tests */,
				50 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testModelCountingForSingleVariableConstraints() {
		SGDPLLTTester.testModelCountingForSingleVariableConstraints(
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				scale(200) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		SGDPLLTTester.testGroupProblemSolvingForSingleVariableConstraints(
				getTestAgainstBruteForce(),
				new Sum(),
				makeTheoryTestingSupport(),
				scale(50) /* number of tests */,
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
				scale(50) /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testSum() {
		SGDPLLTTester.testGroupProblemSolvingForMultipleIndices(
				3, /* number of indices */
				getTestAgainstBruteForce(),
				new Sum(),
				makeTheoryTestingSupport(),
				scale(50) /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMax() {
		SGDPLLTTester.testGroupProblemSolvingForMultipleIndices(
				3, /* number of indices */
				getTestAgainstBruteForce(),
				new Max(),
				makeTheoryTestingSupport(),
				scale(50) /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testSatisfiabilitySpecialCases() {
		String conjunction;
		conjunction = "X != a and X != b and X != c and X != Y and X != Z"; // looks unsatisfiable for type size 5, but it is not
		TheoryTestingSupport theoryTestingSupport = makeTheoryTestingSupport();
		Constraint constraint = new SingleVariableEqualityConstraint(parse("X"), false, theoryTestingSupport.getTheory());
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
		constraint = constraint.conjoinWithConjunctiveClause(parse(conjunction), context);
		Assert.assertNotEquals(null, constraint); // satisfiable if either Y or Z is equal to a, b, c, or each other.
	}

	@Test
	public void testCompleteSatisfiabilitySpecialCases() {
		// This test is to make sure that some more tricky cases are indeed tested,
		// even though hopefully the large amount of generated random problems include them or their variants.

		String conjunction;
		Expression expected;

		TheoryTestingSupport theoryTestingSupport = makeTheoryTestingSupport();
		Map<String, Type> variableNamesAndTypes = new HashMap<>(theoryTestingSupport.getVariableNamesAndTypesForTesting());
		variableNamesAndTypes.put("W", variableNamesAndTypes.get("X"));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(variableNamesAndTypes);
		
		if (theoryTestingSupport.getTheory().singleVariableConstraintIsCompleteWithRespectToItsVariable()) {
			conjunction = "X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
			expected = null;
			runCompleteSatisfiabilityTest(conjunction, expected, theoryTestingSupport);

			conjunction = "X = Y and X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
			expected = null;
			runCompleteSatisfiabilityTest(conjunction, expected, theoryTestingSupport);
		}

		TheoryTestingSupport theoryTestingSupport2 = makeTheoryTestingSupport();
		Categorical type = new Categorical("Type", 1, arrayList(parse("a")));
		theoryTestingSupport2.setVariableNamesAndTypesForTesting(map("X", type, "Y", type, "Z", type, "W", type));
		conjunction = "X != Y";
		expected = null;
		runCompleteSatisfiabilityTest(conjunction, expected, theoryTestingSupport2);

		TheoryTestingSupport theoryTestingSupport3 = makeTheoryTestingSupport();
		type = new Categorical("Type", 2, arrayList(parse("a"), parse("b")));
		theoryTestingSupport3.setVariableNamesAndTypesForTesting(map("X", type, "Y", type, "Z", type, "W", type));
		conjunction = "X != Y and X != a";
		expected = parse("Y != b and X != a and X != Y");
		runCompleteSatisfiabilityTest(conjunction, expected, theoryTestingSupport3);
		
		conjunction = "X != a and X != b and X != c and X != sometype5 and X != Y";
		expected = parse("Y != d and X != a and X != b and X != c and X != sometype5 and X != Y and X != Y");
		runCompleteSatisfiabilityTest(conjunction, expected, theoryTestingSupport);

		conjunction = "X = a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = parse("(W = d) and (Z = c) and (X = a) and (X != Z) and (X != W)");
		runCompleteSatisfiabilityTest(conjunction, expected, theoryTestingSupport);
	}

	/**
	 * @param conjunction
	 * @param expected
	 */
	private void runCompleteSatisfiabilityTest(String conjunction, Expression expected, TheoryTestingSupport theoryTestingSupport) {
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
		for (Expression literal : And.getConjuncts(parse(conjunction))) {
			context = context.conjoin(literal, context);
			if (context.isContradiction()) {
				break;
			}
		}
		if (expected == null && !context.isContradiction()) {
			throw new AssertionError("Expected null but was <" + context + ">");
		}
		else if (expected != null && context.isContradiction()) {
			throw new AssertionError("Expected <" + expected + "> but was null");
		}
		else if (expected != null && !context.isContradiction() && !expected.equals(context)) {
			Simplifier interpreter = (e, c) -> theoryTestingSupport.getTheory().evaluate(e, c);
//			Simplifier interpreter = new Evaluator(theoryTestingSupport.getTheory());
			Expression equivalenceDefinition = apply(EQUIVALENCE, expected, context);
			Expression universallyQuantified = universallyQuantifyFreeVariables(equivalenceDefinition, context);
			Expression equivalent = interpreter.apply(universallyQuantified, context);
			if (equivalent.equals(FALSE)) {
				throw new Error("Expected <" + expected + "> but got <" + context + ">, which is not equivalent either");
			}
		}
	}
}
