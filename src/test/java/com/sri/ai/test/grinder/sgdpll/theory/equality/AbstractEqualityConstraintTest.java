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
package com.sri.ai.test.grinder.sgdpll.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.universallyQuantifyFreeVariables;
import static com.sri.ai.grinder.library.FunctorConstants.EQUIVALENCE;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.map;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.MultiVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll.interpreter.SymbolicCommonInterpreter;
import com.sri.ai.grinder.sgdpll.problemtype.Max;
import com.sri.ai.grinder.sgdpll.problemtype.Sum;
import com.sri.ai.grinder.sgdpll.tester.ConstraintTheoryTester;
import com.sri.ai.grinder.sgdpll.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.equality.SingleVariableEqualityConstraint;
import com.sri.ai.test.grinder.sgdpll.theory.base.AbstractConstraintTheoryIncludingEqualityTest;

@Beta
public abstract class AbstractEqualityConstraintTest extends AbstractConstraintTheoryIncludingEqualityTest {

	@Override
	protected ConstraintTheory makeConstraintTheory() {
		return new EqualityConstraintTheory(true, getPropagateAllLiteralsWhenVariableIsBound());
	}

	

	// DO NOT CHANGE TEST PARAMETERS! IMPLEMENTATIONS HAVE RUN-TIME HISTORY WRITTEN DOWN
	// AND CHANGING THE TEST WILL MAKE THE TRACKIGN OF PROGRESS IMPOSSIBLE
	
	@Test
	public void testSingleVariableConstraints() {
		ConstraintTheoryTester.testSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		ConstraintTheoryTester.testMultiVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(500) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testCompleteMultiVariableConstraints() {
		ConstraintTheoryTester.testCompleteMultiVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(200) /* number of tests */,
				50 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testModelCountingForSingleVariableConstraints() {
		ConstraintTheoryTester.testModelCountingForSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(200) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemSolvingForSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				new Sum(),
				makeConstraintTheory(),
				scale(50) /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMaxForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemSolvingForSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				new Max(),
				makeConstraintTheory(),
				scale(50) /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testSum() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemSolvingForMultipleIndices(
				makeRandom(),
				3, /* number of indices */
				getTestAgainstBruteForce(),
				new Sum(),
				makeConstraintTheory(),
				scale(50) /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMax() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemSolvingForMultipleIndices(
				makeRandom(),
				3, /* number of indices */
				getTestAgainstBruteForce(),
				new Max(),
				makeConstraintTheory(),
				scale(50) /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testSatisfiabilitySpecialCases() {
		String conjunction;
		conjunction = "X != a and X != b and X != c and X != Y and X != Z"; // looks unsatisfiable for type size 5, but it is not
		ConstraintTheory constraintTheory = makeConstraintTheory();
		Constraint constraint = new SingleVariableEqualityConstraint(parse("X"), false, constraintTheory);
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		constraint = constraint.conjoinWithConjunctiveClause(parse(conjunction), process);
		Assert.assertNotEquals(null, constraint); // satisfiable if either Y or Z is equal to a, b, c, or each other.
	}

	@Test
	public void testCompleteSatisfiabilitySpecialCases() {
		// This test is to make sure that some more tricky cases are indeed tested,
		// even though hopefully the large amount of generated random problems include them or their variants.

		String conjunction;
		Expression expected;

		ConstraintTheory constraintTheory = makeConstraintTheory();
		
		if (constraintTheory.singleVariableConstraintIsCompleteWithRespectToItsVariable()) {
			conjunction = "X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
			expected = null;
			runCompleteSatisfiabilityTest(conjunction, expected, constraintTheory);

			conjunction = "X = Y and X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
			expected = null;
			runCompleteSatisfiabilityTest(conjunction, expected, constraintTheory);
		}

		ConstraintTheory constraintTheory2 = makeConstraintTheory();
		Categorical type = new Categorical("Type", 1, arrayList(parse("a")));
		constraintTheory2.setVariableNamesAndTypesForTesting(map("X", type, "Y", type));
		conjunction = "X != Y";
		expected = null;
		runCompleteSatisfiabilityTest(conjunction, expected, constraintTheory2);

		ConstraintTheory constraintTheory3 = makeConstraintTheory();
		type = new Categorical("Type", 2, arrayList(parse("a"), parse("b")));
		constraintTheory3.setVariableNamesAndTypesForTesting(map("X", type, "Y", type));
		conjunction = "X != Y and X != a";
		expected = parse("Y != b and X != a and X != Y");
		runCompleteSatisfiabilityTest(conjunction, expected, constraintTheory3);
		
		conjunction = "X != a and X != b and X != c and X != sometype5 and X != Y";
		expected = parse("Y != d and X != a and X != b and X != c and X != sometype5 and X != Y and X != Y");
		runCompleteSatisfiabilityTest(conjunction, expected, constraintTheory);
		
		conjunction = "X = a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = parse("(W = d) and (Z = c) and (X = a) and (X != Z) and (X != W)");
		runCompleteSatisfiabilityTest(conjunction, expected, constraintTheory);
	}

	/**
	 * @param conjunction
	 * @param expected
	 */
	private void runCompleteSatisfiabilityTest(String conjunction, Expression expected, ConstraintTheory constraintTheory) {
		MultiVariableConstraint constraint = new CompleteMultiVariableConstraint(constraintTheory);
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		for (Expression literal : And.getConjuncts(parse(conjunction))) {
			constraint = constraint.conjoin(literal, process);
			if (constraint == null) {
				break;
			}
		}
		if (expected == null && constraint != null) {
			throw new AssertionError("Expected null but was <" + constraint + ">");
		}
		else if (expected != null && constraint == null) {
			throw new AssertionError("Expected <" + expected + "> but was null");
		}
		else if (expected != null && constraint != null && !expected.equals(constraint)) {
			SymbolicCommonInterpreter interpreter = new SymbolicCommonInterpreter(constraintTheory);
			Expression equivalenceDefinition = apply(EQUIVALENCE, expected, constraint);
			Expression universallyQuantified = universallyQuantifyFreeVariables(equivalenceDefinition, process);
			Expression equivalent = interpreter.apply(universallyQuantified, process);
			if (equivalent.equals(FALSE)) {
				throw new Error("Expected <" + expected + "> but got <" + constraint + ">, which is not equivalent either");
			}
		}
	}
}
