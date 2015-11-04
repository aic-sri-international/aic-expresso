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
package com.sri.ai.test.grinder.sgdpll2;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import java.util.Random;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.problemtype.Max;
import com.sri.ai.grinder.plaindpll.problemtype.Sum;
import com.sri.ai.grinder.sgdpll2.api.MultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.tester.ConstraintTheoryTester;
import com.sri.ai.grinder.sgdpll2.theory.equality.AbstractConstrainTheoryWithFunctionApplicationAtoms;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;

@Beta
public class EqualityConstraintTest {

	@Test
	public void testSingleVariableConstraints() {
		ConstraintTheoryTester.testSingleVariableConstraints(
				new Random(),
				new EqualityConstraintTheory(true),
				100 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		ConstraintTheoryTester.testMultiVariableConstraints(
				new Random(),
				new EqualityConstraintTheory(true),
				500 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testCompleteMultiVariableConstraints() {
		ConstraintTheoryTester.testCompleteMultiVariableConstraints(
				new Random(),
				new EqualityConstraintTheory(true),
				200 /* number of tests */,
				50 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testModelCountingForSingleVariableConstraints() {
		ConstraintTheoryTester.testModelCountingForSingleVariableConstraints(
				new Random(),
				new EqualityConstraintTheory(true),
				200 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemForSingleVariableConstraints(
				new Random(),
				new Sum(),
				new EqualityConstraintTheory(true),
				10 /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMaxForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemForSingleVariableConstraints(
				new Random(),
				new Max(),
				new EqualityConstraintTheory(true),
				10 /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testCompleteSatisfiabilitySpecialCases() {
		// This test is to make sure that some more tricky cases are indeed tested,
		// even though hopefully the large amount of generated random problems include them.

		String conjunction;
		Expression expected;
		
		conjunction = "X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = null;
		runCompleteSatisfiabilityTest(conjunction, expected);
		
		conjunction = "X = Y and X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = null;
		runCompleteSatisfiabilityTest(conjunction, expected);
		
		conjunction = "X = a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = parse("(W = d) and (Z = c) and (X = a) and (X != Z) and (X != W)");
		runCompleteSatisfiabilityTest(conjunction, expected);
	}

	/**
	 * @param conjunction
	 * @param expected
	 */
	private void runCompleteSatisfiabilityTest(String conjunction, Expression expected) {
		AbstractConstrainTheoryWithFunctionApplicationAtoms constraintTheory = new EqualityConstraintTheory(true);
		MultiVariableConstraint constraint = new CompleteMultiVariableConstraint(constraintTheory);
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		for (Expression literal : And.getConjuncts(parse(conjunction))) {
			constraint = constraint.conjoin(literal, process);
		}
		assertEquals(expected, constraint);
	}
}
