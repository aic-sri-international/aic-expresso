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
package com.sri.ai.test.grinder.sgdpll2.theory.propositional;

import java.util.Random;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.plaindpll.problemtype.Max;
import com.sri.ai.grinder.plaindpll.problemtype.Sum;
import com.sri.ai.grinder.sgdpll2.tester.ConstraintTheoryTester;
import com.sri.ai.grinder.sgdpll2.theory.propositional.PropositionalConstraintTheory;
import com.sri.ai.test.grinder.sgdpll2.theory.base.AbstractConstraintTheoryTest;

@Beta
public class PropositionalConstraintTest extends AbstractConstraintTheoryTest {

	@Override
	public PropositionalConstraintTheory makeConstraintTheory() {
		return new PropositionalConstraintTheory();
	}

	@Test
	public void testSingleVariableConstraints() {
		ConstraintTheoryTester.testSingleVariableConstraints(
				new Random(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(30) /* number of tests - only literals are P and not P, and 3 tests already create all cases */,
				2 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		ConstraintTheoryTester.testMultiVariableConstraints(
				new Random(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(100) /* number of tests - many more possibilities when we have multiple variables */,
				10 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testModelCountingForSingleVariableConstraints() {
		ConstraintTheoryTester.testModelCountingForSingleVariableConstraints(
				new Random(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemSolvingForSingleVariableConstraints(
				new Random(),
				getTestAgainstBruteForce(),
				new Sum(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMaxForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemSolvingForSingleVariableConstraints(
				new Random(),
				getTestAgainstBruteForce(),
				new Max(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testCompleteMultiVariableConstraints() {
		ConstraintTheoryTester.testCompleteMultiVariableConstraints(
				new Random(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSum() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemSolvingForMultipleIndices(
				new Random(),
				3, /* number of indices */
				getTestAgainstBruteForce(),
				new Sum(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMax() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemSolvingForMultipleIndices(
				new Random(),
				3, /* number of indices */
				getTestAgainstBruteForce(),
				new Max(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}
}
