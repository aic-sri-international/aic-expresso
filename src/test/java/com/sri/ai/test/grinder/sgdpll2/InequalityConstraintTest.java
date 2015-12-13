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

import java.util.Random;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.tester.ConstraintTheoryTester;
import com.sri.ai.grinder.sgdpll2.theory.inequality.InequalityConstraintTheory;

@Beta
public class InequalityConstraintTest extends AbstractConstraintTheoryTest {

	@Override
	protected ConstraintTheory makeConstraintTheory() {
		return new InequalityConstraintTheory(true, false);
	}

	@Test
	public void testSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		ConstraintTheoryTester.testSingleVariableConstraints(
				new Random(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				500 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		ConstraintTheoryTester.testMultiVariableConstraints(
				new Random(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				500 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	// Next tests commented out because complete satisfiability and model counting are need to be fixed for decent efficiency.
	
	@Test
	public void testCompleteMultiVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		ConstraintTheoryTester.testCompleteMultiVariableConstraints(
				new Random(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				100 /* number of tests */,
				50 /* number of literals per test */,
				true /* output count */);
	}

//	@Test
//	public void testModelCountingForSingleVariableConstraints() {
//		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
//
//		ConstraintTheoryTester.testModelCountingForSingleVariableConstraints(
//				new Random(),
//				makeConstraintTheory(),
// 				100 /* number of tests */,
//				30 /* number of literals per test */,
//				true /* output count */);
//	}

//	@Test
//	public void testSumForSingleVariableConstraints() {
//		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
//		
//		ConstraintTheoryTester.testGroupProblemForSingleVariableConstraints(
//				new Random(),
//				new Sum(),
//				makeConstraintTheory(),
//				10 /* number of tests */,
//				20 /* number of literals per test */,
//				3, /* body depth */
//				true /* output count */);
//	}

//	@Test
//	public void testMaxForSingleVariableConstraints() {
//		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
//		
//		ConstraintTheoryTester.testGroupProblemForSingleVariableConstraints(
//				new Random(), // use seed 2 and body depth 1 for problem 22 taking very long
//				new Max(),
//	            makeConstraintTheory(),
//				10 /* number of tests */,
//				20 /* number of literals per test */,
//				3, /* body depth */
//				true /* output count */);
//	}
}