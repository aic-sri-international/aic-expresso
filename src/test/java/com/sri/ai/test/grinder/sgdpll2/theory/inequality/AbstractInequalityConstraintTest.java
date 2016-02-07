package com.sri.ai.test.grinder.sgdpll2.theory.inequality;

import java.util.Random;

import org.junit.Test;

import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.plaindpll.problemtype.Max;
import com.sri.ai.grinder.plaindpll.problemtype.Sum;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.tester.ConstraintTheoryTester;
import com.sri.ai.grinder.sgdpll2.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.test.grinder.sgdpll2.theory.base.AbstractConstraintTheoryIncludingEqualityTest;

public abstract class AbstractInequalityConstraintTest extends AbstractConstraintTheoryIncludingEqualityTest {

	public AbstractInequalityConstraintTest() {
		super();
	}

	@Override
	protected abstract boolean getPropagateAllLiteralsWhenVariableIsBound();

	@Override
	protected ConstraintTheory makeConstraintTheory() {
		return new InequalityConstraintTheory(true, getPropagateAllLiteralsWhenVariableIsBound());
	}

	@Override
	public Random makeRandom() {
		return new Random();
	}
	
	protected int getBodyDepth() {
		return 3;
	}

	// DO NOT CHANGE TEST PARAMETERS! IMPLEMENTATIONS HAVE RUN-TIME HISTORY WRITTEN DOWN
	// AND CHANGING THE TEST WILL MAKE THE TRACKIGN OF PROGRESS IMPOSSIBLE
	
	@Test
	public void testSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
	
		ConstraintTheoryTester.testSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(500) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
	
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
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
	
		ConstraintTheoryTester.testCompleteMultiVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				50 /* number of literals per test */,
				true /* output count */);
	}

	// Next tests commented out because complete satisfiability and model counting are need to be fixed for decent efficiency.


	@Test
	public void testModelCountingForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		ConstraintTheoryTester.testModelCountingForSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemForSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				new Sum(),
				makeConstraintTheory(),
				scale(10) /* number of tests */,
				20 /* number of literals per test */,
				getBodyDepth(), /* body depth */
				true /* output count */);
	}

	@Test
	public void testMaxForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemForSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				new Max(),
	            makeConstraintTheory(),
	            scale(10) /* number of tests */,
				20 /* number of literals per test */,
				getBodyDepth(), /* body depth */
				true /* output count */);
	}
}