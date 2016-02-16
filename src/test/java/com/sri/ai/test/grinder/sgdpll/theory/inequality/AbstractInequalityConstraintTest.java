package com.sri.ai.test.grinder.sgdpll.theory.inequality;

import java.util.Random;

import org.junit.Test;

import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.problemtype.Max;
import com.sri.ai.grinder.sgdpll.problemtype.Sum;
import com.sri.ai.grinder.sgdpll.tester.SGDPLLTTester;
import com.sri.ai.grinder.sgdpll.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.test.grinder.sgdpll.theory.base.AbstractConstraintTheoryIncludingEqualityTest;

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
	
	protected int getNumberOfIndices() {
		return 3;
	}

	protected int getBodyDepth() {
		return 3;
	}

	// DO NOT CHANGE TEST PARAMETERS! IMPLEMENTATIONS HAVE RUN-TIME HISTORY WRITTEN DOWN
	// AND CHANGING THE TEST WILL MAKE THE TRACKIGN OF PROGRESS IMPOSSIBLE
	
	@Test
	public void testSingleVariableConstraints() {
		SGDPLLTTester.testSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(500) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		SGDPLLTTester.testMultiVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(500) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testCompleteMultiVariableConstraints() {
		SGDPLLTTester.testCompleteMultiVariableConstraints(
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
		SGDPLLTTester.testModelCountingForSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeConstraintTheory(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		SGDPLLTTester.testGroupProblemSolvingForSingleVariableConstraints(
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
		SGDPLLTTester.testGroupProblemSolvingForSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				new Max(),
	            makeConstraintTheory(),
	            scale(10) /* number of tests */,
				20 /* number of literals per test */,
				getBodyDepth(), /* body depth */
				true /* output count */);
	}

	@Test
	public void testSum() {
		SGDPLLTTester.testGroupProblemSolvingForMultipleIndices(
				makeRandom(),
				getNumberOfIndices(),
				getTestAgainstBruteForce(),
				new Sum(),
				makeConstraintTheory(),
				scale(10) /* number of tests */,
				20 /* number of literals per test */,
				getBodyDepth(), /* body depth */
				true /* output count */);
	}

	@Test
	public void testMax() {
		SGDPLLTTester.testGroupProblemSolvingForMultipleIndices(
				makeRandom(),
				getNumberOfIndices(),
				getTestAgainstBruteForce(),
				new Max(),
	            makeConstraintTheory(),
	            scale(10) /* number of tests */,
				20 /* number of literals per test */,
				getBodyDepth(), /* body depth */
				true /* output count */);
	}
}