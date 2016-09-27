package com.sri.ai.test.grinder.sgdpllt.theory.differencearithmetic;

import java.util.Random;

import org.junit.Test;

import com.sri.ai.grinder.sgdpllt.group.Max;
import com.sri.ai.grinder.sgdpllt.group.Sum;
import com.sri.ai.grinder.sgdpllt.tester.SGDPLLTTester;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.test.grinder.sgdpllt.theory.base.AbstractTheoryIncludingEqualityTest;

public abstract class AbstractDifferenceArithmeticConstraintTest extends AbstractTheoryIncludingEqualityTest {

	public AbstractDifferenceArithmeticConstraintTest() {
		super();
	}

	@Override
	protected abstract boolean getPropagateAllLiteralsWhenVariableIsBound();

	@Override
	protected TheoryTestingSupport makeTheoryTestingSupport() {
		return new DifferenceArithmeticTheory(true, getPropagateAllLiteralsWhenVariableIsBound());
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
	// AND CHANGING THE TEST WILL ERASE THE TRACKING OF PROGRESS SO FAR
	
	@Test
	public void testSingleVariableConstraints() {
		SGDPLLTTester.testSingleVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				scale(500) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		SGDPLLTTester.testMultiVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				scale(500) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testCompleteMultiVariableConstraints() {
		SGDPLLTTester.testCompleteMultiVariableConstraints(
				makeRandom(),
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
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
				makeTheoryTestingSupport(),
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
				makeTheoryTestingSupport(),
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
				makeTheoryTestingSupport(),
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
				makeTheoryTestingSupport(),
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
				makeTheoryTestingSupport(),
	            scale(10) /* number of tests */,
				20 /* number of literals per test */,
				getBodyDepth(), /* body depth */
				true /* output count */);
	}
}