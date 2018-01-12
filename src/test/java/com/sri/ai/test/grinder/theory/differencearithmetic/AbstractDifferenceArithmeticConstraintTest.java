package com.sri.ai.test.grinder.theory.differencearithmetic;

import java.util.Random;

import org.junit.Test;

import com.sri.ai.grinder.group.Conjunction;
import com.sri.ai.grinder.group.Disjunction;
import com.sri.ai.grinder.group.Max;
import com.sri.ai.grinder.group.Sum;
import com.sri.ai.grinder.tester.SGDPLLTTester;
import com.sri.ai.grinder.tester.TheoryTestingSupport;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.test.grinder.theory.base.AbstractTheoryIncludingEqualityTest;

public abstract class AbstractDifferenceArithmeticConstraintTest extends AbstractTheoryIncludingEqualityTest {

	public AbstractDifferenceArithmeticConstraintTest() {
		super();
	}

	@Override
	protected abstract boolean getPropagateAllLiteralsWhenVariableIsBound();

	@Override
	protected TheoryTestingSupport makeTheoryTestingSupport() {
		return TheoryTestingSupport.make(makeRandom(), new DifferenceArithmeticTheory(true, getPropagateAllLiteralsWhenVariableIsBound()));
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
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				scale(500) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		SGDPLLTTester.testMultiVariableConstraints(
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
				scale(100) /* number of tests */,
				50 /* number of literals per test */,
				true /* output count */);
	}

	// Next tests commented out because complete satisfiability and model counting are need to be fixed for decent efficiency.


	@Test
	public void testModelCountingForSingleVariableConstraints() {
		SGDPLLTTester.testModelCountingForSingleVariableConstraints(
				getTestAgainstBruteForce(),
				makeTheoryTestingSupport(),
				scale(100) /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		SGDPLLTTester.testGroupProblemSolvingForSingleVariableConstraints(
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
				getNumberOfIndices(),
				getTestAgainstBruteForce(),
				new Max(),
				makeTheoryTestingSupport(),
	            scale(10) /* number of tests */,
				20 /* number of literals per test */,
				getBodyDepth(), /* body depth */
				true /* output count */);
	}

	@Test
	public void testConjunction() {
		SGDPLLTTester.testGroupProblemSolvingForMultipleIndices(
				getNumberOfIndices(),
				getTestAgainstBruteForce(),
				new Conjunction(),
				makeTheoryTestingSupport(),
	            scale(10) /* number of tests */,
				20 /* number of literals per test */,
				getBodyDepth(), /* body depth */
				true /* output count */);
	}

	@Test
	public void testDisjunction() {
		SGDPLLTTester.testGroupProblemSolvingForMultipleIndices(
				getNumberOfIndices(),
				getTestAgainstBruteForce(),
				new Disjunction(),
				makeTheoryTestingSupport(),
	            scale(10) /* number of tests */,
				20 /* number of literals per test */,
				getBodyDepth(), /* body depth */
				true /* output count */);
	}
}