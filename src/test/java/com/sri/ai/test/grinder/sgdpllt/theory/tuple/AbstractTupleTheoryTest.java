package com.sri.ai.test.grinder.sgdpllt.theory.tuple;

import java.util.Random;

import org.junit.Test;

import com.sri.ai.grinder.sgdpllt.group.Max;
import com.sri.ai.grinder.sgdpllt.group.Sum;
import com.sri.ai.grinder.sgdpllt.tester.SGDPLLTTester;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.test.grinder.sgdpllt.theory.base.AbstractTheoryTest;

public abstract class AbstractTupleTheoryTest extends AbstractTheoryTest {

	public AbstractTupleTheoryTest() {
		super();
	}

	public abstract Random makeRandom();

	protected abstract boolean getTestAgainstBruteForce();

	@Override
	protected TheoryTestingSupport makeTheoryTestingSupport() {
		TheoryTestingSupport result = 
				TheoryTestingSupport.make(
						makeRandom(), 
						new CompoundTheory(
								new TupleTheory(),
								new EqualityTheory(false, true)));
		return result;
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		SGDPLLTTester.testGroupProblemSolvingForSingleVariableConstraintsForTheoriesWithoutConstraintLiterals(
				getTestAgainstBruteForce(),
				new Sum(),
				makeTheoryTestingSupport(),
				10 /* number of tests */,
				1, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMaxForSingleVariableConstraints() {
		SGDPLLTTester.testGroupProblemSolvingForSingleVariableConstraintsForTheoriesWithoutConstraintLiterals(
				getTestAgainstBruteForce(),
				new Max(),
				makeTheoryTestingSupport(),
				5  /* number of tests */,
				2, /* body depth */
				true /* output count */);
	}

	@Test
	public void testSumForMultiVariableConstraints() {
		SGDPLLTTester.testGroupProblemSolvingForMultipleIndicesForTheoriesWithoutConstraintLiterals(
				2,
				getTestAgainstBruteForce(),
				new Sum(),
				makeTheoryTestingSupport(),
				3 /* number of tests */,
				2, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMaxForMultiVariableConstraints() {
		SGDPLLTTester.testGroupProblemSolvingForMultipleIndicesForTheoriesWithoutConstraintLiterals(
				3,
				getTestAgainstBruteForce(),
				new Max(),
				makeTheoryTestingSupport(),
				5 /* number of tests */,
				2, /* body depth */
				true /* output count */);
	}
}