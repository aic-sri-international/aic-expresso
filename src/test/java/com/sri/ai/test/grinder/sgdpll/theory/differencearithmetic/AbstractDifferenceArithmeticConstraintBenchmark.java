package com.sri.ai.test.grinder.sgdpll.theory.differencearithmetic;

import java.util.Random;

/**
 * Modifies {@link AbstractDifferenceArithmeticConstraintTest} to work as a benchmark,
 * by using a fixed random seed (0) and deactivating testing against brute-force.
 * 
 * @author braz
 *
 */
public abstract class AbstractDifferenceArithmeticConstraintBenchmark extends AbstractDifferenceArithmeticConstraintTest {

	public AbstractDifferenceArithmeticConstraintBenchmark() {
		super();
	}

	@Override
	public Random makeRandom() {
		return new Random(0);
	}

	@Override
	protected boolean getTestAgainstBruteForce() {
		return false;
	}
}