package com.sri.ai.test.grinder.sgdpll2.theory.equality;

import java.util.Random;

/**
 * Modifies {@link AbstractEqualityConstraintTheoryTest} to work as a benchmark,
 * by using a fixed random seed (0) and deactivating testing against brute-force.
 * 
 * @author braz
 *
 */
public abstract class AbstractEqualityConstraintBenchmark extends AbstractEqualityConstraintTheoryTest {

	public AbstractEqualityConstraintBenchmark() {
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