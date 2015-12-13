package com.sri.ai.test.grinder.sgdpll2;

import java.util.Random;

/**
 * Modifies {@link AbstractEqualityConstraintTest} to work as a benchmark,
 * by using a fixed random seed (0) and deactivating testing against brute-force.
 * 
 * @author braz
 *
 */
public abstract class AbstractEqualityConstraintBenchmark extends AbstractEqualityConstraintTest {

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