package com.sri.ai.test.grinder.sgdpll.theory.inequality;

import java.util.Random;

/**
 * Modifies {@link AbstractInequalityConstraintTest} to work as a benchmark,
 * by using a fixed random seed (0) and deactivating testing against brute-force.
 * 
 * @author braz
 *
 */
public abstract class AbstractInequalityConstraintBenchmark extends AbstractInequalityConstraintTest {

	public AbstractInequalityConstraintBenchmark() {
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