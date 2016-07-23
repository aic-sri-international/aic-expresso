package com.sri.ai.test.grinder.sgdpll.theory.base;

import static java.lang.Math.ceil;
import static java.lang.Math.round;

import java.util.Random;

import com.sri.ai.grinder.sgdpll.api.Theory;

public abstract class AbstractTheoryTest {

	public AbstractTheoryTest() {
		super();
	}

	/**
	 * Provides a way to regulate which seed to use (or none) for all tests at once.
	 * Default is none.
	 */
	protected Random makeRandom() {
		return new Random();
	}

	/**
	 * Provides a way to regulate which theory to use.
	 * @return
	 */
	abstract protected Theory makeTheory();

	/**
	 * Indicates whether correctness should be checked against brute-force methods when possible.
	 * Default is true.
	 * @return
	 */
	protected boolean getTestAgainstBruteForce() {
		return true;
	}

	/**
	 * Provides a double to scale (up or down) an integer, rounding up.
	 * This is useful for scaling up or down the number of tests inherited from a super class,
	 * which need to go through {@link #scale(int)} for this to have an effect.
	 * Default is 1.
	 * @return
	 */
	protected double getScaleFactor() {
		return 1;
	}
	
	/** Scales i according to {@link #getScaleFactor()}, rounding up. */
	protected long scale(int i) {
		return round(ceil(i*getScaleFactor()));
	}
}