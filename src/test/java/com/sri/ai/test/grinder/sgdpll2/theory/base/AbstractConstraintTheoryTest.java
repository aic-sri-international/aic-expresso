package com.sri.ai.test.grinder.sgdpll2.theory.base;

import java.util.Random;

import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;

public abstract class AbstractConstraintTheoryTest {

	public AbstractConstraintTheoryTest() {
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
	 * Provides a way to regulate which constraint theory to use.
	 * @return
	 */
	abstract protected ConstraintTheory makeConstraintTheory();

	/**
	 * Indicates whether correctness should be checked against brute-force methods when possible.
	 * Default is true.
	 * @return
	 */
	protected boolean getTestAgainstBruteForce() {
		return true;
	}

}