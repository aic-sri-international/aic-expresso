package com.sri.ai.expresso.optimization;

/**
 * An interface gathering the methods for optimization of non linear multivariate functions.
 *
 * @author Sarah Perrin
 */
public interface Optimization {
	
	public double findOptimum();
	public double[] findArgopt();

}
