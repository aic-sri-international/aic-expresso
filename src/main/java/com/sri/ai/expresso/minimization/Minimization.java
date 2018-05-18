package com.sri.ai.expresso.minimization;

/**
 * An interface gathering the methods for minimizatin of non linear multivariate functions.
 *
 * @author Sarah Perrin
 */
public interface Minimization {
	
	public double findMinimum();
	public double[] findArgmin();

}
