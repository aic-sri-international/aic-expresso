package com.sri.ai.grinder.library.equality.cardinality.plaindpll.application;

import com.sri.ai.util.math.Rational;

public class SmallTest {

	public static void main(String[] args) {
		int numberOfVariables = 21;
		int domainSize = 2;
		Rational total = new Rational(0);
		long start = System.currentTimeMillis();
		total = compute(0, "", total, numberOfVariables, domainSize);
		long end = System.currentTimeMillis();
		System.out.println("Time: " + (end - start) + " ms");	
	}

	public static Rational compute(int variable, String prefix, Rational total, int numberOfVariables, int domainSize) {
		for (int value = 0; value != domainSize; value++) {
			String newPrefix = prefix + " v" + variable + " = " + value;
			if (variable == numberOfVariables - 1) {
				//System.out.println(newPrefix);	
				total = total.add(prefix.hashCode());
			}
			else {
				total.add(compute(variable + 1, newPrefix, total, numberOfVariables, domainSize));
			}
		}
		return total;
	}
}
