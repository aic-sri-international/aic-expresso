package com.sri.ai.test.expresso;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.minimization.MinimizationWithNelderMead;

/**
 * Class to test the {@link MinimizationWithNelderMead}.
 * @author Sarah Perrin
 *
 */

public class MinimizationWithNelderMeadTest {
	
	@Test
	public void testMinOneVariable() {
		Assert.assertTrue(minAreEquals(0, "X^2", new double[] {-1}));
		Assert.assertTrue(minAreEquals(0, "X^2", new double[] {1}));
		Assert.assertTrue(minAreEquals(0, "X^2", new double[] {10}));
		Assert.assertTrue(minAreEquals(0, "X^2", new double[] {10000}));
		Assert.assertTrue(minAreEquals(1, "X^2+1", new double[] {10000}));
		Assert.assertTrue(minAreEquals(1.0, "(1+X^2)^(1/2)", new double[] {-1}));
		Assert.assertTrue(minAreEquals(1.0, "(1+X^2)^(1/2)", new double[] {-100000}));
		Assert.assertTrue(minAreEquals(-2.2129819407, "3*X^4 + X^3 - 6*X^2 + 2", new double[] {10}));
		Assert.assertTrue(minAreEquals(-2.2129819407, "3*X^4 + X^3 - 6*X^2 + 2", new double[] {10}));
	}
	
	@Test
	public void testArgminOneVariable() {
		Assert.assertTrue(argminAreEquals(new double[] {0}, "X^2", new double[] {-1}));
		Assert.assertTrue(argminAreEquals(new double[] {0}, "X^2", new double[] {1}));
		Assert.assertTrue(argminAreEquals(new double[] {0}, "X^2", new double[] {10}));
		Assert.assertTrue(argminAreEquals(new double[] {0}, "X^2", new double[] {10000}));
		Assert.assertTrue(argminAreEquals(new double[] {0}, "X^2+1", new double[] {10000}));
		Assert.assertTrue(argminAreEquals(new double[] {0}, "(1+X^2)^(1/2)", new double[] {-1}));
		Assert.assertTrue(argminAreEquals(new double[] {0}, "(1+X^2)^(1/2)", new double[] {-100000}));
		Assert.assertTrue(argminAreEquals(new double[] {-1.132782218537319}, "3*X^4 + X^3 - 6*X^2 + 2 ", new double[] {-1}));
		Assert.assertTrue(argminAreEquals(new double[] {-1.132782218537319}, "3*X^4 + X^3 - 6*X^2 + 2 ", new double[] {-100}));
	}
	
	@Test
	public void testMinTwoVariables() {
		Assert.assertTrue(minAreEquals(0, "X^2+Y^2", new double[] {-1,-1}));
		Assert.assertTrue(minAreEquals(0, "X^2+Y^2", new double[] {1,1}));
		Assert.assertTrue(minAreEquals(0, "X^2+Y^2", new double[] {10,10}));
		Assert.assertTrue(minAreEquals(0, "X^2+Y^2", new double[] {10000,10000}));
		Assert.assertTrue(minAreEquals(1, "X^2+Y^2+1", new double[] {10000,1}));
		Assert.assertTrue(minAreEquals(1.0, "(1+X^2+Y^2)^(1/2)", new double[] {-1,-1}));
		Assert.assertTrue(minAreEquals(1.0, "(1+X^2+Y^2)^(1/2)", new double[] {-100000,56}));
		Assert.assertTrue(minAreEquals(-6.425963881403929, "3*X^4 + X^3 - 6*X^2 + 2 + 3*Y^4 + Y^3 - 6*Y^2 ", new double[] {-1,-1}));
		Assert.assertTrue(minAreEquals(-6.425963881403929, "3*X^4 + X^3 - 6*X^2 + 2 + 3*Y^4 + Y^3 - 6*Y^2 ", new double[] {-100,-100}));
	}
	
	@Test
	public void testArgminTwoVariables() {
		Assert.assertTrue(argminAreEquals(new double[] {0,0}, "X^2+Y^2", new double[] {-1,-1}));
		Assert.assertTrue(argminAreEquals(new double[] {0,0}, "X^2+Y^2", new double[] {1,1}));
		Assert.assertTrue(argminAreEquals(new double[] {0,0}, "X^2+Y^2", new double[] {10,10}));
		Assert.assertTrue(argminAreEquals(new double[] {0,0}, "X^2+Y^2", new double[] {10000,10000}));
		Assert.assertTrue(argminAreEquals(new double[] {0,0}, "X^2+Y^2+1", new double[] {10000,1}));
		Assert.assertTrue(argminAreEquals(new double[] {0,0}, "(1+X^2+Y^2)^(1/2)", new double[] {-1,-1}));
		Assert.assertTrue(argminAreEquals(new double[] {0,0}, "(1+X^2+Y^2)^(1/2)", new double[] {-100000,56}));
		Assert.assertTrue(argminAreEquals(new double[] {-1.132782218537319,-1.132782218537319}, "3*X^4 + X^3 - 6*X^2 + 2 + 3*Y^4 + Y^3 - 6*Y^2 ", new double[] {-1,-1}));
		Assert.assertTrue(argminAreEquals(new double[] {-1.132782218537319,-1.132782218537319}, "3*X^4 + X^3 - 6*X^2 + 2 + 3*Y^4 + Y^3 - 6*Y^2 ", new double[] {-100,-100}));
	}
	
	@Test
	public void testMinMultipleVariables() {
		Assert.assertTrue(minAreEquals(0, "A^2+B^2+C^2+D^2+E^2+F^2+G^2", new double[] {-1,-1,-1,-1,-1,-1,-1}));
		Assert.assertTrue(minAreEquals(0, "(A-1)^2+(B-2)^2+(C-3)^2+(D-4)^2+(E-5)^2+(F-6)^2+(G-7)^2", new double[] {-1,-1,-1,-1,-1,-1,-1}));
	}
	
	/**
	 * Method to test if the optimum given by the function is close (precision can be chosen) to the real result.
	 *
	 */
	public static boolean minAreEquals(double expectedResult, String stringToMinimize, double[] initialGuess) {
		MinimizationWithNelderMead minWithNM = new MinimizationWithNelderMead(Expressions.parse(stringToMinimize), initialGuess);
		double minimumOfExpression = minWithNM.findMinimum();
		//System.out.println(minimumOfExpression);
		//System.out.println(expectedResult);
		if (Math.abs(minimumOfExpression - expectedResult) <= 1e-9) {
			return true;
		}
		else {
			return false;
		}
	}
	
	public static boolean argminAreEquals(double[] expectedResult, String stringToMinimize, double[] initialGuess) {
		MinimizationWithNelderMead minWithNM = new MinimizationWithNelderMead(Expressions.parse(stringToMinimize), initialGuess);
		double[] argminOfExpression = minWithNM.findArgmin();
		//System.out.println(argminOfExpression);
		//System.out.println(expectedResult);
		for(int i = 0; i < expectedResult.length; i++) {
			if (Math.abs(argminOfExpression[i] - expectedResult[i]) > 1e-5) {
				return false;
			}
		}
		return true;
	}

}
