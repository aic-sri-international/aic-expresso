package com.sri.ai.test.grinder.performance;

import static com.sri.ai.util.Util.println;

import org.junit.Test;
import com.sri.ai.grinder.tester.ContextSplittingTester;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;



public class ContextPerformanceTest {
	
	// TESTING PARAMETERS
	private static final int numberOfVariables = 4;
	private static final int cardinalityOfVariables = 2;
			
	// RESULT DISPLAY PARAMETER
	private static final boolean verbose = true;
	
	// OTHER GLOBAL CONSTANTS
	private static final Theory THEORY = new DifferenceArithmeticTheory(false, true);


	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// JUNIT TESTS ////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Test
	public void jUnitContextSplittingTest() {
		ContextSplittingTester contextSplittingTest = new ContextSplittingTester(numberOfVariables, cardinalityOfVariables, verbose, THEORY);
		contextSplittingTest.performContextSplittingTest();
		contextSplittingTest.printLastTestdResults();
	}
	
}
