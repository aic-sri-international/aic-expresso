package com.sri.ai.test.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.StepSolver;
import com.sri.ai.grinder.api.StepSolver.Step;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.library.controlflow.IfThenElseStepSolver;

public class IfThenElseStepSolverTest {
	
	
	/////////  TEST PRINTOUT OPTION ////////
	static final boolean verbose = true;
	
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////  JUNIT TESTS  /////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Test
	public void test00() {
		Expression expression = parse("if true then 2 else 3");
		Context context = new TrueContext(new CommonTheory());
		
		Expression expectedResult = parse("2");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test01() {
		Expression expression = parse("if false then 2 else 3");
		Context context = new TrueContext(new CommonTheory());
		
		Expression expectedResult = parse("3");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test02() {
		Expression expression = parse("if X=1 then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if X = 1 then 2 else 3");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test03() {
		Expression expression = parse("if X=1 then 2 else 3");
		String[] symbolsAndTypes = {"X", "1..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("2");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test04() {
		Expression expression = parse("if X=2 then 2 else 3");
		String[] symbolsAndTypes = {"X", "1..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("3");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test05() {
		Expression expression = parse("if (X=1) and (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if X = 1 then if Y = 1 then 2 else 3 else 3");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	
	@Test
	public void test06() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if X = 1 then 2 else if Y = 1 then 2 else 3");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test07() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "1..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("2");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test08() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..0","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if Y = 1 then 2 else 3");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test09() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "1..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("2");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test10() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..0"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if X = 1 then 2 else 3");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test11() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 + 2 else 3 + 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if X = 1 then 4 else if Y = 1 then 4 else 6");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test12() {
		Expression expression = parse("if (X=1) or (Y=1) then (X=1) and (Y=1) else (X=1) or (Y=1)");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if X = 1 then Y = 1 else false");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test13() {
		Expression expression = parse("if (X=1) and (Y=1) then (X!=1) and (Y!=1) else (X!=1) or (Y!=1)");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if X = 1 then if Y = 1 then false else true else true");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test14() {
		Expression expression = parse("if (X=0) and (Y=1) then if (Z = 1) then (X + Z) else (Y + Z) else 4");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1", "Z", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if X = 0 then if Y = 1 then if Z = 1 then 1 else 1 + Z else 4 else 4");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	@Test
	public void test15() {
		Expression expression = parse("if (X=0) or (Y=1) then if (Z = 1) then (X + Z) else (Y + Z) else 4");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1", "Z", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		Expression expectedResult = parse("if X = 0 then if Z = 1 then 1 else Y + Z else if Y = 1 then if Z = 1 then X + 1 else 1 + Z else 4");
		
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		runIfThenElseStepSolverTest(expression, context, expectedResult, testName);
	}
	
	
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	/////////////////////////////////////////////  END OF TESTS  /////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	
	private void runIfThenElseStepSolverTest(Expression expression, Context context, Expression expectedResult, String testName) {
		printTestHeader(testName,expression,context.getSymbolsAndTypes());
		
		IfThenElseStepSolver stepSolver = new IfThenElseStepSolver(expression);
		Expression actualResult = ContextDependentExpressionProblemSolver.staticSolve(stepSolver, context);
		
		if(verbose) {
			printIndividualSteps(stepSolver,context);
		}
		println(" result: " + actualResult);
		
		assertEquals(expectedResult, actualResult);
		
		println("\n");
	}
	
	
	


	
	private String evaluateAndWriteIndividualStepResultsToString(StepSolver stepSolver, Context context) {
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = evaluateByRecursivelyTakingSteps(stepSolver, context, stepCount, nestingOfStep);
		result = removeTrailingWhiteSpaces(result);
		
		return result;
	}
	
	
	
	private String removeTrailingWhiteSpaces(String result) {
		int numWhiteSpaces = 0;
		int originalLength = result.length();
		
		for(int i = originalLength-1; i >= 0; --i) {
			if(Character.isWhitespace(result.charAt(i))) {
				++numWhiteSpaces;
			}
			else {
				break;
			}
		}
		int indexOfLastNonWhiteSpaceChar = originalLength - numWhiteSpaces;
		String trimmedResult = result.substring(0, indexOfLastNonWhiteSpaceChar);
		return trimmedResult;
	}

	// A method for tracing the steps (and results of the steps) the step solver takes
	private String evaluateByRecursivelyTakingSteps(StepSolver stepSolver, Context context, AtomicInteger stepCount, int nestingOfStep) {
		String result;
		
		Step step = stepSolver.step(context);
		stepCount.incrementAndGet();
		
		if(step.itDepends()) {
			ContextSplitting splitting = step.getContextSplittingWhenSplitterIsLiteral();
			
			String nestingString = createNestingString(nestingOfStep);
			//println(nestingString + "[" + stepCount + "] split on: " + splitting.getLiteral());
			result = nestingString + "[" + stepCount + "] split on: " + splitting.getLiteral() + System.lineSeparator();
			
			Context trueContext = splitting.getContextAndLiteral();
			Context falseContext = splitting.getContextAndLiteralNegation();
			int nestingOfNextStep = nestingOfStep + 1;
			
			result += evaluateByRecursivelyTakingSteps(step.getStepSolverForWhenSplitterIs(true), trueContext, stepCount, nestingOfNextStep);
			result += evaluateByRecursivelyTakingSteps(step.getStepSolverForWhenSplitterIs(false), falseContext, stepCount, nestingOfNextStep);
		}
		else {
			String nestingString = createNestingString(nestingOfStep);
			//println(nestingString + "[" + stepCount + "] < " + step.getValue() + " >");
			result = nestingString + "[" + stepCount + "] < " + step.getValue() + " >" + System.lineSeparator();
		}
		return result;
	}
	
	private String createNestingString(int nesting) {
		StringBuffer outputBuffer = new StringBuffer(nesting);
		outputBuffer.append(" ");
		for (int i = 0; i < nesting; ++i){
		   outputBuffer.append("    ");
		}
		//outputBuffer.append(" ");
		return outputBuffer.toString();
	}
	
	private void printTestHeader(String testName, Expression expression, Map<Expression,Expression> symbolsAndTypes) {
		println("=================== "+ testName + " ===================");
		println(" " + expression);
		println(" " + (symbolsAndTypes.size() == 0 ? "[no symbols or types]" : symbolsAndTypes.toString()) );
		println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");

	}
	
	private void printIndividualSteps(StepSolver stepSolver, Context context) {
		println(" [steps] and splittings or < results >...");
		println(" - - - - - - - - - - - - - - - - - - - - -");
		String result = evaluateAndWriteIndividualStepResultsToString(stepSolver, context);
		println(result);
		println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
	}

}
