package com.sri.ai.test.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.fail;

import java.util.Arrays;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.solver.AssociativeCommutativeGroupOperationApplicationStepSolver;
import com.sri.ai.grinder.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.group.AbstractNumericGroup;
import com.sri.ai.grinder.group.AbstractQuantifierBasedGroup;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.group.Conjunction;
import com.sri.ai.grinder.group.Disjunction;
import com.sri.ai.grinder.group.Max;
import com.sri.ai.grinder.group.Sum;
import com.sri.ai.grinder.group.Product;


public class AssociativeCommutativeGroupOperationApplicationStepSolverTest {
	static final boolean printTestDetails = true;
	static final boolean verbose = true;
	
	static final AbstractNumericGroup[] NUMERIC_GROUPS = { new Max(), new Product(), new Sum() };
	static final AbstractQuantifierBasedGroup[] QUANTIFIER_BASED_GROUPS = { new Conjunction(), new Disjunction()};
	
	
	
	@Test
	public void testNumericGroups() {
		for(AbstractNumericGroup group : NUMERIC_GROUPS)
		{
			printGroupBeingTested(group);
			
			for(int testNumber = 1; ; ++testNumber)
			{
				String testNum = testNumber < 10 ? "0"+testNumber : ""+testNumber;
				String testMethodName = "numericGroupTest"+testNum;
				
				Method testMethod = loadTestMethod(testMethodName);
				if(testMethod == null) {
					break;
				}
				
				runTest(group, testMethod);
			}
		}
	}
	
	@Test
	public void testQuantifierBasedGroups() {
		for(AbstractQuantifierBasedGroup group : QUANTIFIER_BASED_GROUPS)
		{
			printGroupBeingTested(group);
			
			for(int testNumber = 1; ; ++testNumber)
			{
				String testNum = testNumber < 10 ? "0"+testNumber : ""+testNumber;
				String testMethodName = "quantifierBasedGroupTest"+testNum;
				
				Method testMethod = loadTestMethod(testMethodName);
				if(testMethod == null) {
					break;
				}
				
				runTest(group, testMethod);
			}
		}
	}
	
	
	
	
	
	

	public void numericGroupTest02(AssociativeCommutativeGroup group) {
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("1"), 
												  parse("2"),
												  parse("0"),
												  parse("3")};
		
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						group, operandExpressions, context);
		
		Expression expressionThatAppliesGroupOperatorToOperands = 
				constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
		Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);


		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestResults(theory, group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
						 actualResult, expectedResult, testName);
		
		assertTestSuccess(actualResult, expectedResult);
		
		println("\n");
	}

	public void numericGroupTest01(AssociativeCommutativeGroup group) {
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("1"), 
												  parse("2") };
		
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						group, operandExpressions, context);
		
		Expression expressionThatAppliesGroupOperatorToOperands = 
				constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
		Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);


		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestResults(theory, group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
						 actualResult, expectedResult, testName);
		
		assertTestSuccess(actualResult, expectedResult);
		
		println("\n");
	}

	
	
	public void numericGroupTest03(AssociativeCommutativeGroup group) {
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("1"), 
												  parse("2"),
												  parse("3") };
		
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						group, operandExpressions, context);
		
		Expression expressionThatAppliesGroupOperatorToOperands = 
				constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
		Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestResults(theory, group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
						 actualResult, expectedResult, testName);
		
		assertTestSuccess(actualResult, expectedResult);
		
		println("\n");
	}

	
	public void numericGroupTest04(AssociativeCommutativeGroup group) {
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"X", "0..2", "Y", "0..2"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("if X = 1 then 2 else 3"), 
												  parse("if Y != 2 then 4 else 5") };
		
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						group, operandExpressions, context);
		
		Expression expressionThatAppliesGroupOperatorToOperands = 
				constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
		Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestResults(theory, group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
						 actualResult, expectedResult, testName);
		
		assertTestSuccess(actualResult, expectedResult);
		
		println("\n");
	}
	
	
	

	public void quantifierBasedGroupTest01(AssociativeCommutativeGroup group) {
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("true"), 
												  parse("true")};
		
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						group, operandExpressions, context);
		
		Expression expressionThatAppliesGroupOperatorToOperands = 
				constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
		Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);


		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestResults(theory, group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
						 actualResult, expectedResult, testName);
		
		assertTestSuccess(actualResult, expectedResult);
		
		println("\n");
	}
	
	public void quantifierBasedGroupTest02(AssociativeCommutativeGroup group) {
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("true"), 
												  parse("false")};
		
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						group, operandExpressions, context);
		
		Expression expressionThatAppliesGroupOperatorToOperands = 
				constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
		Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);


		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestResults(theory, group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
						 actualResult, expectedResult, testName);
		
		assertTestSuccess(actualResult, expectedResult);
		
		println("\n");
	}
	
	public void quantifierBasedGroupTest03(AssociativeCommutativeGroup group) {
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("false"), 
												  parse("true")};
		
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						group, operandExpressions, context);
		
		Expression expressionThatAppliesGroupOperatorToOperands = 
				constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
		Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);


		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestResults(theory, group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
						 actualResult, expectedResult, testName);
		
		assertTestSuccess(actualResult, expectedResult);
		
		println("\n");
	}
	
	public void quantifierBasedGroupTest04(AssociativeCommutativeGroup group) {
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("false"), 
												  parse("false")};
		
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						group, operandExpressions, context);
		
		Expression expressionThatAppliesGroupOperatorToOperands = 
				constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
		Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);


		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestResults(theory, group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
						 actualResult, expectedResult, testName);
		
		assertTestSuccess(actualResult, expectedResult);
		
		println("\n");
	}
	
	public void quantifierBasedGroupTest05(AssociativeCommutativeGroup group) {
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"X", "0..2", "Y", "0..2"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("if X != 1 then false else true"), 
												  parse("if Y = 2 then true else false")};
		
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						group, operandExpressions, context);
		
		Expression expressionThatAppliesGroupOperatorToOperands = 
				constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
		Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);


		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestResults(theory, group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
						 actualResult, expectedResult, testName);
		
		assertTestSuccess(actualResult, expectedResult);
		
		println("\n");
	}
	
	
	
	
	
	
	
	
	
	

	private Method loadTestMethod(String methodName) {
		Method method = null;
		try {
			method = this.getClass().getMethod(methodName, AssociativeCommutativeGroup.class);
		} 
		catch (NoSuchMethodException | SecurityException e) {
		}
		return method;
	}
	


	private void runTest(AssociativeCommutativeGroup group, Method testMethod) {
		try {
			testMethod.invoke(this, group);
		} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
			e.printStackTrace();
			fail("invocation on " + testMethod.getName() + " failed");
		}
	}


	private Expression constructExpectedResult(Expression expressionThatAppliesGroupOperatorToOperands, Context context) {
		
		Expression expectedResult = context.evaluate(expressionThatAppliesGroupOperatorToOperands);
		
		return expectedResult;
	}
	
	private Expression constructExpressionThatAppliesGroupOperatorToOperands(AssociativeCommutativeGroup group,
			Expression[] operandExpressions) {
		String associativeOperation = group.getFunctionString();
		Expression expressionThatAppliesOperatorToOperands = Expressions.apply( associativeOperation, operandExpressions);
		return expressionThatAppliesOperatorToOperands;
	}
	
	
	private Expression solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
				AssociativeCommutativeGroup group, Expression[] operandExpressions, Context context) {
		
		ExpressionLiteralSplitterStepSolver[] operandStepSolvers = 
				constructArrayOfStepSolversForSolvingOperands(context.getTheory(),operandExpressions);
		AssociativeCommutativeGroupOperationApplicationStepSolver stepSolver = 
				new AssociativeCommutativeGroupOperationApplicationStepSolver(group, operandStepSolvers);
		Expression result = ContextDependentExpressionProblemSolver.staticSolve(stepSolver, context);
		
		return result;
	}
	
	
	
	
	private ExpressionLiteralSplitterStepSolver[] constructArrayOfStepSolversForSolvingOperands(
				Theory theory, Expression[] operandExpressions) {
		
		ExpressionLiteralSplitterStepSolver[] operandStepSolvers = mapIntoArray( operandExpressions,
																				 ExpressionLiteralSplitterStepSolver.class, 
																				 (e) -> theory.makeEvaluatorStepSolver(e));
		return operandStepSolvers;
	}
	
	
	private void printTestResults(Theory theory, AssociativeCommutativeGroup group, Expression[] operandExpressions,
			Expression expressionThatAppliesGroupOperatorToOperands, Expression actualResult, Expression expectedResult, String testName) {
		printTestHeader(testName,theory,group,operandExpressions);
		if(printTestDetails)
		{
			if(verbose) {
				printExpressionThatAppliesGroupOperatorToOperands(expressionThatAppliesGroupOperatorToOperands);
			}
			printResults(expectedResult, actualResult);
		}
	}
	
	
	
	private void printGroupBeingTested(AssociativeCommutativeGroup group) {
		println();
		println("  ||||||||||||||||||||||||||||||||||||||||||||");
		println("            Numeric Group:  " + group);
		println("  ||||||||||||||||||||||||||||||||||||||||||||");
		println();		
	}
		
	private void printTestHeader( String 					   testName, 
								  Theory					   theory, 
								  AssociativeCommutativeGroup  group, 
								  Expression[] 				   operandExpressions) {
		
			println("     =================== "+ testName + " ===================");
		if(printTestDetails) {
			println("      Theory:                 " + theory);
			println("      Group:                  " + group);
			println("      Operands:               " + Arrays.toString(operandExpressions));
			println("     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
		}

	}
	
	private void printExpressionThatAppliesGroupOperatorToOperands(Expression ExpressionThatAppliesGroupOperatorToOperands) {
			println("      Equivalent Expression:  " + ExpressionThatAppliesGroupOperatorToOperands);
			println("     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
	}

	private void printResults(Expression expectedResult, Expression actualResult) {
		if(verbose) {
			println("      expected result:        " + expectedResult);
		}
			println("      actual result:          " + actualResult);
			println("     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
	}
	

	private void assertTestSuccess(Expression actualResult, Expression expectedResult) {
		if(expectedResult.equals(actualResult)) {
			println("      test evaluation:        Success");
		}
		else {
			println("      test evaluation:        FAILED!!!");
			fail();
		}
	}
}
