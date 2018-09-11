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
	public void numericGroupTestFreeVariableInsideSummations() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"K", "1..5"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("sum({{(on I in 1..5) if I != 3 and K != 4 then 30 else 40 }})"), 
												  parse("sum({{(on J in 10..13) if J != 5 then if K != 3 then J else 0 else 40 }})") };
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : NUMERIC_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}

	@Test
	public void numericGroupTest00() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("sum({{(on I in 1..5) if I != 3 then 30 else 40 }})"), 
												  parse("sum({{(on J in 10..13) if J != 5 then J else 40 }})") };
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : NUMERIC_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}
	
	@Test
	public void numericGroupTest01() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("1"), 
												  parse("2") };
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : NUMERIC_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}
	
	@Test
	public void numericGroupTest02() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("1"), 
												  parse("2"),
												  parse("0"),
												  parse("3")};
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : NUMERIC_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}
	
	public void numericGroupTest03() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("1"), 
												  parse("2"),
												  parse("3") };
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : NUMERIC_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}

	@Test
	public void numericGroupTest04() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"X", "0..2", "Y", "0..2"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("if X = 1 then 2 else 3"), 
												  parse("if Y != 2 then 4 else 5") };
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : NUMERIC_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}
	
	
	
	@Test
	public void quantifierBasedGroupTest01() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("true"), 
												  parse("true")};
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : QUANTIFIER_BASED_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}
	
	
	@Test
	public void quantifierBasedGroupTest02() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("true"), 
												  parse("false")};
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : QUANTIFIER_BASED_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}
	
	@Test
	public void quantifierBasedGroupTest03() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("false"), 
												  parse("true")};
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : QUANTIFIER_BASED_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}
	
	@Test
	public void quantifierBasedGroupTest04() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string

		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("false"), 
												  parse("false")};
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : QUANTIFIER_BASED_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
	}
	
	@Test
	public void quantifierBasedGroupTest05() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string

		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"X", "0..2", "Y", "0..2"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("if X != 1 then false else true"), 
												  parse("if Y = 2 then true else false")};
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : QUANTIFIER_BASED_GROUPS)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);

			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 actualResult, expectedResult);
			
			assertTestSuccess(actualResult, expectedResult);
			
			println("\n");
		}
		
		println();
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
	
	
	private void printTestResults(AssociativeCommutativeGroup group, Expression[] operandExpressions,
			Expression expressionThatAppliesGroupOperatorToOperands, Expression actualResult, Expression expectedResult) {
		printTestSpecs(group,operandExpressions);
		if(printTestDetails)
		{
			if(verbose) {
				printExpressionThatAppliesGroupOperatorToOperands(expressionThatAppliesGroupOperatorToOperands);
			}
			printResults(expectedResult, actualResult);
		}
	}
	
	
	
	private void printTestHeader(String testName, Theory theory) {
			println();
			println("  ||||||||||||||||||||||||||||||||||||||||||||");
			println("               " + testName);
		if(printTestDetails && verbose) {
			println("  --------------------------------------------");
			println("   Theory: " + theory);
		}
			println("  ||||||||||||||||||||||||||||||||||||||||||||");
			println();		
	}
		
	private void printTestSpecs(  AssociativeCommutativeGroup  group, 
								  Expression[] 				   operandExpressions) {
		
			println("     =======================================================");
			println("                          "+ group);
			println("     =======================================================");
		if(printTestDetails) {
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
