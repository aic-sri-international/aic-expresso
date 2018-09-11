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
	
	
	//////////////////////////////////////////////////////////////
	// GLOBAL TEST SETTINGS  /////////////////////////////////////
	//////////////////////////////////////////////////////////////
		static final boolean printTestDetails = true;
		static final boolean verbose = true;
	//////////////////////////////////////////////////////////////
	
	
	// GLOBAL CONSTANTS	
	static final AbstractNumericGroup[] NUMERIC_GROUPS = { new Max(), new Product(), new Sum() };
	static final AbstractQuantifierBasedGroup[] QUANTIFIER_BASED_GROUPS = { new Conjunction(), new Disjunction()};
	
	
	

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// JUNIT TESTS ////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/// NUMERIC GROUP TESTS //////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////
	
	@Test
	public void numericGroupTestFreeVariableInsideSummations() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"K", "1..5"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("sum({{(on I in 1..5) if I != 3 and K != 4 then 30 else 40 }})"), 
												  parse("sum({{(on J in 10..13) if J != 5 then if K != 3 then J else 0 else 40 }})") };
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
		println();
	}
	
	@Test
	public void numericGroupCombinedTest() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"K", "0..5"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("sum({{(on I in 3..5) if I != 4 and K != 2 then X else 1 }})"), 
												  parse("product({{(on J in 1..5) if J != 3 then if K != 3 then J else Y else 1 }})"),
												  parse("max({{(on L in 0..2) if K != 0 then if L != 0 then J/L else Z else if L!=0 then 0 else 1 }})")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
		println();
	}
	
	@Test
	public void numericGroupTestSumOperandsWithFreeVariables() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"K", "0..5"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("sum({{(on I in 3..5) if I != 4 and K != 2 then X else 1 }})"), 
												  parse("sum({{(on J in 1..5) if J != 3 then if K != 3 then J else Y else 1 }})")};

		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
		println();
	}
	
	@Test
	public void numericGroupTestProductOperandsWithFreeVariables() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"K", "0..5"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("product({{(on I in 3..5) if I != 4 and K != 2 then X else 1 }})"), 
												  parse("product({{(on J in 1..5) if J != 3 then if K != 3 then J else Y else 1 }})")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
		println();
	}
	
	@Test
	public void numericGroupTestMaxOperandsWithFreeVariables() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"K", "0..5"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("max({{(on I in 3..5) if I != 4 and K != 2 then X else 1 }})"), 
												  parse("max({{(on J in 1..5) if J != 3 then if K != 3 then J else Y else 1 }})"),
												  parse("max({{(on L in 0..2) if K != 0 then if L != 0 then J/L else Z else if L!=0 then 0 else 1 }})")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
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
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
		println();
	}
	
	@Test
	public void numericGroupTestWithMaxOperands() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("max({{(on I in 1..5) if I = 3 then 10 else I }})"),
												  parse("max({{(on J in 10..20) if J < 10 then 10 else J }})")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
		println();
	}
	
	@Test
	public void numericGroupTestWithMaxOperandsWithFreeVariables() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("max({{(on I in 0..5) if I < 2 then I*X else Y }})"),
												  parse("max({{(on J in 10..20) if J < 10 then Y else J*Z }})")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
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
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
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
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
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
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
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
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, NUMERIC_GROUPS);
		
		println();
	}
	
	
	
	/// QUANTIFIER-BASED GROUP TESTS //////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	
	@Test
	public void quantifierBasedGroupTest01() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("true"), 
												  parse("true")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, QUANTIFIER_BASED_GROUPS);
		
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
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, QUANTIFIER_BASED_GROUPS);
		
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
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, QUANTIFIER_BASED_GROUPS);
		
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
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, QUANTIFIER_BASED_GROUPS);
		
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
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, QUANTIFIER_BASED_GROUPS);
		
		println();
	}
	
	@Test
	public void quantifierBasedGroupTestFreeVariableInsideSummations() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"K", "1..5"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("there exists I in Integer : I > 3 and I < K"), 
												  parse("for all J in Integer : if J > K and J < 10 then J != X else Y") };
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, QUANTIFIER_BASED_GROUPS);
		
		println();
	}

	@Test
	public void quantifierBasedGroupTest00() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("there exists I in Integer : I > 3 and I < 10"), 
												  parse("for all I in Integer : if I > 3 and I < 10 then true else false") };
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, QUANTIFIER_BASED_GROUPS);
		
		println();
	}
	
	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// AUXILIARY METHODS //////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/// GENERAL HELPERS ///////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////

	private void repeatTestForEachRespectiveGroup(String testName, Theory theory, Context context,
			Expression[] operandExpressions, AssociativeCommutativeGroup[] groupsToTest) {
		
		printTestHeader(testName, theory);
		for(AssociativeCommutativeGroup group : groupsToTest)
		{
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);
			printTestResults(group, operandExpressions, expressionThatAppliesGroupOperatorToOperands, 
							 expectedResult, actualResult);
			assertTestSuccess(expectedResult, actualResult);
			println("\n");
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
	
	
	/// PRINTING METHODS //////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////
	
	private void printTestResults(AssociativeCommutativeGroup group, Expression[] operandExpressions,
			Expression expressionThatAppliesGroupOperatorToOperands, Expression expectedResult, Expression actualResult) {
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
	
	
	/// TEST ASSERTION ////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////

	private void assertTestSuccess(Expression expectedResult, Expression actualResult) {
		if(expectedResult.equals(actualResult)) {
			println("      test evaluation:        Success");
		}
		else {
			println("      test evaluation:        FAILED!!!");
			fail("Expected ||" + expectedResult + "||, but produced ||" + actualResult + "|| instead");
		}
	}
	
}
