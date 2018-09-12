package com.sri.ai.test.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.fail;

import java.util.Arrays;
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
import com.sri.ai.grinder.group.AbstractFunctionBasedGroup;
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
	static final AbstractFunctionBasedGroup[] NUMERIC_GROUPS = { new Max(), new Product(), new Sum() };
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
		
		Expression[] expectedResultPerGroup = 	{ parse("if K != 4 then 160 else 200"),
												  parse("if K != 4 then if K != 3 then 7360 else 0 else 9200"),
												  parse("if K != 4 then if K != 3 then 206 else 160 else 246")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("if K != 2 then if K != 3 then max(max(2 * X + 1, 40), max({{ ( on L in 0..2 ) if K != 0 "
														+ "then if L != 0 then J / L else Z else if L != 0 then 0 else 1 }})) else "
														+ "max(max(2 * X + 1, Y * Y * Y * Y), max(max(Z, J), J / 2)) else max(40, max(max(Z, J), J / 2))"),
												  parse("if K != 2 then if K != 3 then (2 * X + 1) * 40 * max({{ ( on L in 0..2 ) if K != 0 "
												  		+ "then if L != 0 then J / L else Z else if L != 0 then 0 else 1 }}) "
												  		+ "else (2 * X + 1) * Y * Y * Y * Y * max(max(Z, J), J / 2) else 120 * max(max(Z, J), J / 2)"),
												  parse("if K != 2 then if K != 3 then 2 * X + 41 + max({{ ( on L in 0..2 ) if K != 0 "
												  		+ "then if L != 0 then J / L else Z else if L != 0 then 0 else 1 }}) "
												  		+ "else Y ^ 4 + 2 * X + 1 + max(max(Z, J), J / 2) else 43 + max(max(Z, J), J / 2)")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("if K != 2 then if K != 3 then max(2 * X + 1, 13) else max(2 * X + 1, 4 * Y + 1) else 13"),
												  parse("if K != 2 then if K != 3 then (2 * X + 1) * 13 else (2 * X + 1) * (4 * Y + 1) else 39"),
												  parse("if K != 2 then if K != 3 then 2 * X + 14 else 2 * X + 4 * Y + 2 else 16")};

		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("if K != 2 then if K != 3 then max(X ^ 2, 40) else max(X ^ 2, Y * Y * Y * Y) else 40"),
												  parse("if K != 2 then if K != 3 then X ^ 2 * 40 else X ^ 2 * Y * Y * Y * Y else 40"),
												  parse("if K != 2 then if K != 3 then X ^ 2 + 40 else Y ^ 4 + X ^ 2 else 41")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("if K != 2 then if K != 3 then max(max(max(X, 1), 5), max({{ ( on L in 0..2 ) if K != 0 "
														+ "then if L != 0 then J / L else Z else if L != 0 then 0 else 1 }})) "
														+ "else max(max(max(X, 1), max(max(max(max(Y, Y), 1), Y), Y)), max(max(Z, J), J / 2)) "
														+ "else max(5, max(max(Z, J), J / 2))"),
												  parse("if K != 2 then if K != 3 then max(X, 1) * 5 * max({{ ( on L in 0..2 ) if K != 0 "
												  		+ "then if L != 0 then J / L else Z else if L != 0 then 0 else 1 }}) else max(X, 1) * "
												  		+ "max(max(max(max(Y, Y), 1), Y), Y) * max(max(Z, J), J / 2) else 5 * max(max(Z, J), J / 2)"),
												  parse("if K != 2 then if K != 3 then max(X, 1) + 5 + max({{ ( on L in 0..2 ) if K != 0 then if L != 0 "
												  		+ "then J / L else Z else if L != 0 then 0 else 1 }}) else max(X, 1) + "
												  		+ "max(max(max(max(Y, Y), 1), Y), Y) + max(max(Z, J), J / 2) else 6 + max(max(Z, J), J / 2)")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("160"),
												  parse("7360"),
												  parse("206")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("20"),
												  parse("200"),
												  parse("30")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("max(max(max(max(max(max(0, X), Y), Y), Y), Y), "
														+ "max(max(max(max(max(max(max(max(max(max(10 * Z, 11 * Z), 12 * Z), 13 * Z), 14 * Z), "
														+ "15 * Z), 16 * Z), 17 * Z), 18 * Z), 19 * Z), 20 * Z))"),
												  parse("max(max(max(max(max(0, X), Y), Y), Y), Y) * "
												  		+ "max(max(max(max(max(max(max(max(max(max(10 * Z, 11 * Z), 12 * Z), 13 * Z), 14 * Z), "
												  		+ "15 * Z), 16 * Z), 17 * Z), 18 * Z), 19 * Z), 20 * Z)"),
												  parse("max(max(max(max(max(0, X), Y), Y), Y), Y) + "
												  		+ "max(max(max(max(max(max(max(max(max(max(10 * Z, 11 * Z), 12 * Z), 13 * Z), 14 * Z), "
												  		+ "15 * Z), 16 * Z), 17 * Z), 18 * Z), 19 * Z), 20 * Z)")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("2"),
												  parse("2"),
												  parse("3")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("3"),
												  parse("0"),
												  parse("6")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
		println();
	}
	
	@Test
	public void numericGroupTest03() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("1"), 
												  parse("2"),
												  parse("3") };
		
		Expression[] expectedResultPerGroup = 	{ parse("3"),
												  parse("6"),
												  parse("6")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("if Y != 2 then 4 else 5"),
												  parse("if X = 1 then if Y != 2 then 8 else 10 else if Y != 2 then 12 else 15"),
												  parse("if X = 1 then if Y != 2 then 6 else 7 else if Y != 2 then 7 else 8")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, NUMERIC_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("true"),
												  parse("true")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, QUANTIFIER_BASED_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("false"),
												  parse("true")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, QUANTIFIER_BASED_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("false"),
												  parse("true")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, QUANTIFIER_BASED_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("false"),
												  parse("false")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, QUANTIFIER_BASED_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("if X != 1 then false else Y = 2"),
												  parse("if X != 1 then Y = 2 else true")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, QUANTIFIER_BASED_GROUPS);
		
		println();
	}
	
	@Test
	public void quantifierBasedGroupTestFreeVariableInsideSummations() {
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  // get method name as string
		
		Theory theory = 						new CommonTheory();
		String[] symbolsAndTypes = 				{"K", "1..5"};
		Context context = 						new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression[] operandExpressions = 		{ parse("there exists I in Integer : I > 3 and I < K"), 
												  parse("for all J in Integer : if J > K and J < 10 then X else Y") };
		
		Expression[] expectedResultPerGroup = 	{ parse("if 4 < K then X and Y else false"),
												  parse("if 4 < K then true else X and Y")};
		
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, QUANTIFIER_BASED_GROUPS);
		
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
		
		Expression[] expectedResultPerGroup = 	{ parse("false"),
												  parse("true")};
		
		repeatTestForEachRespectiveGroup(testName, theory, context, operandExpressions, expectedResultPerGroup, QUANTIFIER_BASED_GROUPS);
		
		println();
	}
	
	
	
	
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
// AUXILIARY METHODS //////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	/// GENERAL HELPERS ///////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////////////////

	private void repeatTestForEachRespectiveGroup(String testName, Theory theory, Context context,
			Expression[] operandExpressions, Expression[] expectedResultPerGroup, AssociativeCommutativeGroup[] groupsToTest) {
		
		printTestHeader(testName, theory);
		for(int i = 0; i < groupsToTest.length; ++i)
		{
			AssociativeCommutativeGroup group = groupsToTest[i];
			Expression actualResult = 
					solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
							group, operandExpressions, context);
			Expression expressionThatAppliesGroupOperatorToOperands = 
					constructExpressionThatAppliesGroupOperatorToOperands(group, operandExpressions);
			//Expression expectedResult = constructExpectedResult(expressionThatAppliesGroupOperatorToOperands, context);
			Expression expectedResult = expectedResultPerGroup[i];
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
