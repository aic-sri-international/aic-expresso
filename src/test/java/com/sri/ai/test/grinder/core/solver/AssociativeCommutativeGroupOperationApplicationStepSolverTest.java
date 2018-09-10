package com.sri.ai.test.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.mapIntoArray;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.solver.AssociativeCommutativeGroupOperationApplicationStepSolver;
import com.sri.ai.grinder.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.group.Sum;

public class AssociativeCommutativeGroupOperationApplicationStepSolverTest {
	
	@Test
	public void summationTest01() {
		
		Theory theory = new CommonTheory();
		AssociativeCommutativeGroup group = new Sum();
		String[] symbolsAndTypes = {};
		Expression[] operandExpressions = { parse("1"), 
											parse("2") };
		
		Expression expectedResult = parse("3");
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						theory, group, operandExpressions, symbolsAndTypes);

		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestHeader(testName,theory,group,operandExpressions);
		printResult(actualResult);
		assertEquals(expectedResult, actualResult);
		
		println("\n");
	}
	
	@Test
	public void summationTest02() {
		
		Theory theory = new CommonTheory();
		AssociativeCommutativeGroup group = new Sum();
		String[] symbolsAndTypes = {"X", "0..1"};
		Expression[] operandExpressions = { parse("if X=1 then 2 else 3"), 
											parse("4") };
		
		Expression expectedResult = parse("if X = 1 then 6 else 7");
		Expression actualResult = 
				solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
						theory, group, operandExpressions, symbolsAndTypes);
 
		String testName = new Object(){} .getClass().getEnclosingMethod().getName();  //get method name as string
		printTestHeader(testName,theory,group,operandExpressions);
		printResult(actualResult);
		assertEquals(expectedResult, actualResult);
		
		println("\n");
	}





private Expression solveUsingAssociativeCommutativeGroupOperationApplicationStepSolver(
			Theory theory, AssociativeCommutativeGroup group, Expression[] operandExpressions, String[] symbolsAndTypes) {
	
	Context context = new TrueContext(theory).extendWithSymbolsAndTypes(symbolsAndTypes);
	ExpressionLiteralSplitterStepSolver[] operandStepSolvers = 
			constructArrayOfStepSolversForSolvingOperands(theory,operandExpressions);
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


	
	
	private void printTestHeader( String 					   testName, 
								  Theory					   theory, 
								  AssociativeCommutativeGroup  group, 
								  Expression[] 				   operandExpressions) {
		
		println("=================== "+ testName + " ===================");
		println(" Theory:    " + theory);
		println(" Group:     " + group);
		println(" Operands:  " + Arrays.toString(operandExpressions));
		println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");

	}

	private void printResult(Expression actualResult) {
		println(" result:    " + actualResult);
	}
}
