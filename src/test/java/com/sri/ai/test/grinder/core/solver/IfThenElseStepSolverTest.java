package com.sri.ai.test.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionStepSolver;
import com.sri.ai.grinder.api.ExpressionStepSolver.Step;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.core.solver.IfThenElseStepSolver;

public class IfThenElseStepSolverTest {
	
	@Test
	public void test() {
		Expression expression = parse("if X = 1 or Y = 1 then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
						      .getClass()
						      .getEnclosingMethod()
						      .getName();

		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" [no symbols or types]");
		println("---------------------------------------------");
		
		IfThenElseStepSolver stepSolver = new IfThenElseStepSolver(expression);

		Expression actual = ContextDependentExpressionProblemSolver.staticSolve(stepSolver, context);
		
		println(actual);
	}
	
	@Test
	public void test00() {
		Expression expression = parse("if true then 2 else 3");
		Context context = new TrueContext(new CommonTheory());
		
		String testName = new Object() {}
						      .getClass()
						      .getEnclosingMethod()
						      .getName();

		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" [no symbols or types]");
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] < 2 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test01() {
		Expression expression = parse("if false then 2 else 3");
		Context context = new TrueContext(new CommonTheory());
		
		String testName = new Object() {}
						      .getClass()
						      .getEnclosingMethod()
						      .getName();

		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" [no symbols or types]");
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] < 3 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test02() {
		Expression expression = parse("if X=1 then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
						      .getClass()
						      .getEnclosingMethod()
						      .getName();

		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 1\r\n" + 
				"    [2] < 2 >\r\n" + 
				"    [3] < 3 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test03() {
		Expression expression = parse("if X=1 then 2 else 3");
		String[] symbolsAndTypes = {"X", "1..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
						      .getClass()
						      .getEnclosingMethod()
						      .getName();

		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] < 2 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test04() {
		Expression expression = parse("if X=2 then 2 else 3");
		String[] symbolsAndTypes = {"X", "1..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
						      .getClass()
						      .getEnclosingMethod()
						      .getName();

		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] < 3 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test05() {
		Expression expression = parse("if (X=1) and (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 1\r\n" + 
				"    [2] split on: Y = 1\r\n" + 
				"        [3] < 2 >\r\n" + 
				"        [4] < 3 >\r\n" + 
				"    [5] < 3 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	
	@Test
	public void test06() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 1\r\n" + 
				"    [2] < 2 >\r\n" + 
				"    [3] split on: Y = 1\r\n" + 
				"        [4] < 2 >\r\n" + 
				"        [5] < 3 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test07() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "1..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] < 2 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test08() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..0","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: Y = 1\r\n" + 
				"    [2] < 2 >\r\n" + 
				"    [3] < 3 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test09() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "1..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 1\r\n" + 
				"    [2] < 2 >\r\n" + 
				"    [3] < 2 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test10() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 else 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..0"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 1\r\n" + 
				"    [2] < 2 >\r\n" + 
				"    [3] < 3 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test11() {
		Expression expression = parse("if (X=1) or (Y=1) then 2 + 2 else 3 + 3");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 1\r\n" + 
				"    [2] < 4 >\r\n" + 
				"    [3] split on: Y = 1\r\n" + 
				"        [4] < 4 >\r\n" + 
				"        [5] < 6 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test12() {
		Expression expression = parse("if (X=1) or (Y=1) then (X=1) and (Y=1) else (X=1) or (Y=1)");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 1\r\n" + 
				"    [2] split on: Y = 1\r\n" + 
				"        [3] < true >\r\n" + 
				"        [4] < false >\r\n" + 
				"    [5] split on: Y = 1\r\n" + 
				"        [6] < false >\r\n" + 
				"        [7] < false >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test13() {
		Expression expression = parse("if (X=1) and (Y=1) then (X!=1) and (Y!=1) else (X!=1) or (Y!=1)");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 1\r\n" + 
				"    [2] split on: Y = 1\r\n" + 
				"        [3] < false >\r\n" + 
				"        [4] < true >\r\n" + 
				"    [5] < true >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test14() {
		Expression expression = parse("if (X=0) and (Y=1) then if (Z = 1) then (X + Z) else (Y + Z) else 4");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1", "Z", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 0\r\n" + 
				"    [2] split on: Y = 1\r\n" + 
				"        [3] split on: Z = 1\r\n" + 
				"            [4] < 1 >\r\n" + 
				"            [5] < 1 + Z >\r\n" + 
				"        [6] < 4 >\r\n" + 
				"    [7] < 4 >\r\n";

		println(result);
		assertEquals(expected, result);
		
		println();
	}
	
	@Test
	public void test15() {
		Expression expression = parse("if (X=0) or (Y=1) then if (Z = 1) then (X + Z) else (Y + Z) else 4");
		String[] symbolsAndTypes = {"X", "0..1","Y", "0..1", "Z", "0..1"};
		Context context = new TrueContext(new CommonTheory()).extendWithSymbolsAndTypes(symbolsAndTypes);
		
		String testName = new Object() {}
					      .getClass()
					      .getEnclosingMethod()
					      .getName();
		
		println("================== "+ testName + " ==================");
		println(" " + expression);
		println(" " + Arrays.toString(symbolsAndTypes));
		println("---------------------------------------------");
		
		ExpressionStepSolver stepSolver = new IfThenElseStepSolver(expression);
		
		AtomicInteger stepCount =  new AtomicInteger(0);
		int nestingOfStep = 0;
		
		String result = recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		String expected = "[1] split on: X = 0\r\n" + 
				"    [2] split on: Z = 1\r\n" + 
				"        [3] < 1 >\r\n" + 
				"        [4] < Y + Z >\r\n" + 
				"    [5] split on: Y = 1\r\n" + 
				"        [6] split on: Z = 1\r\n" + 
				"            [7] < X + 1 >\r\n" + 
				"            [8] < 1 + Z >\r\n" + 
				"        [9] < 4 >\r\n";

		println(result);
		assertEquals(expected, result);

		println();
	}
	
	
	
	
	
	
	
	
	
	
	private String recursivelyTakeSteps(ExpressionStepSolver stepSolver, Context context, AtomicInteger stepCount, int nestingOfStep) {
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
			
			result += recursivelyTakeSteps(step.getStepSolverForWhenSplitterIsTrue(), trueContext, stepCount, nestingOfNextStep);
			result += recursivelyTakeSteps(step.getStepSolverForWhenSplitterIsFalse(), falseContext, stepCount, nestingOfNextStep);
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
		for (int i = 0; i < nesting; ++i){
		   outputBuffer.append("    ");
		}
		//outputBuffer.append(" ");
		return outputBuffer.toString();
	}

}
