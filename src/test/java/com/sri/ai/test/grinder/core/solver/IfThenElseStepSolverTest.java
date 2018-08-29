package com.sri.ai.test.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;

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
import com.sri.ai.grinder.core.solver.IfThenElseStepSolver;

public class IfThenElseStepSolverTest {
	
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
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
		
		recursivelyTakeSteps(stepSolver, context, stepCount, nestingOfStep);
		
		println();
	}
	
	
	
	
	
	
	
	
	private void recursivelyTakeSteps(ExpressionStepSolver stepSolver, Context context, AtomicInteger stepCount, int nestingOfStep) {
		Step step = stepSolver.step(context);
		stepCount.incrementAndGet();
		
		if(step.itDepends()) {
			ContextSplitting splitting = step.getContextSplittingWhenSplitterIsLiteral();
			
			String nestingString = createNestingString(nestingOfStep);
			println(nestingString + "[" + stepCount + "] split on: " + splitting.getLiteral());
			
			Context trueContext = splitting.getContextAndLiteral();
			Context falseContext = splitting.getContextAndLiteralNegation();
			int nestingOfNextStep = nestingOfStep + 1;
			
			recursivelyTakeSteps(step.getStepSolverForWhenSplitterIsTrue(), trueContext, stepCount, nestingOfNextStep);
			recursivelyTakeSteps(step.getStepSolverForWhenSplitterIsFalse(), falseContext, stepCount, nestingOfNextStep);
		}
		else {
			String nestingString = createNestingString(nestingOfStep);
			println(nestingString + "[" + stepCount + "] < " + step.getValue() + " >");
		}
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
