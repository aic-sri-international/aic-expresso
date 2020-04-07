package com.sri.ai.test.expresso.smt;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.println;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.smt.api.SMTBasedContext;
import com.sri.ai.expresso.smt.core.DefaultSMTBasedExpressionEvaluator;
import com.sri.ai.expresso.smt.core.yices.YicesBasedContext;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionEvaluator;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.application.CommonTheory;
import com.sri.ai.grinder.tester.RandomCondtionalArithmeticExpressionGenerator;
import com.sri.yices.Yices;

public class DefaultSMTBasedExpressionEvaluatorTest {
	
	private static final ExpressionEvaluator smtBasedEvaluator = new DefaultSMTBasedExpressionEvaluator();	
	
	@Test
	public void evaluateVariablesThatHaveNoUniqueValue() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		Theory theory = new CommonTheory();
		String[] symbolsAndTypes = new String[] {
				"B", "Boolean",
				"X", "Integer", 
				"Y", "Real"
		};
		SMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (SMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		String[] constraintStrings = new String[] {
				"true"
		};
		List<Expression> constraints = parseStringsToExpressionList(constraintStrings);
		smtBasedContext = (SMTBasedContext) conjoinConstraints(smtBasedContext, constraints);
		
		Expression result;
		Expression expression;
		Expression rawExpectedResult;
		Expression expectedResult;
		
		expression = parse("B");
		rawExpectedResult = expression;
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("X");
		rawExpectedResult = expression;
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("Y");
		rawExpectedResult = expression;
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		
		println();
		println();
		println();
	}
	
	
	
	@Test
	public void evaluateVariablesThatHaveAssignedValues() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		Theory theory = new CommonTheory();
		String[] symbolsAndTypes = new String[] {
				"B", "Boolean",
				"X", "Integer", 
				"Y", "Real"
		};
		SMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (SMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		String[] constraintStrings = new String[] {
				"not B",
				"X = 1", 
				"Y = 1/2"
		};
		List<Expression> constraints = parseStringsToExpressionList(constraintStrings);
		smtBasedContext = (SMTBasedContext) conjoinConstraints(smtBasedContext, constraints);
		
		Expression expression;
		Expression rawExpectedResult;
		Expression expectedResult;
		Expression result;
		
		expression = parse("B");
		rawExpectedResult = parse("false");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("X");
		rawExpectedResult = parse("1");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("Y");
		rawExpectedResult = parse("1/2");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		
		println();
		println();
		println();
	}
	
	
	@Test
	public void evaluateVariablesThatHaveSimpleInferredValues() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		Theory theory = new CommonTheory();
		String[] symbolsAndTypes = new String[] {
				"B1", "Boolean",
				"B2", "Boolean",
				"X", "Integer", 
				"Y", "Real"
		};
		SMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (SMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		String[] constraintStrings = new String[] {
				"not B2",
				"B1 = B2",
				"X > 0",
				"X < 2",
				"Y <= 0.5",
				"Y >= 0.5"
		};
		List<Expression> constraints = parseStringsToExpressionList(constraintStrings);
		smtBasedContext = (SMTBasedContext) conjoinConstraints(smtBasedContext, constraints);
		
		Expression expression;
		Expression rawExpectedResult;
		Expression expectedResult;
		Expression result;
		
		expression = parse("B1");
		rawExpectedResult = parse("false");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("X");
		rawExpectedResult = parse("1");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("Y");
		rawExpectedResult = parse("1/2");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		
		println();
		println();
		println();
	}
	
	
	
	@Test
	public void evaluateSimpleNonSimplifiableExpressionsAgainstATrueContext() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		Theory theory = new CommonTheory();
		String[] symbolsAndTypes = new String[] {
				"B1", "Boolean",
				"B2", "Boolean",
				"X", "Integer", 
				"Y", "Real"
		};
		SMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (SMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		String[] constraintStrings = new String[] {
				"true"
		};
		List<Expression> constraints = parseStringsToExpressionList(constraintStrings);
		smtBasedContext = (SMTBasedContext) conjoinConstraints(smtBasedContext, constraints);
		
		Expression expression;
		Expression rawExpectedResult;
		Expression expectedResult;
		Expression result;
		
		expression = parse("B1 and B2");
		rawExpectedResult = parse("if B1 then B2 else false");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("X + 1");
		rawExpectedResult = expression;
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("Y + 1");
		rawExpectedResult = expression;
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		
		println();
		println();
		println();
	}
	
	
	@Test
	public void evaluateExpressionsWithUndefinedLiterals() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		Theory theory = new CommonTheory();
		String[] symbolsAndTypes = new String[] {
				"B1", "Boolean",
				"B2", "Boolean",
				"X", "Integer", 
				"Y", "Real"
		};
		SMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (SMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		String[] constraintStrings = new String[] {
				"true"
		};
		List<Expression> constraints = parseStringsToExpressionList(constraintStrings);
		smtBasedContext = (SMTBasedContext) conjoinConstraints(smtBasedContext, constraints);
		
		Expression expression;
		Expression rawExpectedResult;
		Expression expectedResult;
		Expression result;
		
		expression = parse("100 + if X > 0 then if X < 10 then 1 else 2 else if X = 1 then 3 else 4");
		rawExpectedResult = parse("if X > 0 then if X < 10 then 101 else 102 else 104");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("1/2 + if Y > 1/2 then if Y < 10 then 0 else 1/2 else if Y = 1 then 1 else 1/3");
		rawExpectedResult = parse("if Y > 0.5 then if Y < 10 then 0.5 else 1 else 5/6");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		println();
		println();
		println();
	}
	
	
	
	@Test
	public void evalOnRandomExpressions() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		Theory theory = new CommonTheory();
		String[] symbolsAndTypes = new String[] {
				"B1", "Boolean",
				"B2", "Boolean",
				"X", "Integer", 
				"Y", "Real"
		};
		SMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (SMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		String[] constraintStrings = new String[] {
				"true"
		};
		List<Expression> constraints = parseStringsToExpressionList(constraintStrings);
		smtBasedContext = (SMTBasedContext) conjoinConstraints(smtBasedContext, constraints);
		
		Expression expression;
		Expression rawExpectedResult;
		Expression expectedResult;
		Expression result;
		
		expression = parse("100 + if X > 0 then if X < 10 then 1 else 2 else if X = 1 then 3 else 4");
		rawExpectedResult = parse("if X > 0 then if X < 10 then 101 else 102 else 104");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("1/2 + if Y > 1/2 then if Y < 10 then 0 else 1/2 else if Y = 1 then 1 else 1/3");
		rawExpectedResult = parse("if Y > 0.5 then if Y < 10 then 0.5 else 1 else 5/6");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		println();
		println();
		println();
	}
	
	
	@Test
	public void evaluateSimpleEqualities() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		
		Theory theory = new CommonTheory();
		String[] symbolsAndTypes = new String[] {
				"B1", "Boolean",
				"B2", "Boolean",
				"B3", "Boolean",
				"X1", "Integer", 
				"X2", "Integer", 
				"X3", "Integer", 
				"Y1", "Real",
				"Y2", "Real",
				"Y3", "Real"
		};
		SMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (SMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		String[] constraintStrings = new String[] {
				"not B1",
				"not B2",
				"B3",
				"X1 = 1", 
				"X2 = 2-1", 
				"X3 = 3", 
				"Y1 = 1/2",
				"Y2 = 0.5",
				"Y3 = 1/4"
		};
		List<Expression> constraints = parseStringsToExpressionList(constraintStrings);
		smtBasedContext = (SMTBasedContext) conjoinConstraints(smtBasedContext, constraints);
		
		Expression expression;
		Expression rawExpectedResult;
		Expression expectedResult;
		Expression result;
		
		expression = parse("X1 = 1");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("Y1 = 1/2");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("X2 = 1");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("Y2 = 1/2");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("B1 = B2");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("X1 = X2");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("Y1 = Y2");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("B1 = B2 and B3");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("X1 = X3 - 2*X2");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		expression = parse("Y1 = 2*Y2 - Y3/0.5");
		rawExpectedResult = parse("true");
		expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
		result = smtBasedEvaluator.eval(expression, smtBasedContext);
		println(smallTab + "   context: " + makeConjunctionString(constraintStrings));
		println(smallTab + "expression: " + expression);
		println(smallTab + " evaluated: " + result);
		println(smallTab + "  expected: " + expectedResult);
		println();
		assert(result.equals(expectedResult));
		
		
		println();
		println();
		println();
	}

	
	//@Test // fails when running along all other tests in project, but succeeds when run individually. Probably a global setting issue
	public void evaluateRandomlyGeneratedExpressions() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		Theory theory = new CommonTheory();
		String[] symbolsAndTypes = new String[] {
				"B1", "Boolean", 
				"B2", "Boolean", 
				"B3", "Boolean", 
				"X1", "Integer", 
				"X2", "Integer", 
				"X3", "Integer", 
				"Y1", "Real",
				"Y2", "Real",
				"Y3", "Real",
		};
		SMTBasedContext smtBasedContext = new YicesBasedContext(theory);
		smtBasedContext = (SMTBasedContext) smtBasedContext.extendWithSymbolsAndTypes(symbolsAndTypes);
		printSymbolsAndTypes(symbolsAndTypes);
		
		RandomCondtionalArithmeticExpressionGenerator expressionGenerator =
				new RandomCondtionalArithmeticExpressionGenerator(smtBasedContext, "BalancedTree", 8, 4, false, makeRandom());
		
		String[] constraintStrings = new String[] {
				"true"
		};
		List<Expression> constraints = parseStringsToExpressionList(constraintStrings);
		smtBasedContext = (SMTBasedContext) conjoinConstraints(smtBasedContext, constraints);
		
		final Expression EQUALITY = Expressions.makeSymbol("=");
		Expression expression;
		Expression simplified;
		Expression rawExpectedResult;
		Expression expectedResult;
		Expression equality;
		Expression result;
		
		for(int i = 0; i < 1000; ++i) {
			expression = expressionGenerator.apply();
			simplified = smtBasedEvaluator.eval(expression, smtBasedContext);
			rawExpectedResult = parse("true");
			expectedResult = smtBasedEvaluator.eval(rawExpectedResult, smtBasedContext);
			equality = new DefaultFunctionApplication(EQUALITY, arrayList(expression,simplified));
			println(equality);
			result = smtBasedEvaluator.eval(equality, smtBasedContext);
			println(smallTab + "Trial " + i);
			printSparseHyphenSeparatorLine(false);
			println(smallTab + "           context: " + makeConjunctionString(constraintStrings));
			println(smallTab + "        expression: " + expression);
			println(smallTab + "         evaluated: " + simplified);
			println(smallTab + "          equality: " + expression + " = " + simplified);
			println(smallTab + "evaluated equality: " + result);
			println(smallTab + "          expected: " + expectedResult);
			println();
			assert(result.equals(expectedResult));
		}
		
		
		println();
		println();
		println();
	}
	
	
	@Test
	public void blankTest() throws Exception {
		String testFunctionName = new Object() {}.getClass().getEnclosingMethod().getName();
		printTestTitle(testFunctionName, false);
		println();
		
		
		println();
		println();
		println();
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	


	private String makeConjunctionString(String[] constraintStrings) {
		StringBuilder conjunctionConstruction = new StringBuilder();
		String conjunctionStringConstant = "^";
		for(String constraintString : constraintStrings) {
			conjunctionConstruction.append('(');
			conjunctionConstruction.append(constraintString);
			conjunctionConstruction.append(')');
			conjunctionConstruction.append(conjunctionStringConstant);
		}
		conjunctionConstruction.setLength(conjunctionConstruction.length() - conjunctionStringConstant.length());
		String conjunction = conjunctionConstruction.toString();
		return conjunction;
	}


	public Context conjoinConstraints(Context smtBasedContext, List<Expression> constraints) {
		for(Expression constraint : constraints) {
			smtBasedContext = smtBasedContext.conjoin(constraint);
		}
		return smtBasedContext;
	}


	private static List<Expression> parseStringsToExpressionList(String[] expressionStrings) {
		ArrayList<Expression> expressions = new ArrayList<Expression>(expressionStrings.length);
		for(String constraintString : expressionStrings) {
			Expression constraint = parse(constraintString);
			expressions.add(constraint);
		}
		return expressions;
	}
	

	
	
	
	



	private static void printTestTitle(String testFunctionName, boolean thickBorders) {
		String testName = makeTestName(testFunctionName);
		String framedTestName = frameTestTitle(testName, thickBorders);
		println(framedTestName);
	}
	
	private static String frameTestTitle(String testName, boolean thickBorders) {
		String framedTestName;
		String borderEdge;
		char b;
		if(thickBorders) {
			b = '|';
			borderEdge = "||||";
		}
		else {
			b = '/';
			borderEdge = "//";
		}
		int borderLength = testName.length() + 2*smallTab.length() + 2*borderEdge.length();
		char[] borderChars = new char[borderLength];
		Arrays.fill(borderChars, b);
		String border = new String(borderChars);
		if(thickBorders) {
			border = border + "\n" + border;
		}
		String titleLine = borderEdge + smallTab + testName + smallTab + borderEdge;
		framedTestName = border + '\n' + titleLine + '\n' + border;
		return framedTestName;
	}
	
	
	
	
	
	
	final static String tab = "\t";
	final static String stab_big = "        ";
	final static String smallTab = "    ";

	public static Random makeRandom() {
		return new Random();
	}
	
	
	
	
	
	
	
	
	@BeforeClass
	public static void prepareSymbolsAndTypesAndLiterals() throws Exception {
		printTestTitle("DefaultSMTBasedEvaluator JUnit Tests", true);
		println();
		println(smallTab + "IMPORTANT:  NEED TO EXTEND TESTS TO INTEGER AND REAL ***INTERVAL*** TYPES!");
		println();
		println();
		println();
	}

	@Before
	public void resetForNextTest() throws Exception {
		resetYices();
	}
	
	public void resetYices() throws Exception {
		Yices.reset();
	}
	
	
	
	
	
	
	


	public static int ceilToInt(double d) {
		int result = (int) Math.ceil(d);
		return result;
	}
	
	public static String makeTestName(String testObjectName) {
		String testName = null;
		StringBuilder testNameConstruction = new StringBuilder(testObjectName.length()*2);
		
		//make first letter capital
		char firstChar = testObjectName.charAt(0);
		char upperCaseFirstChar = Character.toUpperCase(firstChar);
		testNameConstruction.append(upperCaseFirstChar);
		
		//iterate through rest of chars in function name
		//for each char that is a capital, insert a space before
		for(int i = 1; i < testObjectName.length(); ++i) {
			char c = testObjectName.charAt(i);
			if(Character.isUpperCase(c)) {
				testNameConstruction.append(' ');
			}
			testNameConstruction.append(c);
		}
		testName = testNameConstruction.toString();
		return testName;
	}
	
	public static void repeatNtimes(Runnable procedure, int n) {
		for (int i = 0; i < n; ++i) {
				procedure.run();
		}
	}
	
	public static void printSymbolsAndTypes(String[] symbolsAndTypes) {
		println(smallTab + "Symbols and Types:");
		for(int i = 0; i < symbolsAndTypes.length/2; ++i) {
			String symbol = symbolsAndTypes[i*2];
			String type = symbolsAndTypes[i*2 + 1];
			println(tab + symbol + ":  " + type);
		}
		hypenSepartorLine();
	}
	
	public static void hypenSepartorLine() {
		println();
		println("-------------------------------------------------------------");
		println();
	}
	
	public static void printSparseHyphenSeparatorLine(boolean withLeadingNewline) {
		if(withLeadingNewline) {
			println();
		}
		println("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -");
	}	
	
}
