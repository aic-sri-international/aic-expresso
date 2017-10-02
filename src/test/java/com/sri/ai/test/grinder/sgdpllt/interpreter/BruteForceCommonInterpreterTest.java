package com.sri.ai.test.grinder.sgdpllt.interpreter;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;

import java.util.LinkedHashMap;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.interpreter.AbstractIterativeMultiIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceCommonInterpreter;

public class BruteForceCommonInterpreterTest {
	@Test
	public void test() {
		LinkedHashMap<Expression, Expression> assignment = map(parse("Two"), Expressions.TWO);
		BruteForceCommonInterpreter interpreter = new BruteForceCommonInterpreter();
		Context context = new TrueContext();
		context = AbstractIterativeMultiIndexQuantifierEliminator.extendAssignments(assignment, context);
		
		String expression;
		String expected;
		
		expression = "max({{ (on I in 0..3) max( {{ (on J in 0..3) I + J : I != J }} ) }})";
		expected = "5";
		runTest(expression, expected, interpreter, context);
		
		expression = "3 * (Two + 5 - 3)*(-10)";
		expected = "-120";
		runTest(expression, expected, interpreter, context);
		
		expression = "3 * (2 + 5 - 3)*(-10)";
		expected = "-120";
		runTest(expression, expected, interpreter, context);
		
		expression = "sum({{ (on x in 0..2)  x }} )";
		expected = "3";
		runTest(expression, expected, interpreter, context);
		
		expression = "sum({{ (on f in 0..2 -> Boolean)  if f(0) then 2 else 3 }} )";
		expected = "20";
		runTest(expression, expected, interpreter, context);
		
		expression = "sum({{ (on f in 0..2 -> Boolean)  if f(0) and f(1) then 2 else 3  :  f(2) }} )";
		expected = "11";
		runTest(expression, expected, interpreter, context);
		
		expression = "sum({{ (on f in 0..2 -> Boolean)  "
				+ "if f(0) and f(1) then 2 else | f in 0..2 x 0..2 -> Boolean : f(0, 0) |  "
				+ ":  f(2) }} )";
		expected = "770";
		runTest(expression, expected, interpreter, context);
		
		expression = "sum({{ (on f in '->'(x(1..2), Boolean), g in '->'(x(1..2), Boolean))  if f(1) and g(2) then 2 else 3  :  f(2) }} )";
		expected = "22";
		runTest(expression, expected, interpreter, context);
		
		expression = "| f in '->'(x(0..2, 0..2), Boolean) : f(0, 0) |";
		expected = "256";
		runTest(expression, expected, interpreter, context);
		
		expression = "| f in 0..2 x 0..2 -> Boolean : f(0, 0) |";
		expected = "256";
		runTest(expression, expected, interpreter, context);
		
		expression = "| f in 0..2 x 0..2 -> Boolean, g in 0..2 -> Boolean : f(0, 0) |";
		expected = "2048";
		runTest(expression, expected, interpreter, context);
		
		expression = "sum( {{ (on p in Boolean) if p then 1 else 2 }} )";
		expected = "3";
		runTest(expression, expected, interpreter, context);
	}

	public void runTest(String expression, String expected, BruteForceCommonInterpreter interpreter, Context context) {
		Expression result = interpreter.apply(parse(expression), context);
		Assert.assertEquals(parse(expected), result);
	}
}
