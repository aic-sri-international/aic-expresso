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
	
	/**
	 * Pairs of input-output string tests.
	 * This has been made public so other tests can just re-use them.
	 */
	public static final String[] tests = new String[]{

		"max({{ (on I in 0..3) max( {{ (on J in 0..3) I + J : I != J }} ) }})",
		"5",
		
		"3 * (Two + 5 - 3)*(-10)",
		"-120",
		
		"3 * (2 + 5 - 3)*(-10)",
		"-120",
		
		"sum({{ (on x in 0..2)  x }} )",
		"3",
		
		"sum({{ (on f in 0..2 -> Boolean)  if f(0) then 2 else 3 }} )",
		"20",
		
		"sum({{ (on f in 0..2 -> Boolean)  if f(0) and f(1) then 2 else 3  :  f(2) }} )",
		"11",
		
		"sum({{ (on f in 0..2 -> Boolean)  "
				+ "if f(0) and f(1) then 2 else | f in 0..2 x 0..2 -> Boolean : f(0, 0) |  "
				+ ":  f(2) }} )",
		"770",
		
		"sum({{ (on f in '->'(x(1..2), Boolean), g in '->'(x(1..2), Boolean))  if f(1) and g(2) then 2 else 3  :  f(2) }} )",
		"22",
		
		"| f in '->'(x(0..2, 0..2), Boolean) : f(0, 0) |",
		"256",
		
		"| f in 0..2 x 0..2 -> Boolean : f(0, 0) |",
		"256",
		
		"| f in 0..2 x 0..2 -> Boolean, g in 0..2 -> Boolean : f(0, 0) |",
		"2048",
		
		"sum( {{ (on p in Boolean) if p then 1 else 2 }} )",
		"3",
	};
	
	/**
	 * Makes context with initializations necessary for these tests. 
	 * @return
	 */
	public Context makeContext() {
		LinkedHashMap<Expression, Expression> assignment = map(parse("Two"), Expressions.TWO);
		Context context = new TrueContext();
		context = AbstractIterativeMultiIndexQuantifierEliminator.extendAssignments(assignment, context);
		return context;
	}

	@Test
	public void test() {
		Context context = makeContext();
		
		for(int i = 0; i != tests.length; i += 2) {
			String expression = tests[i];
			String expected = tests[i + 1];
			runTest(expression, expected, context);
		}
	}

	public void runTest(String expression, String expected, Context context) {
		BruteForceCommonInterpreter interpreter = new BruteForceCommonInterpreter();
		Expression result = interpreter.apply(parse(expression), context);
		Assert.assertEquals(parse(expected), result);
	}
}
