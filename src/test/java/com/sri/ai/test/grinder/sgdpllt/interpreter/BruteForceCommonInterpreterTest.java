package com.sri.ai.test.grinder.sgdpllt.interpreter;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceCommonInterpreter;

public class BruteForceCommonInterpreterTest {
	@Test
	public void testAggregationOnFunctionIndex() {
		BruteForceCommonInterpreter bf = new BruteForceCommonInterpreter();
		Context context = new TrueContext();
		
		Expression result = bf.apply(parse("sum({{ (on f in '->'(x(0..2), Boolean))  if f(0) and f(1) then 2 else 3  :  f(2) }} )"), context);
		Assert.assertEquals(parse("11"), result);
		
		result = bf.apply(parse("sum({{ (on f in '->'(x(1..2), Boolean), g in '->'(x(1..2), Boolean))  if f(1) and g(2) then 2 else 3  :  f(2) }} )"), context);
		Assert.assertEquals(parse("22"), result);
		
		result = bf.apply(parse("| f in '->'(x(0..2, 0..2), Boolean) : f(0, 0) |"), context);
		Assert.assertEquals(parse("256"), result);
		
		result = bf.apply(parse("| f in 0..2 x 0..2 -> Boolean : f(0, 0) |"), context);
		Assert.assertEquals(parse("256"), result);
	}
}
