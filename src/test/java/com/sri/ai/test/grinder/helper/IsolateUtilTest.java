package com.sri.ai.test.grinder.helper;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.helper.IsolateUtil;

public class IsolateUtilTest {

	@Test
	public void testTrivialCases() {
		Assert.assertEquals(
			Expressions.parse("x = 2"), 
			IsolateUtil.isolate(Expressions.parse("x = 2"), Expressions.parse("x")));
	
		Assert.assertEquals(
				Expressions.parse("x = 2"), 
				IsolateUtil.isolate(Expressions.parse("-2 = -x"), Expressions.parse("x")));
				
		Assert.assertEquals(
				Expressions.parse("x = 2"), 
				IsolateUtil.isolate(Expressions.parse("2 = x"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("x < 5"), 
				IsolateUtil.isolate(Expressions.parse("3*x < 15"), Expressions.parse("x")));
	}
	
	@Test
	public void testConditionalDivisionByZero() {
		Assert.assertEquals(
				Expressions.parse("if y != 0 then x = 10/y else 0 = 10"), 
				IsolateUtil.isolate(Expressions.parse("y*x = 10"), Expressions.parse("x")));
	}
	
	@Test
	public void testBasicExamples() {
		Assert.assertEquals(
				Expressions.parse("if (-1*y+1) != 0 then x = (3*z + -2)/(-1*y+1) else 0 = (3*z + -2)"), 
				IsolateUtil.isolate(Expressions.parse("(20 - x) = (-2*x + y*x + 3*z + 18)"), Expressions.parse("x")));

		Assert.assertEquals(
				Expressions.parse("age(bob) >= age(beth)"), 
				IsolateUtil.isolate(Expressions.parse("age(bob) <= 2*age(bob) - age(beth)"), Expressions.parse("age(bob)")));
	}
	
	@Test
	public void testVanishes() {
		Assert.assertEquals(
				Expressions.parse("0 = 0"), 
				IsolateUtil.isolate(Expressions.parse("0 = 0"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("0 = y"), 
				IsolateUtil.isolate(Expressions.parse("0 = y"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("0 = 0"), 
				IsolateUtil.isolate(Expressions.parse("x = x"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("0 = 0"), 
				IsolateUtil.isolate(Expressions.parse("0*x = 0"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("0 = 1"), 
				IsolateUtil.isolate(Expressions.parse("0*x = 1"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("0 = 0"), 
				IsolateUtil.isolate(Expressions.parse("xy = xy"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("0 = z"), 
				IsolateUtil.isolate(Expressions.parse("xy = xy + z"), Expressions.parse("x")));
	}
	
	@Test
	public void testInequalityFlipped() {
		Assert.assertEquals(
				Expressions.parse("x < -2"), 
				IsolateUtil.isolate(Expressions.parse("-2*x > 4"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("x <= -2"), 
				IsolateUtil.isolate(Expressions.parse("-2*x >= 4"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("x > -2"), 
				IsolateUtil.isolate(Expressions.parse("-2*x < 4"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("x >= -2"), 
				IsolateUtil.isolate(Expressions.parse("-2*x <= 4"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("x > 2*y + 7"), 
				IsolateUtil.isolate(Expressions.parse("2*y + 7 < x"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("x > 7"), 
				IsolateUtil.isolate(Expressions.parse("12 < x + 5"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("x > 4"), 
				IsolateUtil.isolate(Expressions.parse("-2*x < -8"), Expressions.parse("x")));
	}
	
	@Test
	public void testConditionalInequalityFlipped() {
		Assert.assertEquals(
				Expressions.parse("if b != 0 then if b > 0 then x < 3 else x > 3 else 0 = 3*b"), 
				IsolateUtil.isolate(Expressions.parse("b*x < 3*b"), Expressions.parse("x")));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testIllegalIsolatedVariableArgument1() {
		IsolateUtil.isolate(Expressions.parse("x^2 = 4"), Expressions.parse("x"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testIllegalIsolatedVariableArgument2() {
		IsolateUtil.isolate(Expressions.parse("4 = x^2"), Expressions.parse("x"));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void testIllegalIsolatedVariableArgument3() {
		IsolateUtil.isolate(Expressions.parse("x + x + x^2 = 8"), Expressions.parse("x"));
	}
}
