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
	}
	
	@Test
	public void testBasicExamples() {
		Assert.assertEquals(
				Expressions.parse("x = (3*z + -2)/(-1*y+1)"), 
				IsolateUtil.isolate(Expressions.parse("(20 - x) = (-2*x + y*x + 3*z + 18)"), Expressions.parse("x")));

		Assert.assertEquals(
				Expressions.parse("age(bob) <= age(beth)"), 
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
				IsolateUtil.isolate(Expressions.parse("xy = xy"), Expressions.parse("x")));
		
		Assert.assertEquals(
				Expressions.parse("0 = z"), 
				IsolateUtil.isolate(Expressions.parse("xy = xy + z"), Expressions.parse("x")));
	}
}
