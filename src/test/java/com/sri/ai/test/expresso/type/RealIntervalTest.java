package com.sri.ai.test.expresso.type;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.type.RealInterval;

public class RealIntervalTest {
	@Test
	public void testContains() {
		RealInterval realInterval = new RealInterval("Real");
		Assert.assertTrue(realInterval.contains(parse("0")));
		
		realInterval = new RealInterval("[0;4]");		
		Assert.assertFalse(realInterval.contains(parse("-1")));
		Assert.assertTrue(realInterval.contains(parse("0")));
		Assert.assertTrue(realInterval.contains(parse("1")));
		Assert.assertTrue(realInterval.contains(parse("2")));
		Assert.assertTrue(realInterval.contains(parse("3")));
		Assert.assertTrue(realInterval.contains(parse("4")));
		Assert.assertFalse(realInterval.contains(parse("5")));
		Assert.assertTrue(realInterval.contains(parse("2.5")));
		
		realInterval = new RealInterval("]0;4[");		
		Assert.assertFalse(realInterval.contains(parse("-1")));
		Assert.assertFalse(realInterval.contains(parse("0")));
		Assert.assertTrue(realInterval.contains(parse("1")));
		Assert.assertTrue(realInterval.contains(parse("2")));
		Assert.assertTrue(realInterval.contains(parse("3")));
		Assert.assertFalse(realInterval.contains(parse("4")));
		Assert.assertFalse(realInterval.contains(parse("5")));
		Assert.assertTrue(realInterval.contains(parse("2.5")));
		
		realInterval = new RealInterval("[-infinity;4]");
		Assert.assertFalse(realInterval.contains(parse("-infinity")));
		Assert.assertTrue(realInterval.contains(parse("-100")));
		Assert.assertTrue(realInterval.contains(parse("4")));
		Assert.assertFalse(realInterval.contains(parse("5")));
		
		realInterval = new RealInterval("[0;infinity]");
		Assert.assertFalse(realInterval.contains(parse("infinity")));
		Assert.assertFalse(realInterval.contains(parse("-1")));
		Assert.assertTrue(realInterval.contains(parse("0")));
		Assert.assertTrue(realInterval.contains(parse("4")));
		Assert.assertTrue(realInterval.contains(parse("5")));
		Assert.assertTrue(realInterval.contains(parse("500")));
	}
}
