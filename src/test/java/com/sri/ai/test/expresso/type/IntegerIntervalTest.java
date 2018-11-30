package com.sri.ai.test.expresso.type;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.type.IntegerInterval;

public class IntegerIntervalTest {

	@Test
	public void testContains() {
		IntegerInterval intInterval = new IntegerInterval("Integer");
		Assert.assertTrue(intInterval.contains(parse("0")));
		
		intInterval = new IntegerInterval("0..4");
		Assert.assertFalse(intInterval.contains(parse("-1")));
		Assert.assertTrue(intInterval.contains(parse("0")));
		Assert.assertTrue(intInterval.contains(parse("1")));
		Assert.assertTrue(intInterval.contains(parse("2")));
		Assert.assertTrue(intInterval.contains(parse("3")));
		Assert.assertTrue(intInterval.contains(parse("4")));
		Assert.assertFalse(intInterval.contains(parse("5")));
		
		Assert.assertFalse(intInterval.contains(parse("2.5")));
		
		intInterval = new IntegerInterval("-infinity..4");
		Assert.assertFalse(intInterval.contains(parse("-infinity")));
		Assert.assertTrue(intInterval.contains(parse("-100")));
		Assert.assertTrue(intInterval.contains(parse("4")));
		Assert.assertFalse(intInterval.contains(parse("5")));
		
		intInterval = new IntegerInterval("0..infinity");
		Assert.assertFalse(intInterval.contains(parse("infinity")));
		Assert.assertFalse(intInterval.contains(parse("-1")));
		Assert.assertTrue(intInterval.contains(parse("0")));
		Assert.assertTrue(intInterval.contains(parse("4")));
		Assert.assertTrue(intInterval.contains(parse("5")));
		Assert.assertTrue(intInterval.contains(parse("500")));
	}
	
	@Test
	public void testIsSuperset() {
		IntegerInterval intInterval = new IntegerInterval("Integer");
		Assert.assertTrue(intInterval.isSuperset(parse("-infinity"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("-infinity"), parse("-infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("infinity"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("-infinity"), parse("3")));
		Assert.assertTrue(intInterval.isSuperset(parse("-infinity"), parse("4")));
		Assert.assertTrue(intInterval.isSuperset(parse("-infinity"), parse("5")));
		Assert.assertTrue(intInterval.isSuperset(parse("-1"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("0"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("1"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("0"), parse("4")));
		Assert.assertFalse(intInterval.isSuperset(parse("0.5"), parse("4")));
		Assert.assertFalse(intInterval.isSuperset(parse("0"), parse("3.5")));
		
		intInterval = new IntegerInterval("-infinity..4");
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("-infinity"), parse("-infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("infinity"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("-infinity"), parse("3")));
		Assert.assertTrue(intInterval.isSuperset(parse("-infinity"), parse("4")));
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("5")));
		Assert.assertFalse(intInterval.isSuperset(parse("-1"), parse("infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("0"), parse("infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("1"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("0"), parse("4")));
		Assert.assertTrue(intInterval.isSuperset(parse("1"), parse("3")));
		Assert.assertTrue(intInterval.isSuperset(parse("-1"), parse("3")));
		Assert.assertFalse(intInterval.isSuperset(parse("0"), parse("5")));
		Assert.assertFalse(intInterval.isSuperset(parse("0.5"), parse("4")));
		Assert.assertFalse(intInterval.isSuperset(parse("0"), parse("3.5")));
		
		intInterval = new IntegerInterval("0..infinity");
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("-infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("infinity"), parse("infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("3")));
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("4")));
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("5")));
		Assert.assertFalse(intInterval.isSuperset(parse("-1"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("0"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("1"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("0"), parse("4")));
		Assert.assertTrue(intInterval.isSuperset(parse("1"), parse("3")));
		Assert.assertFalse(intInterval.isSuperset(parse("-1"), parse("3")));
		Assert.assertTrue(intInterval.isSuperset(parse("0"), parse("5")));
		Assert.assertFalse(intInterval.isSuperset(parse("0.5"), parse("4")));
		Assert.assertFalse(intInterval.isSuperset(parse("0"), parse("3.5")));
		
		intInterval = new IntegerInterval("0..4");
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("-infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("infinity"), parse("infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("3")));
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("4")));
		Assert.assertFalse(intInterval.isSuperset(parse("-infinity"), parse("5")));
		Assert.assertFalse(intInterval.isSuperset(parse("-1"), parse("infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("0"), parse("infinity")));
		Assert.assertFalse(intInterval.isSuperset(parse("1"), parse("infinity")));
		Assert.assertTrue(intInterval.isSuperset(parse("0"), parse("4")));
		Assert.assertTrue(intInterval.isSuperset(parse("1"), parse("3")));
		Assert.assertFalse(intInterval.isSuperset(parse("-1"), parse("3")));
		Assert.assertFalse(intInterval.isSuperset(parse("0"), parse("5")));
		Assert.assertFalse(intInterval.isSuperset(parse("0.5"), parse("4")));
		Assert.assertFalse(intInterval.isSuperset(parse("0"), parse("3.5")));
	}
}