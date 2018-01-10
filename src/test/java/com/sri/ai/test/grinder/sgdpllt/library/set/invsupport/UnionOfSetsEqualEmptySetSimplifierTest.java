package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.set.invsupport.UnionOfSetsEqualEmptySetSimplifier;

public class UnionOfSetsEqualEmptySetSimplifierTest {
	private UnionOfSetsEqualEmptySetSimplifier simplifier = new UnionOfSetsEqualEmptySetSimplifier();
	
private Context context = new TrueContext();
	
	@Test
	public void test1() {
		Assert.assertEquals(
				parse("{A} = {}"), 
				simplifier.apply(parse("union({A}) = {}"), context));
	}	

	@Test
	public void test2() {
		Assert.assertEquals(
				parse("{A} = {} and {B} = {}"), 
				simplifier.apply(parse("{A} union {B} = {}"), context));
	}	
	
	@Test
	public void test3() {
		Assert.assertEquals(
				parse("{A} = {} and {B} = {} and {C} = {}"), 
				simplifier.apply(parse("{A} union {B} union {C} = {}"), context));
	}	
}
