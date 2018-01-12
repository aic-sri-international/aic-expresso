package com.sri.ai.test.grinder.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.set.invsupport.SetIntersectExtensionalSetEqualToEmptySetSimplifier;

public class SetIntersectExtensionalSetEqualToEmptySetSimplifierTest {
	private SetIntersectExtensionalSetEqualToEmptySetSimplifier simplifier = new SetIntersectExtensionalSetEqualToEmptySetSimplifier();

private Context context = new TrueContext();
	
	@Test
	public void test1() {
		Assert.assertEquals(
			parse("false"), 
			simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection {}) = {}"), context));
	}

	@Test
	public void test2() {
		Assert.assertEquals(
				parse("not(A in Union({{(on I in 1..10) {I} }})) and ((Union({{(on I in 1..10) {I} }}) intersection {B, C}) = {})"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection {A, B, C}) = {}"), context));
	}	
	
	@Test
	public void test3() {
		Assert.assertEquals(
				parse("not(A in Union({{(on I in 1..10) {I} }}))"), 
				simplifier.apply(parse("(Union({{(on I in 1..10) {I} }}) intersection {A}) = {}"), context));
	}
}
