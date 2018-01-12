package com.sri.ai.test.grinder.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.set.invsupport.IntensionalUnionEqualToEmptySetSimplifier;

public class IntensionalUnionEqualToEmptySetSimplifierTest {
	private IntensionalUnionEqualToEmptySetSimplifier simplifier = new IntensionalUnionEqualToEmptySetSimplifier();
	private Context context = new TrueContext();
	
	@Test
	public void testIntensionalUnionEqualToEmptySet() {
		Assert.assertEquals(
				parse("for all I in 1..10 : {(I, 1)} = {}"), 
				simplifier.apply(parse("Union({{(on I in 1..10) {(I, 1)} }}) = {}"), context));
		
		Assert.assertEquals(
				parse("for all I in 1..10 : (not(I > 4) or {(I, 1)}) = {}"), 
				simplifier.apply(parse("Union({{(on I in 1..10) {(I, 1)} : I > 4 }}) = {}"), context));
	}	
}
