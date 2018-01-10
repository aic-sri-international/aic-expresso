package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.set.invsupport.ElementOfIntensionalUnionSimplifier;

public class ElementOfIntensionalUnionSimplifierTest {
	private ElementOfIntensionalUnionSimplifier simplifier = new ElementOfIntensionalUnionSimplifier();
	private Context context = new TrueContext();
	
	@Test
	public void testElementOfIntensionalUnion() {
		Assert.assertEquals(
				parse("there exists I in 1..10 : A in {I}"), 
				simplifier.apply(parse("A in Union({{(on I in 1..10) {I} }})"), context));
		
		Assert.assertEquals(
				parse("there exists I in 1..10 : I > 4 and A in {I}"), 
				simplifier.apply(parse("A in Union({{(on I in 1..10) {I} : I > 4}})"), context));
	}	
}
