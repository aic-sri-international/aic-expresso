package com.sri.ai.test.grinder.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.set.invsupport.ExtensionalSetEqualEmptySetSimplifier;

public class ExtensionalSetEqualEmptySetSimplifierTest {
	private ExtensionalSetEqualEmptySetSimplifier simplifier = new ExtensionalSetEqualEmptySetSimplifier();
	private Context context = new TrueContext();
	
	@Test
	public void testTrue() {
		Assert.assertEquals(
				parse("true"), 
				simplifier.apply(parse("{} = {}"), context));
		
		Assert.assertEquals(
				parse("true"), 
				simplifier.apply(parse("{{}} = {}"), context));
	}
	
	@Test
	public void testFalse() {
		Assert.assertEquals(
				parse("false"), 
				simplifier.apply(parse("{1} = {}"), context));
		
		Assert.assertEquals(
				parse("false"), 
				simplifier.apply(parse("{{1}} = {}"), context));
		
		Assert.assertEquals(
				parse("false"), 
				simplifier.apply(parse("{1, 2} = {}"), context));
		
		Assert.assertEquals(
				parse("false"), 
				simplifier.apply(parse("{{1, 2}} = {}"), context));
	}
}
