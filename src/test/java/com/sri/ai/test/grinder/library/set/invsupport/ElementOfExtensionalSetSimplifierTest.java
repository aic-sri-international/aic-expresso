package com.sri.ai.test.grinder.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.set.invsupport.ElementOfExtensionalSetSimplifier;

public class ElementOfExtensionalSetSimplifierTest {
	private ElementOfExtensionalSetSimplifier simplifier = new ElementOfExtensionalSetSimplifier();
	private Context context = new TrueContext();
	
	@Test
	public void testElementInEmptySet() {
		Assert.assertEquals(
				parse("false"), 
				simplifier.apply(parse("A in {}"), context));
		
		Assert.assertEquals(
				parse("false"), 
				simplifier.apply(parse("A in {{}}"), context));
	}
	
	@Test
	public void testElementInSingletonSet() {
		Assert.assertEquals(
				parse("A = B"), 
				simplifier.apply(parse("A in {B}"), context));
		
		Assert.assertEquals(
				parse("A = B"), 
				simplifier.apply(parse("A in {{B}}"), context));
		
		Assert.assertEquals(
				parse("true"), 
				simplifier.apply(parse("A in {A}"), context));
		
		Assert.assertEquals(
				parse("true"), 
				simplifier.apply(parse("A in {{A}}"), context));
	}
	
	@Test
	public void testElementInCardinalityGreaterThanOneSet() {
		Assert.assertEquals(
				parse("A = B or A in {C}"), 
				simplifier.apply(parse("A in {B, C}"), context));
		
		Assert.assertEquals(
				parse("A = B or A in {{C}}"), 
				simplifier.apply(parse("A in {{B, C}}"), context));
		
		Assert.assertEquals(
				parse("A = B or A in {C, D}"), 
				simplifier.apply(parse("A in {B, C, D}"), context));
		
		Assert.assertEquals(
				parse("A = B or A in {{C, D}}"), 
				simplifier.apply(parse("A in {{B, C, D}}"), context));
		
		Assert.assertEquals(
				parse("true"), 
				simplifier.apply(parse("A in {A, B, C}"), context));
		
		Assert.assertEquals(
				parse("true"), 
				simplifier.apply(parse("A in {{A, B, C}}"), context));
	}
}
