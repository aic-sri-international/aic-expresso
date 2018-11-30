package com.sri.ai.test.grinder.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.set.invsupport.IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeSimplifier;

public class IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeSimplifierTest {

	private IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeSimplifier simplifier = new IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeSimplifier();
	private Context context = new TrueContext();
	
	@Test
	public void testBaseCase() {
		Expression baseCase = parse("Union({{(on I in 1..10) {(I, 2)} : I != 5}})");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5}}"), 
				simplifier.apply(baseCase, context));
	}
	
	@Test
	public void testRecursionCase() {
		Expression recursionCase = parse("Union({{(on I in 1..10) {(I, 2)} union {(I, 3)} union {(I, 4)} : I != 5}})");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5}} union {{(on I in 1..10) (I, 3) : I != 5}} union {{(on I in 1..10) (I, 4) : I != 5}}"), 
				simplifier.apply(recursionCase, context));
	}
}
