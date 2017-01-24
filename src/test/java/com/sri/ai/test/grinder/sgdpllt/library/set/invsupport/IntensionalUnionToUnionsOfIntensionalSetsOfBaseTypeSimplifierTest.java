package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeSimplifier;

import static com.sri.ai.expresso.helper.Expressions.parse;

public class IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeSimplifierTest {

	private IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeSimplifier simplifier = new IntensionalUnionToUnionsOfIntensionalSetsOfBaseTypeSimplifier();
	private Context context = new TrueContext();
	
	@Test
	public void testBaseCase() {
		Expression baseCase = parse("union({{(on I in 1..10) {(I, 2)} : I != 5}})");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5}}"), 
				simplifier.apply(baseCase, context));
	}
	
	@Test
	public void testRecursionCase() {
		Expression recursionCase = parse("union({{(on I in 1..10) {(I, 2)} union {(I, 3)} union {(I, 4)} : I != 5}})");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5}} union {{(on I in 1..10) (I, 3) : I != 5}} union {{(on I in 1..10) (I, 4) : I != 5}}"), 
				simplifier.apply(recursionCase, context));
	}
}
