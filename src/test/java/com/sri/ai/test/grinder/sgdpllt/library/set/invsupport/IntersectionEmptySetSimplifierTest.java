package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.set.invsupport.IntersectionEmptySetSimplifier;

import static com.sri.ai.expresso.helper.Expressions.parse;

public class IntersectionEmptySetSimplifierTest {

	private IntersectionEmptySetSimplifier simplifier = new IntersectionEmptySetSimplifier();	
	private Context context = new TrueContext();
	
	@Test
	public void testSimplifyUniSets() {
		Expression intersection = parse("{} intersection {(1,2)} intersection {(1,3)}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{(1,2)} intersection {} intersection {(1,3)}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{(1,2)} intersection {(1,3)} intersection {}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{(1,2)} intersection {(I, 3)}");
		Assert.assertEquals(
				intersection,
				simplifier.apply(intersection, context));
	}
	
	@Test
	public void testSimplifyMultiSets() {
		Expression intersection = parse("{{}} intersection {{(1,2)}} intersection {{(1,3)}}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{(1,2)}} intersection {{}} intersection {{(1,3)}}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{(1,2)}} intersection {{(1,3)}} intersection {{}}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
	}
}
