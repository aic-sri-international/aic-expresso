package com.sri.ai.test.grinder.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.library.set.invsupport.UnionEmptySetSimplifier;

public class UnionEmptySetSimplifierTest {

	private UnionEmptySetSimplifier simplifier = new UnionEmptySetSimplifier();	
	private Context context = new TrueContext();
	
	@Test
	public void testSimplifyUniSets() {
		Expression union = parse("{} union {(1,2)} union {(1,3)}");
		Assert.assertEquals(
				parse("{(1,2)} union {(1,3)}"),
				simplifier.apply(union, context));
		
		union = parse("{(1,2)} union {} union {(1,3)}");
		Assert.assertEquals(
				parse("{(1,2)} union {(1,3)}"),
				simplifier.apply(union, context));
		
		union = parse("{(1,2)} union {(1,3)} union {}");
		Assert.assertEquals(
				parse("{(1,2)} union {(1,3)}"),
				simplifier.apply(union, context));
		
		union = parse("{} union {(1,2)}");
		Assert.assertEquals(
				parse("{(1,2)}"),
				simplifier.apply(union, context));
		
		union = parse("{(1,2)} union {}");
		Assert.assertEquals(
				parse("{(1,2)}"),
				simplifier.apply(union, context));
	}
	
	@Test
	public void testSimplifyMultiSets() {
		Expression union = parse("{{}} union {{(1,2)}} union {{(1,3)}}");
		Assert.assertEquals(
				parse("{{(1,2)}} union {{(1,3)}}"),
				simplifier.apply(union, context));
		
		union = parse("{{(1,2)}} union {{}} union {{(1,3)}}");
		Assert.assertEquals(
				parse("{{(1,2)}} union {{(1,3)}}"),
				simplifier.apply(union, context));
		
		union = parse("{{(1,2)}} union {{(1,3)}} union {{}}");
		Assert.assertEquals(
				parse("{{(1,2)}} union {{(1,3)}}"),
				simplifier.apply(union, context));
		
		union = parse("{{}} union {{(1,2)}}");
		Assert.assertEquals(
				parse("{{(1,2)}}"),
				simplifier.apply(union, context));
		
		union = parse("{{(1,2)}} union {{}}");
		Assert.assertEquals(
				parse("{{(1,2)}}"),
				simplifier.apply(union, context));
	}
}
