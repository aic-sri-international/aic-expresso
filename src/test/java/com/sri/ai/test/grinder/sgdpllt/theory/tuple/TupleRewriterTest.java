package com.sri.ai.test.grinder.sgdpllt.theory.tuple;


import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleDisequality;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleEquality;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleEqualityTopRewriter;

import static com.sri.ai.expresso.helper.Expressions.parse;

public class TupleRewriterTest {
	
	private Context context = new TrueContext();

	@Test
	public void testTupleEqualitySimplification() {
		Simplifier tupleEqualitySimplifier = new TupleEquality();
		
		Assert.assertEquals(
				parse("A1 = B1 and A2 = B2 and A3 = B3"),
				tupleEqualitySimplifier.apply(parse("(A1, A2, A3) = (B1, B2, B3)"), context));
		
		Expression expr = parse("(A1, A2) = (B1, B2, B3)");
		Assert.assertTrue(expr == tupleEqualitySimplifier.apply(expr, context));
		
		expr = parse("A1 = B1");
		Assert.assertTrue(expr == tupleEqualitySimplifier.apply(expr, context));
	}
	
	@Test
	public void testTupleDisEqualitySimplification() {
		Simplifier tupleDisequalitySimplifier = new TupleDisequality();
		
		Assert.assertEquals(
				parse("A1 != B1 or A2 != B2 or A3 != B3"),
				tupleDisequalitySimplifier.apply(parse("(A1, A2, A3) != (B1, B2, B3)"), context));
		
		Expression expr = parse("(A1, A2) != (B1, B2, B3)");
		Assert.assertTrue(expr == tupleDisequalitySimplifier.apply(expr, context));
		
		expr = parse("A1 != B1");
		Assert.assertTrue(expr == tupleDisequalitySimplifier.apply(expr, context));
	}
	
	@Test
	public void testTupleEqualityTopRewriter() {
		TopRewriter tupleEqualityTopRewriter = new TupleEqualityTopRewriter();
		
		Assert.assertEquals(
				parse("A1 = B1 and A2 = B2 and A3 = B3"),
				tupleEqualityTopRewriter.apply(parse("(A1, A2, A3) = (B1, B2, B3)"), context));
		
		Assert.assertEquals(
				parse("(A1_1, A1_2, A1_3) = (B1_1, B1_2, B1_3) and A2 = B2 and A3 = B3"),
				tupleEqualityTopRewriter.apply(parse("((A1_1, A1_2, A1_3), A2, A3) = ((B1_1, B1_2, B1_3), B2, B3)"), context));
		
		Assert.assertEquals(
				parse("A1 != B1 or A2 != B2 or A3 != B3"),
				tupleEqualityTopRewriter.apply(parse("(A1, A2, A3) != (B1, B2, B3)"), context));
		
		Assert.assertEquals(
				parse("(A1_1, A1_2, A1_3) != (B1_1, B1_2, B1_3) or A2 != B2 or A3 != B3"),
				tupleEqualityTopRewriter.apply(parse("((A1_1, A1_2, A1_3), A2, A3) != ((B1_1, B1_2, B1_3), B2, B3)"), context));
		
		Expression expr = parse("(A1, A2) = (B1, B2, B3)");
		Assert.assertTrue(expr == tupleEqualityTopRewriter.apply(expr, context));
		
		expr = parse("A1 = B1");
		Assert.assertTrue(expr == tupleEqualityTopRewriter.apply(expr, context));
		
		expr = parse("(A1, A2) != (B1, B2, B3)");
		Assert.assertTrue(expr == tupleEqualityTopRewriter.apply(expr, context));
		
		expr = parse("A1 != B1");
		Assert.assertTrue(expr == tupleEqualityTopRewriter.apply(expr, context));
	}
}