package com.sri.ai.test.grinder.sgdpllt.theory.tuple;


import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleDisequalitySimplifier;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleEqualitySimplifier;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleEqualityTopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleGetSetTopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleGetSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleQuantifierSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleSetSimplifier;

import static com.sri.ai.expresso.helper.Expressions.parse;

public class TupleRewriterTest {
	
	private Context context = new TrueContext();

	@Test
	public void testTupleEqualitySimplification() {
		Simplifier tupleEqualitySimplifier = new TupleEqualitySimplifier();
		
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
		Simplifier tupleDisequalitySimplifier = new TupleDisequalitySimplifier();
		
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
	
	@Test
	public void testTupleQuantifierSimplification() {
		Simplifier tupleQuantifierSimplifier = new TupleQuantifierSimplifier();
		
		Assert.assertEquals(
				parse("for all X_1 in Boolean : for all X_2 in Integer : (X_1, X_2) = (true, 1)"),
				tupleQuantifierSimplifier.apply(parse("for all X in tuple_type(Boolean, Integer) : X = (true, 1)"), context));
		
		Assert.assertEquals(
				parse("there exists X_1 in Boolean : there exists X_2 in Integer : (X_1, X_2) = (true, 1)"),
				tupleQuantifierSimplifier.apply(parse("there exists X in tuple_type(Boolean, Integer) : X = (true, 1)"), context));
		
		Assert.assertEquals(
				parse("| X_1 in Boolean, X_2 in Integer : (X_1, X_2) = (true, 1) |"),
				tupleQuantifierSimplifier.apply(parse("| X in tuple_type(Boolean, Integer) : X = (true, 1) |"), context));
		
		Assert.assertEquals(
				parse("lambda X_1 in Boolean, X_2 in Integer : (X_1, X_2) = (true, 1)"),
				tupleQuantifierSimplifier.apply(parse("lambda X in tuple_type(Boolean, Integer) : X = (true, 1)"), context));
		
		Assert.assertEquals(
				parse("{ (on X_1 in Boolean, X_2 in Integer) (X_1, X_2) }"),
				tupleQuantifierSimplifier.apply(parse("{ (on X in tuple_type(Boolean, Integer)) X }"), context));
		
		Assert.assertEquals(
				parse("{ (on X_1 in Boolean, X_2 in Integer) (X_1, X_2) : (X_1, X_2) != (true, 1) }"),				
				tupleQuantifierSimplifier.apply(parse("{ (on X in tuple_type(Boolean, Integer)) X : X != (true, 1) }"), context));
		
		Assert.assertEquals(
				parse("{{ (on X_1 in Boolean, X_2 in Integer) (X_1, X_2) }}"),
				tupleQuantifierSimplifier.apply(parse("{{ (on X in tuple_type(Boolean, Integer)) X }}"), context));
		
		Assert.assertEquals(
				parse("{{ (on X_1 in Boolean, X_2 in Integer) (X_1, X_2) : (X_1, X_2) != (true, 1) }}"),				
				tupleQuantifierSimplifier.apply(parse("{{ (on X in tuple_type(Boolean, Integer)) X : X != (true, 1) }}"), context));
		
		//
		// Ensure Introduced Variables are Unique
		Assert.assertEquals(
				parse("lambda X_1' in Boolean, X_2' in Integer, X_1 in Boolean, X_2 in Boolean : (X_1', X_2') = (true, 1) and X_1 = X_2"),
				tupleQuantifierSimplifier.apply(parse("lambda X in tuple_type(Boolean, Integer), X_1 in Boolean, X_2 in Boolean : X = (true, 1) and X_1 = X_2"), context));
		
		//
		// Ensure we don't introduce undeclared types
		Assert.assertEquals(
				parse("lambda X_1 in Boolean, X_2 in Integer, Y, Z : (X_1, X_2) = (true, 1) and Y = Z"),
				tupleQuantifierSimplifier.apply(parse("lambda X in tuple_type(Boolean, Integer), Y, Z : X = (true, 1) and Y = Z"), context));	
	}
	
	@Test
	public void testTupleGetSimplification() {
		Simplifier tupleGetSimplifier = new TupleGetSimplifier();
		
		Assert.assertEquals(
				parse("a"),
				tupleGetSimplifier.apply(parse("get((a,b,c),1)"), context));

		Assert.assertEquals(
				parse("b"),
				tupleGetSimplifier.apply(parse("get((a,b,c),2)"), context));
		
		Assert.assertEquals(
				parse("c"),
				tupleGetSimplifier.apply(parse("get((a,b,c),3)"), context));
		
		Assert.assertEquals(
				parse("a"),
				tupleGetSimplifier.apply(parse("get(tuple(a),I)"), context));
		
		Assert.assertEquals(
				parse("if I = 1 then a else b"),
				tupleGetSimplifier.apply(parse("get((a,b),I)"), context));
		
		Assert.assertEquals(
				parse("if I = 1 then a else if I = 2 then b else c"),
				tupleGetSimplifier.apply(parse("get((a,b,c),I)"), context));		
		
		Expression expr = parse("get(I, (a,b,c))");
		Assert.assertTrue(expr == tupleGetSimplifier.apply(expr, context));
	}
	
	@Test(expected = IndexOutOfBoundsException.class)
	public void testTupleGetBadIndex1() {
		System.out.println(""+new TupleGetSimplifier().apply(parse("get((a,b,c),0)"), context));
	}
	
	@Test(expected = IndexOutOfBoundsException.class)
	public void testTupleGetBadIndex2() {
		System.out.println(""+new TupleGetSimplifier().apply(parse("get((a,b,c),4)"), context));
	}
	
	@Test(expected = ArithmeticException.class)
	public void testTupleGetBadIndex3() {
		System.out.println(""+new TupleGetSimplifier().apply(parse("get((a,b,c),1.1)"), context));
	}
	
	@Test
	public void testTupleSetSimplification() {
		Simplifier tupleSetSimplifier = new TupleSetSimplifier();
		
		Assert.assertEquals(
				parse("(e,b,c)"),
				tupleSetSimplifier.apply(parse("set((a,b,c),1,e)"), context));

		Assert.assertEquals(
				parse("(a,e,c)"),
				tupleSetSimplifier.apply(parse("set((a,b,c),2,e)"), context));
		
		Assert.assertEquals(
				parse("(a,b,e)"),
				tupleSetSimplifier.apply(parse("set((a,b,c),3,e)"), context));
		
		Assert.assertEquals(
				parse("tuple(e)"),
				tupleSetSimplifier.apply(parse("set(tuple(a),I,e)"), context));
		
		Assert.assertEquals(
				parse("if I = 1 then (e,b) else (a,e)"),
				tupleSetSimplifier.apply(parse("set((a,b),I,e)"), context));
		
		Assert.assertEquals(
				parse("if I = 1 then (e,b,c) else if I = 2 then (a,e,c) else (a,b,e)"),
				tupleSetSimplifier.apply(parse("set((a,b,c),I,e)"), context));		
		
		Expression expr = parse("set(I, (a,b,c), e)");
		Assert.assertTrue(expr == tupleSetSimplifier.apply(expr, context));
	}
	
	@Test(expected = IndexOutOfBoundsException.class)
	public void testTupleSetBadIndex1() {
		System.out.println(""+new TupleSetSimplifier().apply(parse("set((a,b,c),0,e)"), context));
	}
	
	@Test(expected = IndexOutOfBoundsException.class)
	public void testTupleSetBadIndex2() {
		System.out.println(""+new TupleSetSimplifier().apply(parse("set((a,b,c),4,e)"), context));
	}
	
	@Test(expected = ArithmeticException.class)
	public void testTupleSetBadIndex3() {
		System.out.println(""+new TupleSetSimplifier().apply(parse("set((a,b,c),1.1,e)"), context));
	}
	
	@Test
	public void testTupleGetSetTopRewriter() {
		TopRewriter tupleGetSetTopRewriter = new TupleGetSetTopRewriter();
		
		Assert.assertEquals(
				parse("if I = 1 then a else if I = 2 then b else c"),
				tupleGetSetTopRewriter.apply(parse("get((a,b,c),I)"), context));
		
		Assert.assertEquals(
				parse("if I = 1 then (e,b,c) else if I = 2 then (a,e,c) else (a,b,e)"),
				tupleGetSetTopRewriter.apply(parse("set((a,b,c),I,e)"), context));	
	}
}