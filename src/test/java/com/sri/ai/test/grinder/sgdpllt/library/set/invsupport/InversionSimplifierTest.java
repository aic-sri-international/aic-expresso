package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.InversionSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

public class InversionSimplifierTest {
	private Context context;
	private InversionSimplifier simplifier;
	
	@Before
	public void setUp() {
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false), 
						new TupleTheory()));
		
		simplifier = new InversionSimplifier();
	}
	
	@Test
	public void testCase1() {
		Expression summation = parse("sum({{(on f in 1..10 -> 1..5) product({{(on X in 1..10) f(X) : true }}) : true}})");
	    Expression product   = parse("product({{(on X in 1..10) sum({{(on f in 1..5) f }}) : true}})");
		
	    Assert.assertEquals(
				product, 
				simplifier.apply(summation, context));
	}
	
	@Test
	public void testCase2() {
		Expression summation = parse("sum({{(on f in 1..10 -> 1..5) product({{(on X in 1..10) f(X) + 7 : true }}) : true}})");
	    Expression product   = parse("product({{(on X in 1..10) sum({{(on f in 1..5) f + 7 }}) : true}})");
		
	    Assert.assertEquals(
				product, 
				simplifier.apply(summation, context));
	}
	
	@Test
	public void testCase3() {
		Expression summation = parse("sum({{(on f in 1..10 x 1..10 -> 1..5) product({{(on X in 1..10) product({{(on Y in 1..10) f(X, Y) : true }}) : true }}) : true }})");
	    Expression product   = parse("product({{(on X in 1..10) product({{(on Y in 1..10) sum({{(on f in 1..5) f : true }}) : true }}) : true}})");
		
	    Assert.assertEquals(
				product, 
				simplifier.apply(summation, context));
	}
}