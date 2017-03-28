package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.helper.GrinderUtil;
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
		FunctionType gFunctionType = new FunctionType(new IntegerInterval("1..10"), new IntegerInterval("1..10"));
		context = (Context) GrinderUtil.extendRegistryWith(
				map("g", gFunctionType.toString()), 
				Arrays.asList(gFunctionType), context);
		
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
	
	@Test
	public void testCase4() {
		Expression summation = parse("sum({{(on f in 1..10 -> 1..5) product({{(on X in 1..10) f(X) + 3 : X != 5 }}) : true}})");
	    Expression product   = parse("product({{(on X in 1..10) sum({{(on f in 1..5) f + 3 }}) : X != 5}})");
		
	    Assert.assertEquals(
				product, 
				simplifier.apply(summation, context));
	}
	
	@Test
	public void testCase5() {
		Expression summation = parse("sum({{(on f in 1..10 x 1..10 -> 1..5) product({{(on X in 1..10) product({{(on Y in 1..10) f(X, Y) + 11 : Y != 7 }}) : X != 3 }}) : true }})");
	    Expression product   = parse("product({{(on X in 1..10) product({{(on Y in 1..10) sum({{(on f in 1..5) f + 11 : true }}) : Y != 7 }}) : X != 3 }})");
		
	    Assert.assertEquals(
				product, 
				simplifier.apply(summation, context));
	}
	
	@Test
	public void testCase6() {
		Expression summation = parse("sum({{(on f in 1..10 -> 1..5) product({{(on X in 1..10) f(X) + X : true }}) : true}})");
	    Expression product   = parse("product({{(on X in 1..10) sum({{(on f in 1..5) f + X }}) : true}})");
		
	    Assert.assertEquals(
				product, 
				simplifier.apply(summation, context));
	}
	
	@Test
	public void testCase7() {
		// NOTE: currently inversion won't be applied as this is conditional
		Expression summation = parse("sum({{(on f in 1..10 -> 1..5) product({{(on X in 1..10) f(X) + g(X) : true }}) : true}})");
	    Expression product   = parse("product({{(on X in 1..10) sum({{(on f in 1..5) f + g(X) }}) : true}})");
		
	    Assert.assertEquals(
				product, 
				simplifier.apply(summation, context));
	}
	
	@Ignore
	@Test
	public void testCase8() {
		// NOTE: currently inversion won't be applied as this is conditional
		Expression summation = parse("sum({{(on f in 1..10 -> 1..5) product({{(on X in 1..10) f(g(X)) : true }}) : true}})");
	    Expression product   = parse("sum({{(on f in 1..10 -> 1..5) product({{(on X in 1..10) f(g(X)) : true }}) : true}})");
		
	    Assert.assertEquals(
				product, 
				simplifier.apply(summation, context));
	}
}