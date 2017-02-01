package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.IntersectionExtensionalSetSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

public class IntersectionExtensionalSetSimplifierTest {
	private Context context;
	private IntersectionExtensionalSetSimplifier simplifier;
	
	@Before
	public void setUp() {
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false), 
						new TupleTheory()));
		IntegerInterval intType = new IntegerInterval(1, 10);
		context = (Context) GrinderUtil.extendRegistryWith(
				map("M", intType.toString(), 
					"N", intType.toString()), 
				Arrays.asList(intType), context);
		
		simplifier = new IntersectionExtensionalSetSimplifier();
	}
	
	@Test
	public void testExtensionalInIntensionalBasicCases() {
		Expression intersection = parse("{(2,2),(3,2),(4,2)} intersection {{(on J in 1..10) (J, 2) : J != 4}}");
		Assert.assertEquals(
				parse("{(2,2)} union {(3,2)}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{(2,3),(3,3),(4,3)} intersection {{(on J in 1..10) (J, 2) : J != 4}}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
				
		intersection = parse("{(2,2),(3,2),(4,2)} intersection {{(on J in 1..10) (J, 2) : N != 4}}");
		Assert.assertEquals(
				parse("(if N != 4 then {(2,2)} else {}) union (if N != 4 then {(3,2)} else {}) union (if N != 4 then {(4,2)} else {})"),
				simplifier.apply(intersection, context));
	}
	
	@Test
	public void testExtensionalInExtensonalBasicCases() {		
		Expression intersection = parse("{(2,2),(3,2),(4,2)} intersection {}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context)); 
		
		parse("{} intersection {(2,2),(3,2),(4,2)}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{(2,2),(3,2),(4,2)} intersection {(2,2),(3,2),(5,2),(6,2)}");
		Assert.assertEquals(
				parse("{(2,2)} union {(3,2)}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{(2,2),(3,2),(4,2)} intersection {(N, 2)}");
		Assert.assertEquals(
				parse("(if 2 = N then {(2,2)} else {}) union (if 3 = N then {(3,2)} else {}) union (if 4 = N then {(4,2)} else {})"),
				simplifier.apply(intersection, context));
	}
	
	@Test
	public void testExtensionalInMoreThanOneIntersection() {
		Expression intersection = parse("{(2,2),(3,2),(4,2),(5,2)} intersection {{(on J in 1..10) (J, 2) : J != 2}} intersection {{(on K in 1..10) (K, 2) : K != 4}}");
		Assert.assertEquals(
				parse("{(3,2)} union {(5,2)}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{(2,2),(3,2)} intersection {{(on J in 1..10) (J, 2) : M != 2}} intersection {{(on K in 1..10) (K, 2) : N != 4}}");
		Assert.assertEquals(
				parse("(if (M != 2) and (N != 4) then { (2, 2) } else {  }) union (if (M != 2) and (N != 4) then { (3, 2) } else {  })"),
				simplifier.apply(intersection, context));
	}
	
	@Test
	public void testExtensionalInInteresectionWithConditionals() {
		Expression intersection = parse("{(2,2),(3,2),(4,2)} intersection {{(on J in 1..10) (J, 2) : J != 4}} intersection (if N != 4 then {(4,2)} else {})");
		Assert.assertEquals(
				parse("({(2,2)} union {(3,2)}) intersection (if N != 4 then {(4,2)} else {})"),
				simplifier.apply(intersection, context));
	}
}
