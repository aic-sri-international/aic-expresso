package com.sri.ai.test.grinder.library.set.invsupport;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.set.invsupport.SetExpressionIsEqualToEmptySet;
import com.sri.ai.grinder.rewriter.api.Rewriter;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;

public class SetExpressionIsEqualToEmptySetTest {
	private Context context;
	private Rewriter rewriter;
	
	@Before
	public void setUp() {
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false), 
						new TupleTheory()));
		IntegerInterval intType = new IntegerInterval(1, 10);
		context = (Context) GrinderUtil.extendRegistryWith(
				map("M", intType.toString(), 
					"N", intType.toString(),
				    "X'", intType.toString(),
				    "X''", intType.toString(),
				    "Y", intType.toString()),				  
				Arrays.asList(intType), context);
		
		rewriter = new SetExpressionIsEqualToEmptySet();
	}
	
	@Test
	public void testExtensionalSetBasicCases() {
		Expression setEqualToEmptySet = parse("{} = {}");
		Assert.assertEquals(
				parse("true"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("{(2,2)} = {}");
		Assert.assertEquals(
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("{(N,2)} = {}");
		Assert.assertEquals(
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));
	}
	
	@Test
	public void testExtensionalUnionBasicCases() {
		Expression setEqualToEmptySet = parse("{} union {} = {}");
		Assert.assertEquals(
				parse("true"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("{(2,2)} union {} = {}");
		Assert.assertEquals(
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));	
		
		setEqualToEmptySet = parse("{(2,2)} union {(2,3)} = {}");
		Assert.assertEquals(
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));	
		
		setEqualToEmptySet = parse("{(2,N)} union {(2,3)} = {}");
		Assert.assertEquals(
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("{(2,2)} union Union({{(on I in 1..10) {(I, 2)} : I != 5}}) = {}");
		Assert.assertEquals(
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));
	}
	
	@Test
	public void testIntensionalUnionSetBasicCases() {
		Expression setEqualToEmptySet = parse("Union({{(on I in 1..10) {(I, 2)} : I > 10}}) = {}");
		Assert.assertEquals(
				parse("true"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("Union({{(on I in 1..10) {(I, 2)} : I != 5}}) = {}");
		Assert.assertEquals(
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("Union({{(on I in 1..10) {(I, 2)} : I != N}}) = {}");
		Assert.assertEquals(
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));
	}
	
	@Test
	public void testExtensionalIntersectionBasicCases() {
		Expression setEqualToEmptySet = parse("{(2,2)} intersection {(3,2)} = {}");
		Assert.assertEquals(			
				parse("true"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("{(2,2)} intersection {(2,2)} = {}");
		Assert.assertEquals(			
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("{(2,2)} intersection {(N,2)} = {}");
		Assert.assertEquals(			
				parse("if 2 = N then false else true"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("({(2,2)} union {(3,2)}) intersection (if N != 4 then {(4,2)} else {}) = {}");
		Assert.assertEquals(			
				parse("true"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("{(2,2),(3,2),(4,2)} intersection {{(on J in 1..10) (J, 2) : J != 4}} intersection (if N != 4 then {(4,2)} else {}) = {}");
		Assert.assertEquals(			
				parse("true"),
				rewriter.apply(setEqualToEmptySet, context));
	}
	
	@Test
	public void testIntersectionsOfUnionsOfExtensionalSetsBasicCases() {		
		context = context.conjoin(parse("X' != X''"), context);
		Expression setEqualToEmptySet = parse("({ (X', Y) } union { (X', 3) }) intersection ({ (X'', Y) } union { (X'', 3) }) = {  }");
		Assert.assertEquals(			
				parse("true"),
				rewriter.apply(setEqualToEmptySet, context));
	}
}
