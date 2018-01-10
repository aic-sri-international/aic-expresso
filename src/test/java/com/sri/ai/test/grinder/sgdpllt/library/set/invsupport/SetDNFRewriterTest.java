package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

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
import com.sri.ai.grinder.library.set.invsupport.SetDNFRewriter;
import com.sri.ai.grinder.rewriter.api.Rewriter;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;

public class SetDNFRewriterTest {
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
					"N", intType.toString()), 
				Arrays.asList(intType), context);
		
		rewriter = new SetDNFRewriter();
	}
	
	@Test
	public void testBasicIntersectionCases() {
		Expression intersection = parse("{(2,2)} intersection ({(2,2)} union {(3,2)} union {(4,2)}) = {}");
		Assert.assertEquals(
				parse("{(2,2)} = {}"),
				rewriter.apply(intersection, context));
		
		intersection = parse("{(2,2)} intersection ({(3,2)} union {(4,2)}) = {}");
		Assert.assertEquals(
				parse("true"),
				rewriter.apply(intersection, context));
		
		intersection = parse("{(2,2)} intersection {(N,2)} = {}");
		Assert.assertEquals(			
				parse("if 2 = N then { (2, 2) } = {} else true"),
				rewriter.apply(intersection, context));
		
		intersection = parse("{(1, 2)} intersection Union({{(on I in 1..10) {(I, 2)} : I != 5}}) = {}");
		Assert.assertEquals(			
				parse("{(1, 2)} = {}"),
				rewriter.apply(intersection, context));
	}
	
	@Test
	public void testBasicUnionCases() {
		Expression union = parse("Union({{(on I in 1..10) {(I, 2)} : I != 5}}) = {}");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5}} = {}"), 
				rewriter.apply(union, context));
		
		union = parse("{(1, 2)} union Union({{(on I in 1..10) {(I, 2)} : I != 5}}) = {}");
		Assert.assertEquals(
				parse("{(1, 2)} union {{(on I in 1..10) (I, 2) : I != 5}} = {}"), 
				rewriter.apply(union, context));
	}
}
