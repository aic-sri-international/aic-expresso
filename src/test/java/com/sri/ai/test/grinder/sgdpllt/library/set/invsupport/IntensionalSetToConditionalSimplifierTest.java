package com.sri.ai.test.grinder.sgdpllt.library.set.invsupport;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.set.invsupport.IntensionalSetToConditionalSimplifier;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.tuple.TupleTheory;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;

import java.util.Arrays;

public class IntensionalSetToConditionalSimplifierTest {

	private Context context;
	private IntensionalSetToConditionalSimplifier simplifier;
	
	@Before
	public void setUp() {
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false), 
						new TupleTheory()));
		IntegerInterval nType = new IntegerInterval(1, 10);
		context = (Context) GrinderUtil.extendRegistryWith(
				map("N", nType.toString()), 
				Arrays.asList(nType), context);
		
		simplifier = new IntensionalSetToConditionalSimplifier();
	}
	
	@Test
	public void testSimplify() {
		Expression iSet = parse("{{(on I in 1..10) (I, 2) : I != 5}}");
		Assert.assertEquals( // i.e. true for the condion
				parse("{{(on I in 1..10) (I, 2) : I != 5}}"),
				simplifier.apply(iSet, context));
		
		iSet = parse("{{(on I in 1..10) (I, 2) : I = 11}}");
		Assert.assertEquals( // i.e. false for the condion
				parse("{}"),
				simplifier.apply(iSet, context));		
		
		iSet = parse("{{(on I in 1..10) (I, 2) : N != 5}}");
		Assert.assertEquals( // i.e. conditioned condion on free variable N
				parse("if N != 5 then {{(on I in 1..10) (I, 2) : N != 5}} else {}"),
				simplifier.apply(iSet, context));
	}
}
