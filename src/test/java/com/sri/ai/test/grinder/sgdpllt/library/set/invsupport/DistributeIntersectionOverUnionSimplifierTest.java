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
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.DistributeIntersectionOverUnionSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

public class DistributeIntersectionOverUnionSimplifierTest {
	private Context context;
	private DistributeIntersectionOverUnionSimplifier simplifier;
	
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
		
		simplifier = new DistributeIntersectionOverUnionSimplifier();
	}
	
	@Test
	public void testBasicCases() {
		Expression intersection = parse("{(2,2)} intersection ({(3,2)} union {(4,2)})");
		Assert.assertEquals(
				parse("({(2,2)} intersection {(3,2)}) union ({(2,2)} intersection {(4,2)})"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{(2,2)} intersection ({(3,2)} union {(4,2)}) intersection {(5,2)}");
		Assert.assertEquals(
				parse("({(5,2)} intersection {(2,2)} intersection {(3,2)}) union ({(5,2)} intersection {(2,2)} intersection {(4,2)})"),
				simplifier.apply(intersection, context));
	}
}
