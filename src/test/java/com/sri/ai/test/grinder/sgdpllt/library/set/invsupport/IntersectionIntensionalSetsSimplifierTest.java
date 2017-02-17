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
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.IntersectionIntensionalSetsSimplifier;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

public class IntersectionIntensionalSetsSimplifierTest {
	private Context context;
	private IntersectionIntensionalSetsSimplifier simplifier;
	
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
		
		simplifier = new IntersectionIntensionalSetsSimplifier();
	}
	
	@Test
	public void testBasicCases() {
		Expression intersection = parse("{{(on I in 1..10) (I, 2) : I != 5}} intersection {{(on J in 1..10) (J, 2) : J != 4}}");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5 and I != 4}}"),
				simplifier.apply(intersection, context));		
		
		intersection = parse("{{(on I in 1..10) (I, 2) : I != 5}} intersection {{(on J in 1..10) (J, 3) : J != 4}}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{(on I in 1..10) (I, 2) : I != 5}} intersection {{(on J in 1..10) (J, 2) : J != 4}} intersection {{(10, 2)}}");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5 and I != 4}} intersection {{(10,2)}}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{(on I in 1..10) (I, 2) : I != 5}} intersection {{(on J in 1..10) (J, 2) : J != 4}} intersection {{(on K in 1..10) (K, 2) : K != 3}}");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5 and I != 4 and I != 3}}"),
				simplifier.apply(intersection, context));
	}
	
	@Test
	public void testCombinedConditionFalse() {
		Expression intersection = parse("{{(on I in 1..10) (I, 2) : I != 5}} intersection {{(on J in 1..10) (J, 2) : J = 5}}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
	}
	
	@Test
	public void testStandardizeApart() {
		Expression intersection = parse("{{(on I in 1..10) (I, 2) : I != 5}} intersection {{(on I in 1..10) (I, 2) : I != 4}}");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5 and I != 4}}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{(on I in 1..10) (I, 2) : I != 5}} intersection {{(on I in 1..10) (I, 3) : I != 4}}");
		Assert.assertEquals(
				parse("{}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{(on I in 1..10) (I, 2) : I != 5}} intersection {{(on I in 1..10) (I, 2) : I != 4}} intersection {{(10, 2)}}");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5 and I != 4}} intersection {{(10,2)}}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{(on I in 1..10) (I, 2) : I != 5}} intersection {{(on I in 1..10) (I, 2) : I != 4}} intersection {{(on I in 1..10) (I, 2) : I != 3}}");
		Assert.assertEquals(
				parse("{{(on I in 1..10) (I, 2) : I != 5 and I != 4 and I != 3}}"),
				simplifier.apply(intersection, context));
	}
	
	@Test
	public void testSingleDomainIntersections() {
		Expression intersection = parse("{{ (on I in 0..10) I : I > 3 }} intersection {{ (on I in 5..20) I : I > 2 }}");
		Assert.assertEquals(
				parse("{{ ( on I in 0..10 ) I : (I > 3) and (I > 4) }}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{ (on I in 0..10) I : I > 3 }} intersection {{ (on I in 5..8) I : I > 2 }}");
		Assert.assertEquals(
				parse("{{ ( on I in 0..10 ) I : (I > 3) and (if I > 4 then I <= 8 else false) }}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{ (on I in Integer) I : I > 3 }} intersection {{ (on I in 5..20) I : I > 2 }}");
		Assert.assertEquals(
				parse("{{ ( on I in Integer ) I : (I > 3) and (if I > 4 then I <= 20 else false) }}"),
				simplifier.apply(intersection, context));
	}
	
	@Test
	public void testMultiDomainIntersections() {
		Expression intersection = parse("{{ (on I in 0..10, J in 4..12) J : I > 3 and J > 6}} intersection {{ (on L in 5..20, M in 6..20) M : L >  2 and M > 7 }}");
		Assert.assertEquals(
				parse("{{ ( on I in 0..10, J in 4..12 ) J : (I > 3) and (J > 6) and (J > 7) }}"),
				simplifier.apply(intersection, context));
		
		intersection = parse("{{ (on I in 0..10, J in 4..12) J : I > 3 and J > 6}} intersection {{ (on L in 5..8, M in 6..11) M : L >  2 and M > 7 }}");
		Assert.assertEquals(
				parse("{{ ( on I in 0..10, J in 4..12 ) J : (I > 3) and (J > 6) and (if J > 7 then J <= 11 else false) }}"),
				simplifier.apply(intersection, context));
	}
}
