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
import com.sri.ai.grinder.sgdpllt.library.set.invsupport.SetExpressionIsEqualToEmptySet;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

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
					"N", intType.toString()), 
				Arrays.asList(intType), context);
		
		rewriter = new SetExpressionIsEqualToEmptySet();
	}
	
	@Test
	public void testBasicCases() {
		Expression setEqualToEmptySet = parse("{(2,2)} = {}");
		Assert.assertEquals(
				parse("false"),
				rewriter.apply(setEqualToEmptySet, context));
		
		setEqualToEmptySet = parse("{(2,2)} intersection {(N,2)} = {}");
		Assert.assertEquals(			
				parse("if 2 = N then false else true"),
				rewriter.apply(setEqualToEmptySet, context));
	}
}
