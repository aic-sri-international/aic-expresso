package com.sri.ai.test.grinder.helper;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.helper.AssignmentsSamplingIterator;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.join;

public class AssignmentsSamplingIteratorTest {

	private Random random;
	private Rewriter conditionRewriter;
	private Context context;
	
	@Before
	public void setUp() {
		random = new Random(1); // Make tests repeatable
		conditionRewriter = new Recursive(new Exhaustive(new BruteForceCommonInterpreter()));
		
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false),
						new LinearRealArithmeticTheory(false, false),
						new EqualityTheory(false, false),
						new PropositionalTheory()));
	}
	
	@Test
	public void testSampleOverCategoricalType() {
		updateContextWithIndexAndType("N", 
				new Categorical("People", 5, parse("p1"), parse("p2"), parse("p3"), parse("p4"), parse("p5")));		
		
		
		Assert.assertEquals("{N=p1}:{N=p3}:{N=p5}", join(":", newSamplingIterator("N", 3, "N != p4")));
		Assert.assertEquals("{N=p5}:{N=p5}:{N=p2}:{N=p4}", join(":", newSamplingIterator("N", 4, "N != p1")));
	}
	
	private void updateContextWithIndexAndType(String index, Type type) {
		context = (Context) GrinderUtil.extendRegistryWith(map("N", type.toString()), Arrays.asList(type), context);
	}
	
	private Iterator<Map<Expression, Expression>> newSamplingIterator(String index, int sampleSize, String condition) {
		Iterator<Map<Expression, Expression>> result = new AssignmentsSamplingIterator(Arrays.asList(parse(index)), sampleSize, parse(condition), conditionRewriter, random, context);
		return result;
	}
}
