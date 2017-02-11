package com.sri.ai.test.grinder.sgdpllt.interpreter;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;

import java.util.Arrays;
import java.util.Random;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.interpreter.SampleCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;

public class SampleCommonInterpreterTest {
	private Random random;
	private Context context;
	
	@Before
	public void setUp() {
		random = new Random(1); // Make tests repeatable
		
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(false, false),
						new LinearRealArithmeticTheory(false, false),
						new EqualityTheory(false, false),
						new PropositionalTheory()));
	}
	
	
	@Test
	public void testSumOverCategoricalDomain() {
		updateContextWithIndexAndType("N", 
				new Categorical("People", 5, parse("p1"), parse("p2"), parse("p3"), parse("p4"), parse("p5")));	
		
		// NOTE: random picks p1 and p2
		runTest(2, "sum({{(on N in People) 2}})", "10");
		// NOTE: keep this order of calls as the fixed Random will pick p3 and p4
		runTest(2, "sum({{(on N in People) if N = p3 then 4 else 2}})", "15");
		
// TODO - uncomment, measure of intensional sets with a condition is currently not implemented.		
		// NOTE: picks p5 and p5
		// runTest(2, "sum({{(on N in People) 2 : N != p2}})", "8");
	}
	
	public void runTest(int sampleSizeN, String expression, String expected) {
		SampleCommonInterpreter interpreter = new SampleCommonInterpreter(sampleSizeN, random);
		Expression result = interpreter.apply(parse(expression), context);
		Assert.assertEquals(parse(expected), result);
	}
	
	private void updateContextWithIndexAndType(String index, Type type) {
		context = (Context) GrinderUtil.extendRegistryWith(map(index, type.toString()), Arrays.asList(type), context);
	}
}
