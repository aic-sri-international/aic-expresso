package com.sri.ai.test.grinder.sgdpllt.library.set;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;

import java.util.Arrays;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.library.set.Measure;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.util.math.Rational;

public class MeasureTest {
private Context context;
	
	@Before
	public void setUp() {		
		context = new TrueContext(
				new CompoundTheory(
						new DifferenceArithmeticTheory(true, false),
						new LinearRealArithmeticTheory(true, false),
						new EqualityTheory(true, false),
						new PropositionalTheory()));
	}
	
	@Test
	public void testCategoricalTypeDomain() {
		updateContextWithIndexAndType("N", 
				new Categorical("People", 5, parse("p1"), parse("p2"), parse("p3"), parse("p4"), parse("p5")));
		
		Assert.assertEquals(new Rational(5), measure("{{(on I in People) I : true}}"));
		Assert.assertEquals(new Rational(4), measure("{{(on I in People) I : I != p1}}"));
		Assert.assertEquals(new Rational(3), measure("{{(on I in People) I : I != p1 and I != p5}}"));
	}
	
	@Test
	public void testIntegerTypeDomain() {
		Assert.assertEquals(new Rational(2), measure("{{ (on X in Integer) 3 : X > 4 and X < 7 }}"));
	}
	
	@Test
	public void testIntegerIntervalTypeDomain() {
		Assert.assertEquals(new Rational(5), measure("{{ (on X in 3..7) 3 : true }}"));
		Assert.assertEquals(new Rational(4), measure("{{ (on X in 3..7) 3 : X != 5 }}"));
	}
	
	@Test
	public void testRealTypeDomain() {
		Assert.assertEquals(new Rational(3), measure("{{ (on x in Real) 3 : x > 4 and x < 7 }}"));
	}
	
	@Test
	public void testRealIntervalTypeDomain() {
		Assert.assertEquals(new Rational(4), measure("{{ (on X in [3;7]) 3 : true }}"));
		Assert.assertEquals(new Rational(4), measure("{{ (on X in [3;7]) 3 : X != 5 }}"));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testNotIntensionalSetIllegalArgumentException() {
		measure("1");
	}
	
	@Test(expected=UnsupportedOperationException.class)
	public void testThrowsUnsupportedGreaterThan1Index() {
		updateContextWithIndexAndType("N", 
				new Categorical("People", 5, parse("p1"), parse("p2"), parse("p3"), parse("p4"), parse("p5")));
		
		measure("{{(on I in People, J in People) (I, J) : true}}");
	}
	
	@Test(expected=UnsupportedOperationException.class)
	public void testUnableToCompute() {
		measure("{{(on I in Integer) I : true}}");
	}
	
	private void updateContextWithIndexAndType(String index, Type type) {
		context = (Context) GrinderUtil.extendRegistryWith(map(index, type.toString()), Arrays.asList(type), context);
	}
	
	private Rational measure(String intensionalSet) {
		Rational result = Measure.get(parse(intensionalSet), context);
		return result;
	}
}
