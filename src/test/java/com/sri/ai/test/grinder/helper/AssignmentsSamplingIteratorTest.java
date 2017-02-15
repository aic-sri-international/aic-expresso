package com.sri.ai.test.grinder.helper;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.map;

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
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.expresso.type.TupleType;
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
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.SingleVariableDifferenceArithmeticConstraint;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.SingleVariableLinearRealArithmeticConstraint;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;

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
	
	@Test
	public void testSampleOverIntegerInterval() {
		updateContextWithIndexAndType("I", new IntegerInterval(1,10));				
		
		Assert.assertEquals("{I=3}:{I=6}:{I=5}", join(":", newSamplingIterator("I", 3, "I > 2 and I < 8"))); // Sub-Interval
		Assert.assertEquals("{I=2}:{I=2}:{I=2}", join(":", newSamplingIterator("I", 3, "I = 2"))); // Singleton
		Assert.assertEquals("", join(":", newSamplingIterator("I", 3, "I = 11"))); // Empty Set
		Assert.assertEquals("{I=9}:{I=9}:{I=9}", join(":", newSamplingIterator("I", 3, "I != 1 and I != 3 and I !=4 and I != 5 and I != 6 and I !=7 and I != 8 and I != 10"))); // Broken Interval
	}
	
	@Test
	public void testSampleOverInteger() {
		updateContextWithIndexAndType("I", GrinderUtil.INTEGER_TYPE);				
		
		Assert.assertEquals("{I=3}:{I=6}:{I=5}", join(":", newSamplingIterator("I", 3, "I > 2 and I < 8"))); // Sub-Interval
		Assert.assertEquals("{I=2}:{I=2}:{I=2}", join(":", newSamplingIterator("I", 3, "I = 2"))); // Singleton
		Assert.assertEquals("", join(":", newSamplingIterator("I", 3, "I = 11 and I != 11"))); // Empty Set
		Assert.assertEquals("{I=9}:{I=9}:{I=10}", join(":", newSamplingIterator("I", 3, "I >= 1 and I != 3 and I !=4 and I != 5 and I != 6 and I !=7 and I != 8 and I <= 10"))); // Broken Interval
	}
	
	@Test
	public void testSampleOverRealInterval() {
		updateContextWithIndexAndType("R", new RealInterval("[1;10]"));

		Assert.assertEquals("{R=5.30333}:{R=6.588824}:{R=5.856368}", join(":", newSamplingIterator("R", 3, "R > 2 and R < 8"))); // Sub-Interval
		Assert.assertEquals("{R=2}:{R=2}:{R=2}", join(":", newSamplingIterator("R", 3, "R = 2"))); // Singleton
		Assert.assertEquals("", join(":", newSamplingIterator("R", 3, "R = 11"))); // Empty Set
		Assert.assertEquals("{R=5.53348}:{R=9.146728}:{R=3.1411}", join(":", newSamplingIterator("R", 3, "R != 1 and R != 3 and R !=4 and R != 5 and R != 6 and R !=7 and R != 8 and R != 10"))); // Broken Interval
	}
	
	@Test
	public void testSampleOverReal() {
		updateContextWithIndexAndType("R", GrinderUtil.REAL_TYPE);
		
		Assert.assertEquals("{R=5.30333}:{R=6.588824}:{R=5.856368}", join(":", newSamplingIterator("R", 3, "R > 2 and R < 8"))); // Sub-Interval
		Assert.assertEquals("{R=2}:{R=2}:{R=2}", join(":", newSamplingIterator("R", 3, "R = 2"))); // Singleton
		Assert.assertEquals("", join(":", newSamplingIterator("R", 3, "R = 11 and R != 11"))); // Empty Set
		Assert.assertEquals("{R=5.53348}:{R=9.146728}:{R=3.1411}", join(":", newSamplingIterator("R", 3, "R >= 1 and R != 3 and R !=4 and R != 5 and R != 6 and R !=7 and R != 8 and R <= 10"))); // Broken Interval		
	}
	
	@Test
	public void testSampleOverFunction() {
		updateContextWithIndexAndType("f", new FunctionType(GrinderUtil.BOOLEAN_TYPE, new IntegerInterval(1,10)));
		Assert.assertEquals("{f='->'(1..10, Boolean)}:{f='->'(1..10, Boolean)}:{f='->'(1..10, Boolean)}", join(":", newSamplingIterator("f", 3, "true")));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testSampleOverTuple() {
		updateContextWithIndexAndType("T", new TupleType(new IntegerInterval(1,10), GrinderUtil.BOOLEAN_TYPE));
		newSamplingIterator("T", 3, "true");
	}
	
	private void updateContextWithIndexAndType(String index, Type type) {
		context = (Context) GrinderUtil.extendRegistryWith(map(index, type.toString()), Arrays.asList(type), context);
	}
	
	private Iterator<Map<Expression, Expression>> newSamplingIterator(String indexString, int sampleSize, String conditionString) {
		Expression index     = parse(indexString);
		Expression condition = parse(conditionString);
		
		// Ensure condition of correct type is created
		Type indexType = GrinderUtil.getType(index, context);
		if (indexType instanceof RealExpressoType || indexType instanceof RealInterval) {
			SingleVariableLinearRealArithmeticConstraint svlraConstraint = new SingleVariableLinearRealArithmeticConstraint(index, true, context.getTheory());
			
			svlraConstraint = (SingleVariableLinearRealArithmeticConstraint) svlraConstraint.conjoin(condition, context);
			
			condition = svlraConstraint;
		}
		else if (indexType instanceof IntegerExpressoType || indexType instanceof IntegerInterval) {
			SingleVariableDifferenceArithmeticConstraint svdaConstraint = new SingleVariableDifferenceArithmeticConstraint(index, true, context.getTheory());
			
			svdaConstraint = (SingleVariableDifferenceArithmeticConstraint) svdaConstraint.conjoin(condition, context);
			
			condition = svdaConstraint;
		}		
		
		Iterator<Map<Expression, Expression>> result = new AssignmentsSamplingIterator(Arrays.asList(index), sampleSize, condition, conditionRewriter, random, context);
		return result;
	}
}
