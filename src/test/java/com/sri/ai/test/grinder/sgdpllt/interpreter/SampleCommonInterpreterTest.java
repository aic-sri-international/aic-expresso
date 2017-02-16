package com.sri.ai.test.grinder.sgdpllt.interpreter;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;

import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.interpreter.SampleCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.SingleVariableDifferenceArithmeticConstraint;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.SingleVariableLinearRealArithmeticConstraint;
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
			
		// NOTE: picks p5 and p5
		runTest(2, "sum({{(on N in People) 2 : N != p2}})", "8");
	}
	
	@Test
	public void testSumOverIntegerDomain() {		
		runTest(2, "sum({{(on I in Integer) 2 : I >= 1 and I <= 5 }})", "10");
		runTest(2, "sum({{(on I in Integer) 2 : I >= 1 and I != 3 and I <= 5 }})", "8");
		
		// NOTE: picks 5,5,2
		runTest(3, "sum({{(on I in Integer) I : I >= 1 and I <= 5 }})", "20");
	}
	
	@Test
	public void testSumOverIntegerIntervalDomain() {		
		runTest(2, "sum({{(on I in 1..5) 2 : true }})", "10");
		runTest(2, "sum({{(on I in 1..5) 2 : I != 3}})", "8");
		
		// NOTE: picks 5,5,2
		runTest(3, "sum({{(on I in 1..5) I : true }})", "20");
	}
	
	@Test
	public void testSumOverRealDomain() {		
		runTest(2, "sum({{(on R in Real) 2 : R >= 1 and R <= 5 }})", "8");
		runTest(2, "sum({{(on R in Real) 2 : R >= 1 and R != 3 and R <= 5 }})", "8");
		
		// NOTE: picks 1.255232, 4.259308, 3.01488
		runTest(3, "sum({{(on R in Real) R : R >= 1 and R <= 5 }})", "11.37256");
	}
	
	@Test
	public void testSumOverRealIntervalsDomain() {		
		runTest(2, "sum({{(on R in [1;5]) 2 : true }})", "8");
		runTest(2, "sum({{(on R in [1;5]) 2 : R != 3}})", "8");
		
		// NOTE: picks 1.255232, 4.259308, 3.01488
		runTest(3, "sum({{(on R in [1;5]) R : true }})", "11.37256");
	}
	
	@Test
	public void testSumOverFunctionV1() {		
		runTest(2, "sum({{(on f in Boolean -> 0..3) 2 : true }})", "32");
	}
	
	@Test
	public void testSumOverFunctionV2() {		
		//  NOTE: picks f_1(true)=1, f_2(true)=2 (i.e. two different lazy sampled functors)
		runTest(2, "sum({{(on f in Boolean -> 0..3) f(true) : true }})", "24");
	}
	
	@Test
	public void testSumOverFunctionV3() {		
		//  NOTE: picks f_1(true)=1, f_1(false)=1, f_2(true)=2, f_2(false)=3
		runTest(2, "sum({{(on f in Boolean -> 0..3) sum({{(on X in Boolean) f(X) : true }}) : true }})", "56");
	}
	
	@Test
	public void testSumOverFunctionV4() {		
		//  NOTE: picks f_1(true)=1, f_1(false)=1, f_2(true)=2, f_2(false)=3
		runTest(2, "sum({{(on f in Boolean -> 0..3) product({{(on X in Boolean) f(X) : true }}) : true }})", "56");
	}
	
	@Test
	public void testSumOverTupleV1() {		
		runTest(2, "sum({{(on T in (Boolean x 0..3)) 2 : true }})", "16");
	}
	
	public void runTest(int sampleSizeN, String expressionString, String expectedString) {
		SampleCommonInterpreter interpreter = new SampleCommonInterpreter(sampleSizeN, random);
		
		Expression expression = parse(expressionString);
		if (expression.numberOfArguments() == 1 && Sets.isIntensionalSet(expression.get(0))) {
			IntensionalSet intensionalSet = (IntensionalSet) expression.get(0);
			IndexExpressionsSet indexExpressions = intensionalSet.getIndexExpressions();
			List<Expression> indices = IndexExpressions.getIndices(indexExpressions);

			if (indices.size() == 1) {
				Expression index = indices.get(0);
				Context intensionalSetContext = (Context) GrinderUtil.extendRegistryWithIndexExpressions(indexExpressions, context);			
				// Ensure condition of correct type is created
				Expression condition = intensionalSet.getCondition();
				Type indexType = GrinderUtil.getType(index, intensionalSetContext);
				
				SingleVariableConstraint singleVariableConstraint = null;
				if (indexType instanceof RealExpressoType || indexType instanceof RealInterval) {
					singleVariableConstraint = new SingleVariableLinearRealArithmeticConstraint(index, true, intensionalSetContext.getTheory());
				}
				else if (indexType instanceof IntegerExpressoType || indexType instanceof IntegerInterval) {
					singleVariableConstraint = new SingleVariableDifferenceArithmeticConstraint(index, true, intensionalSetContext.getTheory());
				}
				
				if (singleVariableConstraint != null) {
					singleVariableConstraint = singleVariableConstraint.conjoin(condition, intensionalSetContext);
					intensionalSet = intensionalSet.setCondition(singleVariableConstraint);	
					expression = expression.set(0, intensionalSet);
				}
			}
		}
		
		Expression result = interpreter.apply(expression, context);
		Assert.assertEquals(parse(expectedString), result);
	}


	private void updateContextWithIndexAndType(String index, Type type) {
		context = (Context) GrinderUtil.extendRegistryWith(map(index, type.toString()), Arrays.asList(type), context);
	}
}
