package com.sri.ai.test.grinder.interpreter;

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
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.interpreter.SamplingCommonInterpreter;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.differencearithmetic.SingleVariableDifferenceArithmeticConstraint;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.theory.linearrealarithmetic.SingleVariableLinearRealArithmeticConstraint;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;

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
		runTest(2, true, "sum({{(on N in People) 2}})", "10");
		// NOTE: keep this order of calls as the fixed Random will pick p3 and p4
		runTest(2, true, "sum({{(on N in People) if N = p3 then 4 else 2}})", "15");
			
		// NOTE: picks p5 and p5
		runTest(2, true, "sum({{(on N in People) 2 : N != p2}})", "8");
	}
	
	@Test
	public void testSumOverIntegerDomain() {		
		runTest(2, true, "sum({{(on I in Integer) 2 : I >= 1 and I <= 5 }})", "10");
		runTest(2, true, "sum({{(on I in Integer) 2 : I >= 1 and I != 3 and I <= 5 }})", "8");
		
		// NOTE: picks 5,5,2
		runTest(3, true, "sum({{(on I in Integer) I : I >= 1 and I <= 5 }})", "20");
	}
	
	@Test
	public void testSumOverIntegerIntervalDomain() {		
		runTest(2, true, "sum({{(on I in 1..5) 2 : true }})", "10");
		runTest(2, true, "sum({{(on I in 1..5) 2 : I != 3}})", "8");
		
		// NOTE: picks 5,5,2
		runTest(3, true, "sum({{(on I in 1..5) I : true }})", "20");
	}
	
	@Test
	public void testSumOverRealDomainAlwaysSample() {		
		runTest(2, true, "sum({{(on R in Real) 2 : R >= 1 and R <= 5 }})", "8");
		runTest(2, true, "sum({{(on R in Real) 2 : R >= 1 and R != 3 and R <= 5 }})", "8");
		
		// NOTE: picks 1.255232, 4.259308, 3.01488
		runTest(3, true, "sum({{(on R in Real) R : R >= 1 and R <= 5 }})", "11.37256");
	}
	
	// NOTE: these tests should mirror those in ^^^ testSumOverRealDomainAlwaysSample()
	@Test
	public void testSumOverRealDomainBecauseDomainContinuous() {		
		runTest(2, false, "sum({{(on R in Real) 2 : R >= 1 and R <= 5 }})", "8");
		runTest(2, false, "sum({{(on R in Real) 2 : R >= 1 and R != 3 and R <= 5 }})", "8");
		
		// NOTE: picks 1.255232, 4.259308, 3.01488
		runTest(3, false, "sum({{(on R in Real) R : R >= 1 and R <= 5 }})", "11.37256");
	}
	
	@Test
	public void testSumOverRealIntervalsDomainExplicitlyAlwaysSample() {		
		runTest(2, true, "sum({{(on R in [1;5]) 2 : true }})", "8");
		runTest(2, true, "sum({{(on R in [1;5]) 2 : R != 3}})", "8");
		
		// NOTE: picks 1.255232, 4.259308, 3.01488
		runTest(3, true, "sum({{(on R in [1;5]) R : true }})", "11.37256");
	}
	
	// NOTE: these tests should mirror those in ^^^ testSumOverRealIntervalsDomainExplicitlyAlwaysSample()
	@Test
	public void testSumOverRealIntervalsDomainSampleBecauseDomainContinuous() {		
		runTest(2, false, "sum({{(on R in [1;5]) 2 : true }})", "8");
		runTest(2, false, "sum({{(on R in [1;5]) 2 : R != 3}})", "8");
		
		// NOTE: picks 1.255232, 4.259308, 3.01488
		runTest(3, false, "sum({{(on R in [1;5]) R : true }})", "11.37256");
	}
	
	@Test
	public void testSumOverFunctionV1() {		
		runTest(2, true, "sum({{(on f in Boolean -> 0..3) 2 : true }})", "32");
	}
	
	@Test
	public void testSumOverFunctionV2() {		
		//  NOTE: picks f_1(true)=1, f_2(true)=2 (i.e. two different lazy sampled functors)
		runTest(2, true, "sum({{(on f in Boolean -> 0..3) f(true) : true }})", "24");
	}
	
	@Test
	public void testSumOverFunctionV3() {		
		//  NOTE: picks f_1(true)=1, f_1(false)=1, f_2(true)=2, f_2(false)=3
		runTest(2, false, "sum({{(on f in Boolean -> 0..3) sum({{(on X in Boolean) f(X) : true }}) : true }})", "56");
	}
	
	@Test
	public void testSumOverFunctionV4() {		
		//  NOTE: picks f_1(true)=1, f_1(false)=1, f_2(true)=2, f_2(false)=3
		runTest(2, false, "sum({{(on f in Boolean -> 0..3) product({{(on X in Boolean) f(X) : true }}) : true }})", "56");
	}
	
	@Test
	public void testProductWithLotsOfSamplesV1() {		
		// NOTE: 
		// measure =        9 : 3^2
		// min     =      512 : (1+1)^9
		// max     = 10077696 : (3+3)^9
		runBoundedTest(10000, true, "product({{(on f in Boolean -> 1..3) f(true) + f(false) : true }})", "512", "10077696");
	}
	
	@Test
	public void testProductWithLotsOfSamplesV2() {	
		// NOTE: 
		// measure = 9 : 3^2		
		runBoundedTest(10000, true, "product({{(on f in Boolean -> 999999997..999999999) f(true) + f(false) : true }})", "(999999997+999999997)^9", "(999999999+999999999)^9");
	}
	
	@Test
	public void testProductWithLotsOfSamplesV3() {		
		// NOTE: 
		// measure = 100 : 10^2
		runBoundedTest(10000, true, "product({{(on f in Boolean -> 1..10) f(true) + f(false) : true }})", "(1+1)^100", "(10+10)^100");
	}
	
	@Test
	public void testProductWithLotsOfSamplesV4() {		
		// NOTE: 
		// measure = 400 : 20^2
		runBoundedTest(10000, true, "product({{(on f in Boolean -> 1..20) f(true) + f(false) : true }})", "(1+1)^400", "(20+20)^400");
	}
	
	@Test
	public void testProductWithLotsOfSamplesV5() {		
		// NOTE: 
		// measure = 10000 : 100^2		
		runBoundedTest(10000, true, "product({{(on f in Boolean -> 1..100) f(true) + f(false) : true }})", "(1+1)^10000", "(100+100)^10000");
	}
	
	@Test
	public void testIntegerIntervalExample1() {
		String intensionalSet = "{{(on I in 1..100) I^2 - I + 10 : I > 20 and I < 40 }}";
		
		String[][] functionNamesAndExactValues = new String[][] {
			{"sum", "17290"}, 
			{"max", "1492"}, 
			// Product[10 - k + k^2, {k, 21, 39}]
			{"product", "45909468078488688718259880191530770000000000000000000000"}
		};
		
		runSampleCompareToExact(100, true, intensionalSet, functionNamesAndExactValues);		
	}
	
	@Test
	public void testIntegerIntervalExample2() {
		String intensionalSet = "{{(on I in 1..1000000) I^2 - I + 10 : I > 20 and I < 40 }}";
		
		String[][] functionNamesAndExactValues = new String[][] {
			// 38sec brute, 1364ms sample
			{"sum", "17290"},   
			// 39sec brute,  723ms sample
			{"max", "1492"},    
			// 39sec brute,  619ms sample
			{"product", "45909468078488688718259880191530770000000000000000000000"} 
		};
		
		runSampleCompareToExact(10000, true, intensionalSet, functionNamesAndExactValues);		
	}
	
	@Test
	public void testIntegerIntervalExample3() {
		String intensionalSet = "{{(on I in 1..1000000) I^2 - I + 10  }}";
		
		String[][] functionNamesAndExactValues = new String[][] {
			// 33sec brute, 1147ms sample
			{"sum", "333333333343000000"},   
			// 32sec brute,  639ms sample
			{"max", "999999000010"}
			// Skip as its hard to get a product sample within an order
			// of magnitude of the exact value
			// {"product", null}
		};
		
		runSampleCompareToExact(10000, true, intensionalSet, functionNamesAndExactValues);		
	}
	
	@Test
	public void testRealIntervalExample1() {
		String intensionalSet = "{{(on I in [1;100]) I^2 - I + 10 : I > 20 and I < 40 }}";	
		String[][] functionNamesAndExactValues = new String[][] {
			{"sum", "16326"}, // Integrate[i^2 - i + 10, {i, 21, 39}] = integral_21^39 (i^2 - i + 10) di = 16326
			{"max", "1570"},  // Maximize[{i^2 - i + 10, 20 < i < 40}, {i}] = {{1570., {i -> 40.}}}
			// Skip, not sure how to compute using WolframAlpha and brute force method currently can't handle real intervals.
			// {"product", null}
		};
		
		runSampleCompareToExact(10, true, intensionalSet, functionNamesAndExactValues);		
	}
	
	@Test
	public void testCategoricalExample1() {
		updateContextWithIndexAndType("N", 
				new Categorical("Person", 100));
		
		String intensionalSet = "{{(on I in Person) if I != person1 then 20 else 30 : I != person2 }}";
		
		String[][] functionNamesAndExactValues = new String[][] {
			{"sum", "1990"}, 
			{"max", "30"}, 
			{"product", "950737950171172051122527404032000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"}
		};
		
		runSampleCompareToExact(100, true, intensionalSet, functionNamesAndExactValues);		
	}
	
	@Test
	public void testCategoricalExample2() {
		updateContextWithIndexAndType("N", 
				new Categorical("Person", 1000000));
		
		String intensionalSet = "{{(on I in Person) if I != person1 then 20 else 30 : I != person2 }}";
		
		String[][] functionNamesAndExactValues = new String[][] {
			// 42sec brute, 1186ms sample
			{"sum", "19999990"},
			// 26sec brute,  762ms sample
			{"max", "30"}, 
			// 25sec brute,  703ms sample
			{"product", "7.425492171971923688023442712229336E+1301028"}
		};
		
		runSampleCompareToExact(1, true, intensionalSet, functionNamesAndExactValues);		
	}
	
	@Test
	public void testFunctionExample1() {
		// measure =  1000000 : 10^6
		String intensionalSet = "{{(on f in 1..6 -> 1..10) f(1) + f(2) + f(3) + f(4) + f(5) + f(6) }}";
		
		String[][] functionNamesAndExactValues = new String[][] {
			// 485sec brute, 3033ms sample
			{"sum", "33000000" }, 
			// 478sec brute, 2314ms sample
			{"max", "60"}
			// Skip as its hard to get a product sample within an order
			// of magnitude of the exact value
			// {"product", null}
		};
		
		runSampleCompareToExact(10000, true, intensionalSet, functionNamesAndExactValues);		
	}
	
	@Test
	public void testSumOverFunctionEvaluatingArguments() {		
		//  NOTE: picks f_1(true)=1, f_2(true)=2; never samples f(false) because argument is always true
		runTest(2, false, "sum({{(on f in Boolean -> 0..3) product({{(on X in Boolean) f(not X or X) : true }}) : true }})", "40");
	}
	
	@Test
	public void testSumOverTupleV1() {		
		runTest(2, true, "sum({{(on T in (Boolean x 0..3)) 2 : true }})", "16");
	}
	
	public void runTest(int sampleSizeN, boolean alwaysSample, String expressionString, String expectedString) {
		Expression expected = parse(expectedString);
		Expression actual   = run(sampleSizeN, alwaysSample, expressionString);	
		Assert.assertEquals(expected, actual);
	}
	
	public void runBoundedTest(int sampleSizeN, boolean alwaysSample, String expressionString, String lowerBoundInclusiveString, String upperBoundInclusiveString) {
		Expression lowerBoundInclusive = context.getTheory().evaluate(parse(lowerBoundInclusiveString), context);
		Expression upperBoundInclusive = context.getTheory().evaluate(parse(upperBoundInclusiveString), context);
		Expression actual = run(sampleSizeN, alwaysSample, expressionString);
		System.out.println("upper bound  = "+upperBoundInclusive.doubleValue()+" (as rational="+toString(upperBoundInclusive)+")");
		System.out.println("      actual = "+actual.doubleValue()+" (as rational="+toString(actual)+")");
		System.out.println("lower bound  = "+lowerBoundInclusive.doubleValue()+" (as rational="+toString(lowerBoundInclusive)+")");
		Assert.assertTrue("actual ("+toString(actual)+") is not >= lower bound ("+toString(lowerBoundInclusive)+")", actual.compareTo(lowerBoundInclusive) >= 0);
		Assert.assertTrue("actual ("+toString(actual)+") is not <= upper bound ("+toString(upperBoundInclusive)+")", actual.compareTo(upperBoundInclusive) <= 0);
	}
	
	public void runSampleCompareToExact(int sampleSizeN, boolean alwaysSample, String intensionalSet, String[][] functionNamesAndExactValues) {
		for (int i = 0; i < functionNamesAndExactValues.length; i++) {
			String functionName = functionNamesAndExactValues[i][0];
			String exactValueString = functionNamesAndExactValues[i][1];
					
			String functionApplicationToIntensionalSet = functionName + "(" + intensionalSet + ")";
			Expression exactValue;
			if (exactValueString == null) {
				long start = System.currentTimeMillis();
				exactValue = bruteForceResult(functionApplicationToIntensionalSet);
				long end = System.currentTimeMillis();
				System.out.println("Brute force computed exact value = " + toString(exactValue)+", took " + (end - start) + " ms to compute");
			}
			else {
				exactValue = Expressions.parse(exactValueString);
			}			
			
			System.out.println("Sampling " + functionApplicationToIntensionalSet);
			long start = System.currentTimeMillis();
			Expression sampleValue = run(sampleSizeN, alwaysSample, functionApplicationToIntensionalSet);
			long end = System.currentTimeMillis();
			System.out.println("Sampled value = " + toString(sampleValue) + ", took " + (end - start) + " ms to compute");
			
			// We'll test if the sample value is within an order of magnitude of the exact value
			Expression upper = Expressions.makeSymbol(exactValue.rationalValue().multiply(10));
			Expression lower = Expressions.makeSymbol(exactValue.rationalValue().divide(10));
			
			Assert.assertTrue("sample (" + toString(sampleValue) + ") is not >= lower magnitude bound (" + toString(lower) + ") for exact value (" + toString(exactValue) + ")", 
					sampleValue.compareTo(lower) >= 0);
			Assert.assertTrue("sample (" + toString(sampleValue)+") is not <= upper magnitude bound (" + toString(lower) + ") for exact value (" + toString(exactValue) + ")", 
					sampleValue.compareTo(upper) <= 0);			
		}
	}
	
	private Expression run(int sampleSizeN, boolean alwaysSample, String expressionString) {
		SamplingCommonInterpreter interpreter = new SamplingCommonInterpreter(sampleSizeN, alwaysSample, random);
		
		Expression expression = parse(expressionString);
		if (expression.numberOfArguments() == 1 && Sets.isIntensionalSet(expression.get(0))) {
			IntensionalSet intensionalSet = (IntensionalSet) expression.get(0);
			IndexExpressionsSet indexExpressions = intensionalSet.getIndexExpressions();
			List<Expression> indices = IndexExpressions.getIndices(indexExpressions);

			if (indices.size() == 1) {
				Expression index = indices.get(0);
				Context intensionalSetContext = context.extendWith(indexExpressions);			
				// Ensure condition of correct type is created
				Type indexType = GrinderUtil.getTypeOfExpression(index, intensionalSetContext);
				
				SingleVariableConstraint singleVariableConstraint = null;
				if (indexType instanceof RealExpressoType || indexType instanceof RealInterval) {
					singleVariableConstraint = new SingleVariableLinearRealArithmeticConstraint(index, true, intensionalSetContext.getTheory());
				}
				else if (indexType instanceof IntegerExpressoType || indexType instanceof IntegerInterval) {
					singleVariableConstraint = new SingleVariableDifferenceArithmeticConstraint(index, true, intensionalSetContext.getTheory());
				}
				
				if (singleVariableConstraint != null) {
					singleVariableConstraint = singleVariableConstraint.conjoin(intensionalSet.getCondition(), intensionalSetContext);
					intensionalSet = intensionalSet.setCondition(singleVariableConstraint);	
					expression = expression.set(0, intensionalSet);
				}
			}
		}
		
		Expression result = interpreter.apply(expression, context);
		System.out.println("Evaluation with " + sampleSizeN + " samples of " + expressionString + " = " + result.doubleValue() + " (as rational="+toString(result)+")");
		
		return result;
	}
	
	private String toString(Expression rationalExpr) {		
		String result = rationalExpr.toString();	
		
		return result;
	}
	
	private Expression bruteForceResult(String expression) {
		BruteForceCommonInterpreter bruteForceInterpreter = new BruteForceCommonInterpreter();
		Expression result = bruteForceInterpreter.apply(Expressions.parse(expression), context);
		return result;
	}

	private void updateContextWithIndexAndType(String index, Type type) {
		context = (Context) GrinderUtil.extendRegistryWith(map(index, type.toString()), Arrays.asList(type), context);
	}
}
