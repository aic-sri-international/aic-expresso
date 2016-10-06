package com.sri.ai.test.grinder.sgdpllt.tester;

import java.util.Random;

import org.junit.Test;

import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.helper.GrinderUtil.INTEGER_TYPE;
import static com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheoryTestingSupport.TESTING_INTEGER_INTERVAL_TYPE;
import static com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheoryTestingSupport.TESTING_CATEGORICAL_TYPE;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;

import org.junit.Assert;

import static com.sri.ai.util.Util.map;

public class TheoryTestingSupportTest {

	private Random seededRandom = new Random(1);

	@Test(expected = IllegalArgumentException.class)
	public void testInterpretedPropositionalLogicFunctorOnStandaloneTheory() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true,
				new PropositionalTheory());

		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("and/2", BOOLEAN_TYPE));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInterpretedFunctorOnStandaloneTheory() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true,
				new DifferenceArithmeticTheory(true, true));

		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("</2", INTEGER_TYPE));
	}

	@Test(expected = IllegalArgumentException.class)
	public void testInterpretedFunctorOnCompoundTheory() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true, new CompoundTheory(
				new PropositionalTheory(), new EqualityTheory(true, true), new DifferenceArithmeticTheory(true, true)));

		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("</2", INTEGER_TYPE));
	}

	@Test
	public void testArgumentTypesAutoAssigned() {
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true,
				new DifferenceArithmeticTheory(true, true));

		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("test_diff/2", INTEGER_TYPE));
		testFunctionType(theoryTestingSupport.getVariableNamesAndTypesForTesting().get("test_diff"));

		theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true,
				new CompoundTheory(new PropositionalTheory(), new EqualityTheory(true, true),
						new DifferenceArithmeticTheory(true, true), new LinearRealArithmeticTheory(true, true)));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(
				map("P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R", BOOLEAN_TYPE, "test_diff/4", INTEGER_TYPE));
		testFunctionType(theoryTestingSupport.getVariableNamesAndTypesForTesting().get("test_diff"));
	}

	@Test
	public void testPickGeneralizedTestingVariable() {
		for (int i = 0; i < 10; i++) {
			pickGeneralizedTestingVariable();
		}
	}
	
	void pickGeneralizedTestingVariable() {
		//
		// Propositional
		TheoryTestingSupport theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true,
				new PropositionalTheory());
		theoryTestingSupport.setVariableNamesAndTypesForTesting(
				map("P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R", BOOLEAN_TYPE, "gen_prop/1", BOOLEAN_TYPE, "test_prop/2", BOOLEAN_TYPE));

		testGeneralizedVariable(theoryTestingSupport, "test_prop", BOOLEAN_TYPE);

		//
		// Equality
		theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true, new EqualityTheory(true, true));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("X", TESTING_CATEGORICAL_TYPE, "Y",
				TESTING_CATEGORICAL_TYPE, "Z", TESTING_CATEGORICAL_TYPE, "test_eq/2", TESTING_CATEGORICAL_TYPE));

		testGeneralizedVariable(theoryTestingSupport, "test_eq", TESTING_CATEGORICAL_TYPE);

		//
		// DifferenceArithmetic
		theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true,
				new DifferenceArithmeticTheory(true, true));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(
				map("I", TESTING_INTEGER_INTERVAL_TYPE, "J", TESTING_INTEGER_INTERVAL_TYPE, "K",
						TESTING_INTEGER_INTERVAL_TYPE, "test_diff/2", TESTING_INTEGER_INTERVAL_TYPE));

		testGeneralizedVariable(theoryTestingSupport, "test_diff", TESTING_INTEGER_INTERVAL_TYPE);
	}

	private void testGeneralizedVariable(TheoryTestingSupport theoryTestingSupport,
			String generalizeVariableFunctorName, Type expectedTargetType) {
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
		
		Expression expectedFunctor = Expressions.parse(generalizeVariableFunctorName);
		Expression variableExpression;
		System.out.println("Generating for functor "+expectedFunctor+" of type "+GrinderUtil.getType(expectedFunctor, context));
		do {
			String variable = theoryTestingSupport.pickTestingVariableAtRandom();			
			variableExpression = Expressions.parse(variable);
			// Get until we have a generalized variable with a nested
			// generalized variable argument.
		} while (variableExpression.getFunctor() == null || !expectedFunctor.equals(variableExpression.getFunctor()));
		
		System.out.println("Generated generalized variable function application = " + variableExpression);
	
		Assert.assertTrue(theoryTestingSupport.getTheory().isSuitableFor(variableExpression, expectedTargetType));
		Assert.assertTrue(theoryTestingSupport.getTheory().isVariable(variableExpression, context));
		Assert.assertEquals(Expressions.parse(expectedTargetType.toString()),
				GrinderUtil.getType(variableExpression, context));
	}

	private void testFunctionType(Type type) {
		System.out.println("Function type to test = " + type);
		Assert.assertTrue(type instanceof FunctionType);
		FunctionType diffFuncType = (FunctionType) type;
		for (int i = 0; i < diffFuncType.getArity(); i++) {
			Assert.assertNotNull(diffFuncType.getArgumentTypes().get(i));
		}
	}
}
