package com.sri.ai.test.grinder.sgdpllt.tester;

import java.util.Random;

import org.junit.Test;

import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.helper.GrinderUtil.INTEGER_TYPE;

import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.FunctionType;
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
		
		theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true, new CompoundTheory(
				new PropositionalTheory(), new EqualityTheory(true, true), new DifferenceArithmeticTheory(true, true), new LinearRealArithmeticTheory(true, true)));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R", BOOLEAN_TYPE, "test_diff/4", INTEGER_TYPE));		
		testFunctionType(theoryTestingSupport.getVariableNamesAndTypesForTesting().get("test_diff"));
	}
	
	private void testFunctionType(Type type) {
		System.out.println("Function type to test = "+type);
		Assert.assertTrue(type instanceof FunctionType);
		FunctionType diffFuncType = (FunctionType) type;
		for (int i = 0; i < diffFuncType.getArity(); i++) {
			Assert.assertNotNull(diffFuncType.getArgumentTypes().get(i));
		}
	}
}
