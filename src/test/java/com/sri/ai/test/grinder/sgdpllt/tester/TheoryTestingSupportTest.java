/*
 * Copyright (c) 2016, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.test.grinder.sgdpllt.tester;

import java.util.Random;

import org.junit.Test;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.helper.GrinderUtil.INTEGER_TYPE;
import static com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheoryTestingSupport.TESTING_REAL_INTERVAL_TYPE;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;

import org.junit.Assert;

import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoArrayList;

public class TheoryTestingSupportTest {
	public static final IntegerInterval TESTING_INTEGER_INTERVAL_TYPE = new IntegerInterval("0..2");
	public static final Categorical TESTING_CATEGORICAL_TYPE = new Categorical("SomeType", 3, mapIntoArrayList(list("a", "b"), s -> makeSymbol(s)));
	public static final TupleType TESTING_TUPLE_TYPE = new TupleType(BOOLEAN_TYPE, BOOLEAN_TYPE);

	private Random seededRandom = new Random(2);

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
						new DifferenceArithmeticTheory(true, true), new LinearRealArithmeticTheory(true, true),
						new TupleTheory()));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(
				map("P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R", BOOLEAN_TYPE, "L", TESTING_TUPLE_TYPE, "test_diff/4", INTEGER_TYPE));
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
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R",
				BOOLEAN_TYPE, "gen_prop/1", BOOLEAN_TYPE, "test_prop/2", BOOLEAN_TYPE));

		testGeneralizedVariable(theoryTestingSupport, "test_prop", BOOLEAN_TYPE);

		//
		// Equality
		theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true, new EqualityTheory(true, true));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(
				map("X", TESTING_CATEGORICAL_TYPE, "Y", TESTING_CATEGORICAL_TYPE, "Z", TESTING_CATEGORICAL_TYPE,
						"gen_eq/1", TESTING_CATEGORICAL_TYPE, "test_eq/2", TESTING_CATEGORICAL_TYPE));

		testGeneralizedVariable(theoryTestingSupport, "test_eq", TESTING_CATEGORICAL_TYPE);

		//
		// DifferenceArithmetic
		theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true,
				new DifferenceArithmeticTheory(true, true));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("I", TESTING_INTEGER_INTERVAL_TYPE, "J",
				TESTING_INTEGER_INTERVAL_TYPE, "K", TESTING_INTEGER_INTERVAL_TYPE, "gen_diff/1",
				TESTING_INTEGER_INTERVAL_TYPE, "test_diff/2", TESTING_INTEGER_INTERVAL_TYPE));

		testGeneralizedVariable(theoryTestingSupport, "test_diff", TESTING_INTEGER_INTERVAL_TYPE);

		//
		// LinearRealArithmetic
		theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true,
				new LinearRealArithmeticTheory(true, true));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(
				map("X", TESTING_REAL_INTERVAL_TYPE, "Y", TESTING_REAL_INTERVAL_TYPE, "Z", TESTING_REAL_INTERVAL_TYPE,
						"gen_lr/1", TESTING_REAL_INTERVAL_TYPE, "test_lr/2", TESTING_REAL_INTERVAL_TYPE));

		testGeneralizedVariable(theoryTestingSupport, "test_lr", TESTING_REAL_INTERVAL_TYPE);

		//
		// Compound
		theoryTestingSupport = TheoryTestingSupport.make(seededRandom, true,
				new CompoundTheory(new PropositionalTheory(), new EqualityTheory(true, true),
						new DifferenceArithmeticTheory(true, true), new LinearRealArithmeticTheory(true, true)));
		theoryTestingSupport.setVariableNamesAndTypesForTesting(map("P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R",
				BOOLEAN_TYPE, "gen_prop/1", BOOLEAN_TYPE, "test_prop/2", BOOLEAN_TYPE, "X", TESTING_CATEGORICAL_TYPE,
				"Y", TESTING_CATEGORICAL_TYPE, "Z", TESTING_CATEGORICAL_TYPE, "gen_eq/1", TESTING_CATEGORICAL_TYPE,
				"test_eq/2", TESTING_CATEGORICAL_TYPE, "I", TESTING_INTEGER_INTERVAL_TYPE, "J",
				TESTING_INTEGER_INTERVAL_TYPE, "K", TESTING_INTEGER_INTERVAL_TYPE, "gen_diff/1",
				TESTING_INTEGER_INTERVAL_TYPE, "test_diff/2", TESTING_INTEGER_INTERVAL_TYPE, "U",
				TESTING_REAL_INTERVAL_TYPE, "V", TESTING_REAL_INTERVAL_TYPE, "W", TESTING_REAL_INTERVAL_TYPE,
				"gen_lr/1", TESTING_REAL_INTERVAL_TYPE, "test_lr/2", TESTING_REAL_INTERVAL_TYPE,
				"compound_boolean/4", BOOLEAN_TYPE,
				"compound_categorical/4", TESTING_CATEGORICAL_TYPE,
				"compound_int_interval/4", TESTING_INTEGER_INTERVAL_TYPE,
				"compound_real_interval/4", TESTING_REAL_INTERVAL_TYPE));
		
		testGeneralizedVariable(theoryTestingSupport, "compound_boolean", BOOLEAN_TYPE);
		testGeneralizedVariable(theoryTestingSupport, "compound_categorical", TESTING_CATEGORICAL_TYPE);
		testGeneralizedVariable(theoryTestingSupport, "compound_int_interval", TESTING_INTEGER_INTERVAL_TYPE);
		testGeneralizedVariable(theoryTestingSupport, "compound_real_interval", TESTING_REAL_INTERVAL_TYPE);
	}

	private void testGeneralizedVariable(TheoryTestingSupport theoryTestingSupport,
			String generalizeVariableFunctorName, Type expectedTargetType) {
		Context context = theoryTestingSupport.makeContextWithTestingInformation();

		Expression expectedFunctor = Expressions.parse(generalizeVariableFunctorName);
		Expression variableExpression;
		System.out.println("Generating for functor " + expectedFunctor + " of type "
				+ GrinderUtil.getType(expectedFunctor, context));
		do {
			String variable = theoryTestingSupport.pickTestingVariableAtRandom();
			variableExpression = Expressions.parse(variable);
			// Get until we have a generalized variable of the expected type
		} while (variableExpression.getFunctor() == null || !expectedFunctor.equals(variableExpression.getFunctor()));

		System.out.println("Generated generalized variable function application = " + variableExpression);

		Assert.assertTrue(theoryTestingSupport.getTheory().isSuitableFor(variableExpression, expectedTargetType));
		Assert.assertTrue(theoryTestingSupport.getTheory().isVariable(variableExpression, context));
		Assert.assertEquals(Expressions.parse(expectedTargetType.toString()),
				GrinderUtil.getType(variableExpression, context));
		
		// Ensure we can make a random atom using the generalized variable
		try {
			Expression randomAtom = theoryTestingSupport.makeRandomAtomOn(variableExpression.toString(), context);
			System.out.println("Generated random atom, using generalized variable function application, is = "+randomAtom);
		}
		catch (Error error) {
			// At the moment we permit this error.
			if (error.getMessage().equals("Random generation of linear real arithmetic not yet implemented.")) {
				System.out.println("--- makeRandomAtomOn() call for "+variableExpression.toString()+" not supported: "+error.getMessage());
			}
			else {
				throw error;
			}
		}
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
