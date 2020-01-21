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
package com.sri.ai.grinder.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.pickUniformly;
import static com.sri.ai.util.Util.pickUpToKElementsWithoutReplacement;

import java.util.ArrayList;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.theory.base.AbstractTheoryWithBinaryAtomsTestingSupport;
import com.sri.ai.util.math.BigIntegerNumber;
import com.sri.ai.util.math.BigIntegerNumberExact;
import com.sri.ai.util.math.Rational;

@Beta
public class LinearRealArithmeticTheoryTestingSupport extends AbstractTheoryWithBinaryAtomsTestingSupport {
	public static final RealInterval TESTING_REAL_INTERVAL_TYPE = new RealInterval("[0;4]");
	
	public LinearRealArithmeticTheoryTestingSupport(LinearRealArithmeticTheory theory, Random random) {
		super(theory, random);
		setVariableNamesAndTypesForTesting(map("X", TESTING_REAL_INTERVAL_TYPE, "Y", TESTING_REAL_INTERVAL_TYPE, "Z", TESTING_REAL_INTERVAL_TYPE));
	}
	
	/**
	 * Makes a linear real arithmetic random atom on variable.
	 */
	@Override
	public Expression makeRandomAtomOn(String mainVariable, Context context) {
		String mainVariableName = getVariableName(mainVariable);
		Type mainType = getTestingVariableType(mainVariable);
		
		ArrayList<String> variableNamesThatAreSubtypesOf = arrayListFrom(getVariableNamesWhoseTypesAreSubtypesOf(mainType));
		
		int numberOfOtherVariablesInAtom = getRandom().nextInt(variableNamesThatAreSubtypesOf.size()); //0,1,2...size-1
		int totalNumberOfVariablesInAtom = numberOfOtherVariablesInAtom + 1;

		ArrayList<String> otherVariablesForAtom = pickUpToKElementsWithoutReplacement(
				variableNamesThatAreSubtypesOf, numberOfOtherVariablesInAtom, otherName -> !otherName.equals(mainVariableName), getRandom());
		
		int numberOfOtherVariablesOnLeftHandSide = getRandom().nextInt(totalNumberOfVariablesInAtom); //0,1,2...size
		
		ArrayList<Expression> leftHandSideAlgebraicTerms = constructAlgebraicTermsForFormula(mainVariable,otherVariablesForAtom, numberOfOtherVariablesOnLeftHandSide, true);
		ArrayList<Expression> rightHandSideAlgebraicTerms = constructAlgebraicTermsForFormula(mainVariable,otherVariablesForAtom, numberOfOtherVariablesOnLeftHandSide, false);
		int whichSideOfTheEquationIfAnyToPutConstant = getRandom().nextInt(3);
		if(whichSideOfTheEquationIfAnyToPutConstant == 0) {
			leftHandSideAlgebraicTerms.add(makeRealValuedConstantExpression());
		}
		else if(whichSideOfTheEquationIfAnyToPutConstant == 1) {
			rightHandSideAlgebraicTerms.add(makeRealValuedConstantExpression());
		}

		Expression leftHandSide;
		Expression rightHandSide;
		if(leftHandSideAlgebraicTerms.size() > 1) {
			leftHandSide = apply("+", leftHandSideAlgebraicTerms);
		}
		else {
			leftHandSide = leftHandSideAlgebraicTerms.get(0);
		}
		if(rightHandSideAlgebraicTerms.size() > 1) {
			rightHandSide = apply("+", rightHandSideAlgebraicTerms);
		}
		else {
			rightHandSide = rightHandSideAlgebraicTerms.get(0);
		}
		
		String functor = pickUniformly(getTheoryFunctors(), getRandom());
		Expression randomAtom = apply(functor, leftHandSide, rightHandSide);	
		
		return randomAtom;
	}
	
	private ArrayList<Expression> constructAlgebraicTermsForFormula(String mainVariable, ArrayList<String> otherVariablesForAtom,
			int numberOfOtherVariablesOnLeftHandSide, boolean constructingLeftHandSide) {
		ArrayList<Expression> constructedTerms = new ArrayList<>();
		int inclusiveLowerBound;
		int exclusiveUpperBound;
		
		if(constructingLeftHandSide) {
			constructedTerms.add(makeVariableTermWithRandomCoefficient(mainVariable));
			inclusiveLowerBound = 0;
			exclusiveUpperBound = numberOfOtherVariablesOnLeftHandSide;
			for(int i = inclusiveLowerBound; i < exclusiveUpperBound; ++i) {
				String variableSymbol = otherVariablesForAtom.get(i);
				constructedTerms.add(makeVariableTermWithRandomCoefficient(variableSymbol));
			}
		}
		else {
			inclusiveLowerBound = numberOfOtherVariablesOnLeftHandSide;
			exclusiveUpperBound = otherVariablesForAtom.size();
			if(inclusiveLowerBound == exclusiveUpperBound) {
				constructedTerms.add(makeSymbol(0));
			}
			else {
				for(int i = inclusiveLowerBound; i < exclusiveUpperBound; ++i) {
					String variableSymbol = otherVariablesForAtom.get(i);
					constructedTerms.add(makeVariableTermWithRandomCoefficient(variableSymbol));
				}
			}
		}
		
		return constructedTerms;
	}

	private Expression makeVariableTermWithRandomCoefficient(String variableSymbol) {
		Expression result;
		Expression variable = parse(variableSymbol);
		if(getRandom().nextBoolean()) {
			Expression constantExpression = makeRealValuedConstantExpression();
			Expression variableTermWithCoefficient = apply("*", variable, constantExpression);
			result = variableTermWithCoefficient;
		}
		else {
			result = variable;
		}
		return result;
	}

	public Expression makeRealValuedConstantExpression() {
		BigIntegerNumber num = new BigIntegerNumberExact( getRandom().nextInt(Integer.MAX_VALUE) );
		BigIntegerNumber den = new BigIntegerNumberExact( 1 + getRandom().nextInt(Integer.MAX_VALUE-1) );
		Expression constantExpression = makeSymbol(new Rational(num,den));
		return constantExpression;
	}
}
