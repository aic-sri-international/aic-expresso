/*
 * Copyright (c) 2013, SRI International
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
package com.sri.ai.grinder.sgdpllt.tester;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.util.Util;

@Beta
public interface TheoryTestingSupport extends Theory {

	/** Sets variables to be used in randomly generated literals. */
	void setVariableNamesAndTypesForTesting(Map<String, Type> variableNamesForTesting);

	/** Gets variables to be used in randomly generated literals. */
	Map<String, Type> getVariableNamesAndTypesForTesting();

	/**
	 * Returns the variable names as returned by
	 * {@link #getVariableNamesAndTypesForTesting()} as an array list (this is
	 * cached and updated as needed).
	 * 
	 * @return
	 */
	List<String> getVariableNamesForTesting();

	default ArrayList<Expression> getVariablesForTesting() {
		List<String> variableNames = getVariableNamesForTesting();
		ArrayList<Expression> result = mapIntoArrayList(variableNames, s -> parse(s));
		return result;
	}

	/**
	 * Returns a set of types appropriate for testing this theory; note that
	 * this set may include types not related to any of the testing variables.
	 */
	Collection<Type> getTypesForTesting();
	
	/**
	 * Picks one of the testing variables returned by {@link #getTestingVariables()}
	 * with uniform probability.
	 * @param random
	 * @return
	 */
	default String pickTestingVariableAtRandom(Random random) {
		String result = Util.pickUniformly(getVariableNamesAndTypesForTesting().keySet(), random);
		return result;
	}
	
	/**
	 * Returns a random atom in this theory on a given variable.
	 * This is useful for making random constraints for correctness and performance testing.
	 * @param random a random generator
	 * @param context a context
	 */
	Expression makeRandomAtomOn(String variable, Random random, Context context);
	
	/**
	 * Same as {@link #makeRandomAtomOn(String, Random, Context),
	 * but applied randomly to one of the test variables.
	 */
	default Expression makeRandomAtom(Random random, Context context) {
		String variableToBeUsed = Util.pickUniformly(getVariableNamesAndTypesForTesting().keySet().iterator(), random);
		Expression result = makeRandomAtomOn(variableToBeUsed, random, context);
		return result;
	}
	
	/**
	 * Same as {@link #makeRandomAtomOn(String, Random, Context) for testing variable
	 * (returned by {@link #pickTestingVariableAtRandom()}).
	 */
	default Expression makeRandomAtomOnTestingVariable(Random random, Context context) {
		Expression result = makeRandomAtomOn(pickTestingVariableAtRandom(random), random, context);
		return result;
	}

	/**
	 * @param variable
	 * @param random
	 * @param context
	 * @return
	 */
	default Expression makeRandomLiteralOn(String variable, Random random, Context context) {
		Expression atom = makeRandomAtomOn(variable, random, context);
		Expression literal = random.nextBoolean()? atom : getLiteralNegation(atom, context);
		return literal;
	}

	/**
	 * Same as {@link #makeRandomLiteralOn(String, Random, Context),
	 * but applied randomly to one of the testing variables.
	 */
	default Expression makeRandomLiteral(Random random, Context context) {
		String variableToBeUsed = pickTestingVariableAtRandom(random);
		Expression result = makeRandomLiteralOn(variableToBeUsed, random, context);
		return result;
	}

	Context extendWithTestingInformation(Context context);

	default Context makeContextWithTestingInformation() {
		return extendWithTestingInformation(new TrueContext(this));
	}

	// /** Samples a uniquely named constant of given appropriate for testing
	// this theory. */
	// Expression sampleUniquelyNamedConstantsForTesting(Type type);
	//
	/// **
	// * Sets an iterable of uniquely named constants for given type appropriate
	// for testing this theory.
	// * Note that all types must have testing uniquely named constants
	// associated with them.
	// * If you want a type to simply use {@link Type#sampleConstant(Random)},
	// * use methods {@link #setUseTypeUniquelyNamedConstantSampling(Type,
	// boolean)}
	// * or {@link
	// #setUseTypeUniquelyNamedConstantSamplingForAllTypes(boolean)}.
	// */
	// void setUniquelyNamedConstantsForTesting(Type type, Iterable<Expression>
	// newUniquelyNamedConstantsForTesting);
	//
	/// **
	// * Indicates whether the sampling of a type's uniquely named constant for
	// testing must
	// * use the types {@link Type#sampleConstant(Random)}.
	// * @return
	// */
	// boolean getUseTypeUniquelyNamedConstantSampling(Type type);
	//
	/// **
	// * Sets the flag indicating whether the sampling of a type's uniquely
	// named constant for testing must
	// * use the types {@link Type#sampleConstant(Random)}.
	// * @return
	// */
	// void setUseTypeUniquelyNamedConstantSampling(Type type, boolean
	// newValue);
	//
	/// **
	// * Indicates whether the sampling of <i>all</i> types uniquely named
	// constant for testing must
	// * use the types {@link Type#sampleConstant(Random)}.
	// * @return
	// */
	// boolean getUseTypeUniquelyNamedConstantSamplingForAllTypes();
	//
	/// **
	// * Sets the flag indicating whether the sampling of <i>all</i> types
	// uniquely named constant for testing must
	// * use the types {@link Type#sampleConstant(Random)}.
	// * @return
	// */
	// void setUseTypeUniquelyNamedConstantSamplingForAllTypes(boolean
	// newValue);
	//
}
