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
package com.sri.ai.grinder.sgdpllt.tester;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheory;
import com.sri.ai.grinder.sgdpllt.theory.equality.EqualityTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheoryTestingSupport;
import com.sri.ai.util.Util;

@Beta
public interface TheoryTestingSupport {
	boolean GENERALIZED_VARIABLES_SUPPORTED_BY_DEFAULT = false;
	
	/**
	 * 
	 * @return the theory testing support is being provided for.
	 */
	Theory getTheory();
	
	/**
	 * 
	 * @return get the Random being used for testing support.
	 */
	Random getRandom();

	/**
	 * Set the random to be used for testing support.
	 * 
	 * @param random
	 *            the random to use.
	 */
	void setRandom(Random random);
	
	/**
	 * 
	 * @return true is generalized variables are to be supported, false otherwise.
	 */
	boolean isGeneralizedVariableSupportEnabled();

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
	 * 
	 * @return
	 */
	default String pickTestingVariableAtRandom() {
		String variableName = Util.pickUniformly(getVariableNamesAndTypesForTesting().keySet(), getRandom());
		Type type = getVariableNamesAndTypesForTesting().get(variableName);	
		String result;
		if (type instanceof FunctionType) {
			FunctionType functionType = (FunctionType) type;
			result = makeGeneralizedVariableArgumentAtRandom(variableName, type, functionType.getCodomain());
		}
		else {
			result = makeGeneralizedVariableArgumentAtRandom(variableName, type, type);
		}
		
		return result;
	}
	
	default List<String> pickGeneralizedTestingVariableArgumentsAtRandom(List<Type> argumentTypes) {
		List<String> result = new ArrayList<>();
		for (int i = 0; i < argumentTypes.size(); i++) {
			Type argType = argumentTypes.get(i);
			List<String> variableNamesThatAreSubTypes = getVariableNamesThatAreSubtypesOf(argType);			
			if (variableNamesThatAreSubTypes.size() == 0 || (getRandom().nextBoolean() && !(argType instanceof FunctionType))) {
				// We will use a constant in this instance
				result.add(argType.sampleUniquelyNamedConstant(getRandom()).toString());			
			}
			else {
				String variableName = Util.pickUniformly(variableNamesThatAreSubTypes, getRandom());
				result.add(makeGeneralizedVariableArgumentAtRandom(variableName, getVariableNamesAndTypesForTesting().get(variableName), argType));
			}
		}
		return result;
	}
	
	// TODO - delegate to appropriate subclass?
	default String makeGeneralizedVariableArgumentAtRandom(String variableName, Type variableType, Type targetType) {
		String result = variableName;
		// If the type of the variable is a function type and its codomain is a subtype of the target
		// type then we want to generate a function application of the variable name's type.
		if (variableType instanceof FunctionType) {
			FunctionType functionType = (FunctionType) variableType;
			if (GrinderUtil.isTypeSubtypeOf(functionType.getCodomain(), targetType)) {
				List<String> args = pickGeneralizedTestingVariableArgumentsAtRandom(functionType.getArgumentTypes());
				List<Expression> expArgs = args.stream().map(strArg -> Expressions.parse(strArg)).collect(Collectors.toList());
				result = Expressions.apply(result, expArgs).toString();
			}
		}
		
		return result;
	}
	
	default List<String> getVariableNamesThatAreSubtypesOf(Type type) {
		List<String> result =
				getVariableNamesAndTypesForTesting().entrySet().stream()
					.filter(entry -> {
						boolean include;
						// When the type is a non-function type but the entry's type is a FunctionType
						// we want to use the entry type's codomain as we can use a function application
						// of this variable in this instance.
						if (!(type instanceof FunctionType) && entry.getValue() instanceof FunctionType) {
							include = GrinderUtil.isTypeSubtypeOf(((FunctionType)entry.getValue()).getCodomain(), type);
						}
						else {
							include = GrinderUtil.isTypeSubtypeOf(entry.getValue(), type);
						}						
						return include;
					})
					.map(entry -> entry.getKey())
					.collect(Collectors.toList());		
		return result;
	}
	
	/**
	 * Returns a random atom in this theory on a given variable.
	 * This is useful for making random constraints for correctness and performance testing.
	 * @param variable the name of the variable to make the random atom on.
	 * @param context a context
	 */
	Expression makeRandomAtomOn(String variable, Context context);

	/**
	 * @param variable
	 * @param context
	 * @return
	 */
	default Expression makeRandomLiteralOn(String variable, Context context) {
		Expression atom = makeRandomAtomOn(variable, context);
		Expression literal = getRandom().nextBoolean()? atom : getTheory().getLiteralNegation(atom, context);
		return literal;
	}

	/**
	 * Same as {@link #makeRandomLiteralOn(String, Context),
	 * but applied randomly to one of the testing variables.
	 */
	default Expression makeRandomLiteral(Context context) {
		String variableToBeUsed = pickTestingVariableAtRandom();
		Expression result = makeRandomLiteralOn(variableToBeUsed, context);
		return result;
	}

	Context extendWithTestingInformation(Context context);

	default Context makeContextWithTestingInformation() {
		return extendWithTestingInformation(new TrueContext(getTheory()));
	}
	
	static TheoryTestingSupport make(Theory theory) {
		TheoryTestingSupport result = make(new Random(), theory);
		return result;
	}
	
	static TheoryTestingSupport make(Random random, Theory theory) {
		TheoryTestingSupport result = make(random, GENERALIZED_VARIABLES_SUPPORTED_BY_DEFAULT, theory);
		return result;
	}
	
	static TheoryTestingSupport make(TheoryTestingSupport... theoryTestingSupports) {
		TheoryTestingSupport result = make(new Random(), theoryTestingSupports);
		return result;
	}
	
	static TheoryTestingSupport make(Random random, TheoryTestingSupport... theoryTestingSupports) {
		TheoryTestingSupport result = make(random, GENERALIZED_VARIABLES_SUPPORTED_BY_DEFAULT, theoryTestingSupports);
		return result;
	}
	
	static TheoryTestingSupport make(Random random, boolean generalizedVariableSupportEnabled, TheoryTestingSupport... theoryTestingSupports) {
		TheoryTestingSupport result = new CompoundTheoryTestingSupport(random, generalizedVariableSupportEnabled, theoryTestingSupports);
		return result;
	}
	
	static TheoryTestingSupport make(Random random, boolean generalizedVariableSupportEnabled, Theory theory) {
		TheoryTestingSupport result;
		
		if (theory instanceof CompoundTheory) {
			result = new CompoundTheoryTestingSupport((CompoundTheory) theory, random, generalizedVariableSupportEnabled);
		}
		else if (theory instanceof DifferenceArithmeticTheory) {
			result = new DifferenceArithmeticTheoryTestingSupport((DifferenceArithmeticTheory) theory, random, generalizedVariableSupportEnabled);
		}
		else if (theory instanceof EqualityTheory) {
			result = new EqualityTheoryTestingSupport((EqualityTheory) theory, random, generalizedVariableSupportEnabled);
		}
		else if (theory instanceof LinearRealArithmeticTheory) {
			result = new LinearRealArithmeticTheoryTestingSupport((LinearRealArithmeticTheory) theory, random, generalizedVariableSupportEnabled);
		}
		else if (theory instanceof PropositionalTheory) {
			result = new PropositionalTheoryTestingSupport((PropositionalTheory) theory, random, generalizedVariableSupportEnabled);
		}
		else {
			throw new UnsupportedOperationException(""+theory.getClass().getSimpleName()+" currently does not have testing support in place.");
		}
		
		return result;
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
