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
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.expresso.type.RealExpressoType;
import com.sri.ai.expresso.type.RealInterval;
import com.sri.ai.expresso.type.TupleType;
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
import com.sri.ai.grinder.sgdpllt.theory.function.BruteForceFunctionTheory;
import com.sri.ai.grinder.sgdpllt.theory.function.BruteForceFunctionTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheory;
import com.sri.ai.grinder.sgdpllt.theory.tuple.TupleTheoryTestingSupport;
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
	
	/** 
	 * Sets variables as is (no changes to be made internally) to be used in randomly generated literals.
	 * Intended to be called from compound theories. 
	 */
	void setVariableNamesAndTypesAsIsForTesting(Map<String, Type> variableNamesForTesting);

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
		String result = pickTestingVariableAtRandom(this);
		return result;
	}
	
	default String pickTestingVariableAtRandom(TheoryTestingSupport globalTheoryTestingSupport) {
		Set<String> variableNames = globalTheoryTestingSupport.getVariableNamesAndTypesForTesting().keySet();
		if (variableNames.isEmpty()) {
			throw new Error("There are no testing variables to select from (perhaps the ones provided are all generalized variables and generalized variables are disabled?). Theory testing support is " + globalTheoryTestingSupport);
		}
		String variableName = Util.pickUniformly(variableNames, getRandom());
		Type type = globalTheoryTestingSupport.getVariableNamesAndTypesForTesting().get(variableName);	
		String result;
		if (type instanceof FunctionType) {
			// In this case we want to generate a function application.
			FunctionType functionType = (FunctionType) type;			
			result = makeGeneralizedVariableArgumentAtRandom(variableName, type, functionType.getCodomain(), globalTheoryTestingSupport);
		}
		else {
			result = makeGeneralizedVariableArgumentAtRandom(variableName, type, type, globalTheoryTestingSupport);
		}
		
		return result;
	}
	
	default List<String> pickGeneralizedTestingVariableArgumentsAtRandom(List<Type> argumentTypes, TheoryTestingSupport globalTheoryTestingSupport) {
		List<String> result = new ArrayList<>();
		for (int i = 0; i < argumentTypes.size(); i++) {
			Type argType = argumentTypes.get(i);
			result.add(pickGeneralizedTestingVariableArgumentAtRandom(argType, variableName -> true, globalTheoryTestingSupport));
		}
		return result;
	}
	
	default String pickGeneralizedTestingVariableArgumentAtRandom(Type argumentType, Predicate<String> variableNameFilter, TheoryTestingSupport globalTheoryTestingSupport) {
		String result;
		List<String> variableNamesThatAreSubTypes = globalTheoryTestingSupport.getVariableNamesWhoseTypesAreSubtypesOf(argumentType)
				.stream()
				.filter(variableNameFilter)
				.collect(Collectors.toList());
		
		if (variableNamesThatAreSubTypes.size() == 0 
				||
				// if we should generate a constant, under the conditions that the argument type is not a function type 
				// or tuple type or real expresso type or integer/real intervals with non constant bounds.
				// None of which support sampling uniquely named constants.
				// TODO: have Type have a method indicating the ability to sample uniquely named constants.
				(getRandom().nextBoolean() && 
						!(argumentType instanceof FunctionType) && 
						!(argumentType instanceof TupleType) &&
						!(argumentType instanceof RealExpressoType) && 
						!(argumentType instanceof RealInterval && !((RealInterval)argumentType).boundsAreConstants()) &&
						!(argumentType instanceof IntegerInterval && !((IntegerInterval)argumentType).boundsAreConstants())
						)) {
			// We will use a constant in this instance
			result = argumentType.sampleUniquelyNamedConstant(getRandom()).toString();			
		}
		else {
			String variableName = Util.pickUniformly(variableNamesThatAreSubTypes, getRandom());
			result = makeGeneralizedVariableArgumentAtRandom(variableName, getVariableNamesAndTypesForTesting().get(variableName), argumentType, globalTheoryTestingSupport);
		}
		return result;
	}
	
	default String makeGeneralizedVariableArgumentAtRandom(String variableName, Type variableType, Type targetType, TheoryTestingSupport globalTheoryTestingSupport) {
		String result = variableName;
		// If the type of the variable is a function type and its codomain is a subtype of the target
		// type then we want to generate a function application of the variable name's type.
		if (variableType instanceof FunctionType) {
			FunctionType functionType = (FunctionType) variableType;
			if (GrinderUtil.isTypeSubtypeOf(functionType.getCodomain(), targetType)) {
				List<String> args = pickGeneralizedTestingVariableArgumentsAtRandom(functionType.getArgumentTypes(), globalTheoryTestingSupport);
				List<Expression> expArgs = args.stream().map(strArg -> Expressions.parse(strArg)).collect(Collectors.toList());
				result = Expressions.apply(result, expArgs).toString();
			}
		}
		
		// Restrict extending the generalized variable to simple variables and function applications
		// as it does not make sense to try to extend a functor argument.
		if (!(targetType instanceof FunctionType)) {
			result = extendGeneralizedVariableArgument(result, globalTheoryTestingSupport);
		}
		
		return result;
	}
	
	/**
	 * A hook method for extending the types of generalized (non-functor) variable arguments
	 * that can be generated. For example, if the function application 'f(y)'
	 * returned a real, the LinearRealAritmeticTheoryTestingSupport could extend
	 * this into a general formula something like:<br>
	 * 
	 * <pre>
	 * 2 * x + 3 * f(y) + 1
	 * </pre>
	 * 
	 * @param variable
	 *            the variable to be optionally extended.
	 * @param globalTheoryTestingSupport
	 *            the global theory in which the extension is to occur.
	 * @return a possibly extended representation of the input variable.
	 */
	default String extendGeneralizedVariableArgument(String variable, TheoryTestingSupport globalTheoryTestingSupport) {
		String result = variable; // by default we don't extend
		return result;
	}
	
	/**
	 * Get variable names whose types are subtypes of a given type.
	 * 
	 * @param type
	 *            the type to be checked.
	 * @return a list of variable names whose types are subtypes of the given
	 *         type.
	 */
	default List<String> getVariableNamesWhoseTypesAreSubtypesOf(Type type) {
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
	 * Given a variable (a symbolic name or function application) return its
	 * corresponding identifying name.
	 * 
	 * @param variable
	 *            the variable whose identifying name is to be retrieved.
	 * @return the identifying name for the variable.
	 */
	default String getVariableName(String variable) {
		String result;
		Expression variableExpresison = parse(variable);
		Expression functor = variableExpresison.getFunctor();
		if (functor == null) {
			result = variable;
		}
		else {
			result = functor.toString();
		}
		return result;
	}
	
	/**
	 * Get the type associated with the given testing variable.
	 * 
	 * @param variable
	 *            the testing variable whose type is to be returned.
	 * @return the type of the given testing variable.
	 */
	default Type getTestingVariableType(String variable) {
		String variableName = getVariableName(variable);		
		Type result = getVariableNamesAndTypesForTesting().get(variableName);
		// We need to check if the variable is a function application
		// because if it is we need to use the codomain of its function type
		// as the type of the testing variable.
		Expression variableExpresison = parse(variable);
		Expression functor = variableExpresison.getFunctor();
		if (functor != null) {
			result = ((FunctionType)result).getCodomain();
		}
		return result;
	}
	
	/**
	 * Returns a random atom in this theory on a given variable.
	 * This is useful for making random constraints for correctness and performance testing.
	 * @param variable the name of the variable to make the random atom on.
	 * @param context a context
	 */
	default Expression makeRandomAtomOn(String variable, Context context) {
		Expression result = makeRandomAtomOn(variable, context, this);
		return result;
	}
	
	Expression makeRandomAtomOn(String variable, Context context, TheoryTestingSupport globalTheoryTestingSupport);

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
		else if (theory instanceof BruteForceFunctionTheory) {
			result = new BruteForceFunctionTheoryTestingSupport((BruteForceFunctionTheory) theory, random);
		}
		else if (theory instanceof TupleTheory) {
			result = new TupleTheoryTestingSupport((TupleTheory) theory, random, generalizedVariableSupportEnabled);
		}
		else {
			throw new UnsupportedOperationException(""+theory.getClass().getSimpleName()+" currently does not have testing support in place.");
		}
		
		return result;
	}
}
