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
package com.sri.ai.expresso.type;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.TUPLE_TYPE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.FUNCTION_TYPE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.StringJoiner;
import java.util.concurrent.atomic.AtomicInteger;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.sgdpllt.core.DefaultRegistry;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.math.Rational;

/**
 * Represents function types.
 * 
 * @author oreilly
 *
 */
@Beta
public class FunctionType extends AbstractType {
	private static final long serialVersionUID = 1L;

	private Type codomain;
	private List<Type> argumentTypes;
	//
	private String cachedString;
	// NOTE: Following used for iteration logic
	private Registry cachedIterateRegistry;
	private List<Expression> codomainVariables;
	private Expression genericLambda;

	public FunctionType(Type codomain, Type... argumentTypes) {
		this.codomain = codomain;
		this.argumentTypes = Collections.unmodifiableList(Arrays.asList(argumentTypes));
	}

	public Type getCodomain() {
		return codomain;
	}

	public int getArity() {
		return getArgumentTypes().size();
	}

	public List<Type> getArgumentTypes() {
		return argumentTypes;
	}

	@Override
	public String getName() {
		return toString();
	}

	@Override
	public Iterator<Expression> iterator() {
		if (!(getCodomain().isDiscrete() && getArgumentTypes().stream().allMatch(Type::isFinite))) {
			throw new Error("Only function types with finite argument types and a discrete codomain can be enumerated.");
		}
		
		if (cachedIterateRegistry == null) {
			// Pre-compute
			cachedIterateRegistry = new DefaultRegistry();		
			int numCodomainValues = argumentTypes.stream()
					.map(Type::cardinality)
					.map(Expression::rationalValue)						
					.reduce(Rational.ONE, Rational::multiply)
					.intValue();
			cachedIterateRegistry = cachedIterateRegistry.makeCloneWithAddedType(getCodomain());
			
			Expression codomainTypeExpression = parse(getCodomain().getName());
			codomainVariables = new ArrayList<>(numCodomainValues);
			Map<Expression, Expression> symbolsAndTypes = new LinkedHashMap<>();
			for (int i = 0; i < numCodomainValues; i++) {
				Expression coDomainVariableI = makeSymbol("C" + (i + 1));
				codomainVariables.add(coDomainVariableI);
				symbolsAndTypes.put(coDomainVariableI, codomainTypeExpression);
			}
			
			List<Expression> argVariables = new ArrayList<>();
			for (int i = 0; i < getArgumentTypes().size(); i++) {
				cachedIterateRegistry = cachedIterateRegistry.makeCloneWithAddedType(getArgumentTypes().get(i));
				argVariables.add(makeSymbol("A" + (i + 1)));
				symbolsAndTypes.put(argVariables.get(i), parse(getArgumentTypes().get(i).getName()));
			}
			cachedIterateRegistry = cachedIterateRegistry.setSymbolsAndTypes(symbolsAndTypes);
			
			StringJoiner lambdaApplicationPrefix = new StringJoiner(", ", "(lambda ", " : ");
			for (Expression argVar : argVariables) {
				lambdaApplicationPrefix.add(argVar + " in " + symbolsAndTypes.get(argVar));
			}
			
			AssignmentsIterator assignmentsIterator = new AssignmentsIterator(argVariables, cachedIterateRegistry);
			StringJoiner lambdaApplicationBody = new StringJoiner(" else ", "", ")");
			AtomicInteger counter = new AtomicInteger(0);
			assignmentsIterator.forEachRemaining(assignment -> {
				if (counter.incrementAndGet() != numCodomainValues) { 
					StringJoiner condition = new StringJoiner(" and ", "if ", " then C" + counter);
					for (int i = 0; i < argVariables.size(); i++) {
						Expression argVariable = argVariables.get(i);
						condition.add(argVariable + " = " + assignment.get(argVariable));
					}
					lambdaApplicationBody.add(condition.toString());
				}
				else {
					lambdaApplicationBody.add("C" + numCodomainValues);
				}
			});
			
			genericLambda = parse(lambdaApplicationPrefix.toString() + lambdaApplicationBody.toString());
		}
		
		return FunctionIterator.functionIterator(new AssignmentsIterator(codomainVariables, cachedIterateRegistry), assignment -> {
			
			Expression lambda = genericLambda;
			for (int i = 0; i < codomainVariables.size(); i++) {
				Expression codomainVariable = codomainVariables.get(i);
				// NOTE: There will only be one occurrence of the codomain variable to replace, 
				// so can avoid doing an AllOccurrences replacement.
				lambda = lambda.replaceFirstOccurrence(codomainVariable, assignment.get(codomainVariable), cachedIterateRegistry);
			}
			
			return lambda;
		});
	}

	@Override
	public boolean contains(Expression uniquelyNamedConstant) {
		// Function types do not contain uniquely named constants.
		return false;
	}
	
	@Override
	public boolean isSampleUniquelyNamedConstantSupported() {
		return false;
	}

	@Override
	public Expression sampleUniquelyNamedConstant(Random random) {
		throw new Error("Cannot sample uniquely named constant from function type that is infinite and/or defined by variables: " + getName());
	}

	@Override
	public Expression cardinality() {
		if (isFinite()) {		
			// cardinality of a finite function is = |co-domain|^|domain|
			Rational cardinality = codomain.cardinality()
					.rationalValue()
					.pow(argumentTypes.stream()
							.map(Type::cardinality)
							.map(Expression::rationalValue)
							.reduce(Rational.ONE, Rational::multiply).intValue()
					);
			return makeSymbol(cardinality);
		}
		return Expressions.INFINITY;
	}
	
	@Override
	public boolean isDiscrete() {
		return codomain.isDiscrete() && argumentTypes.stream().allMatch(Type::isDiscrete);
	}
	
	@Override
	public boolean isFinite() {
		return codomain.isFinite() && argumentTypes.stream().allMatch(Type::isFinite);
	}

	@Override
	public String toString() {
		if (cachedString == null) {
			if (getArgumentTypes().size() == 0) {
				cachedString = apply(FUNCTION_TYPE, getCodomain()).toString();
				
			} else if (getArgumentTypes().size() == 1) {
				cachedString = apply(FUNCTION_TYPE, getArgumentTypes().get(0), getCodomain()).toString();
			} else {
				cachedString = apply(FUNCTION_TYPE, apply(TUPLE_TYPE, getArgumentTypes()), getCodomain())
						.toString();
			}
		}
		return cachedString;
	}
	
	@Override
	public Set<Type> getEmbeddedTypes() {
		Set<Type> result = new LinkedHashSet<>();
		result.add(codomain);
		result.addAll(codomain.getEmbeddedTypes());
		argumentTypes.forEach(argType -> {
			result.add(argType);
			result.addAll(argType.getEmbeddedTypes());
		});
		return result;
	}

	/**
	 * Make a function type expression.
	 * 
	 * @param codomainType
	 *            the codomain type of the function type.
	 * @param argumentTypes
	 *            0 or more argument types to the function type.
	 * @return a function type expression.
	 */
	public static Expression make(Expression codomainType, Expression... argumentTypes) {
		Expression result = make(codomainType, Arrays.asList(argumentTypes));
		return result;
	}

	/**
	 * Make a function type expression.
	 * 
	 * @param codomainType
	 *            the codomain type of the function type.
	 * @param argumentTypes
	 *            0 or more argument types to the function type.
	 * @return a function type expression.
	 */
	public static Expression make(Expression codomainType, List<Expression> argumentTypes) {
		Expression result;
		if (argumentTypes.size() == 0) {
			result = apply(FUNCTION_TYPE, codomainType);
		} else if (argumentTypes.size() == 1) {
			result = apply(FUNCTION_TYPE, argumentTypes.get(0), codomainType);
		} else {
			result = apply(FUNCTION_TYPE, apply(TUPLE_TYPE, argumentTypes), codomainType);
		}

		return result;
	}

	/**
	 * Get the codomain type expression from the function type.
	 * 
	 * @param functionTypeExpression
	 *            the function type expression whose codomain is to be
	 *            retrieved.
	 * 
	 * @return the codomain type expression for the given function type
	 *         expression.
	 */
	public static Expression getCodomain(Expression functionTypeExpression) {
		assertFunctionType(functionTypeExpression);
		Expression result;
		if (functionTypeExpression.numberOfArguments() == 1) {
			result = functionTypeExpression.get(0);
		} else {
			result = functionTypeExpression.get(1);
		}
		return result;
	}

	/**
	 * Get the given function type expression's argument type expression list.
	 * 
	 * @param functionTypeExpression
	 *            the function type expression list whose argument type
	 *            expressions are to be retrieved.
	 * @return the argument type expressions of the given function type
	 *         expression.
	 */
	public static List<Expression> getArgumentList(Expression functionTypeExpression) {
		assertFunctionType(functionTypeExpression);
		List<Expression> result = new ArrayList<>();

		// If arity is 2 then we have argument types defined
		if (functionTypeExpression.numberOfArguments() == 2) {
			if (functionTypeExpression.get(0).hasFunctor(TUPLE_TYPE)) {
				result.addAll(functionTypeExpression.get(0).getArguments());
			}
			else {
				result.add(functionTypeExpression.get(0));
			}
		}

		return result;
	}

	/**
	 * Determine if a given expression is a function type expression.
	 * 
	 * @param expression
	 *            the expresison to be tested.
	 * @return true if the given expression is a function type, false otherwise.
	 */
	public static boolean isFunctionType(Expression expression) {
		boolean result = false;

		if (expression.hasFunctor(FUNCTION_TYPE)) {
			// A Nullary function, with a codomain defined
			if (expression.numberOfArguments() == 1) {
				result = true;
			} else if (expression.numberOfArguments() == 2) {				
				result = true;
			}
		}

		return result;
	}

	/**
	 * Assert that the give expression represents a function type application.
	 * 
	 * @param expression
	 *            the expression to be tested.
	 */
	public static void assertFunctionType(Expression expression) {
		Util.myAssert(expression.hasFunctor(FUNCTION_TYPE), () -> "Functor in expression " + expression
				+ " should be a functional type (that is, have functor '->')");
		Util.myAssert(expression.numberOfArguments() == 1 || expression.numberOfArguments() == 2,
				() -> "Function type has illegal number of arguments (should be 1 or 2), has "
						+ expression.numberOfArguments() + " for " + expression);
	}
}