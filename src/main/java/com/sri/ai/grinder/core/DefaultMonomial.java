/*
 * Copyright (c) 2015, SRI International
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
 * Neither the name of the aic-praise nor the names of its
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
package com.sri.ai.grinder.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.helper.ExpressionComparator;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Monomial;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

/**
 * Default implementation of the Monomial Inteface.
 * 
 * @author oreilly
 *
 */
@Beta
public class DefaultMonomial extends DefaultFunctionApplication implements Monomial {
	//
	private static final long serialVersionUID = 1L;
	//
	private static final Expression MONOMIAL_FUNCTOR       = Expressions.makeSymbol(FunctorConstants.PRODUCT);
	private static final Expression EXPONENTIATION_FUNCTOR = Expressions.makeSymbol(FunctorConstants.EXPONENTIATION);
	//
	private static final ExpressionComparator _variableComparator = new ExpressionComparator();
	private static final Monomial             _zeroMonomial       = new DefaultMonomial(Rational.ZERO, Collections.emptyList(), Collections.emptyList());
	//
	private Rational                  coefficient      = null; 
	private List<Expression>          orderedVariables = null;
	private List<Rational>            orderedPowers    = null;
	private Map<Expression, Rational> variableToPower  = new LinkedHashMap<>();
	
	public static Monomial make(Expression expression) {
		Monomial result = make(Times.getMultiplicands(expression));
		return result;
	}
	
	public static Monomial make(List<Expression> numericalConstantsAndTerms) {
		Rational coefficient = Rational.ONE;
		Map<Expression, Rational> variableToPower = new LinkedHashMap<>();
		for (Expression numericalConstantOrTerm : numericalConstantsAndTerms) {
			if (Expressions.isNumber(numericalConstantOrTerm)) {
				coefficient = coefficient.multiply(numericalConstantOrTerm.rationalValue());
			}
			else { // Is a term				
				Expression variable = numericalConstantOrTerm;
				Rational   power    = Rational.ONE;
				// If exponentiation using a number then we need to extract the variable and the power
				if (Expressions.hasFunctor(variable, EXPONENTIATION_FUNCTOR) && Expressions.isNumber(variable.get(1))) {
					power    = variable.get(1).rationalValue();
					variable = variable.get(0); // The variable is actually the base of the exponentiation
				}
// TODO - validate that the variable is a legal 'general variable' expression	
				variableToPower.put(variable, power);
			}
		}
		
		Monomial result = null;
		if (coefficient.equals(Rational.ZERO)) {
			result = _zeroMonomial;
		}
		else {
			List<Expression> orderedVariables = new ArrayList<>(variableToPower.keySet());
			Collections.sort(orderedVariables, _variableComparator);
			
			List<Rational> orderedPowers = new ArrayList<>(orderedVariables.size());
			orderedVariables.forEach(variable -> orderedPowers.add(variableToPower.get(variable)));
			
			result = new DefaultMonomial(coefficient, orderedVariables, orderedPowers);
		}
		
		return result;
	}
	
	//
	// START-Monomial
	@Override
	public Rational getCoefficient() {
		return coefficient;
	}

	@Override
	public Set<Expression> getVariables() {
		return variableToPower.keySet();
	}

	@Override
	public List<Expression> getVariablesLexicographicallyOrdered() {
		return orderedVariables;
	}

	@Override
	public List<Rational> getPowersOfLexicographicallyOrderedVariables() {
		return orderedPowers;
	}
	
	@Override
	public Rational getPowerOfVariable(Expression variable) {
		Rational result = variableToPower.getOrDefault(variable, Rational.ZERO);
		return result;
	}

	@Override
	public List<Rational> getSignature(List<Expression> variables) {
		List<Rational> result = new ArrayList<>(variables.size());
		variables.forEach(variable -> variableToPower.getOrDefault(variable, Rational.ZERO));
		return result;
	}

	@Override
	public Monomial times(Monomial multiplier) {
// TODO
		throw new UnsupportedOperationException("To implement yet");
	}

	@Override
	public Pair<Monomial, Monomial> divide(Monomial divisor) {
// TODO
		throw new UnsupportedOperationException("To implement yet");
	}

	@Override
	public Monomial exponentiate(int exponent) {
		
		Rational       resultCoefficient = coefficient.pow(exponent);
		List<Rational> resultPowers      = new ArrayList<>(orderedPowers.size());
		
		orderedPowers.forEach(currentPower -> resultPowers.add(currentPower.multiply(exponent)));
		
		Monomial result = new DefaultMonomial(resultCoefficient, orderedVariables, resultPowers);
		
		return result;
	}
	// END-Monomial
	//
	
	//
	// PRIVATE
	//
	private DefaultMonomial(Rational coefficient, List<Expression> orderedVariables, List<Rational> orderedPowers) {
		// NOTE: we use Collections.unmodifiable<...> to ensure Monomials are immutable.
		super(MONOMIAL_FUNCTOR, makeAsArgumentsToProduct(coefficient, orderedVariables, orderedPowers));
		this.coefficient      = coefficient;
		this.orderedVariables = Collections.unmodifiableList(orderedVariables);
		this.orderedPowers    = Collections.unmodifiableList(orderedPowers);
		
		for (int i = 0; i < orderedVariables.size(); i++) {
			this.variableToPower.put(orderedVariables.get(i), orderedPowers.get(i));
		}
		this.variableToPower = Collections.unmodifiableMap(this.variableToPower);
	}
	
	private static List<Expression> makeAsArgumentsToProduct(Rational coefficient, List<Expression> orderedVariables, List<Rational> orderedPowers) {
		List<Expression> result = new ArrayList<>(1+orderedVariables.size());
		result.add(Expressions.makeSymbol(coefficient));
		result.addAll(Util.zipWith((base, power) -> {
			return new DefaultFunctionApplication(EXPONENTIATION_FUNCTOR, Arrays.asList(base, Expressions.makeSymbol(power)));
		}, orderedVariables, orderedPowers));
		
		return result;
	}
}
