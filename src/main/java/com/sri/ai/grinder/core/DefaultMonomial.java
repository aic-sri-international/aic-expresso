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

import static com.sri.ai.util.Util.zipWith;

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
import com.sri.ai.grinder.helper.MonomialComparator;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

/**
 * Default implementation of the Monomial interface.
 * 
 * @author oreilly
 *
 */
@Beta
public class DefaultMonomial extends DefaultFunctionApplication implements Monomial {
	//
	public static final Expression MONOMIAL_FUNCTOR = Expressions.makeSymbol(FunctorConstants.TIMES);
	//
	private static final long serialVersionUID = 1L;
	//
	private static final ExpressionComparator _variableComparator = new ExpressionComparator();
	private static final MonomialComparator   _monomialComparator = new MonomialComparator();
	//	
	private static final Monomial ZERO = make(Rational.ZERO, Collections.emptyList(), Collections.emptyList());
	private static final Monomial ONE  = make(Rational.ONE, Collections.emptyList(), Collections.emptyList());
	//
	private Rational                  coefficient      = null; 
	private List<Expression>          orderedVariables = null;
	private List<Rational>            orderedPowers    = null;
	private Map<Expression, Rational> variableToPower  = new LinkedHashMap<>();
	
	public static Monomial make(Expression expression) {
		Monomial result = make(Times.getMultiplicands(expression));
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
	public Monomial times(Monomial multiplier) {
		Monomial result;
		// return 0 if either coefficient is 0
		if (this.coefficient.equals(Rational.ZERO) || multiplier.getCoefficient().equals(Rational.ZERO)) {
			result = ZERO;
		}
		else {
			List<Expression> combinedVariables = Monomial.unionVariablesLexicographically(this, multiplier);
			
			List<Rational> thisSignature       = this.getSignature(combinedVariables);
			List<Rational> multiplierSignature = multiplier.getSignature(combinedVariables);
			
			Rational resultCoefficient = coefficient.multiply(multiplier.getCoefficient());
			
			List<Rational> resultPowers = zipWith((power1, power2) -> power1.add(power2), thisSignature, multiplierSignature);
			
			result = make(resultCoefficient, combinedVariables, resultPowers);
		}
		
		return result;
	}

	@Override
	public Pair<Monomial, Monomial> divide(Monomial divisor) {
		Pair<Monomial, Monomial> result;
		
		if (divisor.getCoefficient().equals(Rational.ZERO)) {
			throw new IllegalArgumentException("Argument divisor is 0.");
		}
		
		if (this.getCoefficient().equals(Rational.ZERO)) {
			result = new Pair<>(ZERO, ZERO);
		}		
		else if (getVariables().containsAll(divisor.getVariables())) {
			List<Expression> combinedVariables = Monomial.unionVariablesLexicographically(this, divisor);
			
			List<Rational> thisSignature    = this.getSignature(combinedVariables);
			List<Rational> divisorSignature = divisor.getSignature(combinedVariables);
						
			Rational resultCoefficient = this.coefficient.divide(divisor.getCoefficient());
			
			List<Rational> resultPowers = zipWith((power1, power2) -> power1.subtract(power2), thisSignature, divisorSignature);
			if (resultPowers.stream().anyMatch(power -> power.signum() == -1)) {
				result = new Pair<>(ZERO, this); 
			}
			else {
				result = new Pair<>(make(resultCoefficient, combinedVariables, resultPowers), ZERO);
			}
		}
		else {
			result = new Pair<>(ZERO, this);
		}
		
		return result;
	}

	@Override
	public Monomial exponentiate(int exponent) {
		if (exponent < 0) {
			throw new IllegalArgumentException("Exponent must be a non-negative integer, given: "+exponent);
		}
		Monomial result;		
		if (exponent == 0) {
			result = ONE;
		}
		else {
			Rational       resultCoefficient = coefficient.pow(exponent);
			List<Rational> resultPowers      = new ArrayList<>(orderedPowers.size());
			
			orderedPowers.forEach(currentPower -> resultPowers.add(currentPower.multiply(exponent)));
			
			result = make(resultCoefficient, orderedVariables, resultPowers);
		}
		
		return result;
	}
	// END-Monomial
	//
	
	//
	// START-FunctionApplication
	@Override
	public Expression set(int i, Expression newIthArgument) {
		Expression result = super.set(i, newIthArgument);
		// Ensure we can make a Monomial out of the modified expression
		result = make(result);
		return result;
	}
	
	@Override
	public int compareTo(Object anotherObject) {
		int result;
		if (anotherObject instanceof Monomial) {
			// Monomial compareTo logic is different than the standard expression compareTo logic
			result = _monomialComparator.compare(this, (Monomial) anotherObject);
		}
		else {
			result = super.compareTo(anotherObject);
		}
		return result;
	}
	// END-FunctionApplication
	//
	
	//
	// PRIVATE
	//
	private DefaultMonomial(Rational coefficient, List<Expression> orderedVariables, List<Rational> orderedPowers) {
		super(MONOMIAL_FUNCTOR, makeAsArgumentsToTimes(coefficient, orderedVariables, orderedPowers));
		// NOTE: we use Collections.unmodifiable<...> to ensure Monomials are immutable.
		this.coefficient      = coefficient;
		this.orderedVariables = Collections.unmodifiableList(orderedVariables);
		this.orderedPowers    = Collections.unmodifiableList(orderedPowers);
		
		for (int i = 0; i < orderedVariables.size(); i++) {
			this.variableToPower.put(orderedVariables.get(i), orderedPowers.get(i));
		}
		
		this.variableToPower  = Collections.unmodifiableMap(this.variableToPower);
	}
	
	private static Monomial make(List<Expression> numericalConstantsAndTerms) {
		Rational coefficient = Rational.ONE;
		
		Map<Expression, Rational> variableToPower = new LinkedHashMap<>();
		for (Expression numericalConstantOrTerm : numericalConstantsAndTerms) {
			if (Expressions.isNumber(numericalConstantOrTerm)) {
				coefficient = coefficient.multiply(numericalConstantOrTerm.rationalValue());
			}
			else { // Is a term				
				Expression variable          = numericalConstantOrTerm;
				Rational   power             = Rational.ONE;
				boolean    attemptFlattening = false;
				
				// If exponentiation using a constant integer exponent then we need to extract the variable and the power
				if (Expressions.hasFunctor(variable, Exponentiation.EXPONENTIATION_FUNCTOR)) {
					Expression simplifiedPower = simplifyExponentIfPossible(variable.get(1));
					if (isLegalExponent(simplifiedPower)) {
						power    = simplifiedPower.rationalValue();
						// The variable is actually the base of the exponentiation
						variable = variable.get(0); 
						attemptFlattening = true;
					}
					else if (!simplifiedPower.equals(variable.get(1))) {
						// Use the simplified version of the non legal exponent in the variable
						// i.e. is a generalized variable where the exponent has been simplified
						// as best as possible.
						variable = new DefaultFunctionApplication(Exponentiation.EXPONENTIATION_FUNCTOR, Arrays.asList(variable.get(0), simplifiedPower));
					}
				}
				
				// Handle case where variable is negated, e.g.: -x
				if (variable.hasFunctor(FunctorConstants.MINUS) && variable.numberOfArguments() == 1) {
					variable    = variable.get(0);
					// i.e. same as having an explicit constant '-1' multiplicand in the expression
					coefficient = coefficient.negate();
					attemptFlattening = true;
				}
				
				// Handle nested *'s arguments
				if (variable.hasFunctor(MONOMIAL_FUNCTOR)) {
					attemptFlattening = true;
				}
				
				// We attempt flattening if we were/are able to simplify the variable in some way
				if (attemptFlattening) {
					// Treat the variable as a Monomial and merge it in
					// This lets you handle nested monomial expressions
					// in a simplified manner.
					Monomial variableAsMonomial = make(Times.getMultiplicands(variable));
					// Need to raise to the current power
					variableAsMonomial = variableAsMonomial.exponentiate(power.intValue());
					coefficient = coefficient.multiply(variableAsMonomial.getCoefficient());
					List<Expression> varVariables = variableAsMonomial.getVariablesLexicographicallyOrdered();
					List<Rational>   varPowers    = variableAsMonomial.getPowersOfLexicographicallyOrderedVariables();
					int varSize = varVariables.size();
					for (int i = 0; i < varSize; i++) {
						Expression varVariable = varVariables.get(i);
						Rational   varPower    = varPowers.get(i);
						updateVariableToPowerMap(variableToPower, varVariable, varPower);
					} 
				}
				else {
					updateVariableToPowerMap(variableToPower, variable, power);
				}
			}
		}
		
		Monomial result = null;
		if (coefficient.equals(Rational.ZERO)) {
			result = ZERO;
		}
		else {
			List<Expression> orderedVariables = new ArrayList<>(variableToPower.keySet());
			Collections.sort(orderedVariables, _variableComparator);
			
			List<Rational> orderedPowers = new ArrayList<>(orderedVariables.size());
			orderedVariables.forEach(variable -> orderedPowers.add(variableToPower.get(variable)));
			
			result = make(coefficient, orderedVariables, orderedPowers);
		}
		
		return result;
	}
	
	private static boolean isLegalExponent(Expression exponentExpression) {
		boolean result = false;
		if (Expressions.isNumber(exponentExpression)) {
			Rational exponent = exponentExpression.rationalValue();
			if (exponent.isInteger() && exponent.signum() != -1) {
				result = true;
			}
		}
		return result;
	}
	
	private static Expression simplifyExponentIfPossible(Expression exponent) {
		Expression result = exponent;
		
		if (exponent.hasFunctor(Exponentiation.EXPONENTIATION_FUNCTOR)) {
			Expression base  = exponent.get(0);
			Expression power = exponent.get(1);
			
			Expression simplifiedPower = simplifyExponentIfPossible(power);
			if (Expressions.isNumber(base) && isLegalExponent(simplifiedPower)) {
				result = Expressions.makeSymbol(base.rationalValue().pow(simplifiedPower.intValueExact()));
			}
			else if (!power.equals(simplifiedPower)){
				result = new DefaultFunctionApplication(Exponentiation.EXPONENTIATION_FUNCTOR, Arrays.asList(base, simplifiedPower));
			}
		}
		
		return result;
	}
	
	private static void updateVariableToPowerMap(Map<Expression, Rational> variableToPower, Expression variable, Rational power) {
		// Ensure duplicate variables in the monomial are handled correctly
		Rational existingPower = variableToPower.get(variable);
		if (existingPower == null) {
			variableToPower.put(variable, power);
		}
		else {
			variableToPower.put(variable, existingPower.add(power));
		}
	}
	
	private static Monomial make(Rational coefficient, List<Expression> orderedVariables, List<Rational> orderedPowers) {
		List<Expression> variables = new ArrayList<>(orderedVariables.size());
		List<Rational>   powers    = new ArrayList<>(orderedPowers.size());		
		for (int i = 0; i < orderedPowers.size(); i++) {
			Rational power = orderedPowers.get(i);
			// Power must not be negative as this is illegal for a polynomial
			if (power.signum() == -1) {
				throw new IllegalArgumentException("Negative powers are not allowed.");
			}
			// 0 power means the variable is equivalent to 1 so we can just drop it.
			if (power.signum() > 0) {
				Expression variable = orderedVariables.get(i);
				variables.add(variable);
				powers.add(power);				
			}
		}
		
		Monomial result = new DefaultMonomial(coefficient, variables, powers);
		
		return result;
	}
	
	private static List<Expression> makeAsArgumentsToTimes(Rational coefficient, List<Expression> orderedVariables, List<Rational> orderedPowers) {
		List<Expression> result = new ArrayList<>(1+orderedVariables.size());
		result.add(Expressions.makeSymbol(coefficient));
		result.addAll(zipWith((base, power) -> Exponentiation.make(base, power), orderedVariables, orderedPowers));
		return result;
	}
}
