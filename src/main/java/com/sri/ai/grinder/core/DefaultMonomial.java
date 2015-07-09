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
	public static final Monomial ZERO = make(Rational.ZERO, Collections.emptyList(), Collections.emptyList());
	public static final Monomial ONE  = make(Rational.ONE, Collections.emptyList(), Collections.emptyList());
	//
	private static final long serialVersionUID = 1L;
	//
	private static final ExpressionComparator _factorComparator   = new ExpressionComparator();
	private static final MonomialComparator   _monomialComparator = new MonomialComparator();
	//	
	private Expression                numericConstantFactorExpression  = null; 
	private List<Expression>          orderedNonNumericConstantFactors = null;
	private List<Rational>            orderedNonNumericConstantPowers  = null;
	private Map<Expression, Rational> factorToPower                    = new LinkedHashMap<>();
	
	public static Monomial make(Expression expression) {
		Monomial result = make(Times.getMultiplicands(expression));
		return result;
	}
	
	//
	// START-Monomial
	@Override
	public Rational getNumericConstantFactor() {
		return numericConstantFactorExpression.rationalValue();
	}

	@Override
	public Set<Expression> getFactors() {
		return factorToPower.keySet();
	}

	@Override
	public List<Expression> getOrderedNonNumericConstantFactors() {
		return orderedNonNumericConstantFactors;
	}

	@Override
	public List<Rational> getPowersOfOrderedNonNumericConstantFactors() {
		return orderedNonNumericConstantPowers;
	}
	
	@Override
	public Monomial getCoefficient(Set<Expression> factors) {
		Monomial result;
		
		if (getNumericConstantFactor().equals(Rational.ZERO)) {
			result = ZERO;
		}
		else if (factors.size() == 0) {
			result = this;
		}
		else {
			// If the factors provided don't overlap with the factors in this monomial
			// then you just want to return this monomial.
			boolean factorsOverlap = false;
			Rational resultNumericConstantFactor = getNumericConstantFactor();
			if (factors.contains(this.numericConstantFactorExpression)) {
				// reduces to 1.
				resultNumericConstantFactor = Rational.ONE;
				factorsOverlap              = true;
			}
			List<Expression> resultOrderedNonNumericConstantFactors = new ArrayList<>(this.orderedNonNumericConstantFactors.size());
			List<Rational>   resultOrderedNonNumericConstantPowers  = new ArrayList<>(this.orderedNonNumericConstantPowers.size());
			int numFactors = this.orderedNonNumericConstantFactors.size();
			for (int i = 0; i < numFactors; i++) {
				Expression factor = this.orderedNonNumericConstantFactors.get(i);
				if (factors.contains(factor)) {
					factorsOverlap = true;
				} else {
					Rational power = this.orderedNonNumericConstantPowers.get(i);
					
					resultOrderedNonNumericConstantFactors.add(factor);
					resultOrderedNonNumericConstantPowers.add(power);
				}
			}
			
			if (factorsOverlap) {
				result = make(resultNumericConstantFactor, resultOrderedNonNumericConstantFactors, resultOrderedNonNumericConstantPowers);
			}
			else {
				result = this;
			}
		}
		
		return result;
	}
	
	@Override
	public Rational getPowerOfFactor(Expression factor) {
		Rational result = factorToPower.getOrDefault(factor, Rational.ZERO);
		return result;
	}

	@Override
	public Monomial times(Monomial multiplier) {
		Monomial result;
		// Optimization: return 0 if either numeric constant factor is 0
		if (getNumericConstantFactor().equals(Rational.ZERO) || multiplier.getNumericConstantFactor().equals(Rational.ZERO)) {
			result = ZERO;
		}
		else {
			List<Expression> combinedNonNumericConstantFactors = Monomial.orderedUnionOfNonNumericConstantFactors(this, multiplier);
			
			List<Rational> thisSignature       = this.getSignature(combinedNonNumericConstantFactors);
			List<Rational> multiplierSignature = multiplier.getSignature(combinedNonNumericConstantFactors);
			
			Rational resultNumericConstantFactor = getNumericConstantFactor().multiply(multiplier.getNumericConstantFactor());
			
			List<Rational> resultPowers = zipWith((power1, power2) -> power1.add(power2), thisSignature, multiplierSignature);
			
			result = make(resultNumericConstantFactor, combinedNonNumericConstantFactors, resultPowers);
		}
		
		return result;
	}

	@Override
	public Pair<Monomial, Monomial> divide(Monomial divisor) {
		Pair<Monomial, Monomial> result;
		
		if (divisor.getNumericConstantFactor().equals(Rational.ZERO)) {
			throw new IllegalArgumentException("Argument divisor is 0.");
		}
		
		if (this.getNumericConstantFactor().equals(Rational.ZERO)) {
			result = new Pair<>(ZERO, ZERO);
		} // TODO - will likely want to make this containsAll call more efficient by using sets.		
		else if (getOrderedNonNumericConstantFactors().containsAll(divisor.getOrderedNonNumericConstantFactors())) {
			List<Expression> combinedNonNumericConstantFactors = Monomial.orderedUnionOfNonNumericConstantFactors(this, divisor);
			
			List<Rational> thisSignature    = this.getSignature(combinedNonNumericConstantFactors);
			List<Rational> divisorSignature = divisor.getSignature(combinedNonNumericConstantFactors);
						
			Rational resultNumericConstantFactor = getNumericConstantFactor().divide(divisor.getNumericConstantFactor());
			
			List<Rational> resultPowers = zipWith((power1, power2) -> power1.subtract(power2), thisSignature, divisorSignature);
			if (resultPowers.stream().anyMatch(power -> power.signum() == -1)) {
				result = new Pair<>(ZERO, this); 
			}
			else {
				result = new Pair<>(make(resultNumericConstantFactor, combinedNonNumericConstantFactors, resultPowers), ZERO);
			}
		}
		else {
			result = new Pair<>(ZERO, this);
		}
		
		return result;
	}

	@Override
	public Monomial exponentiate(int exponent) throws IllegalArgumentException {
		if (exponent < 0) {
			throw new IllegalArgumentException("Exponent must be a non-negative integer, given: "+exponent);
		}
		Monomial result;		
		if (exponent == 0) {
			result = ONE;
		}
		else {
			Rational       resultNumericConstantFactor = getNumericConstantFactor().pow(exponent);
			List<Rational> resultPowers                = new ArrayList<>(orderedNonNumericConstantPowers.size());
			
			orderedNonNumericConstantPowers.forEach(currentPower -> resultPowers.add(currentPower.multiply(exponent)));
			
			result = make(resultNumericConstantFactor, orderedNonNumericConstantFactors, resultPowers);
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
	private DefaultMonomial(Rational numericConstantFactor, List<Expression> orderedNonNumericConstantFactors, List<Rational> orderedNonNumericConstantPowers) {
		super(MONOMIAL_FUNCTOR, makeAsArgumentsToTimes(numericConstantFactor, orderedNonNumericConstantFactors, orderedNonNumericConstantPowers));
		// NOTE: we use Collections.unmodifiable<...> to ensure Monomials are immutable.
		this.numericConstantFactorExpression  = Expressions.makeSymbol(numericConstantFactor);
		this.orderedNonNumericConstantFactors = Collections.unmodifiableList(orderedNonNumericConstantFactors);
		this.orderedNonNumericConstantPowers  = Collections.unmodifiableList(orderedNonNumericConstantPowers);
		
		this.factorToPower.put(this.numericConstantFactorExpression, Rational.ONE);
		for (int i = 0; i < orderedNonNumericConstantFactors.size(); i++) {
			this.factorToPower.put(orderedNonNumericConstantFactors.get(i), orderedNonNumericConstantPowers.get(i));
		}
		
		this.factorToPower = Collections.unmodifiableMap(this.factorToPower);
	}
	
	private static Monomial make(List<Expression> numericConstantsAndTerms) {
		Rational numericConstantFactor = Rational.ONE;
		
		Map<Expression, Rational> factorToPower = new LinkedHashMap<>();
		for (Expression numericConstantOrTerm : numericConstantsAndTerms) {
			if (Expressions.isNumber(numericConstantOrTerm)) {
				numericConstantFactor = numericConstantFactor.multiply(numericConstantOrTerm.rationalValue());
			}
			else { // Is a term				
				Expression factor            = numericConstantOrTerm;
				Rational   power             = Rational.ONE;
				boolean    attemptFlattening = false;
				
				// If exponentiation using a constant integer exponent then we need to extract the factor and the power
				if (Expressions.hasFunctor(factor, Exponentiation.EXPONENTIATION_FUNCTOR)) {
					Expression simplifiedPower = simplifyExponentIfPossible(factor.get(1));
					if (isLegalExponent(simplifiedPower)) {
						power  = simplifiedPower.rationalValue();
						// The factor is actually the base of the exponentiation
						factor = factor.get(0); 
						attemptFlattening = true;
					}
					else if (!simplifiedPower.equals(factor.get(1))) {
						// Use the simplified version of the non legal exponent in the factor
						// i.e. is a non numeric constant factor where the exponent has been simplified
						// as best as possible.
						factor = new DefaultFunctionApplication(Exponentiation.EXPONENTIATION_FUNCTOR, Arrays.asList(factor.get(0), simplifiedPower));
					}
				}
				
				// Handle case where factor is negated, e.g.: -x
				if (factor.hasFunctor(FunctorConstants.MINUS) && factor.numberOfArguments() == 1) {
					factor = factor.get(0);
					// i.e. same as having an explicit constant '-1' multiplicand in the expression
					numericConstantFactor = numericConstantFactor.negate();
					attemptFlattening = true;
				}
				
				// Handle nested *'s arguments
				if (factor.hasFunctor(MONOMIAL_FUNCTOR)) {
					attemptFlattening = true;
				}
				
				// We attempt flattening if we were/are able to simplify the factor in some way
				if (attemptFlattening) {
					// Treat the factor as a Monomial and merge it in
					// This lets you handle nested monomial expressions
					// in a simplified/recursive manner.
					Monomial factorAsMonomial = make(Times.getMultiplicands(factor));
					// Need to raise to the current power
					factorAsMonomial      = factorAsMonomial.exponentiate(power.intValue());
					numericConstantFactor = numericConstantFactor.multiply(factorAsMonomial.getNumericConstantFactor());
					List<Expression> factorFactors = factorAsMonomial.getOrderedNonNumericConstantFactors();
					List<Rational>   factorPowers  = factorAsMonomial.getPowersOfOrderedNonNumericConstantFactors();
					int factorSize = factorFactors.size();
					for (int i = 0; i < factorSize; i++) {
						Expression factorFactor = factorFactors.get(i);
						Rational   factorPower  = factorPowers.get(i);
						updateFactorToPowerMap(factorToPower, factorFactor, factorPower);
					} 
				}
				else {
					updateFactorToPowerMap(factorToPower, factor, power);
				}
			}
		}
		
		Monomial result = null;
		if (numericConstantFactor.equals(Rational.ZERO)) {
			result = ZERO;
		}
		else {
			List<Expression> orderedFactors = new ArrayList<>(factorToPower.keySet());
			Collections.sort(orderedFactors, _factorComparator);
			
			List<Rational> orderedPowers = new ArrayList<>(orderedFactors.size());
			orderedFactors.forEach(factor -> orderedPowers.add(factorToPower.get(factor)));
			
			result = make(numericConstantFactor, orderedFactors, orderedPowers);
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
	
	private static void updateFactorToPowerMap(Map<Expression, Rational> factorToPower, Expression factor, Rational power) {
		// Ensure duplicate factors in the monomial are handled correctly
		Rational existingPower = factorToPower.get(factor);
		if (existingPower == null) {
			factorToPower.put(factor, power);
		}
		else {
			factorToPower.put(factor, existingPower.add(power));
		}
	}
	
	private static Monomial make(Rational numericConstantFactor, List<Expression> orderedNonNumericConstantFactors, List<Rational> orderedNonNumericConstantPowers) {
		Monomial result;
		
		// if numeric constant is = 0 the whole expression is 0, so reduce to that.
		if (numericConstantFactor.equals(Rational.ZERO)) {
			result = new DefaultMonomial(Rational.ZERO, Collections.emptyList(), Collections.emptyList());
		}
		else {
			List<Expression> factors = new ArrayList<>(orderedNonNumericConstantFactors.size());
			List<Rational>   powers  = new ArrayList<>(orderedNonNumericConstantPowers.size());		
			for (int i = 0; i < orderedNonNumericConstantPowers.size(); i++) {
				Rational power = orderedNonNumericConstantPowers.get(i);
				// Power must not be negative as this is illegal for a monomial
				if (power.signum() == -1) {
					throw new IllegalArgumentException("Negative powers are not allowed.");
				}
				// 0 power means the factor is equivalent to 1 so we can just drop it.
				if (power.signum() > 0) {
					Expression factor = orderedNonNumericConstantFactors.get(i);
					factors.add(factor);
					powers.add(power);				
				}
			}
			
			result = new DefaultMonomial(numericConstantFactor, factors, powers);
		}
		
		return result;
	}
	
	private static List<Expression> makeAsArgumentsToTimes(Rational numericConstantFactor, List<Expression> orderedNonNumericConstantFactors, List<Rational> orderedNonNumericConstantPowers) {
		List<Expression> result = new ArrayList<>(1+orderedNonNumericConstantFactors.size());
		result.add(Expressions.makeSymbol(numericConstantFactor));
		result.addAll(zipWith((base, power) -> Exponentiation.make(base, power), orderedNonNumericConstantFactors, orderedNonNumericConstantPowers));
		return result;
	}
}
