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
package com.sri.ai.grinder.polynomial.core;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.sgdpllt.library.number.Exponentiation.EXPONENTIATION_FUNCTOR;
import static com.sri.ai.util.Util.zipWith;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultFunctionApplication;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.expresso.helper.ExpressionComparator;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.polynomial.api.Monomial;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.number.Exponentiation;
import com.sri.ai.grinder.sgdpllt.library.number.Times;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

/**
 * Default implementation of the Monomial interface.
 * 
 * @author oreilly
 *
 */
@Beta
public class DefaultMonomial extends AbstractExpressionWrapper implements Monomial {
	//
	public static final Expression MONOMIAL_FUNCTOR = Expressions.makeSymbol(FunctorConstants.TIMES);
	//
	public static final Monomial MINUS_ONE = make(Rational.MINUS_ONE, Collections.emptyList(), Collections.emptyList());
	public static final Monomial ZERO = make(Rational.ZERO, Collections.emptyList(), Collections.emptyList());
	public static final Monomial ONE  = make(Rational.ONE, Collections.emptyList(), Collections.emptyList());
	//
	private static final long serialVersionUID = 1L;
	//
	private static final ExpressionComparator _factorComparator   = new ExpressionComparator();
	//
	private int                       degree                          = 0;
	private Expression                numericFactorExpression = null; 
	private List<Expression>          orderedNonNumericFactors        = null; // these include the exponents
	private List<Rational>            orderedNonNumericFactorPowers   = null;
	private Map<Expression, Rational> factorToPower                   = new LinkedHashMap<>();
	
	public static Monomial make(Expression expression) {
		Monomial result = make(Times.getMultiplicands(expression));
		return result;
	}
	
	//
	// START-Monomial
	@Override
	public Rational getNumericFactor() {
		return numericFactorExpression.rationalValue();
	}

	@Override
	public Set<Expression> getFactors() {
		return factorToPower.keySet();
	}

	@Override
	public List<Expression> getOrderedNonNumericFactors() {
		return orderedNonNumericFactors;
	}

	@Override
	public List<Rational> getPowersOfNonNumericFactors() {
		return orderedNonNumericFactorPowers;
	}
	
	@Override
	public Monomial getCoefficient(List<Expression> factors) {
		Monomial result;
		
		if (isZero()) {
			result = ZERO;
		}
		else if (factors.size() == 0) {
			result = this;
		}
		else {
			// NOTE: This set will preserve the required ordering			
			Set<Expression> coefficientFactors = new LinkedHashSet<>(getFactors());
			coefficientFactors.removeAll(factors);
			
			// If the factors provided don't overlap with the factors in this monomial
			// then you just want to return this monomial.
			if (coefficientFactors.size() == getFactors().size()) {
				result = this;
			}
			else {
				// TODO: (Sept 2017) This treatment of coefficient factors (basically, combining factors into a single one)
				// seems overly complicated and unnecessary.

				// Check if the numeric constant is to be considered part of the coefficient or not
				// (i.e. could be included in set of given factors to exclude)
				Rational resultNumericFactor = Rational.ONE;
				if (coefficientFactors.contains(this.numericFactorExpression)) {
					resultNumericFactor = getNumericFactor();
				}
				
				List<Expression> resultOrderedFactors = new ArrayList<>(this.orderedNonNumericFactors.size());
				List<Rational>   resultOrderedFactorPowers  = new ArrayList<>(this.orderedNonNumericFactorPowers.size());
				for (Expression coefficientFactor : coefficientFactors) {
					if (!Expressions.isNumber(coefficientFactor)) {
						resultOrderedFactors.add(coefficientFactor);
						resultOrderedFactorPowers.add(getPowerOfFactor(coefficientFactor));
					}
				}
			
				result = make(resultNumericFactor, resultOrderedFactors, resultOrderedFactorPowers);
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
	public int degree() {
		return degree;
	}

	public Monomial negate() {
		DefaultMonomial result = clone();
		result.numericFactorExpression = Expressions.makeSymbol(getNumericFactor().negate());
		return result;
	}
	
	@Override
	public Monomial times(Monomial multiplier) {
		Monomial result;
		// Optimization: return 0 if either numeric factor is 0
		if (isZero() || multiplier.isZero()) {
			result = ZERO;
		}
		else if (isOne()) { // Optimization, neutral element
			result = multiplier;
		}
		else if (multiplier.isOne()) { // Optimization, neutral element
			result = this;
		}
		else {
			List<Expression> combinedNonNumericFactors = Monomial.orderedUnionOfNonNumericFactors(this, multiplier);
			
			List<Rational> thisSignature       = this.getSignature(combinedNonNumericFactors);
			List<Rational> multiplierSignature = multiplier.getSignature(combinedNonNumericFactors);
			
			Rational resultNumericFactor = getNumericFactor().multiply(multiplier.getNumericFactor());
			
			List<Rational> resultPowers = zipWith((power1, power2) -> power1.add(power2), thisSignature, multiplierSignature);
			
			result = make(resultNumericFactor, combinedNonNumericFactors, resultPowers);
		}
		
		return result;
	}

	@Override
	public Pair<Monomial, Monomial> divide(Monomial divisor) {
		Pair<Monomial, Monomial> result;
		
		if (divisor.isZero()) {
			throw new IllegalArgumentException("Argument divisor is 0.");
		}
		
		if (isZero()) {
			result = new Pair<>(ZERO, ZERO);
		} // TODO - will likely want to make this containsAll call more efficient by using sets.		
		else if (getOrderedNonNumericFactors().containsAll(divisor.getOrderedNonNumericFactors())) {
			List<Expression> combinedNonNumericFactors = Monomial.orderedUnionOfNonNumericFactors(this, divisor);
			
			List<Rational> thisSignature    = this.getSignature(combinedNonNumericFactors);
			List<Rational> divisorSignature = divisor.getSignature(combinedNonNumericFactors);
						
			Rational resultNumericFactor = getNumericFactor().divide(divisor.getNumericFactor());
			
			List<Rational> resultPowers = zipWith((power1, power2) -> power1.subtract(power2), thisSignature, divisorSignature);
			if (resultPowers.stream().anyMatch(power -> power.signum() == -1)) {
				result = new Pair<>(ZERO, this); 
			}
			else {
				result = new Pair<>(make(resultNumericFactor, combinedNonNumericFactors, resultPowers), ZERO);
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
		else if (exponent == 1) {
			result = this;
		}
		else {
			Rational       resultNumericFactor = getNumericFactor().pow(exponent);
			List<Rational> resultPowers                = new ArrayList<>(orderedNonNumericFactorPowers.size());
			
			orderedNonNumericFactorPowers.forEach(currentPower -> resultPowers.add(currentPower.multiply(exponent)));
			
			result = make(resultNumericFactor, orderedNonNumericFactors, resultPowers);
		}
		
		return result;
	}
	// END-Monomial
	//
	
	//
	// START-Expression
	@Override
	public DefaultMonomial clone() {
		DefaultMonomial result = make(getNumericFactor(), orderedNonNumericFactors, orderedNonNumericFactorPowers);
		return result;
	}
	
	@Override
	public Expression get(int index) {
		if (index == -1) {
			return getInnerExpression().getFunctor();
		}
		return super.get(index);
	}
	
	@Override
	public Expression set(int i, Expression newIthArgument) {
		Expression result = super.set(i, newIthArgument);
		// Ensure we can make a Monomial out of the modified expression
		result = make(result);
		return result;
	}
	
	@Override
	public String toString() {
		return toString(0);
//		// We don't need to re-compute as are immutable
//		return getInnerExpression().toString();
	}
	
	private String toString(int depth) {
		Expression innerExpression = getInnerExpression();
		if (innerExpression instanceof DefaultMonomial) {
			return ((DefaultMonomial)innerExpression).toString(depth + 1);
		}
		return innerExpression.toString();
	}
	// END-Expression
	//
	
	//
	// PACKAGE PROTECTED
	//
	static boolean isLegalExponent(Expression exponentExpression) {
		boolean result = false;
		if (Expressions.isNumber(exponentExpression)) {
			Rational exponent = exponentExpression.rationalValue();
			if (exponent.isInteger() && exponent.signum() != -1) {
				result = true;
			}
		}
		return result;
	}
	
	static Expression simplifyExponentIfPossible(Expression exponent) {
		Expression result = exponent;
		
		if (exponent.hasFunctor(Exponentiation.EXPONENTIATION_FUNCTOR)) {
			Expression base  = exponent.get(0);
			Expression power = exponent.get(1);
			
			Expression simplifiedPower = simplifyExponentIfPossible(power);
			if (Expressions.isNumber(base) && isLegalExponent(simplifiedPower)) {
				result = Expressions.makeSymbol(base.rationalValue().pow(simplifiedPower.intValueExact()));
			}
			else if (!power.equals(simplifiedPower)) {
				result = new DefaultFunctionApplication(Exponentiation.EXPONENTIATION_FUNCTOR, Arrays.asList(base, simplifiedPower));
			}
		}
		
		return result;
	}
	
	static DefaultMonomial make(Rational numericFactor, List<Expression> orderedNonNumericFactors, List<Rational> orderedNonNumericPowers) {
		DefaultMonomial result;
		
		// if numeric constant is 0, the whole expression is 0, so reduce to that.
		if (numericFactor.equals(Rational.ZERO)) {
			result = new DefaultMonomial(Rational.ZERO, Collections.emptyList(), Collections.emptyList());
		}
		else {
			List<Expression> factors = new ArrayList<>(orderedNonNumericFactors.size());
			List<Rational>   powers  = new ArrayList<>(orderedNonNumericPowers.size());		
			for (int i = 0; i < orderedNonNumericPowers.size(); i++) {
				Rational power = orderedNonNumericPowers.get(i);
				// Power must not be negative as this is illegal for a monomial
				if (power.signum() == -1) {
					throw new IllegalArgumentException("Negative powers are not allowed.");
				}
				// 0 power means the factor is equivalent to 1 so we can just drop it.
				if (power.signum() > 0) {
					Expression factor = orderedNonNumericFactors.get(i);
					factors.add(factor);
					powers.add(power);				
				}
			}
			
			result = new DefaultMonomial(numericFactor, factors, powers);
		}
		
		return result;
	}
	
	//
	// PROTECTED
	//
	@Override
	protected Expression computeInnerExpression() {
		Expression result;
		
		if (isNumericConstant()) {
			result = numericFactorExpression;
		}
		else {
			List<Expression> args = new ArrayList<>(1 + orderedNonNumericFactors.size());
			if (!getNumericFactor().equals(Rational.ONE)) {
				args.add(numericFactorExpression);
			}
			
			args.addAll(zipWith((base, power) -> {
				Expression arg;
				if (power.equals(Rational.ONE)) {
					arg = base; // No need to include exponentiation
				}
				else {
					arg = Exponentiation.make(base, power);
				}
				return arg;
			}, orderedNonNumericFactors, orderedNonNumericFactorPowers));
			
			if (args.size() == 1) {
				// simplified to a single argument 
				// (i.e. numeric constant was 1 as we know we have at least one 
				//  non-numeric constant term here).
				result = args.get(0);
			}
			else {
				result = new DefaultFunctionApplication(MONOMIAL_FUNCTOR, args);
			}
		}
		
		return result;
	}
	
	//
	// PRIVATE
	//
	private DefaultMonomial(Rational numericFactor, List<Expression> orderedNonNumericFactors, List<Rational> orderedNonNumericPowers) {
		// NOTE: we use Collections.unmodifiable<...> to ensure Monomials are immutable.
		this.numericFactorExpression  = Expressions.makeSymbol(numericFactor);
		this.orderedNonNumericFactors = Collections.unmodifiableList(orderedNonNumericFactors);
		this.orderedNonNumericFactorPowers  = Collections.unmodifiableList(orderedNonNumericPowers);
		
		this.factorToPower.put(this.numericFactorExpression, Rational.ONE);
		for (int i = 0; i < orderedNonNumericFactors.size(); i++) {
			this.factorToPower.put(orderedNonNumericFactors.get(i), orderedNonNumericPowers.get(i));
		}
		
		this.factorToPower = Collections.unmodifiableMap(this.factorToPower);
		
		this.degree = this.orderedNonNumericFactorPowers.stream().reduce(Rational.ZERO, (r1, r2) -> r1.add(r2)).intValue();
	}
	
	private static Monomial make(List<Expression> numericConstantsAndTerms) {
		Rational numericFactor = Rational.ONE;
		
		Map<Expression, Rational> factorToPower = new LinkedHashMap<>();
		for (Expression numericConstantOrTerm : numericConstantsAndTerms) {
			if (Expressions.isNumber(numericConstantOrTerm)) {
				numericFactor = numericFactor.multiply(numericConstantOrTerm.rationalValue());
			}
			else { // Is a term				
				Expression factor            = numericConstantOrTerm;
				Rational   power             = Rational.ONE;
				boolean    attemptFlattening = false;
				
				// Handle case where factor is negated, e.g.: -x
				if (factor.hasFunctor(MINUS) && factor.numberOfArguments() == 1) {
					factor = factor.get(0);
					// i.e. same as having an explicit constant '-1' multiplicand in the expression
					numericFactor = numericFactor.negate();
					attemptFlattening = true;
				}
				
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
						// i.e. is a non numeric factor where the exponent has been simplified
						// as best as possible.
						factor = apply(EXPONENTIATION_FUNCTOR, factor.get(0), simplifiedPower);
					}
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
					numericFactor = numericFactor.multiply(factorAsMonomial.getNumericFactor());
					List<Expression> factors = factorAsMonomial.getOrderedNonNumericFactors();
					List<Rational>   powers  = factorAsMonomial.getPowersOfNonNumericFactors();
					int factorSize = factors.size();
					for (int i = 0; i < factorSize; i++) {
						updateFactorToPowerMap(factorToPower, factors.get(i), powers.get(i));
					} 
				}
				else {
					updateFactorToPowerMap(factorToPower, factor, power);
				}
			}
		}
		
		Monomial result = null;
		if (numericFactor.equals(Rational.ZERO)) {
			result = ZERO;
		}
		else {
			List<Expression> orderedFactors = new ArrayList<>(factorToPower.keySet());
			Collections.sort(orderedFactors, _factorComparator);
			
			List<Rational> orderedPowers = new ArrayList<>(orderedFactors.size());
			orderedFactors.forEach(factor -> orderedPowers.add(factorToPower.get(factor)));
			
			result = make(numericFactor, orderedFactors, orderedPowers);
		}
		
		return result;
	}
		
	private static void updateFactorToPowerMap(Map<Expression, Rational> factorToPower, Expression factor, Rational power) {
		// Ensure duplicate variables in the monomial are handled correctly
		Rational existingPower = factorToPower.get(factor);
		if (existingPower == null) {
			factorToPower.put(factor, power);
		}
		else {
			factorToPower.put(factor, existingPower.add(power));
		}
	}
}