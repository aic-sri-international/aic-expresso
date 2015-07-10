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

import static com.sri.ai.grinder.core.DefaultMonomial.isLegalExponent;
import static com.sri.ai.grinder.core.DefaultMonomial.simplifyExponentIfPossible;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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
import com.sri.ai.grinder.api.Monomial;
import com.sri.ai.grinder.api.Polynomial;
import com.sri.ai.grinder.helper.MonomialComparator;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

/**
 * Default implementation of the Polynomial interface.
 * 
 * @author oreilly
 *
 */
@Beta
public class DefaultPolynomial extends AbstractExpressionWrapper implements
		Polynomial {
	//
	public static final Expression POLYNOMIAL_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.PLUS);
	//
	private static final long serialVersionUID = 1L;
	//
	private static final Expression PLUS_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.PLUS);
	private static final Expression MINUS_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.MINUS);
	private static final Expression TIMES_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.TIMES);
	private static final Expression DIVISION_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.DIVISION);
	private static final Expression EXPONENTIATION_FUNCTOR = Expressions
			.makeSymbol(FunctorConstants.EXPONENTIATION);
	//
	private static final ExpressionComparator _factorComparator = new ExpressionComparator();
	//
	private List<Expression>              signatureFactors          = null;
	private MonomialComparator            monomialComparator        = null;
	private List<Monomial>                orderedSummands           = null;
	private Set<Expression>               nonNumericConstantFactors = null;
	private Map<List<Rational>, Monomial> signatureTermMap          = null;

	public static Polynomial make(Expression expression,
			List<Expression> signatureFactors) {
		Polynomial result = null;
		if (expression.hasFunctor(PLUS_FUNCTOR)) {
			// E1 + E2 --> make(E1, F).add(make(E2, F))
			// Note: as + can have 0 or more arguments we start with 0 (i.e. the
			// neutral element).
			result = makeFromMonomial(Expressions.ZERO, signatureFactors);
			for (Expression summandExpression : expression.getArguments()) {
				Polynomial summand = make(summandExpression, signatureFactors);
				result = result.add(summand);
			}
		} else if (expression.hasFunctor(MINUS_FUNCTOR)) {
			// E1 - E2 --> make(E1, F).minus(make(E2, F))
			if (expression.numberOfArguments() == 1) {
				// is '-(something)' which is equivalent to
				// -1 * (something)
				Polynomial p1 = makeFromMonomial(Expressions.MINUS_ONE,
						signatureFactors);
				Polynomial p2 = make(expression.get(0), signatureFactors);
				result = p1.times(p2);
			} else if (expression.numberOfArguments() == 2) {
				Polynomial p1 = make(expression.get(0), signatureFactors);
				Polynomial p2 = make(expression.get(1), signatureFactors);
				result = p1.minus(p2);
			} else {
				throw new IllegalArgumentException(
						"Not a legal minus expression: " + expression);
			}
		} else if (expression.hasFunctor(TIMES_FUNCTOR)) {
			// E1 * E2 --> make(E1, F).times(make(E2, F))
			// Note: as * can have 0 or more arguments we start with 1 (i.e. the
			// neutral element).
			result = makeFromMonomial(Expressions.ONE, signatureFactors);
			// Note: handle case when one of the multipliers is 0 (i.e. an
			// absorbing element).
			Polynomial absorbingElement = makeFromMonomial(Expressions.ZERO,
					signatureFactors);
			for (Expression multiplierExpression : expression.getArguments()) {
				Polynomial multiplier = make(multiplierExpression,
						signatureFactors);
				if (multiplier.equals(absorbingElement)) {
					// * by 0 so answer is 0, no need to worry about any other
					// multipliers.
					result = absorbingElement;
					break;
				} else {
					result = result.times(multiplier);
				}
			}
		} else if (expression.hasFunctor(DIVISION_FUNCTOR)) {
			// E1 / E2 --> make(E1, F).divide(make(E2, F))
			Polynomial p1 = make(expression.get(0), signatureFactors);
			Polynomial p2 = make(expression.get(1), signatureFactors);

			Pair<Polynomial, Polynomial> quotientAndRemainder = p1.divide(p2);
			Polynomial quotient = quotientAndRemainder.first;
			Polynomial remainder = quotientAndRemainder.second;
			Polynomial zero = makeFromMonomial(Expressions.ZERO,
					signatureFactors);
			if (remainder.equals(zero)) {
				throw new UnsupportedOperationException(
						"Constructing a polynomial form a division operation that results in a remainder is not supported: "
								+ expression
								+ "\nquotient ="
								+ quotient
								+ "\nremainder=" + remainder);
			}
		} else if (expression.hasFunctor(EXPONENTIATION_FUNCTOR)) {
			// E1 ^ m with m an integer constant --> make(E1).exponentiate(m)
			Expression base  = expression.get(0);
			Expression power = simplifyExponentIfPossible(expression.get(1));
			if (isLegalExponent(power)) {
				Polynomial p1 = make(base, signatureFactors);
				result = p1.exponentiate(power.intValue());
			}
		}

		// E is a single-factor Monomial.
		if (result == null) {
			result = makeFromMonomial(expression, signatureFactors);
		}

		return result;
	}

	//
	// START-Polynomial
	@Override
	public List<Expression> getSignatureFactors() {
		return signatureFactors;
	}
	
	@Override
	public Map<List<Rational>, Monomial> getSignatureTermMap() {
		return signatureTermMap;
	}

	@Override
	public Monomial asMonomial() throws IllegalStateException {
		if (!isMonomial()) {
			throw new IllegalStateException("This polynomial, " + this
					+ ", is not a monomial");
		}

		Monomial result = orderedSummands.get(0);

		return result;
	}

	@Override
	public List<Monomial> getOrderedSummands() {
		return orderedSummands;
	}

	@Override
	public Set<Expression> getNonNumericConstantFactors() {
		return nonNumericConstantFactors;
	}

	@Override
	public Polynomial add(Polynomial summand) throws IllegalArgumentException {
		assertSameSignatures(summand);
		
		Polynomial result;
		
		if (isZero()) {
			result = summand;
		}
		else if (summand.isZero()) {
			result = this;
		}
		else {
			List<Monomial>      summands           = new ArrayList<>();
			Set<List<Rational>> combinedSignatures = new LinkedHashSet<>(this.getOrderedSummands().size()+summand.getOrderedSummands().size());
			combinedSignatures.addAll(this.getSignatureTermMap().keySet());
			combinedSignatures.addAll(summand.getSignatureTermMap().keySet());
			Set<Expression> signatureFactorsSet = new LinkedHashSet<>(getSignatureFactors());
			for (List<Rational> signature : combinedSignatures) {
				// NOTE: at least one of these assignments is guaranteed to be non-null.
				Monomial m1 = this.getSignatureTermMap().get(signature);
				Monomial m2 = summand.getSignatureTermMap().get(signature);
				if (m1 == null) {
					if (!m2.isZero()) {
						summands.add(m2);
					}
				}
				else if (m2 == null) {
					if (!m1.isZero()) {
						summands.add(m1);
					}
				}
				else {
					// Both have the same signature
					Monomial m1Coefficient = m1.getCoefficient(signatureFactorsSet);
					Monomial m2Coefficient = m2.getCoefficient(signatureFactorsSet);
					Expression summedCoefficient;
					if (m1Coefficient.isNumericConstant() && m2Coefficient.isNumericConstant()) {
						// We can add them
						summedCoefficient = Expressions.makeSymbol(m1Coefficient.getNumericConstantFactor().add(m2Coefficient.getNumericConstantFactor()));
					}
					else {
						List<Expression> plusArgs = new ArrayList<>();
						if (!m1Coefficient.isZero()) {
							plusArgs.add(m1Coefficient);
						}
						if (!m2Coefficient.isZero()) {
							plusArgs.add(m2Coefficient);
						}
						if (plusArgs.size() == 2) {
							summedCoefficient = new DefaultFunctionApplication(PLUS_FUNCTOR, Arrays.asList(m1Coefficient, m2Coefficient));
						}
						else {
							summedCoefficient = plusArgs.get(0);
						}
					}
					if (!Expressions.ZERO.equals(summedCoefficient)) {
						List<Expression> args = new ArrayList<Expression>();
						Rational numericConstantFactor = Rational.ONE;
						if (Expressions.isNumber(summedCoefficient)) {
							numericConstantFactor = summedCoefficient.rationalValue();
						}
						else {
							args.add(summedCoefficient);
						}
						args.addAll(getSignatureFactors());
						Collections.sort(args, _factorComparator);
						List<Rational> orderedPowers = new ArrayList<>();
						for (Expression factor : args) {
							if (factor == summedCoefficient) {
								orderedPowers.add(Rational.ONE);
							}
							else {
								orderedPowers.add(m1.getPowerOfFactor(factor));
							}
						}
						Monomial sum = DefaultMonomial.make(numericConstantFactor, args, orderedPowers);
						summands.add(sum);
					}
				}
			}
			// In case all the summands cancel each other out
			if (summands.isEmpty()) {
				result = makeFromMonomial(Expressions.ZERO, signatureFactors);
			}
			else {
				result = new DefaultPolynomial(summands, getSignatureFactors());
			}
		}
		
		return result;
	}

	@Override
	public Polynomial minus(Polynomial subtrahend)
			throws IllegalArgumentException {
		assertSameSignatures(subtrahend);
		
		Polynomial result;
		
		if (subtrahend.isZero()) {
			result = this;
		}
		else {
			List<Monomial> negatedSubtrahendSummands = new ArrayList<>();
			subtrahend.getOrderedSummands().forEach(summand -> negatedSubtrahendSummands.add(summand.times(DefaultMonomial.MINUS_ONE)));
			Polynomial negatedSubtrahend = new DefaultPolynomial(negatedSubtrahendSummands, getSignatureFactors());
			
			result = add(negatedSubtrahend);
		}
		
		return result;
	}

	@Override
	public Polynomial times(Polynomial multiplier)
			throws IllegalArgumentException {
		assertSameSignatures(multiplier);
		
		Polynomial result;
		
		// Optimization: return 0 if either numeric constant factor is 0
		if (isZero()) {
			result = this;
		}
		else if (multiplier.isZero()) {
			result = multiplier;
		}
		else if (isOne()) { // Optimization, neutral element
			result = multiplier;
		}
		else if (multiplier.isOne()) { // Optimization, neutral element
			result = this;
		}
		else {
			// Base case
			if (isMonomial() && multiplier.isMonomial()) {
				result = makeFromMonomial(this.asMonomial().times(multiplier.asMonomial()), getSignatureFactors());
			}
			else {
// TODO - implement			
				result = null; 
			}
		}
		
		return result;
	}

	@Override
	public Pair<Polynomial, Polynomial> divide(Polynomial divisor)
			throws IllegalArgumentException {
		assertSameSignatures(divisor);
		
		return null; // TODO - implement
	}

	@Override
	public Polynomial exponentiate(int exponent)
			throws IllegalArgumentException {
		if (exponent < 0) {
			throw new IllegalArgumentException("Exponent must be a non-negative integer, given: "+exponent);
		}
		
		Polynomial result;
		// Base case
		if (isMonomial()) {
			result = makeFromMonomial(asMonomial().exponentiate(exponent), getSignatureFactors());
		}
		else {
// TODO - implement			
			result = null; 
		}
		
		return result;
	}

	@Override
	public Polynomial project(Set<Expression> subsetOfSignatureFactors)
			throws IllegalArgumentException {
		if (!getSignatureFactors().containsAll(subsetOfSignatureFactors)) {
			throw new IllegalArgumentException("Argument is not a subset of this Polynomials signature of factors.");
		}
		
		return null; // TODO - implement
	}

	// END-Polynomial
	//

	//
	// START-Expression
	@Override
	public Polynomial clone() {
		Polynomial result = new DefaultPolynomial(orderedSummands, signatureFactors);
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
		List<Expression> signatureFactors = getSignatureFactors();
		Expression result = super.set(i, newIthArgument);
		// Ensure we make a Polynomial out of the modified expression
		result = make(result, signatureFactors);
		return result;
	}
	
	@Override
	public String toString() {
		// We don't need to re-compute as are immutable
		return getInnerExpression().toString();
	}
	// END-Expression
	//
	
	//
	// PROTECTED
	//
	protected Expression computeInnerExpression() {
		Expression result;
		
		if (orderedSummands.size() == 1) {
			result = orderedSummands.get(0);
		}
		else {
			result = new DefaultFunctionApplication(POLYNOMIAL_FUNCTOR, new ArrayList<>(orderedSummands));
		}
		
		return result;
	}

	//
	// PRIVATE
	//
	private DefaultPolynomial(List<Monomial> summands,
			List<Expression> signatureFactors) {
		// NOTE: we use Collections.unmodifiable<...> to ensure Polynomials are
		// immutable.
		this.monomialComparator = new MonomialComparator(signatureFactors);
		Collections.sort(summands, monomialComparator);
		this.orderedSummands  = Collections.unmodifiableList(summands);
		this.signatureFactors = Collections.unmodifiableList(signatureFactors);

		this.signatureTermMap          = new LinkedHashMap<>();
		this.nonNumericConstantFactors = new LinkedHashSet<>();
		for (Monomial monomial : orderedSummands) {
			this.nonNumericConstantFactors.addAll(monomial
					.getOrderedNonNumericConstantFactors());
			List<Rational> signature = monomial.getSignature(this.signatureFactors);
			if (this.signatureTermMap.containsKey(signature)) {
				throw new IllegalArgumentException("Trying to create a polynomial with like terms");
			}
			this.signatureTermMap.put(signature, monomial);
		}
		this.nonNumericConstantFactors = Collections.unmodifiableSet(this.nonNumericConstantFactors);
		this.signatureTermMap          = Collections.unmodifiableMap(this.signatureTermMap);
	}
	
	private void assertSameSignatures(Polynomial other) {
		if (!getSignatureFactors().equals(other.getSignatureFactors())) {
			throw new IllegalArgumentException("Signature factors are not equal between polynomials");
		}
	}

	private static Polynomial makeFromMonomial(Expression monomialExpression,
			List<Expression> signatureFactors) {
		Monomial monomial = DefaultMonomial.make(monomialExpression);
		if (!monomial.isNumericConstant()) {
			// Need to pull out the factors are to be treated together as a single constant
			// based on the polynomial's signature of factors.
			Set<Expression> signatureFactorsSet = new HashSet<>(signatureFactors);
			Monomial        coefficient         = monomial.getCoefficient(signatureFactorsSet);
			if (!coefficient.isNumericConstant()) {
				// Have factors of the monomial that need to be treated as a single constant
				// based on the polynomials signature of factors.
				List<Expression>          orderedFactors = new ArrayList<>();  
				Map<Expression, Rational> factorToPower  = new HashMap<>();
				
				orderedFactors.add(coefficient);
				factorToPower.put(coefficient, Rational.ONE);
				
				for (Expression factor : monomial.getOrderedNonNumericConstantFactors()) {
					if (signatureFactorsSet.contains(factor)) {
						orderedFactors.add(factor);
						factorToPower.put(factor, monomial.getPowerOfFactor(factor));
					}
				}
				
				Rational numericConstant = monomial.getNumericConstantFactor();
				// Handle the case where we called
				// coefficient(3*x^2*y^2, {3, x}) = 1*y^2
				// in that the coefficent gets the numeric constant from the
				// original monomial as the numeric constant is listed as one
				// of the signature factors.
				if (coefficient.getNumericConstantFactor().equals(numericConstant)) {
					numericConstant = Rational.ONE; // i.e. numeric constant was in signature factors.
				}
				List<Rational> orderedPowers = new ArrayList<>();
				Collections.sort(orderedFactors, _factorComparator);
				orderedFactors.forEach(factor -> orderedPowers.add(factorToPower.get(factor)));
			
			
				monomial = DefaultMonomial.make(numericConstant, orderedFactors, orderedPowers);
			}
		}
		
		Polynomial result = new DefaultPolynomial(
				Collections.singletonList(monomial), signatureFactors);

		return result;
	}
}
