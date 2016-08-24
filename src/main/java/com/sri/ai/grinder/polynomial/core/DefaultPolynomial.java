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

import static com.sri.ai.grinder.polynomial.core.DefaultMonomial.isLegalExponent;
import static com.sri.ai.grinder.polynomial.core.DefaultMonomial.simplifyExponentIfPossible;

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
import com.sri.ai.grinder.polynomial.api.Monomial;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Multinomial;
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
	private List<Expression>              variables          = null;
	private MonomialComparator            monomialComparator = null;
	private List<Monomial>                orderedSummands    = null;
	private Map<List<Rational>, Monomial> signatureTermMap   = null;

	public static Polynomial make(Expression expression) {
		List<Expression> generalizedVariables = extractGeneralizedVariables(expression);
		Polynomial       result               = make(expression, generalizedVariables);
		
		return result;
	}
			
	public static Polynomial make(Expression expression,
			List<Expression> variables) {
		Polynomial result = null;
		if (expression.hasFunctor(PLUS_FUNCTOR)) {
			// E1 + E2 --> make(E1, F).add(make(E2, F))
			// Note: as + can have 0 or more arguments we start with 0 (i.e. the
			// neutral element).
			result = makeFromMonomial(Expressions.ZERO, variables);
			for (Expression summandExpression : expression.getArguments()) {
				Polynomial summand = make(summandExpression, variables);
				result = result.add(summand);
			}
		} else if (expression.hasFunctor(MINUS_FUNCTOR)) {
			// E1 - E2 --> make(E1, F).minus(make(E2, F))
			if (expression.numberOfArguments() == 1) {
				// is '-(something)' which is equivalent to
				// -1 * (something)
				Polynomial p1 = makeFromMonomial(Expressions.MINUS_ONE,
						variables);
				Polynomial p2 = make(expression.get(0), variables);
				result = p1.times(p2);
			} else if (expression.numberOfArguments() == 2) {
				Polynomial p1 = make(expression.get(0), variables);
				Polynomial p2 = make(expression.get(1), variables);
				result = p1.minus(p2);
			} else {
				throw new IllegalArgumentException(
						"Not a legal minus expression: " + expression);
			}
		} else if (expression.hasFunctor(TIMES_FUNCTOR)) {
			// E1 * E2 --> make(E1, F).times(make(E2, F))
			// Note: as * can have 0 or more arguments we start with 1 (i.e. the
			// neutral element).
			result = makeFromMonomial(Expressions.ONE, variables);
			// Note: handle case when one of the multipliers is 0 (i.e. an
			// absorbing element).
			Polynomial absorbingElement = makeFromMonomial(Expressions.ZERO,
					variables);
			for (Expression multiplierExpression : expression.getArguments()) {
				Polynomial multiplier = make(multiplierExpression,
						variables);
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
			Polynomial dividend = make(expression.get(0), variables);
			Polynomial divisor  = make(expression.get(1), variables);

			Pair<Polynomial, Polynomial> quotientAndRemainder = dividend.divide(divisor);
			Polynomial quotient  = quotientAndRemainder.first;
			Polynomial remainder = quotientAndRemainder.second;
			if (!remainder.isZero()) {
				// We have a remainder so want to treat it as a constant factor.
				// so we can construct an expression of the form: 
				//    f(x)                      remainder(x)
				// ----------  =  quotient(x) + ------------
				// divisor(x)                    divisor(x)
				Expression remainderDividedByDivisor = new DefaultFunctionApplication(DIVISION_FUNCTOR, Arrays.asList(remainder, divisor));
				result = quotient.add(makeFromMonomial(remainderDividedByDivisor, variables));
			}
			else {
				result = quotient;
			}
		} else if (expression.hasFunctor(EXPONENTIATION_FUNCTOR)) {
			// E1 ^ m with m an integer constant --> make(E1).exponentiate(m)
			Expression base  = expression.get(0);
			Expression power = simplifyExponentIfPossible(expression.get(1));
			if (isLegalExponent(power)) {
				Polynomial p1 = make(base, variables);
				result = p1.exponentiate(power.intValue());
			}
		}

		// E is a single-factor Monomial.
		if (result == null) {
			result = makeFromMonomial(expression, variables);
		}

		return result;
	}
	
	public static List<Expression> extractGeneralizedVariables(Expression polynomialExpression) {
		List<Expression> result               = new ArrayList<>();
		Set<Expression>  generalizedVariables = new HashSet<>();
		
		extractGeneralizedVariables(polynomialExpression, generalizedVariables);
		result.addAll(generalizedVariables);
		
		Collections.sort(result, _factorComparator);
		
		return result;
	}

	//
	// START-Polynomial
	@Override
	public List<Expression> getVariables() {
		return variables;
	}
	
	@Override
	public Map<List<Rational>, Monomial> getSignatureTermMap() {
		return signatureTermMap;
	}

	@Override
	public List<Monomial> getOrderedSummands() {
		return orderedSummands;
	}

	@Override
	public Polynomial add(Polynomial summand) throws IllegalArgumentException {
		assertSameVariables(summand);
		
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
					Monomial sum = addMonomialsWithSameSignature(m1, m2);
					if (!sum.isZero()) {
						summands.add(sum);
					}
				}
			}
			// In case all the summands cancel each other out
			if (summands.isEmpty()) {
				result = makeFromMonomial(Expressions.ZERO, variables);
			}
			else {
				result = new DefaultPolynomial(summands, getVariables());
			}
		}
		
		return result;
	}

	@Override
	public Polynomial minus(Polynomial subtrahend)
			throws IllegalArgumentException {
		assertSameVariables(subtrahend);
		
		Polynomial result;
		
		if (subtrahend.isZero()) {
			result = this;
		}
		else {
			List<Monomial> negatedSubtrahendSummands = new ArrayList<>();
			subtrahend.getOrderedSummands().forEach(summand -> negatedSubtrahendSummands.add(summand.times(DefaultMonomial.MINUS_ONE)));
			Polynomial negatedSubtrahend = new DefaultPolynomial(negatedSubtrahendSummands, getVariables());
			
			result = add(negatedSubtrahend);
		}
		
		return result;
	}

	@Override
	public Polynomial times(Polynomial multiplier)
			throws IllegalArgumentException {
		assertSameVariables(multiplier);
		
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
				result = makeFromMonomial(this.asMonomial().times(multiplier.asMonomial()), getVariables());
			}
			else {
				// OPTIMIZATION NOTE: Instead of incrementally adding to a result polynomial for each of the
				// products from the cross product of this polynomials and the multipliers terms. We instead
				// collect up the cross product computations first, then sort them based on monomial 'comes before'
				// and then add like monomials together. Only then is an actual Polynomial result constructed.
				// This reduces the number of additions required by the '# term in the result' and also removes 
				// the need for the creation of 'cross product # of terms' of intermediate polynomial objects 
				// in order to come up with a final result.
				List<Monomial> products = new ArrayList<>(getOrderedSummands().size()+multiplier.getOrderedSummands().size());
				for (Monomial multiplicandMonomial : getOrderedSummands()) {
					for (Monomial multiplierMonomial : multiplier.getOrderedSummands()) {
						Monomial monomialProduct = multiplicandMonomial.times(multiplierMonomial);
						products.add(monomialProduct);						
					}
				}
				// Ensure we sort so that it is easy to add up like terms together for the final result
				Collections.sort(products, monomialComparator);
				List<Monomial> summedLikeProducts = new ArrayList<>(products.size());
				for (Monomial product : products) {
					int summedIdx = summedLikeProducts.size()-1;
					if (summedIdx < 0) {
						summedLikeProducts.add(product);
					}
					else {
						Monomial sumOfLikeTerms = summedLikeProducts.get(summedIdx);
						// are like terms, add them and track their sum
						if (sumOfLikeTerms.areLikeTerms(product, getVariables())) {
							sumOfLikeTerms = addMonomialsWithSameSignature(sumOfLikeTerms, product);
							summedLikeProducts.set(summedIdx, sumOfLikeTerms);
						}
						else {
							summedLikeProducts.add(product);
						}
					}
				}
				
				summedLikeProducts.removeIf(term -> term.isZero());
				
				result = new DefaultPolynomial(summedLikeProducts, getVariables());
			}
		}
		
		return result;
	}

	@Override
	public Pair<Polynomial, Polynomial> divide(Polynomial divisor)
			throws IllegalArgumentException {
		assertSameVariables(divisor);
		
		Pair<Polynomial, Polynomial> result;
		
		if (isZero()) {
			// 0 / divisor = 0
			result = new Pair<>(this, this);
		} // Base case
		else if (isMonomial() && divisor.isMonomial()) { 
			Pair<Monomial, Monomial> monomialQuotientAndRemainder = asMonomial().divide(divisor.asMonomial());
			result = new Pair<>(makeFromMonomial(monomialQuotientAndRemainder.first, getVariables()),
					            makeFromMonomial(monomialQuotientAndRemainder.second, getVariables()));
		}
		else if (divisor.isNumericConstant()) {
			// In this case do not need to worry about remainders as can always
			// divide using a numeric constant divisor.
			Monomial       monomialDivisor = divisor.asMonomial();
			List<Monomial> quotients       = new ArrayList<>();				
			for (Monomial term : getOrderedSummands()) {
				Pair<Monomial, Monomial> monomialQuotientAndRemainder = term.divide(monomialDivisor);
				if (!monomialQuotientAndRemainder.second.isZero()) {
					throw new IllegalStateException("Got an unexpected remainder from " + term + " / " + divisor);						
				}
				quotients.add(monomialQuotientAndRemainder.first);
			}
			result = new Pair<>(new DefaultPolynomial(quotients, getVariables()), 
					            makeFromMonomial(DefaultMonomial.ZERO, getVariables()));						
		}
		else {
			// Univariate case
			if (getVariables().size() == 1) {
// TODO - implement faster synthetic division version
//        see: https://en.wikipedia.org/wiki/Synthetic_division
				// Perform Polynomial Long Division
				Polynomial quotient  = makeFromMonomial(DefaultMonomial.ZERO, getVariables());
				Polynomial remainder = this;
				
				Monomial leadingDivisorTerm = divisor.getOrderedSummands().get(0);
				do {
					Monomial leadingNumeratorTerm = remainder.getOrderedSummands().get(0);
					
					Pair<Monomial, Monomial> monomialQuotientAndRemainder = leadingNumeratorTerm.divide(leadingDivisorTerm);
					if (!monomialQuotientAndRemainder.second.isZero()) {
						break; // Could not divide, i.e. have a remainder
					}
					Polynomial monomialQuotient = makeFromMonomial(monomialQuotientAndRemainder.first, getVariables());
					quotient  = quotient.add(monomialQuotient);
					remainder = remainder.minus(divisor.times(monomialQuotient));		
				} while (!remainder.isZero());
				
				result = new Pair<>(quotient, remainder);
			}
			else {
// TODO - Multivariate case, currently not supported 
// See: https://en.wikipedia.org/wiki/Gr%C3%B6bner_basis
// for generalization of long division to the multivariate case.
				result = new Pair<>(makeFromMonomial(DefaultMonomial.ZERO, getVariables()), this);
			}
		}
		
		return result;
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
			result = makeFromMonomial(asMonomial().exponentiate(exponent), getVariables());
		}
		else {
			if (exponent == 0) {
				result = makeFromMonomial(DefaultMonomial.ONE, getVariables());
			}
			else if (exponent == 1 || isZero() || isOne()) {
				result = this;
			}
			else {
				Map<List<Rational>, Monomial> expandedLikeTerms = new HashMap<>();
				Multinomial multinomial                         = new Multinomial(exponent, numberOfTerms());
				do {					
					Monomial coefficient = DefaultMonomial.make(Expressions.makeSymbol(multinomial.choose()));
					Monomial product     = getOrderedSummands().get(0).exponentiate(multinomial.getClassSize(0));
					for (int i = 1; i < numberOfTerms(); i++) {
						product = product.times(getOrderedSummands().get(i).exponentiate(multinomial.getClassSize(i)));
					}
					product = coefficient.times(product);

					List<Rational> productSignature = product.getSignature(getVariables());
					Monomial       existingLikeTerm = expandedLikeTerms.get(productSignature);
					if (existingLikeTerm == null) {
						if (!product.isZero()) {
							expandedLikeTerms.put(productSignature, product);
						}
					}
					else {
						Monomial sumOfLikeTerms = addMonomialsWithSameSignature(existingLikeTerm, product);
						if (sumOfLikeTerms.isZero()) {							
							expandedLikeTerms.remove(productSignature);
						}
						else {
							expandedLikeTerms.put(productSignature, sumOfLikeTerms);
						}
					}
						
				} while (multinomial.iterate());
				
				List<Monomial> expandedTerms = new ArrayList<>(expandedLikeTerms.values());
				result = new DefaultPolynomial(expandedTerms, getVariables());
			}
		}
		
		return result;
	}

	// END-Polynomial
	//

	//
	// START-Expression
	@Override
	public Polynomial clone() {
		Polynomial result = new DefaultPolynomial(orderedSummands, variables);
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
//		List<Expression> variables = getVariables();
//		Expression result = super.set(i, newIthArgument);
//		// Ensure we make a Polynomial out of the modified expression
//		result = make(result, variables);
//		return result;

		// the above is incorrect because Expression.set is not supposed to change the structure of an expression,
		// for example changing the number of sub-expressions.
		Expression result = super.set(i, newIthArgument);
		return result;
	}
	
	@Override
	public int compareTo(Object anotherObject) {
		int result;
		if (anotherObject instanceof Monomial) {
			// NOTE: Don't know the monomials signature of factors, so work with assumption are the same
			// if being compared to.
			result = monomialComparator.compare(getOrderedSummands().get(0), (Monomial) anotherObject);
		}
		else if (anotherObject instanceof Polynomial) {
			result = 0; // Set to something, we are guaranteed to go through the following for loop 
			Polynomial otherPolynomial = (Polynomial) anotherObject;
			if (this.getVariables().equals(otherPolynomial.getVariables())) {
				for (int i = 0, j = 0; i < this.getOrderedSummands().size() && j < otherPolynomial.getOrderedSummands().size(); i++, j++) {
					result = monomialComparator.compare(getOrderedSummands().get(i), otherPolynomial.getOrderedSummands().get(j));
					if (result != 0) {
						break;
					}
				}
				if (result == 0) {
					if (this.getOrderedSummands().size() > otherPolynomial.getOrderedSummands().size()) {
						result = -1;
					}
					else if (this.getOrderedSummands().size() < otherPolynomial.getOrderedSummands().size()) {
						result = 1;
					}
				}
			}
			else {
				// Can only compare degrees of leading term as they have different variables
				result = otherPolynomial.getOrderedSummands().get(0).degree() - getOrderedSummands().get(0).degree();
				// Ensure we normalize.
				if (result < 0) {
					result = -1;
				}
				else if (result > 0) {
					result = 1;
				}
			}
		}
		else {
			result = super.compareTo(anotherObject);
		}
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
	@Override
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
			List<Expression> variables) {
		// NOTE: we use Collections.unmodifiable<...> to ensure Polynomials are
		// immutable.
		this.monomialComparator = new MonomialComparator(variables);
		Collections.sort(summands, monomialComparator);
		this.orderedSummands  = Collections.unmodifiableList(summands);
		this.variables = Collections.unmodifiableList(variables);

		this.signatureTermMap          = new LinkedHashMap<>();
		for (Monomial monomial : orderedSummands) {
			List<Rational> signature = monomial.getSignature(this.variables);
			if (this.signatureTermMap.containsKey(signature)) {
				throw new IllegalArgumentException("Trying to create a polynomial with like terms: "+orderedSummands);
			}
			this.signatureTermMap.put(signature, monomial);
		}
		this.signatureTermMap          = Collections.unmodifiableMap(this.signatureTermMap);
	}
	
	private void assertSameVariables(Polynomial other) {
		if (!getVariables().equals(other.getVariables())) {
			throw new IllegalArgumentException("Variables are not equal between polynomials");
		}
	}

	private static Polynomial makeFromMonomial(Expression monomialExpression,
			List<Expression> variables) {
		Monomial monomial = DefaultMonomial.make(monomialExpression);
		if (!monomial.isNumericConstant()) {
			// Need to pull out the factors are to be treated together as a single constant
			// based on the polynomial's signature of factors.
			Monomial coefficient = monomial.getCoefficient(variables);
			if (!coefficient.isNumericConstant()) {
				// Have factors of the monomial that need to be treated as a single constant
				// based on the polynomials signature of factors.
				List<Expression>          orderedFactors = new ArrayList<>();  
				Map<Expression, Rational> factorToPower  = new HashMap<>();
				
				orderedFactors.add(coefficient);
				factorToPower.put(coefficient, Rational.ONE);
				Set<Expression> coefficientFactors = coefficient.getFactors();
				for (Expression factor : monomial.getOrderedNonNumericFactors()) {
					if (!coefficientFactors.contains(factor)) {
						orderedFactors.add(factor);
						factorToPower.put(factor, monomial.getPowerOfFactor(factor));
					}
				}
				
				Rational numericConstant = monomial.getNumericConstantFactor();
				// Handle the case where we called
				// coefficient(3*x^2*y^2, {3, x}) = 1*y^2
				// in that the coefficent gets the numeric constant from the
				// original monomial as the numeric constant is listed as one
				// of the variables.
				if (coefficient.getNumericConstantFactor().equals(numericConstant)) {
					numericConstant = Rational.ONE; // i.e. numeric constant was in variables.
				}
				List<Rational> orderedPowers = new ArrayList<>();
				Collections.sort(orderedFactors, _factorComparator);
				orderedFactors.forEach(factor -> orderedPowers.add(factorToPower.get(factor)));
				
				monomial = DefaultMonomial.make(numericConstant, orderedFactors, orderedPowers);
			}
		}
		
		Polynomial result = new DefaultPolynomial(
				Collections.singletonList(monomial), variables);

		return result;
	}
	
	private Monomial addMonomialsWithSameSignature(Monomial m1, Monomial m2) {
		Monomial result;
		
		// Both have the same signature
		Monomial m1Coefficient = m1.getCoefficient(getVariables());
		Monomial m2Coefficient = m2.getCoefficient(getVariables());
		Expression summedCoefficient;
		if (m1Coefficient.isNumericConstant() && m2Coefficient.isNumericConstant()) {
			// We can add them
			summedCoefficient = Expressions.makeSymbol(m1Coefficient.getNumericConstantFactor().add(m2Coefficient.getNumericConstantFactor()));
		}
		else if (m1Coefficient.equals(m2Coefficient)) { // Compactly represent non-numeric coefficients that are equal 
			summedCoefficient = new DefaultFunctionApplication(TIMES_FUNCTOR, Arrays.asList(Expressions.TWO, m1Coefficient));
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
			else if (summedCoefficient.hasFunctor(TIMES_FUNCTOR)) { // i.e. coefficients are equal so write in compact form.
				numericConstantFactor = summedCoefficient.get(0).rationalValue();
				args.add(summedCoefficient.get(1));
			} else {
				args.add(summedCoefficient);
			}
			args.addAll(getVariables());
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
			result = DefaultMonomial.make(numericConstantFactor, args, orderedPowers);
		}
		else {
			result = DefaultMonomial.ZERO;
		}
		
		return result;
	}
	
	private static void extractGeneralizedVariables(Expression polynomialExpression, Set<Expression> generalizedVariables) {
		if (Expressions.isSymbol(polynomialExpression)) {
			if (!Expressions.isNumber(polynomialExpression)) {
				generalizedVariables.add(polynomialExpression);
			}
		}
		else if (Expressions.hasFunctor(polynomialExpression, PLUS_FUNCTOR)     ||
				 Expressions.hasFunctor(polynomialExpression, MINUS_FUNCTOR)    ||
				 Expressions.hasFunctor(polynomialExpression, TIMES_FUNCTOR)    ||
				 Expressions.hasFunctor(polynomialExpression, DIVISION_FUNCTOR) ||
				 Expressions.hasFunctor(polynomialExpression, EXPONENTIATION_FUNCTOR)) {
			for (Expression arg : polynomialExpression.getArguments()) {
				extractGeneralizedVariables(arg, generalizedVariables);
			}
		}
		else {
			// An unknown functor or other type of expression not expected
			// by a standard polynomial expression
			generalizedVariables.add(polynomialExpression);
		}
	}
}