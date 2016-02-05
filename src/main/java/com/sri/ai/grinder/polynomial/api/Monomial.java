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
package com.sri.ai.grinder.polynomial.api;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

/**
 * A monomial is a product of factors, where the numeric constant factor (if
 * any) is the first argument and the non-numeric constant factors are in order.<br>
 * 
 * Examples: <br>
 * 
 * <pre>
 * <code>
 * 3*x^2
 * y
 * y^2
 * 10
 * 4*x^3*y^2
 * 150*x^2
 * </code>
 * </pre>
 * 
 * @author oreilly
 */
@Beta
public interface Monomial extends Expression {
	/**
	 * 
	 * @return the numeric constant of the monomial (if not explicitly defined
	 *         is 1).
	 */
	Rational getNumericConstantFactor();

	/**
	 * 
	 * @return true if this monomial is equivalent to a numeric constant;
	 */
	default boolean isNumericConstant() {
		boolean result = getOrderedNonNumericFactors().size() == 0;
		return result;
	}

	/**
	 * 
	 * @return true if this monomial is equivalent to the numeric constant zero.
	 */
	default boolean isZero() {
		boolean result = getNumericConstantFactor().equals(Rational.ZERO);
		return result;
	}

	/**
	 * 
	 * @return true if this monomial is equivalent to the numeric constant one.
	 */
	default boolean isOne() {
		boolean result = getNumericConstantFactor().equals(Rational.ONE)
				&& getOrderedNonNumericFactors().size() == 0;
		return result;
	}

	/**
	 * The <b>factors of a monomial</b> <em>M</em> is the Collection factors(M).
	 * They are the expressions being multiplied, and possibly exponentiated by
	 * non-negative integers. Example:
	 * 
	 * <pre>
	 * <code>
	 * factors(10 * |Dogs|^2 * |People|^3 * f(y) * (x + 2) * x^2) = { 10, |Dogs|, |People|, f(y), x + 2, x }
	 * </code>
	 * </pre>
	 * 
	 * @return a Set representation of the factors contained within the Monomial
	 *         (this includes the numeric constant factor).
	 */
	Set<Expression> getFactors();

	/**
	 * Get a unique list, ordered, of the non-numeric constant factors in the
	 * monomial.
	 * 
	 * @return an ordered list of the non-numeric constant factors contained in
	 *         the monomial.
	 */
	List<Expression> getOrderedNonNumericFactors();

	/**
	 * 
	 * @return the powers of the non-numeric constant factors in the monomial,
	 *         which map to the order of these factors.
	 */
	List<Rational> getPowersOfNonNumericFactors();

	/**
	 * The <b>coefficient</b> of a monomial <em>M</em> wrt a set of factors
	 * <em>F</em> is the product of the remaining factors of <em>M</em>.
	 * Example:
	 * 
	 * <pre>
	 * <code>
	 * coefficient(3*x^2*y^4, { x }) = 3* y^4
	 * </code>
	 * </pre>
	 * 
	 * @param factors
	 *            what are to be considered the factors of the monomial (i.e.
	 *            not part of the coefficient).
	 * @return the product of the remaining factors in this monomial not in the
	 *         given list of factors.
	 */
	Monomial getCoefficient(List<Expression> factors);

	/**
	 * Get the power of the given factor if it is a factor of the monomial
	 * otherwise return 0.
	 * 
	 * @param factor
	 *            the factor whose power is to be retrieved.
	 * @return the power of the given factor if the factor is contained in the
	 *         monomial. If not contained in the monomial will return 0.
	 */
	Rational getPowerOfFactor(Expression factor);

	/**
	 * The <b>signature</b> of a monomial <em>M</em> wrt a tuple of factors
	 * <em>F</em> is the tuple of the powers of the factors in <em>F</em> in
	 * <em>M</em> (the power is 0 if the factor is not present in <em>M</em>).<br>
	 * Examples:<br>
	 * 
	 * <pre>
	 * <code>
	 * signature(3 * y * x^2,  (x, y, z))            =  (2, 1, 0)
	 * signature(3,            (x, y, z))            =  (0, 0, 0)
	 * signature(3 * (x + 2),  (x + 2, x, y, z))     =  (1, 0, 0, 0)
	 * signature(3 * (x + 2),  (3, x + 2, x, y, z))  =  (1, 1, 0, 0, 0)
	 * </code>
	 * </pre>
	 * 
	 * @param factors
	 *            a list of factors.
	 * @return the tuple of the powers of the given factors that are in this
	 *         Monomial (0 is returned for the power of factors not present in
	 *         this monomial).
	 */
	default List<Rational> getSignature(List<Expression> factors) {
		List<Rational> result = new ArrayList<>(factors.size());
		factors.forEach(factor -> result.add(getPowerOfFactor(factor)));
		return result;
	}

	/**
	 * Convenience method that gets the signature of this Monomial using the
	 * non-numeric constant factors contained within.
	 * 
	 * @return the tuple of the powers of the non-numeric constant factors that
	 *         are in this Monomial.
	 */
	default List<Rational> getSignature() {
		List<Expression> factors = this.getOrderedNonNumericFactors();

		List<Rational> result = getSignature(factors);

		return result;
	}

	/**
	 * Two monomials <em>M1</em> and <em>M2</em> are <b>like terms
	 * <em> wrt a tuple of factors <em>F</em> if signature(M1, F) =
	 * signature(M2, F).<br>
	 * Examples:
	 * 
	 * <pre>
	 * </code>
	 * x^2 * y  and 3 * y * x^2 wrt F = (x, y)
	 * x^2 * y  and 3 * y * x^2 wrt F = (x)
	 * x^2 * y  and 3 * y * x^2 wrt F = (y)
	 * x^2 * y  and 3 * y * x^2 wrt F = ()
	 * x and 3 * x wrt F = (x)
	 * 10 and 20 wrt F = ()
	 * </code>
	 * </pre>
	 * 
	 * @param monomial
	 *            the monomial to check if a like term to this monomial.
	 * @param factors
	 *            the factors to construct the signature used to determine if
	 *            two monomials are like terms.
	 * @return true if are like terms based on the given factors, false
	 *         otherwise.
	 */
	default boolean areLikeTerms(Monomial other, List<Expression> factors) {
		List<Rational> thisSignature = getSignature(factors);
		List<Rational> otherSignature = other.getSignature(factors);

		boolean result = thisSignature.equals(otherSignature);

		return result;
	}

	/**
	 * Convenience method that uses the combined non-numeric constant factors
	 * from this and the other Monomial to call areLikeTerms(other,
	 * combinedNonNumericConstantFactors).
	 * 
	 * @param other
	 *            the monomial to check if a like term to this monomial.
	 * @return true if are like terms based on the combined non-numeric constant
	 *         factors, false otherwise.
	 */
	default boolean areLikeTerms(Monomial other) {
		List<Expression> combinedNonNumericConstantFactors = Monomial
				.orderedUnionOfNonNumericConstantFactors(this, other);

		boolean result = areLikeTerms(other, combinedNonNumericConstantFactors);

		return result;
	}

	/**
	 * The degree of a monomial is the sum of the exponents of all its
	 * non-numeric constant factors (numeric constant factors have a degree of
	 * 0).
	 * 
	 * @return the degree of the monomial.
	 */
	int degree();

	/**
	 * Multiply this Monomial by another.
	 * 
	 * @param multiplier
	 *            the multiplier.
	 * @return a new Monomial representing the result of the multiplication.
	 */
	Monomial times(Monomial multiplier);

	/**
	 * Divide this monomial by another.
	 * 
	 * <pre>
	 * <code>
	 * this.divide(that) = this/that
	 * </code>
	 * </pre>
	 * 
	 * @param divisor
	 *            the divisor
	 * @return a pair consisting of the quotient and remainder of this/divisor.
	 */
	Pair<Monomial, Monomial> divide(Monomial divisor);

	/**
	 * Raise this monomial to a given power.
	 * 
	 * @param exponent
	 *            a non negative integer exponent to raise the monomial to.
	 * @return a monomial which is the result of raising this monomial to the
	 *         given exponent.
	 * @throws IllegalArgumentException
	 *             if the exponent is negative.
	 */
	Monomial exponentiate(int exponent) throws IllegalArgumentException;

	/**
	 * Create an ordered union (no duplicates) of the non-numeric constant
	 * factors contained in two Monomials.
	 * 
	 * @param m1
	 *            the first monomial.
	 * @param m2
	 *            the second monomial.
	 * @return the ordered union (no duplicates) of the non-numeric constant
	 *         factors contained in two Monomials.
	 */
	public static List<Expression> orderedUnionOfNonNumericConstantFactors(
			Monomial m1, Monomial m2) {
		List<Expression> m1Factors = m1.getOrderedNonNumericFactors();
		List<Expression> m2Factors = m2.getOrderedNonNumericFactors();
		// For efficiency ensure we have enough capacity in the union up front.
		List<Expression> result = new ArrayList<>(m1Factors.size()
				+ m2Factors.size());

		// NOTE: we know m1 and m2's non-numeric constant factors are
		// ordered, which we can take advantage of to perform the
		// ordered union more efficiently.
		int m1FactorsSize = m1Factors.size();
		int m2FactorsSize = m2Factors.size();
		Expression m1Factor, m2Factor;
		for (int i1 = 0, i2 = 0; i1 < m1FactorsSize || i2 < m2FactorsSize;) {
			if (i1 < m1FactorsSize && i2 < m2FactorsSize) {
				m1Factor = m1Factors.get(i1);
				m2Factor = m2Factors.get(i2);
				int m1FactorComparisonToM2Factor = m1Factor.compareTo(m2Factor);

				if (m1FactorComparisonToM2Factor == 0) { // are considered equal
					// just add one of them (as considered same) as do not want
					// duplicates in the returned union.
					result.add(m1Factor);
					i1++;
					i2++;
				} else if (m1FactorComparisonToM2Factor < 0) { // i.e. m1Factor
																// is before
																// m2Factor
					result.add(m1Factor);
					i1++;
				} else { // i.e. m2Factor is before m1Factor
					result.add(m2Factor);
					i2++;
				}
			} else if (i1 < m1FactorsSize) { // implies i2 is exhausted
				result.add(m1Factors.get(i1));
				i1++;
			} else { // implies i2 < m2FactorsSize and i1 is exhausted
				result.add(m2Factors.get(i2));
				i2++;
			}
		}

		return result;
	}
}