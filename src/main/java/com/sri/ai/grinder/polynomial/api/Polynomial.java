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

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

/**
 * A polynomial is a monomial, or a sum of monomials, associated with a tuple of
 * factors <em>F</em>, called the <b>signature factors</b>, such that no two
 * monomials are like terms to each other wrt <em>F</em>, and monomials are in
 * order according to "comes before" wrt <em>F</em>.
 * 
 * More informally, signature factors are the "variables" involved in the polynomial.
 * The reason this library does not simply use the term "variable"
 * is that it allows for more complex, unknown, terms to be treated as variables.
 * For example, we may not know what a set <em>D</em> is, but need to
 * represent a polynomial on its cardinality: <em>2*|D|^2 + |D|</em>.
 * 
 * A "signature" (with respect to signature factors)
 * of a monomial is the list of powers associated
 * to each of the signature factors.
 * Note that the signature factors are ordered,
 * and the signature follows that order.
 * For example, the monomial <em>x^2*y</em> has signature <code>(2,1)</code>
 * for signature factors <em>(x,y)</em>.
 * The choice of the word "signature" for this concept is due to the
 * fact that the signature of a monomial "gives away its identity"
 * and forces it to be grouped with other monomials of the same signature
 * when summed in a polynomial. 
 * 
 * Therefore:<br>
 * 
 * <pre>
 * <code>
 * 3*x + 2*x + 10 + x^2 wrt F = (x) 
 * 
 * is not a valid polynomial in this representation, but 
 * 
 * x^2 + 5*x + 10 wrt F = (x) 
 * 
 * is a valid polynomial in this representation.
 * </code>
 * </pre>
 * 
 * 
 * @author oreilly
 *
 */
@Beta
public interface Polynomial extends Expression {

	/**
	 * 
	 * @return the factors that are used to identify like terms in the
	 *         polynomial.
	 */
	List<Expression> getSignatureFactors();
	
	
	/**
	 *
	 * @return the signature term map for this polynomial (NOTE: no two terms
	 *         have the same signature in this representation).
	 */
	Map<List<Rational>, Monomial> getSignatureTermMap();

	/**
	 * 
	 * @return true if this Polynomial is equivalent to a Monomial.
	 */
	default boolean isMonomial() {
		boolean result = getOrderedSummands().size() == 1;
		return result;
	}

	/**
	 * If the Polynomial is equivalent to a Monomial get its representation as
	 * such.
	 * 
	 * @return this Polynomial's representation as a Monomial.
	 * @throws IllegalStateException
	 *             if this Polynomial is not equivalent to a Monomial.
	 */
	default Monomial asMonomial() throws IllegalStateException {
		if (!isMonomial()) {
			throw new IllegalStateException("This polynomial, " + this
					+ ", is not a monomial");
		}
	
		Monomial result = getOrderedSummands().get(0);
	
		return result;
	}
	
	/**
	 * 
	 * @return the number of terms in this polynomial.
	 */
	default int numberOfTerms() {
		int result = getOrderedSummands().size();
		return result;
	}
	
	/**
	 * 
	 * @return the monomial summands of this polynomial.
	 */
	List<Monomial> getOrderedSummands();
	
	/**
	 * 
	 * @return true if this Polynomial is equivalent to a numeric constant;
	 */
	default boolean isNumericConstant() {
		boolean result = isMonomial() && asMonomial().isNumericConstant();
		return result;
	}
	
	/**
	 * 
	 * @return true if this Polynomial is equivalent to the numeric constant zero.
	 */
	default boolean isZero() {
		boolean result = isMonomial() && asMonomial().isZero();
		return result;
	}
	
	/**
	 * 
	 * @return true if this Polynomial is equivalent to the numeric constant one.
	 */
	default boolean isOne() {
		boolean result = isMonomial() && asMonomial().isOne();
		return result;
	}
 
	/**
	 * 
	 * @return the set of non-numeric constant factors contained within this
	 *         Polynomial.
	 */
	Set<Expression> getNonNumericConstantFactors();
	
	/**
	 * The degree of a polynomial is the highest degree of its terms.
	 * 
	 * @return the degree of the polynomial.
	 */
	default int degree() {
		// Default implementation makse assumption that the polynomial
		// is represented in canonical form with terms with the highests
		// degree listed first.
		int result = getOrderedSummands().get(0).degree();
		return result;
	}

	/**
	 * Add this polynomial to another polynomial and return a new Polynomial
	 * representing the sum.
	 * 
	 * <pre>
	 * <code>
	 * summand(this) + summand = sum.
	 * </code>
	 * </pre>
	 * 
	 * @param summand
	 *            the summand to be added to this polynomial.
	 * @return the sum of this and the summand.
	 * @throws IllegalArgumentException
	 *             if the summand does not share the same <b>signature
	 *             factors</b> as this polynomial.
	 */
	Polynomial add(Polynomial summand) throws IllegalArgumentException;

	/**
	 * Subtract a given polynomial from this polynomial and return a new
	 * polynomial representing the difference of the two.
	 * 
	 * <pre>
	 * <code>
	 * minuend(this) - subtrahend = difference.
	 * </code>
	 * </pre>
	 * 
	 * @param subtrahend
	 *            the subtrahend.
	 * @return the difference of this polynomial minus the subtrahend.
	 * @throws IllegalArgumentException
	 *             if the subtrahend does not share the same <b>signature
	 *             factors</b> as this polynomial.
	 */
	Polynomial minus(Polynomial subtrahend) throws IllegalArgumentException;

	/**
	 * Mulitply this polynomial by another.
	 * 
	 * <pre>
	 * <code>
	 * multiplicand(this) * multiplier = product.
	 * </code>
	 * </pre>
	 * 
	 * @param multiplier
	 *            the multiplier.
	 * @return the project of this polynomial and the given multiplier.
	 * @throws IllegalArgumentException
	 *             if the multiplier does not share the same <b>signature
	 *             factors</b> as this polynomial.
	 */
	Polynomial times(Polynomial multiplier) throws IllegalArgumentException;

	/**
	 * Divide this polynomial by another.
	 * 
	 * <pre>
	 * <code>
	 * dividend(this) / divisor = (quotient, remainder).
	 * </code>
	 * </pre>
	 * 
	 * @param divisor
	 * @return a pair consisting of the quotient and remainder of this/divisor.
	 * @throws IllegalArgumentException
	 *             if the divisor does not share the same <b>signature
	 *             factors</b> as this polynomial.
	 */
	Pair<Polynomial, Polynomial> divide(Polynomial divisor)
			throws IllegalArgumentException;

	/**
	 * Raise this polynomial to a given power.
	 * 
	 * @param exponent
	 *            a non negative integer exponent to raise the polynomial to.
	 * @return a polynomial which is the result of raising this polynomial to
	 *         the given exponent.
	 * @throws IllegalArgumentException
	 *             if the exponent is negative.
	 */
	Polynomial exponentiate(int exponent) throws IllegalArgumentException;
}
