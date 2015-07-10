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
package com.sri.ai.grinder.api;

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
 * order according to "comes before" wrt <em>F</em>. Therefore:<br>
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
	Monomial asMonomial() throws IllegalStateException;
	
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
	 * @return the set of non-numeric constant factors contained within this
	 *         Polynomial.
	 */
	Set<Expression> getNonNumericConstantFactors();

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

	/**
	 * Formally,if <em>S</em> is a set of <b>signature factors</b> and
	 * <em>s</em> is a signature on them (a tuple of integers representing their
	 * powers), <em>S^s</em> is the product of the factors in <em>S</em>
	 * exponentiated by the corresponding integer powers. For example if
	 * <em>S</em> is <em>{x, y, z}</em> and the signature <em>s</em> is
	 * <em>(2, 3, 4)</em>, then <em>S^s</em> is <em>x^2 * y^3 * z^4</em>.<br>
	 * <br>
	 * The projection of <em>P</em> defined on <b>signature factors</b>
	 * <em>F</em> on subset <em>S</em> of <em>F</em> is defined by:<br>
	 * 
	 * <pre>
	 * <code>
	 * projection(P, S):
	 *     P' = empty polynomial of monomials, based on variable factors S
	 *     for each signature s on S present in P,
	 *         a_s = sum of all monomials t_i such that there is a monomial t_i*S^s in P
	 *         P ' = P' + a_s * S^s
	 *    return polynomial on set of monomials P' 
	 * </code>
	 * </pre>
	 * 
	 * For example:<br>
	 * 
	 * <pre>
	 * <code>
	 * x^2 * y^4 * z + x^2 * y^3 + y + 10
	 * 
	 * with F = (x,y,z), and the polynomial is organized around the signatures for these 
	 * three factors (one monomial per possible signature).
	 * 
	 * The projection of the polynomial on a subset S of F re-organizes it around the 
	 * signatures on S alone.
	 * 
	 * So, if S = { x }, we get two monomials:
	 * 
	 * x^2 * y^4 * z (signature (2), coefficient z*y^4)
	 * +
	 * x^2 * y^3 (signature (2), coefficient y^3)
	 * +
	 * y (signature (0), coefficient y)
	 * +
	 * 10 (signature (0), coefficient 10) 
	 * 
	 * =
	 * 
	 * (y^4*z + y^3)*x^2 + (y + 10),  with respect to S
	 * 
	 * that is, the other factors (y and z) are treated like constants.
	 * We simply aggregate things multiplying variables in S and group them per signature in S.
	 * 
	 * If S = {y, z}, we get:
	 * 
	 * x^2 * y^4 * z (signature (4, 1), coefficient x^2)
	 * +
	 * x^2 * y^3 (signature (3, 0), coefficient x^2)
	 * +
	 * y (signature (1, 0), coefficient 1).
	 * +
	 * 10 (signature (0, 0), coefficient 10).
	 * 
	 * =
	 * 
	 * (x^2)y^4*z^1 + (x^2)^y^3 + y + 10 
	 * 
	 * with respect to S.
	 * 
	 * if S = { y }, we get (grouping on multiple signatures of S):
	 * 
	 * x^2 * y^4 * z (signature (4), coefficient x^2*z)
	 * +
	 * x^2 * y^3 (signature (3), coefficient x^2)
	 * +
	 * y (signature (1), coefficient 1)
	 * +
	 * 10 (signature (0), coefficient 10) 
	 * 
	 * = 
	 * 
	 * (x^2*z)*y^4 + (x^2)^y^3 + y + 10
	 * 
	 * 
	 * </code>
	 * </pre>
	 * 
	 * @param subsetOfSignatureFactors
	 *            a subset of the signature factors of this polynomial on which
	 *            to perform the projection.
	 * @return the project of this Polynomial based on the given subset of
	 *         signature factors.
	 * @throws IllegalArgumentException
	 *             if the subsetSignatureFactors is not actually a subset of
	 *             this polynomials signature factors.
	 */
	Polynomial project(Set<Expression> subsetOfSignatureFactors)
			throws IllegalArgumentException;
}
