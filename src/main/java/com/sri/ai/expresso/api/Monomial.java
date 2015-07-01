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
package com.sri.ai.expresso.api;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.util.math.Rational;

/**
 * A monomial is a product of a number and powers of variables, where the number
 * constant (if any) is the first argument and the variables are in alphabetical
 * order.<br>
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
 *
 */
@Beta
public interface Monomial extends FunctionApplication {
	/**
	 * 
	 * @return the numerical constant (i.e. coefficient) of the monomial.
	 */
	Rational getCoefficient();

	/**
	 * <b>NOTE:</b> To make the library as general as possible, any expression
	 * that is not a numeric constant or an application of +, -, *, / or ^ with
	 * constant integer exponent, is considered a "generalized variable".
	 * Therefore:
	 * 
	 * <pre>
	 * <code>
	 * vars(10 * |Dogs|^2 * |People|^3 * f(y) * x^2) = { |Dogs|, |People|, f(y), x }
	 * </code>
	 * </pre>
	 * 
	 * @return a lexicographically ordered list of the variables contained in
	 *         the monomial.
	 */
	List<Expression> getVariables();

	/**
	 * 
	 * @return the powers of the variables in the monomial, which map to the
	 *         lexicographical order of the variables.
	 */
	List<Rational> getPowersOfVariables();

	/**
	 * The <b>signature</b> of a monomial <em>M</em> wrt a tuple of variables
	 * <em>V</em> is the tuple of the powers of variables in <em>V</em> in
	 * <em>M</em> (the power is 0 if the variable is not present in <em>M</em>).<br>
	 * Examples:<br>
	 * 
	 * <pre>
	 * <code>
	 * signature(3 * y * x^2,  (x, y, z))  =  (2, 1, 0)
	 * signature(3,            (x, y, z))  =  (0, 0, 0)
	 * </code>
	 * </pre>
	 * 
	 * @param variables
	 *            a lexicographically ordered list of variables.
	 * @return the tuple of the powers of the given variables that are in this
	 *         Monomial (0 is returned for the power of variables not present in
	 *         this monomial).
	 */
	List<Rational> getSignature(List<Expression> variables);

	/**
	 * Two monomials <em>M1</em> and <em>M2</em> have <b>like terms</b> if they
	 * contain the same variables raised to the same powers, i.e. if vars(M1) =
	 * vars(M2) and signature(M1, vars(M1)) = signature(M2, vars(M2)).<br>
	 * Compatible Examples:<br>
	 * 
	 * <pre>
	 * <code>
	 *  x^2 * y and 3 * y * x^2
	 *  x and 3 * x
	 *  10 and 20
	 * </code>
	 * </pre>
	 * 
	 * @param monomial
	 *            the monomial to check compatibility with.
	 * @return true if this monomial is compatible with the given monomial,
	 *         false otherwise.
	 */
	default boolean haveLikeTerms(Monomial other) {
		boolean result = getVariables().equals(other.getVariables())
				&& getPowersOfVariables().equals(other.getPowersOfVariables());
		return result;
	}

	default Rational degree() {
		Rational result = getPowersOfVariables().stream().reduce(Rational.ZERO,
				(r1, r2) -> r1.add(r2));
		return result;
	}

// TODO - JavaDoc	
	Monomial times(Monomial another);

// TODO - JavaDoc	
	Monomial divide(Monomial another);
}