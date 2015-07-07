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

import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.util.base.Pair;

/**
 * A polynomial is an expression consisting of variables (or indeterminates) and
 * coefficients, that involves only the operations of addition, subtraction,
 * multiplication, and non-negative integer exponents. 
 * 
 * In this API a polynomial is a monomial, or a sum of monomials such that no two 
 * monomials have like terms, and the monomials are ordered from highest to lowest. 
 * Therefore:<br>
 * <pre><code>
 * 3*x  + 2*x  + 10 + x^2 
 * 
 * is not a valid polynomial in this representation, but:
 * 
 * x^2 + 5*x + 10 
 *  
 * is a valid polynomial.
 * </code></pre>  
 * 
 * @author oreilly
 *
 */
@Beta
public interface Polynomial extends FunctionApplication {

	/**
	 * 
	 * @return true if this Polynomial is equivalent to a Monomial.
	 */
	boolean isMonomial();
	
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
	 * @return the set of 'generalized variables' contained within this Polynomial.
	 */
	Set<Expression> getVariables();

// TODO - Complete JavaDoc of API	
	/**
	 * Add this polynomial to another polynomial and return a new Polynomial representing the sum.
	 * 
	 * <pre><code>
	 * summand(this) + summand = sum.
	 * </code></pre>
	 * 
	 * @param summand
	 * @return
	 */
	Polynomial add(Polynomial summand);
	
	/**
	 * minuend(this) - subtrahend = difference.
	 * 
	 * @param subtrahend
	 * @return
	 */
	Polynomial minus(Polynomial subtrahend);
	
	/**
	 * multiplicand(this) * multiplier = product.
	 * 
	 * @param multiplier
	 * @return
	 */
	Polynomial times(Polynomial multiplier);
	
	/**
	 * dividend(this) / divisor = quotient.
	 * 
	 * @param divisor
	 * @return
	 */
	Pair<Polynomial, Polynomial> divide(Polynomial divisor);
	
	Polynomial exponentiate(int exponent);
	
	Polynomial project(Set<Expression> variables);
}
