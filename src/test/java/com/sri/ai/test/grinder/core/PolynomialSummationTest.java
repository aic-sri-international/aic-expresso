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
package com.sri.ai.test.grinder.core;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Polynomial;
import com.sri.ai.grinder.core.DefaultPolynomial;
import com.sri.ai.grinder.core.PolynomialSummation;

public class PolynomialSummationTest {

	@Test
	public void testKnownUpperAndLowerBounds() {
		// i.e.: n(n+1)/2
		Assert.assertEquals(makePolynomial("1",  "tuple(x)"), polynomialSummationSum("x", "0", "1", "x"));
		Assert.assertEquals(makePolynomial("3",  "tuple(x)"), polynomialSummationSum("x", "0", "2", "x"));
		Assert.assertEquals(makePolynomial("6",  "tuple(x)"), polynomialSummationSum("x", "0", "3", "x"));
		Assert.assertEquals(makePolynomial("10", "tuple(x)"), polynomialSummationSum("x", "0", "4", "x"));
		
		Assert.assertEquals(makePolynomial("3",  "tuple(x)"), polynomialSummationSum("x", "0", "1", "x + 2"));
		Assert.assertEquals(makePolynomial("7",  "tuple(x)"), polynomialSummationSum("x", "0", "2", "x + 2"));
		Assert.assertEquals(makePolynomial("12", "tuple(x)"), polynomialSummationSum("x", "0", "3", "x + 2"));
		Assert.assertEquals(makePolynomial("18", "tuple(x)"), polynomialSummationSum("x", "0", "4", "x + 2"));
		
		// i.e.: n(n+1)(2n+1)/6
		Assert.assertEquals(makePolynomial("1",  "tuple(x)"), polynomialSummationSum("x", "0", "1", "x^2"));
		Assert.assertEquals(makePolynomial("5",  "tuple(x)"), polynomialSummationSum("x", "0", "2", "x^2"));
		Assert.assertEquals(makePolynomial("14", "tuple(x)"), polynomialSummationSum("x", "0", "3", "x^2"));
		Assert.assertEquals(makePolynomial("30", "tuple(x)"), polynomialSummationSum("x", "0", "4", "x^2"));
		
		Assert.assertEquals(makePolynomial("3",  "tuple(x)"), polynomialSummationSum("x", "0", "1", "x^2 + 2"));
		Assert.assertEquals(makePolynomial("9",  "tuple(x)"), polynomialSummationSum("x", "0", "2", "x^2 + 2"));
		Assert.assertEquals(makePolynomial("20", "tuple(x)"), polynomialSummationSum("x", "0", "3", "x^2 + 2"));
		Assert.assertEquals(makePolynomial("38", "tuple(x)"), polynomialSummationSum("x", "0", "4", "x^2 + 2"));
		
		Assert.assertEquals(makePolynomial("4",  "tuple(x)"), polynomialSummationSum("x", "0", "1", "x^2 + x + 2"));
		Assert.assertEquals(makePolynomial("12", "tuple(x)"), polynomialSummationSum("x", "0", "2", "x^2 + x + 2"));
		Assert.assertEquals(makePolynomial("26", "tuple(x)"), polynomialSummationSum("x", "0", "3", "x^2 + x + 2"));
		Assert.assertEquals(makePolynomial("48", "tuple(x)"), polynomialSummationSum("x", "0", "4", "x^2 + x + 2"));
	}
	
	@Test
	public void testUnknownUpperAndLowerBounds() {
		Assert.assertEquals(makePolynomial("-1.5*y^2 + 1.5*z^2 + -6.5*y + 9.5*z + 8",  "tuple(x)"), 
				polynomialSummationSum("x", "y + 1", "z + 2", "2 + 3*x"));
		// y = -1, z = -1 => 5 (if y and z substituted into above equation).
		Assert.assertEquals(makePolynomial("5",  "tuple(x)"), polynomialSummationSum("x", "-1 + 1", "-1 + 2", "2 + 3*x"));
		// y = 2, z = 4 => 51 (if y and z substituted into above equation).
		Assert.assertEquals(makePolynomial("51",  "tuple(x)"), polynomialSummationSum("x", "2 + 1", "4 + 2", "2 + 3*x"));

// TODO - looks incorrect		
//		Assert.assertEquals(makePolynomial("-1.5*y^3 + 0.5*y*z^2 + z^3 + -7*y^2 + 2.5*y*z + 8.5*z^2 + -7.5*y + 23.5*z + 16",  "tuple(x)"), 
//				polynomialSummationSum("x", "y + 1", "z + 2", "2 + 3*x^2 + y"));
		// y = -1, z = -1 => y - 5 (if y and z substituted into above expression)
		Assert.assertEquals(makePolynomial("y + 5",  "tuple(x)"), 
				polynomialSummationSum("x", "-1 + 1", "-1 + 2", "2 + 3*x^2 + y"));
// TODO - looks incorrect		
//		// y = 2, z = 4 => y - 5 (if y and z substituted into above expression)
//		Assert.assertEquals(makePolynomial("15*y + 261",  "tuple(x)"), 
//				polynomialSummationSum("x", "2 + 1", "4 + 2", "2 + 3*x^2 + y"));
	}
	
	//
	// PRIVATE
	//
	private static Polynomial makePolynomial(String polynomial, String tupleSignatureFactors) {
		Polynomial result = DefaultPolynomial.make(Expressions.parse(polynomial), Expressions.parse(tupleSignatureFactors).getArguments());
		return result;
	}
	
	private static Polynomial polynomialSummationSum(String indexOfSummation, String lowerBoundExclusive, String upperBoundInclusive, String summand) {
		Polynomial result = PolynomialSummation.sum(Expressions.parse(indexOfSummation), Expressions.parse(lowerBoundExclusive), Expressions.parse(upperBoundInclusive), 
				makePolynomial(summand, "tuple("+indexOfSummation+")"));
		return result;
	}
}
