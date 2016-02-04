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
package com.sri.ai.test.grinder.polynomial;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.grinder.polynomial.core.DefaultPolynomial;
import com.sri.ai.grinder.polynomial.core.PolynomialSummation;

/**
 * NOTE: Used <a hfref="http://www.wolframalpha.com">WolframAlpha</a> to
 * validate expected results, e.g. input given:<br>
 * 
 * <pre>
 * <code>
 * sum_{x=y +1 + 1}^{z - 3}  (y^2 + 4*x^3)
 * </pre>
 * 
 * </code> Remembering that WolframAlpha's lower and upper bounds are inclusive,
 * while PolynomialSummation's lower bound is exclusive.
 * 
 * @author oreilly
 *
 */
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
		
		Assert.assertEquals(makePolynomial("-1*y^3 + z^3 + -4.5*y^2 + 7.5*z^2 + -8.5*y + 20.5*z + 14",  "tuple(x)"), 
				polynomialSummationSum("x", "y + 1", "z + 2", "2 + 3*x^2"));
		// y = -1, z = -1 => 5 (if y and z substituted into above equation).
		Assert.assertEquals(makePolynomial("5",  "tuple(x)"), 
				polynomialSummationSum("x", "-1 + 1", "-1 + 2", "2 + 3*x^2"));
		// y = 2, z = 4 => 237 (if y and z substituted into above equation).
		Assert.assertEquals(makePolynomial("237",  "tuple(x)"), 
				polynomialSummationSum("x", "2 + 1", "4 + 2", "2 + 3*x^2"));
		
		Assert.assertEquals(makePolynomial("-1*y^3 + z^3 + -5*y^2 + 8*z^2 + -10*y + 23*z + 16",  "tuple(x)"), 
				polynomialSummationSum("x", "y + 1", "z + 2", "2 + 3*x^2 + x"));
		// y = -1, z = -1 => 6 (if y and z substituted into above equation).
		Assert.assertEquals(makePolynomial("6",  "tuple(x)"), 
				polynomialSummationSum("x", "-1 + 1", "-1 + 2", "2 + 3*x^2 + x"));
		// y = 2, z = 4 => 252 (if y and z substituted into above equation).
		Assert.assertEquals(makePolynomial("252",  "tuple(x)"), 
				polynomialSummationSum("x", "2 + 1", "4 + 2", "2 + 3*x^2 + x"));
	}
	
	@Test
	public void testUnknownUpperAndLowerBoundsWithFreeVariablesInTerms() {		
		Assert.assertEquals(makePolynomial("-1*w*y + w*z + -1.5*y^2 + 1.5*z^2 + w + -6.5*y + 9.5*z + 8",  "tuple(x)"), 
				polynomialSummationSum("x", "y + 1", "z + 2", "3*x + w + 2"));
		// y = -1, z = -1 => w + 5 (if y and z substituted into above expression)
		Assert.assertEquals(makePolynomial("w + 5",  "tuple(x)"), 
				polynomialSummationSum("x", "-1 + 1", "-1 + 2", "3*x + w + 2"));
		// y = 2, z = 4 => 3*w + 51 (if y and z substituted into above expression)
		Assert.assertEquals(makePolynomial("3*w + 51",  "tuple(x)"), 
				polynomialSummationSum("x", "2 + 1", "4 + 2", "3*x + w + 2"));
				
		Assert.assertEquals(makePolynomial("-1*y^3 + z^3 + -1*w*y + w*z + -4.5*y^2 + 7.5*z^2 + w + -8.5*y + 20.5*z + 14",  "tuple(x)"), 
				polynomialSummationSum("x", "y + 1", "z + 2", "2 + 3*x^2 + w"));
		// y = -1, z = -1 => w + 5 (if y and z substituted into above expression)
		Assert.assertEquals(makePolynomial("w+5",  "tuple(x)"), 
				polynomialSummationSum("x", "-1 + 1", "-1 + 2", "2 + 3*x^2 + w"));
		// y = 2, z = 4 => 3*w + 237 (if y and z substituted into above expression)
		Assert.assertEquals(makePolynomial("3*w + 237",  "tuple(x)"), 
				polynomialSummationSum("x", "2 + 1", "4 + 2", "2 + 3*x^2 + w"));
						
		Assert.assertEquals(makePolynomial("-1*y^3 + z^3 + -5.5*y^2 + y*z + 7.5*z^2 + -7.5*y + 20.5*z + 14",  "tuple(x)"), 
				polynomialSummationSum("x", "y + 1", "z + 2", "2 + 3*x^2 + y"));
		// y = -1, z = -1 => y + 5 (if y and z substituted into above expression)
		Assert.assertEquals(makePolynomial("y + 5",  "tuple(x)"), 
				polynomialSummationSum("x", "-1 + 1", "-1 + 2", "2 + 3*x^2 + y"));	
		// y = 2, z = 4 => 3*y + 237 (if y and z substituted into above expression)
		Assert.assertEquals(makePolynomial("3*y + 237",  "tuple(x)"), 
				polynomialSummationSum("x", "2 + 1", "4 + 2", "2 + 3*x^2 + y"));		

		// NOTE: based on Hybrid Reasoning 2015 workshop paper example in section 4.1		
		Assert.assertEquals(makePolynomial("-1*y^4 + z^4 + -7*y^3 + y^2*z + -10*z^3 + -17*y^2 + 37*z^2 + -12*y + -60*z + 32",  "tuple(x)"), 
				polynomialSummationSum("x", "y + 1", "z - 3", "y^2 + 4*x^3"));
	}
	
	@Test
	public void testHigherDegreePolynomialBounds() {
		Assert.assertEquals(makePolynomial("1.5*z^6 + -1.5*y^4 + 3*z^4 + 9.5*z^3 + -6.5*y^2 + 1.5*z^2 + 9.5*z + 8",  "tuple(x)"), 
				polynomialSummationSum("x", "y^2 + 1", "z^3 + z + 2", "2 + 3*x"));
		
		Assert.assertEquals(makePolynomial("(-4/3)*y^6 + (4/3)*z^6 + 4*z^5 + -6*y^4 + 14*z^4 + -1*y^3 + y*z^2 + (64/3)*z^3 + (-26/3)*y^2 + y*z + (104/3)*z^2 + y + (74/3)*z + 16",  "tuple(x)"), 
				polynomialSummationSum("x", "y^2 + 1", "z^2 + z + 2", "y + 4*x^2"));

		Assert.assertEquals(makePolynomial("(4/3)*z^6 + 4*r*z^4 + 4*r^2*z^2 + 2*z^4 + -4*r^2*y + -4*r*y^2 + 4*r*z^2 + (-4/3)*y^3 + y*z^2 + -4*r^2 + -12*r*y + -7*y^2 + (2/3)*z^2 + -8*r + (-29/3)*y + -4",  "tuple(x)"), 
				polynomialSummationSum("x", "y + r + 1", "z^2 + r", "y + 4*x^2"));
		
		Assert.assertEquals(makePolynomial("(4/3)*z^6 + 4*y*z^4 + 4*y^2*z^2 + 2*z^4 + -4*y^2*z + y*z^2 + (-4/3)*z^3 + -4*y^2 + -13*y*z + (-16/3)*z^2 + -9*y + (-26/3)*z + -4",  "tuple(x)"), 
				polynomialSummationSum("x", "y + z + 1", "z^2 + y", "y + 4*x^2"));
	}
	
	@Test
	public void testNumericConstantSummand() {
		Assert.assertEquals(makePolynomial("9 - 3*j",  "tuple(j)"), 
				polynomialSummationSum("k", "j+1", "4", "3"));
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
