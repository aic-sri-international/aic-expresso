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

public class DefaultPolynomialTest {

	@Test
	public void testMake() {			
		//
		// From Trivial Monomials
		Assert.assertEquals(Expressions.parse("0"), makePolynomial("0", "tuple()"));
		Assert.assertEquals(Expressions.parse("2"), makePolynomial("2", "tuple()"));
		Assert.assertEquals(Expressions.parse("-2"), makePolynomial("-2", "tuple()"));
		Assert.assertEquals(Expressions.parse("0"), makePolynomial("0", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("2"), makePolynomial("2", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("-2"), makePolynomial("-2", "tuple(x)"));
		
		Assert.assertEquals(Expressions.parse("x"), makePolynomial("x", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("(x)"), makePolynomial("x", "tuple()"));
		Assert.assertEquals(Expressions.parse("(x)"), makePolynomial("x", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("-1*x^2"), makePolynomial("-1*x^2", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("(-1*x^2)"), makePolynomial("-1*x^2", "tuple()"));
		Assert.assertEquals(Expressions.parse("(-1*x^2)"), makePolynomial("-1*x^2", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("-1*x^2"), makePolynomial("-(x^2)", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("(-1*x^2)"), makePolynomial("-(x^2)", "tuple()"));
		Assert.assertEquals(Expressions.parse("(-1*x^2)"), makePolynomial("-(x^2)", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("x^2"), makePolynomial("-x^2", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("(x^2)"), makePolynomial("-x^2", "tuple()"));
		Assert.assertEquals(Expressions.parse("(x^2)"), makePolynomial("-x^2", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("-1*x^3"), makePolynomial("-x^3", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("(-1*x^3)"), makePolynomial("-x^3", "tuple()"));
		Assert.assertEquals(Expressions.parse("(-1*x^3)"), makePolynomial("-x^3", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("x*(y)"), makePolynomial("x*y", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("(x*y)"), makePolynomial("x*y", "tuple()"));
		Assert.assertEquals(Expressions.parse("(x*y)"), makePolynomial("x*y", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("16"), makePolynomial("2^2^2", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("16"), makePolynomial("2^2^2", "tuple()"));
		Assert.assertEquals(Expressions.parse("16"), makePolynomial("2^2^2", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("16*x"), makePolynomial("2^2^2*x", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("(16*x)"), makePolynomial("2^2^2*x", "tuple()"));
		Assert.assertEquals(Expressions.parse("(16*x)"), makePolynomial("2^2^2*x", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("2^x^4"), makePolynomial("2^x^2^2", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("2^x^4"), makePolynomial("2^x^2^2", "tuple()"));
		Assert.assertEquals(Expressions.parse("2^x^4"), makePolynomial("2^x^2^2", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("2^2^x^4"), makePolynomial("2^2^x^2^2", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("2^2^x^4"), makePolynomial("2^2^x^2^2", "tuple()"));
		Assert.assertEquals(Expressions.parse("2^2^x^4"), makePolynomial("2^2^x^2^2", "tuple(z)"));

		Assert.assertEquals(Expressions.parse("2*x*y"), makePolynomial("2*x*y", "(x, y)"));
		Assert.assertEquals(Expressions.parse("2*x*y"), makePolynomial("2*x*y", "(2, x, y)"));
		Assert.assertEquals(Expressions.parse("x*(2*y)"), makePolynomial("2*x*y", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("y*(2*x)"), makePolynomial("2*x*y", "tuple(y)"));

		Assert.assertEquals(Expressions.parse("16*x"), makePolynomial("*(2*4*(2*x))", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("16*x"), makePolynomial("*(2*4*(2*x))", "tuple()"));
		Assert.assertEquals(Expressions.parse("16*x"), makePolynomial("*(2*4*(2*x))", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("-16*x"), makePolynomial("*(2*4*-(2*x))", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("-16*x"), makePolynomial("*(2*4*-(2*x))", "tuple()"));
		Assert.assertEquals(Expressions.parse("-16*x"), makePolynomial("*(2*4*-(2*x))", "tuple(z)"));
		
		Assert.assertEquals(Expressions.parse("3*x^2"), makePolynomial("3*x^2", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("(3*x^2)"), makePolynomial("3*x^2", "tuple()"));
		Assert.assertEquals(Expressions.parse("(3*x^2)"), makePolynomial("3*x^2", "tuple(z)"));
		Assert.assertEquals(Expressions.parse("3*(x^2)"), makePolynomial("3*x^2", "tuple(3)"));
		Assert.assertEquals(Expressions.parse("3*x^2"), makePolynomial("3*x^2", "(3, x)"));
	
		Assert.assertEquals(Expressions.parse("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "(x, y)"));
		Assert.assertEquals(Expressions.parse("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "(3, x, y)"));
		Assert.assertEquals(Expressions.parse("x^2*(3*y^4)"), makePolynomial("3*x^2*y^4", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("3*x^2*(y^4)"), makePolynomial("3*x^2*y^4", "(3, x)"));
		Assert.assertEquals(Expressions.parse("y^4*(3*x^2)"), makePolynomial("3*x^2*y^4", "tuple(y)"));
		Assert.assertEquals(Expressions.parse("3*y^4*(x^2)"), makePolynomial("3*x^2*y^4", "(3, y)"));
		Assert.assertEquals(Expressions.parse("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "tuple()"));


		//
		// Additions
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y + 10", "tuple(y)"));
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y + 10", "(10, y)"));
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y + 10", "(y, 10)"));
		Assert.assertEquals(Expressions.parse("(y + 10)"), makePolynomial("y + 10", "tuple()"));

		Assert.assertEquals(Expressions.parse("2*y + 10"), makePolynomial("y + 10 + y", "tuple(y)"));

		Assert.assertEquals(Expressions.parse("x^3*y^2 + 2*x^2 + y^2 + x + y + 10"), makePolynomial("x^2 + 7 + x^3*y^2 + 3 + y^2 + x^2 + x + y", "tuple(x, y)"));
		Assert.assertEquals(Expressions.parse("5*x*y + 4*y^2 + x + 6"), makePolynomial("(3*x^2 - 2*x + 5*x*y - 2) + (-3*x^2 + 3*x + 4*y^2 + 8)", "(x, y)"));
		
		//
		// Subtractions
		Assert.assertEquals(Expressions.parse("-1"), makePolynomial("1 - 2", "tuple()"));
		Assert.assertEquals(Expressions.parse("x"), makePolynomial("2*x - x", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("0"), makePolynomial("x - x", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("0"), makePolynomial("2*x - x - x", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("x"), makePolynomial("x - 0", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("-1*x"), makePolynomial("0 - x", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("2*x^3 + -1*x^2 + -1*x"), makePolynomial("2*x^3 - x^2 - x", "tuple(x)"));
		
		//
		// Multiplications
		Assert.assertEquals(Expressions.parse("-1*y + -10"), makePolynomial("-(y + 10)", "tuple(y)"));
		Assert.assertEquals(Expressions.parse("x^2*y^4* z + x^2*y^3"), makePolynomial("(y^3 + y^4*z)*x^2", "tuple(x, y, z)"));
		Assert.assertEquals(Expressions.parse("2*x^2*y + 3*x*y^2 + 4*x^2 + 21*x*y + 15*y^2 + 12*x + 28*y + 5"), makePolynomial("(2*x + 3*y + 5) * (2*x + 5*y + x*y + 1)", "tuple(x, y)"));
	}
	
	@Test
	public void testGetSignatureFactors() {
		// TODO
	}
	
	@Test
	public void testGetSignatureTermMap() {
		// TODO
	}
	
	@Test
	public void testIsMonomial() {
		// TODO
	}
	
	@Test
	public void testAsMonomial() {
		// TODO
	}
	
	@Test
	public void testNumberOfTerms() {
		// TODO
	}
	
	@Test
	public void testGetOrderedSummands() {
		// TODO
	}
	
	@Test
	public void testIsNumericConstant() {
		// TODO
	}
	
	@Test
	public void testIsZero() {
		// TODO
	}
	
	@Test
	public void testIsOne() {
		// TODO
	}
	
	@Test
	public void testGetNonNumericConstantFactors() {
		// TODO
	}
	
	@Test
	public void testAdd() {
		// TODO
	}
	
	@Test
	public void testMinus() {
		// TODO
	}
	
	@Test
	public void testTimes() {
		// TODO
	}
	
	@Test
	public void testDivide() {
		// TODO
	}
	
	@Test
	public void testExponentiate() {
		// TODO
	}
	
	@Test
	public void testProject() {
		// TODO
	}
	
	//
	// FunctionaApplication API related tests
	@Test
	public void testGetFunctor() {
		// TODO
	}
	
	@Test
	public void testGetArguments() {
		// TODO
	}
	
	@Test
	public void testNumberOfArguments() {
		// TODO
	}
	
	@Test
	public void testGet() {
		// TODO
	}
	
	@Test
	public void testSet() {
		// TODO
	}
	
	@Test
	public void testCompareTo() {
		// TODO
	}
	
	//
	// Additional Tests
	@Test
	public void testEquals() {
		// TODO
	}
	
	@Test
	public void testToString() {
		// TODO
	}
	
	//
	// PRIVATE
	//	
	private static Polynomial makePolynomial(String polynomial, String tupleSignatureFactors) {
		Polynomial result = DefaultPolynomial.make(Expressions.parse(polynomial), Expressions.parse(tupleSignatureFactors).getArguments());
		return result;
	}
}

