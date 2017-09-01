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

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.polynomial.api.Monomial;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.grinder.polynomial.core.DefaultMonomial;
import com.sri.ai.grinder.polynomial.core.DefaultPolynomial;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

public class DefaultPolynomialTest {

	@Test
	public void testMakeExplicitSignatureOfFactors() {			
		//
		// From Trivial Monomials
		assertEquals(parse("0"), makePolynomial("0", "tuple()"));
		assertEquals(parse("2"), makePolynomial("2", "tuple()"));
		assertEquals(parse("-2"), makePolynomial("-2", "tuple()"));
		assertEquals(parse("0"), makePolynomial("0", "tuple(x)"));
		assertEquals(parse("2"), makePolynomial("2", "tuple(x)"));
		assertEquals(parse("-2"), makePolynomial("-2", "tuple(x)"));
		
		assertEquals(parse("x"), makePolynomial("x", "tuple(x)"));
		assertEquals(parse("(x)"), makePolynomial("x", "tuple()"));
		assertEquals(parse("(x)"), makePolynomial("x", "tuple(z)"));
		
		assertEquals(parse("-1*x^2"), makePolynomial("-1*x^2", "tuple(x)"));
		assertEquals(parse("(-1*x^2)"), makePolynomial("-1*x^2", "tuple()"));
		assertEquals(parse("(-1*x^2)"), makePolynomial("-1*x^2", "tuple(z)"));
		
		assertEquals(parse("-1*x^2"), makePolynomial("-(x^2)", "tuple(x)"));
		assertEquals(parse("(-1*x^2)"), makePolynomial("-(x^2)", "tuple()"));
		assertEquals(parse("(-1*x^2)"), makePolynomial("-(x^2)", "tuple(z)"));
		
		assertEquals(parse("x^2"), makePolynomial("-x^2", "tuple(x)"));
		assertEquals(parse("(x^2)"), makePolynomial("-x^2", "tuple()"));
		assertEquals(parse("(x^2)"), makePolynomial("-x^2", "tuple(z)"));
		
		assertEquals(parse("-1*x^3"), makePolynomial("-x^3", "tuple(x)"));
		assertEquals(parse("(-1*x^3)"), makePolynomial("-x^3", "tuple()"));
		assertEquals(parse("(-1*x^3)"), makePolynomial("-x^3", "tuple(z)"));
		
		assertEquals(parse("x^2"), makePolynomial("x*x", "tuple(x)"));

		assertEquals(parse("x*(y)"), makePolynomial("x*y", "tuple(x)"));
		assertEquals(parse("(x*y)"), makePolynomial("x*y", "tuple()"));
		assertEquals(parse("(x*y)"), makePolynomial("x*y", "tuple(z)"));
		
		assertEquals(parse("16"), makePolynomial("2^2^2", "tuple(x)"));
		assertEquals(parse("16"), makePolynomial("2^2^2", "tuple()"));
		assertEquals(parse("16"), makePolynomial("2^2^2", "tuple(z)"));
		
		assertEquals(parse("16*x"), makePolynomial("2^2^2*x", "tuple(x)"));
		assertEquals(parse("(16*x)"), makePolynomial("2^2^2*x", "tuple()"));
		assertEquals(parse("(16*x)"), makePolynomial("2^2^2*x", "tuple(z)"));
		
		assertEquals(parse("2^x^4"), makePolynomial("2^x^2^2", "tuple(x)"));
		assertEquals(parse("2^x^4"), makePolynomial("2^x^2^2", "tuple()"));
		assertEquals(parse("2^x^4"), makePolynomial("2^x^2^2", "tuple(z)"));
		
		assertEquals(parse("2^2^x^4"), makePolynomial("2^2^x^2^2", "tuple(x)"));
		assertEquals(parse("2^2^x^4"), makePolynomial("2^2^x^2^2", "tuple()"));
		assertEquals(parse("2^2^x^4"), makePolynomial("2^2^x^2^2", "tuple(z)"));

		assertEquals(parse("2*x*y"), makePolynomial("2*x*y", "(x, y)"));
		assertEquals(parse("2*x*y"), makePolynomial("2*x*y", "(2, x, y)"));
		assertEquals(parse("x*(2*y)"), makePolynomial("2*x*y", "tuple(x)"));
		assertEquals(parse("y*(2*x)"), makePolynomial("2*x*y", "tuple(y)"));

		assertEquals(parse("16*x"), makePolynomial("*(2*4*(2*x))", "tuple(x)"));
		assertEquals(parse("16*x"), makePolynomial("*(2*4*(2*x))", "tuple()"));
		assertEquals(parse("16*x"), makePolynomial("*(2*4*(2*x))", "tuple(z)"));
		
		assertEquals(parse("-16*x"), makePolynomial("*(2*4*-(2*x))", "tuple(x)"));
		assertEquals(parse("-16*x"), makePolynomial("*(2*4*-(2*x))", "tuple()"));
		assertEquals(parse("-16*x"), makePolynomial("*(2*4*-(2*x))", "tuple(z)"));
		
		assertEquals(parse("3*x^2"), makePolynomial("3*x^2", "tuple(x)"));
		assertEquals(parse("(3*x^2)"), makePolynomial("3*x^2", "tuple()"));
		assertEquals(parse("(3*x^2)"), makePolynomial("3*x^2", "tuple(z)"));
		assertEquals(parse("3*(x^2)"), makePolynomial("3*x^2", "tuple(3)"));
		assertEquals(parse("3*x^2"), makePolynomial("3*x^2", "(3, x)"));
	
		assertEquals(parse("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "(x, y)"));
		assertEquals(parse("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "(3, x, y)"));
		assertEquals(parse("x^2*(3*y^4)"), makePolynomial("3*x^2*y^4", "tuple(x)"));
		assertEquals(parse("3*x^2*(y^4)"), makePolynomial("3*x^2*y^4", "(3, x)"));
		assertEquals(parse("y^4*(3*x^2)"), makePolynomial("3*x^2*y^4", "tuple(y)"));
		assertEquals(parse("3*y^4*(x^2)"), makePolynomial("3*x^2*y^4", "(3, y)"));
		assertEquals(parse("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "tuple()"));


		//
		// Additions
		assertEquals(parse("12"), makePolynomial("2 + 10", "tuple(y)"));
		assertEquals(parse("y + 10"), makePolynomial("y + 10", "tuple(y)"));
		assertEquals(parse("y + 10"), makePolynomial("y + 10", "(10, y)"));
		assertEquals(parse("y + 10"), makePolynomial("y + 10", "(y, 10)"));
		assertEquals(parse("(y + 10)"), makePolynomial("y + 10", "tuple()"));

		assertEquals(parse("2*y + 10"), makePolynomial("y + 10 + y", "tuple(y)"));

		assertEquals(parse("x^3*y^2 + 2*x^2 + y^2 + x + y + 10"), makePolynomial("x^2 + 7 + x^3*y^2 + 3 + y^2 + x^2 + x + y", "tuple(x, y)"));
		assertEquals(parse("5*x*y + 4*y^2 + x + 6"), makePolynomial("(3*x^2 - 2*x + 5*x*y - 2) + (-3*x^2 + 3*x + 4*y^2 + 8)", "(x, y)"));
		
		//
		// Subtractions
		assertEquals(parse("-8"), makePolynomial("2 - 10", "tuple(y)"));
		assertEquals(parse("-1"), makePolynomial("1 - 2", "tuple()"));
		assertEquals(parse("x"), makePolynomial("2*x - x", "tuple(x)"));
		assertEquals(parse("0"), makePolynomial("x - x", "tuple(x)"));
		assertEquals(parse("0"), makePolynomial("2*x - x - x", "tuple(x)"));
		assertEquals(parse("x"), makePolynomial("x - 0", "tuple(x)"));
		assertEquals(parse("-1*x"), makePolynomial("0 - x", "tuple(x)"));
		assertEquals(parse("2*x^3 + -1*x^2 + -1*x"), makePolynomial("2*x^3 - x^2 - x", "tuple(x)"));
		
		//
		// Multiplications
		assertEquals(parse("-1*y + -10"), makePolynomial("-(y + 10)", "tuple(y)"));
		assertEquals(parse("x^2*y^4* z + x^2*y^3"), makePolynomial("(y^3 + y^4*z)*x^2", "tuple(x, y, z)"));
		assertEquals(parse("2*x^2*y + 3*x*y^2 + 4*x^2 + 21*x*y + 15*y^2 + 12*x + 28*y + 5"), makePolynomial("(2*x + 3*y + 5) * (2*x + 5*y + x*y + 1)", "tuple(x, y)"));
	
		//
		// Exponentiation
		assertEquals(parse("1"), makePolynomial("0^0", "tuple(x)"));
		assertEquals(parse("0"), makePolynomial("0^1", "tuple(x)"));
		assertEquals(parse("0"), makePolynomial("0^3", "tuple(x)"));
		assertEquals(parse("1"), makePolynomial("1^0", "tuple(x)"));
		assertEquals(parse("1"), makePolynomial("1^1", "tuple(x)"));
		assertEquals(parse("1"), makePolynomial("1^3", "tuple(x)"));
		assertEquals(parse("1"), makePolynomial("x^0", "tuple(x)"));
		assertEquals(parse("x"), makePolynomial("x^1", "tuple(x)"));
		assertEquals(parse("x^3"), makePolynomial("x^3", "tuple(x)"));
		assertEquals(parse("1"), makePolynomial("(x + 2)^0", "tuple(x)"));
		assertEquals(parse("x + 2"), makePolynomial("(x + 2)^1", "tuple(x)"));
		assertEquals(parse("x^2 + 4*x + 4"), makePolynomial("(x + 2)^2", "tuple(x)"));
		assertEquals(parse("x^3 + 6*x^2 + 12*x + 8"), makePolynomial("(x + 2)^3", "tuple(x)"));
		
		//
		// Division
		assertEquals(parse("0"), makePolynomial("0 / (x^2 + 3)", "tuple(x)"));
		assertEquals(parse("1.5"), makePolynomial("3 / 2", "tuple(x)"));
		assertEquals(parse("x^2 + 1.5*x + 3"), makePolynomial("(2*x^2 + 3*x + 6) / 2", "tuple(x)"));
		assertEquals(parse("x + -5"), makePolynomial("(x^3 - 5*x^2 + 3*x - 15) / (x^2 + 3)", "tuple(x)"));
		assertEquals(parse("x^2 + -2*x + (4 + -9/(x + 2))"), makePolynomial("(x^3 - 1) / (x + 2)", "tuple(x)"));
		assertEquals(parse("3*x + (-11 + (28*x + 30)/(x^2 + 3*x + 3))"), makePolynomial("(3*x^3 - 2*x^2 + 4*x - 3) / (x^2 + 3*x + 3)", "tuple(x)"));	
	}
	
	@Test
	public void testMakeVariablesFromExtractedGeneralizedVariables() {
		assertEquals(parse("16*x"), makePolynomial("2^2^2*x"));
		assertEquals(parse("2^x^4"), makePolynomial("2^x^2^2"));
		assertEquals(parse("2^2^x^4"), makePolynomial("2^2^x^2^2"));
		assertEquals(parse("2*x*y"), makePolynomial("2*x*y"));
		assertEquals(parse("16*x"), makePolynomial("*(2*4*(2*x))"));
		assertEquals(parse("-16*x"), makePolynomial("*(2*4*-(2*x))"));		
		assertEquals(parse("3*x^2"), makePolynomial("3*x^2"));
		assertEquals(parse("3*x^2*y^4"), makePolynomial("3*x^2*y^4"));
		assertEquals(parse("y + 10"), makePolynomial("y + 10"));
		assertEquals(parse("2*y + 10"), makePolynomial("y + 10 + y"));

		assertEquals(parse("x^3*y^2 + 2*x^2 + y^2 + x + y + 10"), makePolynomial("x^2 + 7 + x^3*y^2 + 3 + y^2 + x^2 + x + y"));
		assertEquals(parse("5*x*y + 4*y^2 + x + 6"), makePolynomial("(3*x^2 - 2*x + 5*x*y - 2) + (-3*x^2 + 3*x + 4*y^2 + 8)"));
		
		assertEquals(parse("x"), makePolynomial("2*x - x"));
		assertEquals(parse("0"), makePolynomial("x - x"));
		assertEquals(parse("0"), makePolynomial("2*x - x - x"));
		assertEquals(parse("x"), makePolynomial("x - 0"));
		assertEquals(parse("-1*x"), makePolynomial("0 - x"));
		assertEquals(parse("2*x^3 + -1*x^2 + -1*x"), makePolynomial("2*x^3 - x^2 - x"));

		assertEquals(parse("-1*y + -10"), makePolynomial("-(y + 10)"));
		assertEquals(parse("x^2*y^4* z + x^2*y^3"), makePolynomial("(y^3 + y^4*z)*x^2"));
		assertEquals(parse("2*x^2*y + 3*x*y^2 + 4*x^2 + 21*x*y + 15*y^2 + 12*x + 28*y + 5"), makePolynomial("(2*x + 3*y + 5) * (2*x + 5*y + x*y + 1)"));
	
		assertEquals(parse("x"), makePolynomial("x^1"));
		assertEquals(parse("x^3"), makePolynomial("x^3"));
		assertEquals(parse("1"), makePolynomial("(x + 2)^0"));
		assertEquals(parse("x + 2"), makePolynomial("(x + 2)^1"));
		assertEquals(parse("x^2 + 4*x + 4"), makePolynomial("(x + 2)^2"));
		assertEquals(parse("x^3 + 6*x^2 + 12*x + 8"), makePolynomial("(x + 2)^3"));	
		
		assertEquals(parse("0"), makePolynomial("0 / (x^2 + 3)"));
		assertEquals(parse("1.5"), makePolynomial("3 / 2"));
		assertEquals(parse("x^2 + 1.5*x + 3"), makePolynomial("(2*x^2 + 3*x + 6) / 2"));
		assertEquals(parse("x + -5"), makePolynomial("(x^3 - 5*x^2 + 3*x - 15) / (x^2 + 3)"));
		assertEquals(parse("x^2 + -2*x + (4 + -9/(x + 2))"), makePolynomial("(x^3 - 1) / (x + 2)"));
		assertEquals(parse("3*x + (-11 + (28*x + 30)/(x^2 + 3*x + 3))"), makePolynomial("(3*x^3 - 2*x^2 + 4*x - 3) / (x^2 + 3*x + 3)"));			
	}
	
	@Test
	public void testExtractGeneralizedVariables() {
		assertEquals(
				parse("tuple(x)").getArguments(),
				DefaultPolynomial.extractGeneralizedVariables(parse("x + 2"))
		);
		assertEquals(
				parse("tuple(x, y)").getArguments(),
				DefaultPolynomial.extractGeneralizedVariables(parse("x + 2 + y"))
		);
		assertEquals(
				parse("tuple(x, |x|)").getArguments(),
				DefaultPolynomial.extractGeneralizedVariables(parse("|x| + x + 2"))
		);	
		assertEquals(
				parse("tuple(x, afunction(y))").getArguments(),
				DefaultPolynomial.extractGeneralizedVariables(parse("x + 2 + afunction(y)"))
		);
	}
	
	@Test
	public void testGetVariables() {
		Polynomial p = makePolynomial("3*x^2*y^4", "tuple()");
		assertEquals(Collections.emptyList(), p.getVariables());
		
		p = makePolynomial("3*x^2*y^4", "tuple(x)");
		assertEquals(parse("tuple(x)").getArguments(), p.getVariables());
		
		p = makePolynomial("3*x^2*y^4", "tuple(y)");
		assertEquals(parse("tuple(y)").getArguments(), p.getVariables());
		
		p = makePolynomial("3*x^2*y^4", "tuple(3)");
		assertEquals(parse("tuple(3)").getArguments(), p.getVariables());
		
		p = makePolynomial("3*x^2*y^4", "(x, y)");
		assertEquals(parse("(x, y)").getArguments(), p.getVariables());
		
		p = makePolynomial("3*x^2*y^4", "(x, y, z)");
		assertEquals(parse("(x, y, z)").getArguments(), p.getVariables());
		
		p = makePolynomial("3*x^2*y^4", "(x, y, 3)");
		assertEquals(parse("(x, y, 3)").getArguments(), p.getVariables());
	}
	
	@Test
	public void testGetMapFromSignatureToMonomial() {
		Polynomial p = makePolynomial("3*x^2*y^4", "tuple()");
		Map<List<Rational>, Expression> expectedMapFromSignatureToMonomial = new HashMap<>();
		expectedMapFromSignatureToMonomial.put(Collections.emptyList(), makeMonomial("3*x^2*y^4"));
		assertEquals(expectedMapFromSignatureToMonomial, p.getMapFromSignatureToMonomial());
		
		p = makePolynomial("3*x^2*y^4", "tuple(x)");
		expectedMapFromSignatureToMonomial = new HashMap<>();
		expectedMapFromSignatureToMonomial.put(Arrays.asList(new Rational(2)), parse("x^2*(3*y^4)"));
		assertEquals(expectedMapFromSignatureToMonomial, p.getMapFromSignatureToMonomial());
		
		p = makePolynomial("3*x^2*y^4", "tuple(y)");
		expectedMapFromSignatureToMonomial = new HashMap<>();
		expectedMapFromSignatureToMonomial.put(Arrays.asList(new Rational(4)), parse("y^4*(3*x^2)"));
		assertEquals(expectedMapFromSignatureToMonomial, p.getMapFromSignatureToMonomial());
		
		p = makePolynomial("3*x^2*y^4", "tuple(x, y)");
		expectedMapFromSignatureToMonomial = new HashMap<>();
		expectedMapFromSignatureToMonomial.put(Arrays.asList(new Rational(2), new Rational(4)), parse("3*x^2*y^4"));
		assertEquals(expectedMapFromSignatureToMonomial, p.getMapFromSignatureToMonomial());
		
		p = makePolynomial("3*x^2*y^4", "tuple(y, x)");
		expectedMapFromSignatureToMonomial = new HashMap<>();
		expectedMapFromSignatureToMonomial.put(Arrays.asList(new Rational(4), new Rational(2)), parse("3*x^2*y^4"));
		assertEquals(expectedMapFromSignatureToMonomial, p.getMapFromSignatureToMonomial());
		
		p = makePolynomial("3*x^2*y^4", "tuple(x, y, z)");
		expectedMapFromSignatureToMonomial = new HashMap<>();
		expectedMapFromSignatureToMonomial.put(Arrays.asList(new Rational(2), new Rational(4), new Rational(0)), parse("3*x^2*y^4"));
		assertEquals(expectedMapFromSignatureToMonomial, p.getMapFromSignatureToMonomial());
	}
	
	@Test
	public void testIsMonomial() {
		Polynomial p = makePolynomial("0", "(x, y)");
		Assert.assertTrue(p.isMonomial());
		
		p = makePolynomial("1", "(x, y)");
		Assert.assertTrue(p.isMonomial());
		
		// Note: the numeric constants should just be added up together
		p = makePolynomial("1 + 2 + 4", "(x, y)");
		Assert.assertTrue(p.isMonomial());
		
		p = makePolynomial("3*x^2*y^4", "tuple(y, x)");
		Assert.assertTrue(p.isMonomial());
		
		// Note: the 0 should be dropped, so remains a monomial
		p = makePolynomial("3*x^2*y^4 + 0", "tuple(y, x)");
		Assert.assertTrue(p.isMonomial());
		
		p = makePolynomial("3*x^2*y^4 + 1", "tuple(y, x)");
		Assert.assertFalse(p.isMonomial());
	}
	
	@Test
	public void testAsMonomial() {
		assertEquals(makeMonomial("0"), makePolynomial("0", "(x, y)").asMonomial());
		assertEquals(makeMonomial("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "(x, y)").asMonomial());
	
		// Note: Due to the signature of factors being a subset they should not equal as variables
		// in the monomials should be getting treated as constants.
		Assert.assertNotEquals(makeMonomial("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "tuple(x)").asMonomial());
		Assert.assertNotEquals(makeMonomial("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "tuple(y)").asMonomial());
	}
	
	@Test(expected=IllegalStateException.class)
	public void testAsMonomialIllegalStateException() {
		makePolynomial("3*x^2*y^4 + x^1", "tuple(y, x)").asMonomial();
	}
	
	@Test
	public void testNumberOfTerms() {
		assertEquals(1, makePolynomial("0", "(x, y)").numberOfTerms());
		assertEquals(1, makePolynomial("3*x^2*y^4", "(x, y)").numberOfTerms());
		// NOTE: like terms, so are added together automatically
		assertEquals(1, makePolynomial("3*x^2*y^4 + x^2*y^4", "(x, y)").numberOfTerms());
		
		assertEquals(2, makePolynomial("3*x^2*y^4 + 3", "(x, y)").numberOfTerms());
		assertEquals(3, makePolynomial("3*x^2*y^4 + x*y^3 + 3", "(x, y)").numberOfTerms());
	}
	
	@Test
	public void testGetOrderedSummands() {
		assertEquals(Arrays.asList(makeMonomial("0")),  makePolynomial("0", "(x, y)").getMonomials());
		assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4")),  makePolynomial("3*x^2*y^4", "(x, y)").getMonomials());
		assertEquals(Arrays.asList(makeMonomial("4*x^2*y^4")),  makePolynomial("3*x^2*y^4  + x^2*y^4", "(x, y)").getMonomials());
		
		assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("3")),  makePolynomial("3*x^2*y^4  + 3", "(x, y)").getMonomials());
		assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("3")),  makePolynomial("3 + 3*x^2*y^4", "(x, y)").getMonomials());
		
		assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("x*y^3"), makeMonomial("3")),  makePolynomial("3*x^2*y^4 + x*y^3 + 3", "(x, y)").getMonomials());
		assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("x*y^3"), makeMonomial("3")),  makePolynomial("x*y^3 + 3*x^2*y^4 + 3", "(x, y)").getMonomials());
		assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("x*y^3"), makeMonomial("3")),  makePolynomial("x*y^3 + 3 + 3*x^2*y^4", "(x, y)").getMonomials());
	}
	
	@Test
	public void testIsNumericConstant() {
		Assert.assertTrue(makePolynomial("0", "(x, y)").isNumericConstant());
		Assert.assertTrue(makePolynomial("1", "(x, y)").isNumericConstant());
		
		Assert.assertFalse(makePolynomial("x", "(x, y)").isNumericConstant());
		Assert.assertFalse(makePolynomial("x + 1", "(x, y)").isNumericConstant());
	}
	
	@Test
	public void testIsZero() {
		Assert.assertTrue(makePolynomial("0", "(x, y)").isZero());
		
		Assert.assertFalse(makePolynomial("1", "(x, y)").isZero());
		Assert.assertFalse(makePolynomial("x + 0", "(x, y)").isZero());
	}
	
	@Test
	public void testIsOne() {
		Assert.assertTrue(makePolynomial("1", "(x, y)").isOne());
		
		Assert.assertFalse(makePolynomial("0", "(x, y)").isOne());
		Assert.assertFalse(makePolynomial("x + 1", "(x, y)").isOne());
	}
	
	@Test
	public void testDegree() {
		assertEquals(0, makePolynomial("0", "(x, y)").degree());
		assertEquals(6, makePolynomial("3*x^2*y^4", "(x, y)").degree());
		assertEquals(8, makePolynomial("3*x^2*y^4 + x^8", "(x, y)").degree());
	}
	
	@Test
	public void testAdd() {
		assertEquals(parse("12"), makePolynomial("2", "tuple(y)").add(makePolynomial("10", "tuple(y)")));
		assertEquals(parse("y + 10"), makePolynomial("y", "tuple(y)").add(makePolynomial("10", "tuple(y)")));
		assertEquals(parse("y + 10"), makePolynomial("y", "tuple(10, y)").add(makePolynomial("10", "tuple(10, y)")));
		assertEquals(parse("y + 10"), makePolynomial("y", "tuple(y, 10)").add(makePolynomial("10", "tuple(y, 10)")));
		assertEquals(parse("y + 10"), makePolynomial("y", "tuple()").add(makePolynomial("10", "tuple()")));
	
		assertEquals(parse("2*y + 10"), makePolynomial("2*y", "tuple(y)").add(makePolynomial("10", "tuple(y)")));
		

		assertEquals(parse("x^3*y^2 + 2*x^2 + y^2 + x + y + 10"), 
				makePolynomial("x^2", "(x, y)")
					.add(makePolynomial("7", "(x, y)"))
					.add(makePolynomial("x^3*y^2", "(x, y)"))
					.add(makePolynomial("3", "(x, y)"))
					.add(makePolynomial("y^2", "(x, y)"))
					.add(makePolynomial("x^2", "(x, y)"))
					.add(makePolynomial("x", "(x, y)"))
					.add(makePolynomial("y", "(x, y)"))
		);
		assertEquals(parse("5*x*y + 4*y^2 + x + 6"), 
				makePolynomial("(3*x^2 - 2*x + 5*x*y - 2)", "(x, y)")
					.add(makePolynomial("-3*x^2 + 3*x + 4*y^2 + 8", "(x, y)"))
		);
		assertEquals(parse("2*x + 2*(y+1)"), 
				makePolynomial("x + y + 1", "tuple(x)")
					.add(makePolynomial("x + y + 1", "tuple(x)"))
		);
	}
	
	@Test
	public void testMinus() {
		assertEquals(parse("-8"), makePolynomial("2", "tuple(y)").minus(makePolynomial("10", "tuple(y)")));
		assertEquals(parse("-1"), makePolynomial("1", "tuple()").minus(makePolynomial("2", "tuple()")));
		assertEquals(parse("x"), makePolynomial("2*x", "tuple(x)").minus(makePolynomial("x", "tuple(x)")));
		assertEquals(parse("0"), makePolynomial("x", "tuple(x)").minus(makePolynomial("x", "tuple(x)")));
		assertEquals(parse("0"), makePolynomial("2*x", "tuple(x)").minus(makePolynomial("x", "tuple(x)"))
				.minus(makePolynomial("x", "tuple(x)")));
		assertEquals(parse("x"), makePolynomial("x", "tuple(x)").minus(makePolynomial("0", "tuple(x)")));
		assertEquals(parse("-1*x"), makePolynomial("0", "tuple(x)").minus(makePolynomial("x", "tuple(x)")));
		assertEquals(parse("2*x^3 + -1*x^2 + -1*x"), makePolynomial("2*x^3", "tuple(x)")
				.minus(makePolynomial("x^2", "tuple(x)"))
				.minus(makePolynomial("x", "tuple(x)")));
	}
	
	@Test
	public void testTimes() {
		assertEquals(parse("-1*y + -10"), makePolynomial("-1", "tuple(y)")
				.times(makePolynomial("y + 10", "tuple(y)")));
		assertEquals(parse("x^2*y^4* z + x^2*y^3"), makePolynomial("y^3 + y^4*z", "tuple(x, y, z)")
				.times(makePolynomial("x^2", "tuple(x, y, z)")));
		assertEquals(parse("2*x^2*y + 3*x*y^2 + 4*x^2 + 21*x*y + 15*y^2 + 12*x + 28*y + 5"), makePolynomial("2*x + 3*y + 5", "tuple(x, y)")
				.times(makePolynomial("2*x + 5*y + x*y + 1", "tuple(x, y)")));
		assertEquals(parse("x^2 + 2*x*(y + 1) + (y + 1)^2"), 
				makePolynomial("x + y + 1", "tuple(x)")
					.times(makePolynomial("x + y + 1", "tuple(x)"))
		);
	}
	
	@Test
	public void testUnivariateDivide() {
		Polynomial dividend = makePolynomial("0", "tuple(x)");
		Polynomial divisor  = makePolynomial("x^2 + 3", "tuple(x)");
		Pair<Polynomial, Polynomial> quotientAndRemainder = dividend.divide(divisor);
		assertEquals(parse("0"), quotientAndRemainder.first);
		assertEquals(parse("0"), quotientAndRemainder.second);
		
		dividend             = makePolynomial("3", "tuple(x)");
		divisor              = makePolynomial("2", "tuple(x)");
		quotientAndRemainder = dividend.divide(divisor);
		assertEquals(parse("1.5"), quotientAndRemainder.first);
		assertEquals(parse("0"), quotientAndRemainder.second);
		
		dividend             = makePolynomial("2*x^2 + 3*x + 6", "tuple(x)");
		divisor              = makePolynomial("2", "tuple(x)");
		quotientAndRemainder = dividend.divide(divisor);
		assertEquals(parse("x^2 + 1.5*x + 3"), quotientAndRemainder.first);
		assertEquals(parse("0"), quotientAndRemainder.second);
		
		dividend             = makePolynomial("x^3 - 5*x^2 + 3*x - 15", "tuple(x)");
		divisor              = makePolynomial("x^2 + 3", "tuple(x)");
		quotientAndRemainder = dividend.divide(divisor);
		assertEquals(parse("x + -5"), quotientAndRemainder.first);
		assertEquals(parse("0"), quotientAndRemainder.second);
		
		dividend             = makePolynomial("x^3 - 1", "tuple(x)");
		divisor              = makePolynomial("x + 2", "tuple(x)");
		quotientAndRemainder = dividend.divide(divisor);
		assertEquals(parse("x^2 + -2*x + 4"), quotientAndRemainder.first);
		assertEquals(parse("-9"), quotientAndRemainder.second);
		
		dividend             = makePolynomial("3*x^3 - 2*x^2 + 4*x - 3", "tuple(x)");
		divisor              = makePolynomial("x^2 + 3*x + 3", "tuple(x)");
		quotientAndRemainder = dividend.divide(divisor);
		assertEquals(parse("3*x + -11"), quotientAndRemainder.first);
		assertEquals(parse("28*x + 30"), quotientAndRemainder.second);
		
		Polynomial p = makePolynomial("(3*x^3 - 2*x^2 + 4*x - 3) / (x^2 + 3*x + 3)", "tuple(x)");
		assertEquals(parse("3*x + (-11 + ((28*x + 30) / (x^2 + 3*x + 3)))"), p);
		// Note: the -11 from the quotient gets absorbed into the remainder/divisor term 
		// as they are like terms under the variables [x]
		assertEquals(2, p.numberOfTerms());
		assertEquals(parse("3*x"), p.getMonomials().get(0));
		assertEquals(parse("-11 + ((28*x + 30) / (x^2 + 3*x + 3))"), p.getMonomials().get(1));
	}
	
	@Test
	public void testMultivariateDivide() {
		// NOTE: We currently don't support multivariate division so the remainder will be
		//       the same as the dividend.	
		Polynomial dividend = makePolynomial("x^3*y^3 + y^2 + 5", "tuple(x, y)");
		Polynomial divisor  = makePolynomial("x^2 + y", "tuple(x, y)");
		Pair<Polynomial, Polynomial> quotientAndRemainder = dividend.divide(divisor);
		assertEquals(parse("0"), quotientAndRemainder.first);
		assertEquals(parse("x^3*y^3 + y^2 + 5"), quotientAndRemainder.second);
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testDivideByZero1() {
		makePolynomial("3 / 0");
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testDivideByZero2() {
		makePolynomial("(x + y + 2) / 0");
	}
	
	@Test
	public void testExponentiate() {		
		Polynomial p = makePolynomial("0", "tuple(x)");
		assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(0));
		assertEquals(makePolynomial("0", "tuple(x)"), p.exponentiate(1));
		assertEquals(makePolynomial("0", "tuple(x)"), p.exponentiate(3));
		
		p = makePolynomial("1", "tuple(x)");
		assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(0));
		assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(1));
		assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(3));
		
		p = makePolynomial("x", "tuple(x)");
		assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(0));
		assertEquals(makePolynomial("x", "tuple(x)"), p.exponentiate(1));
		assertEquals(makePolynomial("x^3", "tuple(x)"), p.exponentiate(3));
		
		p = makePolynomial("x + 2", "tuple(x)");
		assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(0));
		assertEquals(makePolynomial("x + 2", "tuple(x)"), p.exponentiate(1));
		assertEquals(makePolynomial("x^2 + 4*x + 4", "tuple(x)"), p.exponentiate(2));
		assertEquals(makePolynomial("x^3 + 6*x^2 + 12*x + 8", "tuple(x)"), p.exponentiate(3));
		
		p = makePolynomial("x + y + 2", "(x, y)");
		assertEquals(makePolynomial("1", "(x, y)"), p.exponentiate(0));
		assertEquals(makePolynomial("x + y + 2", "(x, y)"), p.exponentiate(1));
		assertEquals(makePolynomial("x^2 + 2*x*y + 4*x + y^2 + 4*y + 4", "(x, y)"), p.exponentiate(2));
		assertEquals(makePolynomial("x^3 + 3*x^2*y + 6*x^2 + 3*x*y^2 + 12*x*y + 12*x + y^3 + 6*y^2 + 12*y + 8", "(x, y)"), p.exponentiate(3));
		
		p = makePolynomial("x^2*y + y + z^3 + 2", "(x, y, z)");
		assertEquals(makePolynomial("1", "(x, y, z)"), p.exponentiate(0));
		assertEquals(makePolynomial("x^2*y + y + z^3 + 2", "tuple(x, y, z)"), p.exponentiate(1));
		assertEquals(makePolynomial("x^4*y^2 + 2*x^2*y^2 + 2*x^2*y*z^3 + 4*x^2*y + y^2 + 2*y*z^3 + 4*y + z^6 + 4*z^3 + 4", "(x, y, z)"), p.exponentiate(2));
	
		p = makePolynomial("a + b + c", "(a, b, c)");
		assertEquals(makePolynomial("a^3 + b^3 + c^3 + 3*a^2*b + 3*a^2*c + 3*b^2*a + 3*b^2*c + 3*c^2*a + 3*c^2*b + 6*a*b*c", "(a, b, c)"), p.exponentiate(3));
	
		p = makePolynomial("x + y + 1", "tuple(x)");
		assertEquals(parse("x^2 + 2*x*(y + 1) + (y + 1)^2"), p.exponentiate(2));
		
		// NOTE: of interest as it will cause terms to cancel each other out during computation
		//       due to the negative values and odd exponent.
		p = makePolynomial("z^2 + -1*z + -1", "tuple(z)");
		assertEquals(parse("z^6 + -3*z^5 + 5*z^3 + -3*z + -1"), p.exponentiate(3));
	}
	
	//
	// FunctionaApplication API related tests
	@Test
	public void testGetFunctor() {
		Polynomial p = makePolynomial("0", "(x, y)");
		Assert.assertNull(p.getFunctor());
		
		p = makePolynomial("z^4*y^2*x^3", "(x, y)");
		assertEquals(Expressions.makeSymbol("*"), p.getFunctor());
		
		p = makePolynomial("z^4*y^2*x^3 + 10", "(x, y)");
		assertEquals(Expressions.makeSymbol("+"), p.getFunctor());
	}
	
	@Test
	public void testGetArguments() {
		List<Expression> args = makePolynomial("0", "(x, y)").getArguments();
		assertEquals(0, args.size());
		
		args = makePolynomial("x^2*y^3", "(x, y)").getArguments();
		assertEquals(2, args.size());
		assertEquals(parse("(x^2, y^3)").getArguments(), args);
		
		args = makePolynomial("x^2*y^3 + 10", "(x, y)").getArguments();
		assertEquals(2, args.size());
		assertEquals(parse("(x^2*y^3, 10)").getArguments(), args);
	}
	
	@Test
	public void testNumberOfArguments() {
		Polynomial p = makePolynomial("0", "(x, y)");
		assertEquals(0, p.numberOfArguments());
		
		p = makePolynomial("x^2*y^3", "(x, y)");
		assertEquals(2, p.numberOfArguments());
		
		p = makePolynomial("x^2*y^3 + 10", "(x, y)");
		assertEquals(2, p.numberOfArguments());
	}
	
	@Test
	public void testGet() {
		Polynomial p = makePolynomial("x^2*y^3", "(x, y)");
		assertEquals(Expressions.makeSymbol("*"), p.get(-1));
		assertEquals(parse("x^2"), p.get(0));
		assertEquals(parse("y^3"), p.get(1));
		
		p = makePolynomial("x^2*y^3 + 10", "(x, y)");
		assertEquals(Expressions.makeSymbol("+"), p.get(-1));
		assertEquals(parse("x^2*y^3"), p.get(0));
		assertEquals(parse("10"), p.get(1));
	}
	
	@Test
	public void testSet() {
		Polynomial p = makePolynomial("x^2*y^3", "(x, y)");
		assertEquals(parse("2*y^3"), p.set(0, parse("2")));
		assertEquals(parse("x^5"), p.set(1, parse("x^3")));
		assertEquals(parse("x^2*z^4"), p.set(1, parse("z^4")));
		
		p = makePolynomial("x^2*y^3 + 10", "(x, y)");
		assertEquals(parse("2 + 10"), p.set(0, parse("2")));
		assertEquals(parse("x^2*y^3 + 0"), p.set(1, parse("0")));
		assertEquals(parse("(2 + 3) + 10"), p.set(0, parse("2 + 3")));
	}
	
	@Test
	public void testCompareTo() {		
		assertEquals(1, makePolynomial("10", "(x, y)").compareTo(makePolynomial("5*x", "(x, y)")));
		assertEquals(1, makePolynomial("10", "(x, y)").compareTo(makePolynomial("x^2", "(x, y)")));
		assertEquals(1, makePolynomial("5*x", "(x, y)").compareTo(makePolynomial("x^2", "(x, y)")));
		
		assertEquals(0, makePolynomial("0", "(x, y)").compareTo(makePolynomial("7", "(x, y)")));
		assertEquals(0, makePolynomial("2*x^2*y^3", "(x, y)").compareTo(makePolynomial("4*x^2*y^3", "(x, y)")));
		
		assertEquals(1, makePolynomial("x^2*y^3", "(x, y, z)").compareTo(makePolynomial("z^6", "(x, y, z)")));		
		assertEquals(-1, makePolynomial("x^2*y^3", "(x, y, z)").compareTo(makePolynomial("z^5", "(x, y, z)")));
	
		// Mismatched lengths
		assertEquals(-1, makePolynomial("2*x^2*y^3 + 10", "(x, y)").compareTo(makePolynomial("2*x^2*y^3", "(x, y)")));
		assertEquals(1, makePolynomial("2*x^2*y^3", "(x, y)").compareTo(makePolynomial("2*x^2*y^3 + 10", "(x, y)")));
	
		// Mismatched signatures so can only compare based on degree
		assertEquals(-1, makePolynomial("x^2*y^3", "(x, y)").compareTo(makePolynomial("w^1", "tuple(w)")));
		assertEquals(0, makePolynomial("x^2*y^3", "(x, y)").compareTo(makePolynomial("w^5", "tuple(w)")));
		assertEquals(1, makePolynomial("x^2*y^3", "(x, y)").compareTo(makePolynomial("w^6", "tuple(w)")));
	}
	
	//
	// Additional Tests
	@Test
	public void testEquals() {
		Assert.assertTrue(makePolynomial("0", "(x, y)").equals(makePolynomial("0", "(x, y)")));
		Assert.assertTrue(makePolynomial("0", "(x, y)").equals(parse("0")));
		
		Assert.assertTrue(makePolynomial("x^2*y^3", "(x, y)").equals(makePolynomial("y^3*x^2", "(x, y)")));
		Assert.assertFalse(makePolynomial("x^2*y^3", "(x, y)").equals(parse("y^3*x^2")));
		
		Assert.assertTrue(makePolynomial("x^2*y^3", "(x, y)").equals(parse("x^2*y^3")));
		Assert.assertFalse(makePolynomial("x^2*y^3", "(x, y)").equals(parse("1*x^2*y^3")));
		
		Assert.assertTrue(makePolynomial("x^2*y^3 + 10", "(x, y)").equals(parse("x^2*y^3 + 10")));
		Assert.assertFalse(makePolynomial("x^2*y^3 + 10", "(x, y)").equals(parse("1*x^2*y^3 + 10")));
	}
	
	@Test
	public void testToString() {
		assertEquals("0", makePolynomial("0", "(x, y)").toString());
		assertEquals("1", makePolynomial("1", "(x, y)").toString());
		assertEquals("x", makePolynomial("x", "(x, y)").toString());
		assertEquals("2 * x ^ 3", makePolynomial("2*x^3", "(x, y)").toString());
		assertEquals("x * y", makePolynomial("y*x", "(x, y)").toString());
	}
	
	@Test
	public void testNormalization() {
		assertEquals("2 * J", makePolynomial("-2*J + 4*J").toString());
	}
	//
	// PRIVATE
	//	
	private static Monomial makeMonomial(String monomial) {
		Monomial result = DefaultMonomial.make(parse(monomial));
		return result;
	}
	
	private static Polynomial makePolynomial(String polynomial) {
		Polynomial result = DefaultPolynomial.make(parse(polynomial));
		return result;
	}
	
	private static Polynomial makePolynomial(String polynomial, String tupleVariables) {
		Polynomial result = DefaultPolynomial.make(parse(polynomial), parse(tupleVariables).getArguments());
		return result;
	}
}

