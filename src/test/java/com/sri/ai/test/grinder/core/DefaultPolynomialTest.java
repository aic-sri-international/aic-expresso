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

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Monomial;
import com.sri.ai.grinder.api.Polynomial;
import com.sri.ai.grinder.core.DefaultMonomial;
import com.sri.ai.grinder.core.DefaultPolynomial;
import com.sri.ai.util.math.Rational;

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
		Assert.assertEquals(Expressions.parse("12"), makePolynomial("2 + 10", "tuple(y)"));
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y + 10", "tuple(y)"));
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y + 10", "(10, y)"));
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y + 10", "(y, 10)"));
		Assert.assertEquals(Expressions.parse("(y + 10)"), makePolynomial("y + 10", "tuple()"));

		Assert.assertEquals(Expressions.parse("2*y + 10"), makePolynomial("y + 10 + y", "tuple(y)"));

		Assert.assertEquals(Expressions.parse("x^3*y^2 + 2*x^2 + y^2 + x + y + 10"), makePolynomial("x^2 + 7 + x^3*y^2 + 3 + y^2 + x^2 + x + y", "tuple(x, y)"));
		Assert.assertEquals(Expressions.parse("5*x*y + 4*y^2 + x + 6"), makePolynomial("(3*x^2 - 2*x + 5*x*y - 2) + (-3*x^2 + 3*x + 4*y^2 + 8)", "(x, y)"));
		
		//
		// Subtractions
		Assert.assertEquals(Expressions.parse("-8"), makePolynomial("2 - 10", "tuple(y)"));
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
	
		//
		// Exponentiation
		Assert.assertEquals(Expressions.parse("1"), makePolynomial("0^0", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("0"), makePolynomial("0^1", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("0"), makePolynomial("0^3", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("1"), makePolynomial("1^0", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("1"), makePolynomial("1^1", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("1"), makePolynomial("1^3", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("1"), makePolynomial("x^0", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("x"), makePolynomial("x^1", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("x^3"), makePolynomial("x^3", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("1"), makePolynomial("(x + 2)^0", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("x + 2"), makePolynomial("(x + 2)^1", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("x^2 + 4*x + 4"), makePolynomial("(x + 2)^2", "tuple(x)"));
		Assert.assertEquals(Expressions.parse("x^3 + 6*x^2 + 12*x + 8"), makePolynomial("(x + 2)^3", "tuple(x)"));
		
		//
		// Division
// TODO		
	
	}
	
	@Test
	public void testGetSignatureFactors() {
		Polynomial p = makePolynomial("3*x^2*y^4", "tuple()");
		Assert.assertEquals(Collections.emptyList(), p.getSignatureFactors());
		
		p = makePolynomial("3*x^2*y^4", "tuple(x)");
		Assert.assertEquals(Expressions.parse("tuple(x)").getArguments(), p.getSignatureFactors());
		
		p = makePolynomial("3*x^2*y^4", "tuple(y)");
		Assert.assertEquals(Expressions.parse("tuple(y)").getArguments(), p.getSignatureFactors());
		
		p = makePolynomial("3*x^2*y^4", "tuple(3)");
		Assert.assertEquals(Expressions.parse("tuple(3)").getArguments(), p.getSignatureFactors());
		
		p = makePolynomial("3*x^2*y^4", "(x, y)");
		Assert.assertEquals(Expressions.parse("(x, y)").getArguments(), p.getSignatureFactors());
		
		p = makePolynomial("3*x^2*y^4", "(x, y, z)");
		Assert.assertEquals(Expressions.parse("(x, y, z)").getArguments(), p.getSignatureFactors());
		
		p = makePolynomial("3*x^2*y^4", "(x, y, 3)");
		Assert.assertEquals(Expressions.parse("(x, y, 3)").getArguments(), p.getSignatureFactors());
	}
	
	@Test
	public void testGetSignatureTermMap() {
		Polynomial p = makePolynomial("3*x^2*y^4", "tuple()");
		Map<List<Rational>, Expression> expectedSignatureTermMap = new HashMap<>();
		expectedSignatureTermMap.put(Collections.emptyList(), makeMonomial("3*x^2*y^4"));
		Assert.assertEquals(expectedSignatureTermMap, p.getSignatureTermMap());
		
		p = makePolynomial("3*x^2*y^4", "tuple(x)");
		expectedSignatureTermMap = new HashMap<>();
		expectedSignatureTermMap.put(Arrays.asList(new Rational(2)), Expressions.parse("x^2*(3*y^4)"));
		Assert.assertEquals(expectedSignatureTermMap, p.getSignatureTermMap());
		
		p = makePolynomial("3*x^2*y^4", "tuple(y)");
		expectedSignatureTermMap = new HashMap<>();
		expectedSignatureTermMap.put(Arrays.asList(new Rational(4)), Expressions.parse("y^4*(3*x^2)"));
		Assert.assertEquals(expectedSignatureTermMap, p.getSignatureTermMap());
		
		p = makePolynomial("3*x^2*y^4", "tuple(x, y)");
		expectedSignatureTermMap = new HashMap<>();
		expectedSignatureTermMap.put(Arrays.asList(new Rational(2), new Rational(4)), Expressions.parse("3*x^2*y^4"));
		Assert.assertEquals(expectedSignatureTermMap, p.getSignatureTermMap());
		
		p = makePolynomial("3*x^2*y^4", "tuple(y, x)");
		expectedSignatureTermMap = new HashMap<>();
		expectedSignatureTermMap.put(Arrays.asList(new Rational(4), new Rational(2)), Expressions.parse("3*x^2*y^4"));
		Assert.assertEquals(expectedSignatureTermMap, p.getSignatureTermMap());
		
		p = makePolynomial("3*x^2*y^4", "tuple(x, y, z)");
		expectedSignatureTermMap = new HashMap<>();
		expectedSignatureTermMap.put(Arrays.asList(new Rational(2), new Rational(4), new Rational(0)), Expressions.parse("3*x^2*y^4"));
		Assert.assertEquals(expectedSignatureTermMap, p.getSignatureTermMap());
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
		Assert.assertEquals(makeMonomial("0"), makePolynomial("0", "(x, y)").asMonomial());
		Assert.assertEquals(makeMonomial("3*x^2*y^4"), makePolynomial("3*x^2*y^4", "(x, y)").asMonomial());
	
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
		Assert.assertEquals(1, makePolynomial("0", "(x, y)").numberOfTerms());
		Assert.assertEquals(1, makePolynomial("3*x^2*y^4", "(x, y)").numberOfTerms());
		// NOTE: like terms, so are added together automatically
		Assert.assertEquals(1, makePolynomial("3*x^2*y^4 + x^2*y^4", "(x, y)").numberOfTerms());
		
		Assert.assertEquals(2, makePolynomial("3*x^2*y^4 + 3", "(x, y)").numberOfTerms());
		Assert.assertEquals(3, makePolynomial("3*x^2*y^4 + x*y^3 + 3", "(x, y)").numberOfTerms());
	}
	
	@Test
	public void testGetOrderedSummands() {
		Assert.assertEquals(Arrays.asList(makeMonomial("0")),  makePolynomial("0", "(x, y)").getOrderedSummands());
		Assert.assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4")),  makePolynomial("3*x^2*y^4", "(x, y)").getOrderedSummands());
		Assert.assertEquals(Arrays.asList(makeMonomial("4*x^2*y^4")),  makePolynomial("3*x^2*y^4  + x^2*y^4", "(x, y)").getOrderedSummands());
		
		Assert.assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("3")),  makePolynomial("3*x^2*y^4  + 3", "(x, y)").getOrderedSummands());
		Assert.assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("3")),  makePolynomial("3 + 3*x^2*y^4", "(x, y)").getOrderedSummands());
		
		Assert.assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("x*y^3"), makeMonomial("3")),  makePolynomial("3*x^2*y^4 + x*y^3 + 3", "(x, y)").getOrderedSummands());
		Assert.assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("x*y^3"), makeMonomial("3")),  makePolynomial("x*y^3 + 3*x^2*y^4 + 3", "(x, y)").getOrderedSummands());
		Assert.assertEquals(Arrays.asList(makeMonomial("3*x^2*y^4"), makeMonomial("x*y^3"), makeMonomial("3")),  makePolynomial("x*y^3 + 3 + 3*x^2*y^4", "(x, y)").getOrderedSummands());
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
	public void testGetNonNumericConstantFactors() {
		Assert.assertEquals(Collections.emptySet(), makePolynomial("0", "(x, y)").getNonNumericConstantFactors());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(x, y)").getArguments()), makePolynomial("3*x^2*y^4", "(x, y)").getNonNumericConstantFactors());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(x, 3*y^4)").getArguments()), makePolynomial("3*x^2*y^4", "tuple(x)").getNonNumericConstantFactors());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(y, 3*x^2)").getArguments()), makePolynomial("3*x^2*y^4", "tuple(y)").getNonNumericConstantFactors());
		Assert.assertEquals(new HashSet<>(Expressions.parse("tuple(x^2*y^4)").getArguments()), makePolynomial("3*x^2*y^4", "tuple(3)").getNonNumericConstantFactors());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(x, y)").getArguments()), makePolynomial("3*x^2*y^4", "(x, y, 3)").getNonNumericConstantFactors());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(x, y, z)").getArguments()), makePolynomial("3*x^2*y^4 + z + 10", "(x, y, z)").getNonNumericConstantFactors());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(x, 3*y^4, 10*z)").getArguments()), makePolynomial("3*x^2*y^4 + 10*z", "tuple(x)").getNonNumericConstantFactors());

	}
	
	@Test
	public void testAdd() {
		Assert.assertEquals(Expressions.parse("12"), makePolynomial("2", "tuple(y)").add(makePolynomial("10", "tuple(y)")));
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y", "tuple(y)").add(makePolynomial("10", "tuple(y)")));
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y", "tuple(10, y)").add(makePolynomial("10", "tuple(10, y)")));
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y", "tuple(y, 10)").add(makePolynomial("10", "tuple(y, 10)")));
		Assert.assertEquals(Expressions.parse("y + 10"), makePolynomial("y", "tuple()").add(makePolynomial("10", "tuple()")));
	
		Assert.assertEquals(Expressions.parse("2*y + 10"), makePolynomial("2*y", "tuple(y)").add(makePolynomial("10", "tuple(y)")));
		

		Assert.assertEquals(Expressions.parse("x^3*y^2 + 2*x^2 + y^2 + x + y + 10"), 
				makePolynomial("x^2", "(x, y)")
					.add(makePolynomial("7", "(x, y)"))
					.add(makePolynomial("x^3*y^2", "(x, y)"))
					.add(makePolynomial("3", "(x, y)"))
					.add(makePolynomial("y^2", "(x, y)"))
					.add(makePolynomial("x^2", "(x, y)"))
					.add(makePolynomial("x", "(x, y)"))
					.add(makePolynomial("y", "(x, y)"))
		);
		Assert.assertEquals(Expressions.parse("5*x*y + 4*y^2 + x + 6"), 
				makePolynomial("(3*x^2 - 2*x + 5*x*y - 2)", "(x, y)")
					.add(makePolynomial("-3*x^2 + 3*x + 4*y^2 + 8", "(x, y)"))
		);
	}
	
	@Test
	public void testMinus() {
		Assert.assertEquals(Expressions.parse("-8"), makePolynomial("2", "tuple(y)").minus(makePolynomial("10", "tuple(y)")));
		Assert.assertEquals(Expressions.parse("-1"), makePolynomial("1", "tuple()").minus(makePolynomial("2", "tuple()")));
		Assert.assertEquals(Expressions.parse("x"), makePolynomial("2*x", "tuple(x)").minus(makePolynomial("x", "tuple(x)")));
		Assert.assertEquals(Expressions.parse("0"), makePolynomial("x", "tuple(x)").minus(makePolynomial("x", "tuple(x)")));
		Assert.assertEquals(Expressions.parse("0"), makePolynomial("2*x", "tuple(x)").minus(makePolynomial("x", "tuple(x)"))
				.minus(makePolynomial("x", "tuple(x)")));
		Assert.assertEquals(Expressions.parse("x"), makePolynomial("x", "tuple(x)").minus(makePolynomial("0", "tuple(x)")));
		Assert.assertEquals(Expressions.parse("-1*x"), makePolynomial("0", "tuple(x)").minus(makePolynomial("x", "tuple(x)")));
		Assert.assertEquals(Expressions.parse("2*x^3 + -1*x^2 + -1*x"), makePolynomial("2*x^3", "tuple(x)")
				.minus(makePolynomial("x^2", "tuple(x)"))
				.minus(makePolynomial("x", "tuple(x)")));
	}
	
	@Test
	public void testTimes() {
		Assert.assertEquals(Expressions.parse("-1*y + -10"), makePolynomial("-1", "tuple(y)")
				.times(makePolynomial("y + 10", "tuple(y)")));
		Assert.assertEquals(Expressions.parse("x^2*y^4* z + x^2*y^3"), makePolynomial("y^3 + y^4*z", "tuple(x, y, z)")
				.times(makePolynomial("x^2", "tuple(x, y, z)")));
		Assert.assertEquals(Expressions.parse("2*x^2*y + 3*x*y^2 + 4*x^2 + 21*x*y + 15*y^2 + 12*x + 28*y + 5"), makePolynomial("2*x + 3*y + 5", "tuple(x, y)")
				.times(makePolynomial("2*x + 5*y + x*y + 1", "tuple(x, y)")));
	}
	
	@Test
	public void testDivide() {
// TODO
	}
	
	@Test
	public void testExponentiate() {		
		Polynomial p = makePolynomial("0", "tuple(x)");
		Assert.assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(0));
		Assert.assertEquals(makePolynomial("0", "tuple(x)"), p.exponentiate(1));
		Assert.assertEquals(makePolynomial("0", "tuple(x)"), p.exponentiate(3));
		
		p = makePolynomial("1", "tuple(x)");
		Assert.assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(0));
		Assert.assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(1));
		Assert.assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(3));
		
		p = makePolynomial("x", "tuple(x)");
		Assert.assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(0));
		Assert.assertEquals(makePolynomial("x", "tuple(x)"), p.exponentiate(1));
		Assert.assertEquals(makePolynomial("x^3", "tuple(x)"), p.exponentiate(3));
		
		p = makePolynomial("x + 2", "tuple(x)");
		Assert.assertEquals(makePolynomial("1", "tuple(x)"), p.exponentiate(0));
		Assert.assertEquals(makePolynomial("x + 2", "tuple(x)"), p.exponentiate(1));
		Assert.assertEquals(makePolynomial("x^2 + 4*x + 4", "tuple(x)"), p.exponentiate(2));
		Assert.assertEquals(makePolynomial("x^3 + 6*x^2 + 12*x + 8", "tuple(x)"), p.exponentiate(3));
		
		p = makePolynomial("x + y + 2", "(x, y)");
		Assert.assertEquals(makePolynomial("1", "(x, y)"), p.exponentiate(0));
		Assert.assertEquals(makePolynomial("x + y + 2", "(x, y)"), p.exponentiate(1));
		Assert.assertEquals(makePolynomial("x^2 + 2*x*y + 4*x + y^2 + 4*y + 4", "(x, y)"), p.exponentiate(2));
		Assert.assertEquals(makePolynomial("x^3 + 3*x^2*y + 6*x^2 + 3*x*y^2 + 12*x*y + 12*x + y^3 + 6*y^2 + 12*y + 8", "(x, y)"), p.exponentiate(3));
		
		p = makePolynomial("x^2*y + y + z^3 + 2", "(x, y, z)");
		Assert.assertEquals(makePolynomial("1", "(x, y, z)"), p.exponentiate(0));
		Assert.assertEquals(makePolynomial("x^2*y + y + z^3 + 2", "tuple(x, y, z)"), p.exponentiate(1));
		Assert.assertEquals(makePolynomial("x^4*y^2 + 2*x^2*y^2 + 2*x^2*y*z^3 + 4*x^2*y + y^2 + 2*y*z^3 + 4*y + z^6 + 4*z^3 + 4", "(x, y, z)"), p.exponentiate(2));
	
		p = makePolynomial("a + b + c", "(a, b, c)");
		Assert.assertEquals(makePolynomial("a^3 + b^3 + c^3 + 3*a^2*b + 3*a^2*c + 3*b^2*a + 3*b^2*c + 3*c^2*a + 3*c^2*b + 6*a*b*c", "(a, b, c)"), p.exponentiate(3));
	}
	
	@Test
	public void testProject() {		
		Polynomial p = makePolynomial("x^2 * y^4 * z + x^2 * y^3 + y + 10", "(x, y, z)");
		Assert.assertEquals(Expressions.parse("x^2*(y^4*z + y^3) + (y + 10)"), p.project(new HashSet<>(Expressions.parse("tuple(x)").getArguments())));
		Assert.assertEquals(Expressions.parse("y^4*z*(x^2) + y^3*(x^2) + y + 10"), p.project(new HashSet<>(Expressions.parse("tuple(y, z)").getArguments())));
		Assert.assertEquals(Expressions.parse("y^4*(x^2*z) + y^3*(x^2) + y + 10"), p.project(new HashSet<>(Expressions.parse("tuple(y)").getArguments())));
	}
	
	//
	// FunctionaApplication API related tests
	@Test
	public void testGetFunctor() {
		Polynomial p = makePolynomial("0", "(x, y)");
		Assert.assertNull(p.getFunctor());
		
		p = makePolynomial("z^4*y^2*x^3", "(x, y)");
		Assert.assertEquals(Expressions.makeSymbol("*"), p.getFunctor());
		
		p = makePolynomial("z^4*y^2*x^3 + 10", "(x, y)");
		Assert.assertEquals(Expressions.makeSymbol("+"), p.getFunctor());
	}
	
	@Test
	public void testGetArguments() {
		List<Expression> args = makePolynomial("0", "(x, y)").getArguments();
		Assert.assertEquals(0, args.size());
		
		args = makePolynomial("x^2*y^3", "(x, y)").getArguments();
		Assert.assertEquals(2, args.size());
		Assert.assertEquals(Expressions.parse("(x^2, y^3)").getArguments(), args);
		
		args = makePolynomial("x^2*y^3 + 10", "(x, y)").getArguments();
		Assert.assertEquals(2, args.size());
		Assert.assertEquals(Expressions.parse("(x^2*y^3, 10)").getArguments(), args);
	}
	
	@Test
	public void testNumberOfArguments() {
		Polynomial p = makePolynomial("0", "(x, y)");
		Assert.assertEquals(0, p.numberOfArguments());
		
		p = makePolynomial("x^2*y^3", "(x, y)");
		Assert.assertEquals(2, p.numberOfArguments());
		
		p = makePolynomial("x^2*y^3 + 10", "(x, y)");
		Assert.assertEquals(2, p.numberOfArguments());
	}
	
	@Test
	public void testGet() {
		Polynomial p = makePolynomial("x^2*y^3", "(x, y)");
		Assert.assertEquals(Expressions.makeSymbol("*"), p.get(-1));
		Assert.assertEquals(Expressions.parse("x^2"), p.get(0));
		Assert.assertEquals(Expressions.parse("y^3"), p.get(1));
		
		p = makePolynomial("x^2*y^3 + 10", "(x, y)");
		Assert.assertEquals(Expressions.makeSymbol("+"), p.get(-1));
		Assert.assertEquals(Expressions.parse("x^2*y^3"), p.get(0));
		Assert.assertEquals(Expressions.parse("10"), p.get(1));
	}
	
	@Test
	public void testSet() {
		Polynomial p = makePolynomial("x^2*y^3", "(x, y)");
		Assert.assertEquals(makePolynomial("2*y^3", "(x, y)"), p.set(0, Expressions.parse("2")));
		Assert.assertEquals(makePolynomial("x^5", "(x, y)"), p.set(1, Expressions.parse("x^3")));
		Assert.assertEquals(makePolynomial("x^2*z^4", "(x, y)"), p.set(1, Expressions.parse("z^4")));
		
		p = makePolynomial("x^2*y^3 + 10", "(x, y)");
		Assert.assertEquals(makePolynomial("12", "(x, y)"), p.set(0, Expressions.parse("2")));
		Assert.assertEquals(makePolynomial("x^2*y^3", "(x, y)"), p.set(1, Expressions.parse("0")));
		Assert.assertEquals(makePolynomial("15", "(x, y)"), p.set(0, Expressions.parse("2 + 3")));
	}
	
	@Test
	public void testCompareTo() {		
		Assert.assertEquals(1, makePolynomial("10", "(x, y)").compareTo(makePolynomial("5*x", "(x, y)")));
		Assert.assertEquals(1, makePolynomial("10", "(x, y)").compareTo(makePolynomial("x^2", "(x, y)")));
		Assert.assertEquals(1, makePolynomial("5*x", "(x, y)").compareTo(makePolynomial("x^2", "(x, y)")));
		
		Assert.assertEquals(0, makePolynomial("0", "(x, y)").compareTo(makePolynomial("7", "(x, y)")));
		Assert.assertEquals(0, makePolynomial("2*x^2*y^3", "(x, y)").compareTo(makePolynomial("4*x^2*y^3", "(x, y)")));
		
		Assert.assertEquals(1, makePolynomial("x^2*y^3", "(x, y, z)").compareTo(makePolynomial("z^6", "(x, y, z)")));		
		Assert.assertEquals(-1, makePolynomial("x^2*y^3", "(x, y, z)").compareTo(makePolynomial("z^5", "(x, y, z)")));
	
		// Mismatched lengths
		Assert.assertEquals(-1, makePolynomial("2*x^2*y^3 + 10", "(x, y)").compareTo(makePolynomial("2*x^2*y^3", "(x, y)")));
		Assert.assertEquals(1, makePolynomial("2*x^2*y^3", "(x, y)").compareTo(makePolynomial("2*x^2*y^3 + 10", "(x, y)")));
	
		// Mismatched signatures so can only compare based on degree
		Assert.assertEquals(-1, makePolynomial("x^2*y^3", "(x, y)").compareTo(makePolynomial("w^1", "tuple(w)")));
		Assert.assertEquals(0, makePolynomial("x^2*y^3", "(x, y)").compareTo(makePolynomial("w^5", "tuple(w)")));
		Assert.assertEquals(1, makePolynomial("x^2*y^3", "(x, y)").compareTo(makePolynomial("w^6", "tuple(w)")));
	}
	
	//
	// Additional Tests
	@Test
	public void testEquals() {
		Assert.assertTrue(makePolynomial("0", "(x, y)").equals(makePolynomial("0", "(x, y)")));
		Assert.assertTrue(makePolynomial("0", "(x, y)").equals(Expressions.parse("0")));
		
		Assert.assertTrue(makePolynomial("x^2*y^3", "(x, y)").equals(makePolynomial("y^3*x^2", "(x, y)")));
		Assert.assertFalse(makePolynomial("x^2*y^3", "(x, y)").equals(Expressions.parse("y^3*x^2")));
		
		Assert.assertTrue(makePolynomial("x^2*y^3", "(x, y)").equals(Expressions.parse("x^2*y^3")));
		Assert.assertFalse(makePolynomial("x^2*y^3", "(x, y)").equals(Expressions.parse("1*x^2*y^3")));
		
		Assert.assertTrue(makePolynomial("x^2*y^3 + 10", "(x, y)").equals(Expressions.parse("x^2*y^3 + 10")));
		Assert.assertFalse(makePolynomial("x^2*y^3 + 10", "(x, y)").equals(Expressions.parse("1*x^2*y^3 + 10")));
	}
	
	@Test
	public void testToString() {
		Assert.assertEquals("0", makePolynomial("0", "(x, y)").toString());
		Assert.assertEquals("1", makePolynomial("1", "(x, y)").toString());
		Assert.assertEquals("x", makePolynomial("x", "(x, y)").toString());
		Assert.assertEquals("2 * x ^ 3", makePolynomial("2*x^3", "(x, y)").toString());
		Assert.assertEquals("x * y", makePolynomial("y*x", "(x, y)").toString());
	}
	
	//
	// PRIVATE
	//	
	private static Monomial makeMonomial(String monomial) {
		Monomial result = DefaultMonomial.make(Expressions.parse(monomial));
		return result;
	}
	
	private static Polynomial makePolynomial(String polynomial, String tupleSignatureFactors) {
		Polynomial result = DefaultPolynomial.make(Expressions.parse(polynomial), Expressions.parse(tupleSignatureFactors).getArguments());
		return result;
	}
}

