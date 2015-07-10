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
import java.util.HashSet;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Monomial;
import com.sri.ai.grinder.core.DefaultMonomial;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

public class DefaultMonomialTest {

	//
	// Monomial API
	@Test
	public void testMake() {	
		Assert.assertEquals(Expressions.parse("0"), makeMonomial("0"));
		Assert.assertEquals(Expressions.parse("2"), makeMonomial("2"));
		Assert.assertEquals(Expressions.parse("x"), makeMonomial("x"));
		Assert.assertEquals(Expressions.parse("(-2)"), makeMonomial("-2"));
		
		Assert.assertEquals(Expressions.parse("-1*x^2"), makeMonomial("-1*x^2"));
		
		Assert.assertEquals(Expressions.parse("x^2"), makeMonomial("-x^2"));
		Assert.assertEquals(Expressions.parse("x^2"), makeMonomial("(-x)^2"));
		Assert.assertEquals(Expressions.parse("x^2"), makeMonomial("1*-x^2"));
		Assert.assertEquals(Expressions.parse("-1*x^3"), makeMonomial("-x^3"));
		Assert.assertEquals(Expressions.parse("-1*x^3"), makeMonomial("(-x)^3"));
		Assert.assertEquals(Expressions.parse("-1*x^3"), makeMonomial("1*-x^3"));
		
		Assert.assertEquals(Expressions.parse("x*y"), makeMonomial("x*y"));
		Assert.assertEquals(Expressions.parse("16"), makeMonomial("2^2^2"));
		Assert.assertEquals(Expressions.parse("16*x"), makeMonomial("2^2^2*x"));
		Assert.assertEquals(Expressions.parse("(2^x^4)"), makeMonomial("2^x^2^2"));
		Assert.assertEquals(Expressions.parse("(2^2^x^4)"), makeMonomial("2^2^x^2^2"));
		Assert.assertEquals(Expressions.parse("2*x*y"), makeMonomial("2*x*y"));
		Assert.assertEquals(Expressions.parse("16*x"), makeMonomial("*(2*4*(2*x))"));
		Assert.assertEquals(Expressions.parse("-16*x"), makeMonomial("*(2*4*-(2*x))"));
		Assert.assertEquals(Expressions.parse("(y+10)"), makeMonomial("(y + 10)"));
		Assert.assertEquals(Expressions.parse("-1*(y+10)"), makeMonomial("-(y + 10)"));
		Assert.assertEquals(Expressions.parse("x^2*(y^3 + y^4*z)"), makeMonomial("(y^3 + y^4*z)*x^2"));
	}
	
	@Test
	public void testGetNumericConstantFactor() {
		Assert.assertEquals(new Rational(0), makeMonomial("0").getNumericConstantFactor());
		Assert.assertEquals(new Rational(0), makeMonomial("0*x^2").getNumericConstantFactor());
		Assert.assertEquals(new Rational(1), makeMonomial("1").getNumericConstantFactor());
		Assert.assertEquals(new Rational(-1), makeMonomial("-1").getNumericConstantFactor());
		Assert.assertEquals(new Rational(2), makeMonomial("2").getNumericConstantFactor());
		
		// Ensure the numeric constant is set to 1 if not explicitly represented in the expression
		Assert.assertEquals(new Rational(1), makeMonomial("x^2").getNumericConstantFactor());
		
		Assert.assertEquals(new Rational(4), makeMonomial("4*x^2").getNumericConstantFactor());
		
		Assert.assertEquals(new Rational(4), makeMonomial("y*4*x^2").getNumericConstantFactor());
		
		// Test that numerical constants are multiplied together
		Assert.assertEquals(new Rational(8), makeMonomial("y^3*2*x^2*4").getNumericConstantFactor());
		
		// Test edge case where the numeric constant is represented as a power	
		Assert.assertEquals(new Rational(8), makeMonomial("2^3*x^2").getNumericConstantFactor());
		
		// Test edge case where the numeric constant is represented as a power and a separate constant
		Assert.assertEquals(new Rational(24), makeMonomial("2^3*x^2*3").getNumericConstantFactor());
	}
	
	@Test
	public void testIsNumericConstant() {
		Assert.assertTrue(makeMonomial("-2").isNumericConstant());
		Assert.assertTrue(makeMonomial("-1").isNumericConstant());
		Assert.assertTrue(makeMonomial("0").isNumericConstant());
		Assert.assertTrue(makeMonomial("1").isNumericConstant());
		Assert.assertTrue(makeMonomial("2").isNumericConstant());
		Assert.assertTrue(makeMonomial("2.2").isNumericConstant());
		
		Assert.assertFalse(makeMonomial("x").isNumericConstant());
		Assert.assertFalse(makeMonomial("4*x").isNumericConstant());
	}
	
	@Test
	public void testGetFactors() {
		// A numeric constant, 1 factor.
		Monomial m = makeMonomial("2");
		Assert.assertEquals(1, m.getFactors().size());
		
		// If a zero numeric constant factor, the monomial is automatically collapsed to 0
		m = makeMonomial("0*x^2");
		Assert.assertEquals(1, m.getFactors().size());
		
		// A single non numeric constant factor, the default numeric constant 1 is present as a factor
		m = makeMonomial("x");
		Assert.assertEquals(2, m.getFactors().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(1, x)").getArguments()), m.getFactors());
		
		// A numeric constant factor and a single non numeric factor
		m = makeMonomial("2*x");
		Assert.assertEquals(2, m.getFactors().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(2, x)").getArguments()), m.getFactors());
		
		// Two non-numeric constant factors
		m = makeMonomial("y^2*x^3");
		Assert.assertEquals(3, m.getFactors().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(1, x, y)").getArguments()), m.getFactors());
		
		// A numeric constant factor and two non-numeric constant factors
		m = makeMonomial("4*y^2*x^3");
		Assert.assertEquals(3, m.getFactors().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(4, x, y)").getArguments()), m.getFactors());
		
		// Ensure duplicates are correctly collapsed
		m = makeMonomial("4*y^2*y^3");
		Assert.assertEquals(2, m.getFactors().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(4, y)").getArguments()), m.getFactors());		
		
		//
		// Factors examples
		m = makeMonomial("10 * |Dogs|^2 * |People|^3 * f(y) * x^2");
		Assert.assertEquals(5, m.getFactors().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(10, |Dogs|, |People|, f(y), x)").getArguments()), m.getFactors());
	
		//
		m = makeMonomial("10 * 2^a");  // treated as a factors as a non-integer exponent
		Assert.assertEquals(2, m.getFactors().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(10, 2^a)").getArguments()), m.getFactors());
		
		m = makeMonomial("10 * 2^1.1"); // treated as a factors as a non-integer exponent
		Assert.assertEquals(2, m.getFactors().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(10, 2^1.1)").getArguments()), m.getFactors());
	}
	
	@Test
	public void testGetOrderedNonNumericConstantFactors() {
		Monomial m = makeMonomial("2");
		Assert.assertEquals(0, m.getOrderedNonNumericConstantFactors().size());	
		
		m = makeMonomial("2*x");
		Assert.assertEquals(1, m.getOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Expressions.parse("tuple(x)").getArguments(), m.getOrderedNonNumericConstantFactors());
		
		m = makeMonomial("2*x^3*y^7*z^11");
		Assert.assertEquals(3, m.getOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Expressions.parse("(x, y, z)").getArguments(), m.getOrderedNonNumericConstantFactors());
		
		m = makeMonomial("2*z^3*y^7*x^11");
		Assert.assertEquals(3, m.getOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Expressions.parse("(x, y, z)").getArguments(), m.getOrderedNonNumericConstantFactors());
		
		m = makeMonomial("2*z^3*x^7*y^11");
		Assert.assertEquals(3, m.getOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Expressions.parse("(x, y, z)").getArguments(), m.getOrderedNonNumericConstantFactors());
		
		m = makeMonomial("2*z^3*x^7*x^11");
		Assert.assertEquals(2, m.getOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Expressions.parse("(x, z)").getArguments(), m.getOrderedNonNumericConstantFactors());
		
		// Note: z is dropped as we know is = 1
		m = makeMonomial("2*x^3*y^7*z^0");
		Assert.assertEquals(2, m.getOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Expressions.parse("(x, y)").getArguments(), m.getOrderedNonNumericConstantFactors());
	}
	
	@Test
	public void testGetPowersOfOrderedNonNumericConstantFactors() {
		Monomial m = makeMonomial("2");
		Assert.assertEquals(0, m.getPowersOfOrderedNonNumericConstantFactors().size());	
		
		m = makeMonomial("2*x");
		Assert.assertEquals(1, m.getPowersOfOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Arrays.asList(new Rational(1)), m.getPowersOfOrderedNonNumericConstantFactors());
		
		m = makeMonomial("2*x^3*y^7*z^11");
		Assert.assertEquals(3, m.getPowersOfOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Arrays.asList(new Rational(3), new Rational(7), new Rational(11)), m.getPowersOfOrderedNonNumericConstantFactors());
		
		m = makeMonomial("2*z^3*y^7*x^11");
		Assert.assertEquals(3, m.getPowersOfOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Arrays.asList(new Rational(11), new Rational(7), new Rational(3)), m.getPowersOfOrderedNonNumericConstantFactors());
		
		m = makeMonomial("2*z^3*x^7*y^11");
		Assert.assertEquals(3, m.getPowersOfOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Arrays.asList(new Rational(7), new Rational(11), new Rational(3)), m.getPowersOfOrderedNonNumericConstantFactors());
		
		m = makeMonomial("2*z^3*x^7*x^11");
		Assert.assertEquals(2, m.getPowersOfOrderedNonNumericConstantFactors().size());
		Assert.assertEquals(Arrays.asList(new Rational(18), new Rational(3)), m.getPowersOfOrderedNonNumericConstantFactors());	
	}
	
	@Test
	public void testGetCoefficient() {
		Monomial m = makeMonomial("0");
		Assert.assertEquals(makeMonomial("0"), m.getCoefficient(new HashSet<>(Collections.emptyList())));
		Assert.assertEquals(makeMonomial("0"), m.getCoefficient(new HashSet<>(Expressions.parse("tuple(x)").getArguments())));
	
		m = makeMonomial("1");
		Assert.assertEquals(makeMonomial("1"), m.getCoefficient(new HashSet<>(Collections.emptyList())));
		Assert.assertEquals(makeMonomial("1"), m.getCoefficient(new HashSet<>(Expressions.parse("tuple(x)").getArguments())));

		m = makeMonomial("x");
		Assert.assertEquals(makeMonomial("x"), m.getCoefficient(new HashSet<>(Collections.emptyList())));
		Assert.assertEquals(makeMonomial("1"), m.getCoefficient(new HashSet<>(Expressions.parse("tuple(x)").getArguments())));	
		
		m = makeMonomial("3*x^2");
		Assert.assertEquals(makeMonomial("3*x^2"), m.getCoefficient(new HashSet<>(Collections.emptyList())));
		Assert.assertEquals(makeMonomial("3*x^2"), m.getCoefficient(new HashSet<>(Expressions.parse("tuple(1)").getArguments())));
		Assert.assertEquals(makeMonomial("3*x^2"), m.getCoefficient(new HashSet<>(Expressions.parse("tuple(z)").getArguments())));
		Assert.assertEquals(makeMonomial("x^2"), m.getCoefficient(new HashSet<>(Expressions.parse("tuple(3)").getArguments())));
		Assert.assertEquals(makeMonomial("3"), m.getCoefficient(new HashSet<>(Expressions.parse("tuple(x)").getArguments())));
		Assert.assertEquals(makeMonomial("1"), m.getCoefficient(new HashSet<>(Expressions.parse("(3, x)").getArguments())));

		m = makeMonomial("3*x^2*y^4");
		Assert.assertEquals(makeMonomial("3*x^2*y^4"), m.getCoefficient(new HashSet<>(Collections.emptyList())));
		Assert.assertEquals(makeMonomial("3*x^2*y^4"), m.getCoefficient(new HashSet<>(Expressions.parse("tuple(1)").getArguments())));
		Assert.assertEquals(makeMonomial("3*y^4"), m.getCoefficient(new HashSet<>(Expressions.parse("tuple(x)").getArguments())));	
	}
	
	@Test
	public void testGetPowerOfFactor() {
		// Implicit power of 1
		Monomial m = makeMonomial("2*x");
		Assert.assertEquals(2, m.getFactors().size());
		Assert.assertEquals(new Rational(0), m.getPowerOfFactor(Expressions.parse("1")));
		Assert.assertEquals(new Rational(1), m.getPowerOfFactor(Expressions.parse("2")));
		Assert.assertEquals(new Rational(1), m.getPowerOfFactor(Expressions.parse("x")));	
		
		m = makeMonomial("2*x^3*y^7*z^11");
		Assert.assertEquals(new Rational(3), m.getPowerOfFactor(Expressions.parse("x")));	
		Assert.assertEquals(new Rational(7), m.getPowerOfFactor(Expressions.parse("y")));
		Assert.assertEquals(new Rational(11), m.getPowerOfFactor(Expressions.parse("z")));	
		
		m = makeMonomial("2*z^3*y^7*x^11");
		Assert.assertEquals(new Rational(11), m.getPowerOfFactor(Expressions.parse("x")));	
		Assert.assertEquals(new Rational(7), m.getPowerOfFactor(Expressions.parse("y")));
		Assert.assertEquals(new Rational(3), m.getPowerOfFactor(Expressions.parse("z")));
		
		m = makeMonomial("2*z^3*x^7*y^11");
		Assert.assertEquals(new Rational(7), m.getPowerOfFactor(Expressions.parse("x")));	
		Assert.assertEquals(new Rational(11), m.getPowerOfFactor(Expressions.parse("y")));
		Assert.assertEquals(new Rational(3), m.getPowerOfFactor(Expressions.parse("z")));
		
		m = makeMonomial("2*z^3*x^7*x^11");
		Assert.assertEquals(new Rational(18), m.getPowerOfFactor(Expressions.parse("x")));	
		Assert.assertEquals(new Rational(0), m.getPowerOfFactor(Expressions.parse("y")));
		Assert.assertEquals(new Rational(3), m.getPowerOfFactor(Expressions.parse("z")));
		
		// If factor not present in the monomial its power is 0.
		m = makeMonomial("2");
		Assert.assertEquals(new Rational(0), m.getPowerOfFactor(Expressions.parse("x")));	
	}
	
	@Test
	public void testGetSignature() {
		Monomial m = makeMonomial("2*x");
		Assert.assertEquals(Arrays.asList(new Rational(1)), m.getSignature(Expressions.parse("tuple(x)").getArguments()));	
		
		//
		// Examples form Javadoc
		// signature(3 * y * x^2,  (x, y, z))  =  (2, 1, 0)
		m = makeMonomial("3 * y * x^2");
		Assert.assertEquals(Arrays.asList(new Rational(2), new Rational(1), new Rational(0)), m.getSignature(Expressions.parse("tuple(x, y, z)").getArguments()));	
		Assert.assertEquals(Arrays.asList(new Rational(2), new Rational(1)), m.getSignature());	
		
		// signature(3, (x, y, z))  =  (0, 0, 0)
		m = makeMonomial("3");
		Assert.assertEquals(Arrays.asList(new Rational(0), new Rational(0), new Rational(0)), m.getSignature(Expressions.parse("tuple(x, y, z)").getArguments()));
		Assert.assertEquals(Collections.emptyList(), m.getSignature());	
		
		// signature(3 * (x + 2),  (x + 2, x, y, z)) =  (1, 0, 0, 0)
		m = makeMonomial("3 * (x + 2)");
		Assert.assertEquals(Arrays.asList(new Rational(1), new Rational(0), new Rational(0), new Rational(0)), m.getSignature(Expressions.parse("tuple(x + 2, x, y, z)").getArguments()));		
		Assert.assertEquals(Arrays.asList(new Rational(1)), m.getSignature());	
		
		// signature(3 * (x + 2),  (3, x + 2, x, y, z)) = (1, 1, 0, 0, 0)
		m = makeMonomial("3 * (x + 2)");
		Assert.assertEquals(Arrays.asList(new Rational(1), new Rational(1), new Rational(0), new Rational(0), new Rational(0)), m.getSignature(Expressions.parse("tuple(3, x + 2, x, y, z)").getArguments()));		
		Assert.assertEquals(Arrays.asList(new Rational(1)), m.getSignature());
	}
	
	@Test
	public void testAreLikeTermsUnionOfFactors() {
		Monomial m1 = makeMonomial("2");
		Monomial m2 = makeMonomial("2");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(true, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("2");
		m2 = makeMonomial("3");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(true, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("x");
		m2 = makeMonomial("x");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(true, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("x");
		m2 = makeMonomial("y");
		Assert.assertEquals(false, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(false, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("2");
		m2 = makeMonomial("2*x");
		Assert.assertEquals(false, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(false, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("2*x");
		m2 = makeMonomial("2*x");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(true, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("2*x^1");
		m2 = makeMonomial("2*x");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(true, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("2*x");
		m2 = makeMonomial("2*x^2");
		Assert.assertEquals(false, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(false, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("2*y^1*x");
		m2 = makeMonomial("2*x^1*y");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(true, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("2*y^3*x^2");
		m2 = makeMonomial("2*x^2*y^7");
		Assert.assertEquals(false, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(false, m1.areLikeTerms(m2));
		
		m1 = makeMonomial("2*y^1*x");
		m2 = makeMonomial("2*x^1*y*z^4");
		Assert.assertEquals(false, m1.areLikeTerms(m2, Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2)));
		Assert.assertEquals(false, m1.areLikeTerms(m2));
	}
	
	@Test
	public void testAreLikeTermsExplicitSignatureOfFactors() {
		Monomial m1 = makeMonomial("2");
		Monomial m2 = makeMonomial("2");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(2)").getArguments()));
		
		m1 = makeMonomial("2");
		m2 = makeMonomial("3");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("tuple(2)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("tuple(3)").getArguments()));
		
		m1 = makeMonomial("x");
		m2 = makeMonomial("x");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(x)").getArguments()));
		
		m1 = makeMonomial("x");
		m2 = makeMonomial("y");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(z)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("tuple(x)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("tuple(y)").getArguments()));
		
		m1 = makeMonomial("2");
		m2 = makeMonomial("2*x");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(2)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("tuple(x)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("(2, x)").getArguments()));
		
		m1 = makeMonomial("2*x");
		m2 = makeMonomial("2*x");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(z)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(2)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(x)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("(2, x)").getArguments()));
		
		m1 = makeMonomial("2*x^1");
		m2 = makeMonomial("2*x");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(z)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(2)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(x)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("(2, x)").getArguments()));
		
		m1 = makeMonomial("2*x");
		m2 = makeMonomial("2*x^2");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(z)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(2)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("tuple(x)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("(2, x)").getArguments()));
		
		m1 = makeMonomial("2*y^1*x");
		m2 = makeMonomial("2*x^1*y");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(z)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(2)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(x)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(y)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("(2, x)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("(2, y)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("(2, x, y)").getArguments()));
		
		m1 = makeMonomial("2*y^3*x^2");
		m2 = makeMonomial("2*x^2*y^7");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(z)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(2)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(x)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("tuple(y)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("(2, x)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("(2, y)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("(2, x, y)").getArguments()));
		
		m1 = makeMonomial("2*y^1*x");
		m2 = makeMonomial("2*x^1*y*z^4");
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(1)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("tuple(z)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(2)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(x)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("tuple(y)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("(2, x)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("(2, y)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("(2, z)").getArguments()));
		Assert.assertEquals(true, m1.areLikeTerms(m2, Expressions.parse("(2, x, y)").getArguments()));
		Assert.assertEquals(false, m1.areLikeTerms(m2, Expressions.parse("(2, x, y, z)").getArguments()));
	}

	
	@Test
	public void testDegree() {
		Monomial m = makeMonomial("2");
		Assert.assertEquals(new Rational(0), m.degree());
		
		m = makeMonomial("x");
		Assert.assertEquals(new Rational(1), m.degree());
		
		m = makeMonomial("2*x");
		Assert.assertEquals(new Rational(1), m.degree());
		
		m = makeMonomial("2*x^3");
		Assert.assertEquals(new Rational(3), m.degree());
		
		m = makeMonomial("2*x^3*y");
		Assert.assertEquals(new Rational(4), m.degree());
		
		m = makeMonomial("2*x^3*y*z^5");
		Assert.assertEquals(new Rational(9), m.degree());
	}
	
	@Test
	public void testTimes() {
		Monomial m1 = makeMonomial("2");
		Monomial m2 = makeMonomial("3");		
		Assert.assertEquals(makeMonomial("6"), m1.times(m2));
		
		m1 = makeMonomial("x");
		m2 = makeMonomial("x");		
		Assert.assertEquals(makeMonomial("x^2"), m1.times(m2));
		
		m1 = makeMonomial("2*x^2");
		m2 = makeMonomial("3*x^3");		
		Assert.assertEquals(makeMonomial("6*x^5"), m1.times(m2));
		
		m1 = makeMonomial("3*x^2*y");
		m2 = makeMonomial("3*x^3*z^2");		
		Assert.assertEquals(makeMonomial("9*x^5*y*z^2"), m1.times(m2));
		
		m1 = makeMonomial("0");
		m2 = makeMonomial("3*x^3*z^2");		
		Assert.assertEquals(makeMonomial("0"), m1.times(m2));
		
		m1 = makeMonomial("3*x^2*y");
		m2 = makeMonomial("0");		
		Assert.assertEquals(makeMonomial("0"), m1.times(m2));
	}
	
	@Test
	public void testDivide() {
		Monomial m1 = makeMonomial("1");
		Monomial m2 = makeMonomial("2");		
		Assert.assertEquals(new Pair<>(makeMonomial("0.5"), makeMonomial("0")), m1.divide(m2));
		
		m1 = makeMonomial("3");
		m2 = makeMonomial("2");		
		Assert.assertEquals(new Pair<>(makeMonomial("1.5"), makeMonomial("0")), m1.divide(m2));
		
		m1 = makeMonomial("3*x^3");
		m2 = makeMonomial("2*x^2");		
		Assert.assertEquals(new Pair<>(makeMonomial("1.5*x"), makeMonomial("0")), m1.divide(m2));
		
		m1 = makeMonomial("3*x^3");
		m2 = makeMonomial("2*x^3");		
		Assert.assertEquals(new Pair<>(makeMonomial("1.5"), makeMonomial("0")), m1.divide(m2));
		
		m1 = makeMonomial("3*x^3*z^2");	
		m2 = makeMonomial("3*x^2");
		Assert.assertEquals(new Pair<>(makeMonomial("x*z^2"), makeMonomial("0")), m1.divide(m2));
		
		m1 = makeMonomial("3*x^3*z^2");	
		m2 = makeMonomial("3*x^4");
		Assert.assertEquals(new Pair<>(makeMonomial("0"), makeMonomial("3*x^3*z^2")), m1.divide(m2));
		
		m1 = makeMonomial("3*x^2*y");
		m2 = makeMonomial("3*x^3*z^2");		
		Assert.assertEquals(new Pair<>(makeMonomial("0"), makeMonomial("3*x^2*y")), m1.divide(m2));
		
		m1 = makeMonomial("0");
		m2 = makeMonomial("3*x^3*z^2");	
		Assert.assertEquals(new Pair<>(makeMonomial("0"), makeMonomial("0")), m1.divide(m2));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testDivideIllegalArgumentException() {
		Monomial m1 = makeMonomial("2*x^3");
		Monomial m2 = makeMonomial("0*x");
		
		m1.divide(m2);
	}
	
	@Test
	public void testExponentiate() {
		Monomial m = makeMonomial("0");
		Assert.assertEquals(makeMonomial("0"), m.exponentiate(3));
		
		m = makeMonomial("2");
		Assert.assertEquals(makeMonomial("8"), m.exponentiate(3));
		
		m = makeMonomial("2*x");
		Assert.assertEquals(makeMonomial("8*x^3"), m.exponentiate(3));
		
		m = makeMonomial("2*x^2");
		Assert.assertEquals(makeMonomial("8*x^6"), m.exponentiate(3));
		
		m = makeMonomial("2*y^2*x^3");
		Assert.assertEquals(makeMonomial("8*x^9*y^6"), m.exponentiate(3));
		
		m = makeMonomial("2*y^2*x^3");
		Assert.assertEquals(makeMonomial("1"), m.exponentiate(0));
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testIllegalExponeniateArgument() {
		makeMonomial("x^2").exponentiate(-1);
	}
	
	@Test
	public void testOrderedUnionOfNonNumericConstantFactors() {
		Monomial m1 = makeMonomial("0");
		Monomial m2 = makeMonomial("0");
		Assert.assertEquals(Collections.emptyList(), Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2));
		
		m1 = makeMonomial("x");
		m2 = makeMonomial("x");
		Assert.assertEquals(Expressions.parse("tuple(x)").getArguments(), Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2));
		
		m1 = makeMonomial("x^2*y^3");
		m2 = makeMonomial("y^2*x^3");
		Assert.assertEquals(Expressions.parse("tuple(x, y)").getArguments(), Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2));
		
		m1 = makeMonomial("x^2*y^3");
		m2 = makeMonomial("z^4*y^2*x^3");
		Assert.assertEquals(Expressions.parse("tuple(x, y, z)").getArguments(), Monomial.orderedUnionOfNonNumericConstantFactors(m1, m2));
	}
	
	//
	// Expression API related tests	
	@Test
	public void testGetFunctor() {
		Monomial m = makeMonomial("0");
		Assert.assertNull(m.getFunctor());
		
		m = makeMonomial("z^4*y^2*x^3");
		Assert.assertEquals(Expressions.makeSymbol("*"), m.getFunctor());
	}
	
	@Test
	public void testGetArguments() {
		List<Expression> args = makeMonomial("0").getArguments();
		Assert.assertEquals(0, args.size());
		
		args = makeMonomial("x^2*y^3").getArguments();
		Assert.assertEquals(2, args.size());
		Assert.assertEquals(Expressions.parse("(x^2, y^3)").getArguments(), args);
		
		args = makeMonomial("z^4*y^2*x^3*6").getArguments();
		Assert.assertEquals(4, args.size());
		Assert.assertEquals(Expressions.parse("(6, x^3, y^2, z^4)").getArguments(), args);
	}
	
	@Test
	public void testNumberOfArguments() {
		Monomial m = makeMonomial("0");
		Assert.assertEquals(0, m.numberOfArguments());
		
		m = makeMonomial("x^2*y^3");
		Assert.assertEquals(2, m.numberOfArguments());
		
		m = makeMonomial("z^4*y^2*x^3*6");
		Assert.assertEquals(4, m.numberOfArguments());
	}
	
	@Test
	public void testGet() {		
		Monomial m = makeMonomial("x^2*y^3");
		Assert.assertEquals(Expressions.makeSymbol("*"), m.get(-1));
		Assert.assertEquals(Expressions.parse("x^2"), m.get(0));
		Assert.assertEquals(Expressions.parse("y^3"), m.get(1));
		
		m = makeMonomial("z^4*y^2*x^3*6");
		Assert.assertEquals(Expressions.makeSymbol("*"), m.get(-1));
		Assert.assertEquals(Expressions.parse("6"), m.get(0));
		Assert.assertEquals(Expressions.parse("x^3"), m.get(1));
		Assert.assertEquals(Expressions.parse("y^2"), m.get(2));
		Assert.assertEquals(Expressions.parse("z^4"), m.get(3));
	}
	
	@Test
	public void testSet() {		
		Monomial m = makeMonomial("x^2*y^3");
		Assert.assertEquals(makeMonomial("2*y^3"), m.set(0, Expressions.parse("2")));
		Assert.assertEquals(makeMonomial("x^5"), m.set(1, Expressions.parse("x^3")));
		Assert.assertEquals(makeMonomial("x^2*z^4"), m.set(1, Expressions.parse("z^4")));
		
		m = makeMonomial("x^2*y^3");
		// Note: x^2 is set to a factor (i.e. negative exponent)
		Assert.assertEquals(makeMonomial("(x^(-2))^1*y^3"), m.set(0, Expressions.parse("x^(-2)")));
		
		m = makeMonomial("x^2*y^3");
		//  Note: by changing the functor the old monomial is transformed into a factor
		Assert.assertEquals(makeMonomial("1*(x^2+y^3)^1"), m.set(-1, Expressions.makeSymbol("+")));

		m = makeMonomial("x^2*y^3");
		//  Note: the x^2 becomes a factor (2+3)^1
		Assert.assertEquals(makeMonomial("1*(2+3)^1*y^3"), m.set(0, Expressions.parse("2 + 3")));
		
		m = makeMonomial("x^2*y^3");
		//  Note: the x^2 becomes a factor (2-3)^1
		Assert.assertEquals(makeMonomial("1*(2-3)^1*y^3"), m.set(0, Expressions.parse("2 - 3")));
		
		m = makeMonomial("x^2*y^3");
		//  Note: the x^2 becomes a factor (2/3)^1
		Assert.assertEquals(makeMonomial("1*(2/3)^1*y^3"), m.set(0, Expressions.parse("2 / 3")));
	}
	
	@Test
	public void testCompareTo() {
		Assert.assertEquals(1, makeMonomial("x^2*y^3").compareTo(makeMonomial("w^1")));
		Assert.assertEquals(1, makeMonomial("10").compareTo(makeMonomial("5*x")));
		Assert.assertEquals(1, makeMonomial("10").compareTo(makeMonomial("x^2")));
		Assert.assertEquals(1, makeMonomial("5*x").compareTo(makeMonomial("x^2")));
		
		Assert.assertEquals(0, makeMonomial("0").compareTo(makeMonomial("7")));
		Assert.assertEquals(0, makeMonomial("2*x^2*y^3").compareTo(makeMonomial("4*x^2*y^3")));
		
		Assert.assertEquals(-1, makeMonomial("x^2*y^3").compareTo(makeMonomial("z^6")));		
	}
	
	//
	// Additional Test
	@Test
	public void testEquals() {
		Assert.assertTrue(makeMonomial("0").equals(makeMonomial("0")));
		Assert.assertTrue(makeMonomial("0").equals(Expressions.parse("0")));
		
		Assert.assertTrue(makeMonomial("x^2*y^3").equals(makeMonomial("y^3*x^2")));
		Assert.assertFalse(makeMonomial("x^2*y^3").equals(Expressions.parse("y^3*x^2")));
		
		Assert.assertTrue(makeMonomial("x^2*y^3").equals(Expressions.parse("x^2*y^3")));
		Assert.assertFalse(makeMonomial("x^2*y^3").equals(Expressions.parse("1*x^2*y^3")));
	}	
	
	@Test
	public void testToString() {
		Assert.assertEquals("0", makeMonomial("0").toString());
		Assert.assertEquals("1", makeMonomial("1").toString());
		Assert.assertEquals("x", makeMonomial("x").toString());
		Assert.assertEquals("2 * x ^ 3", makeMonomial("2*x^3").toString());
		Assert.assertEquals("x * y", makeMonomial("y*x").toString());
	}
	
	//
	// PRIVATE
	//
	private static Monomial makeMonomial(String monomial) {
		Monomial result = DefaultMonomial.make(Expressions.parse(monomial));
		return result;
	}
}
