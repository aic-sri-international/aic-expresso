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

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Monomial;
import com.sri.ai.grinder.core.DefaultMonomial;
import com.sri.ai.util.math.Rational;

public class DefaultMonomialTest {

	//
	// Monomial API
	@Test
	public void testMake() {
// TODO		
	}
	
	@Test
	public void testGetCoefficient() {
		Assert.assertEquals(new Rational(0), makeMonomial("0").getCoefficient());
		Assert.assertEquals(new Rational(0), makeMonomial("0*x^2").getCoefficient());
		Assert.assertEquals(new Rational(1), makeMonomial("1").getCoefficient());
		Assert.assertEquals(new Rational(-1), makeMonomial("-1").getCoefficient());
		Assert.assertEquals(new Rational(2), makeMonomial("2").getCoefficient());
		
		// Ensure the coefficient is set to 1 if not explicitly repesented in the expression
		Assert.assertEquals(new Rational(1), makeMonomial("x^2").getCoefficient());
		
		Assert.assertEquals(new Rational(4), makeMonomial("4*x^2").getCoefficient());
		
		Assert.assertEquals(new Rational(4), makeMonomial("y*4*x^2").getCoefficient());
		
		// Test that numerical constants are multiplied together to create the coefficient
		Assert.assertEquals(new Rational(8), makeMonomial("y^3*2*x^2*4").getCoefficient());
	}
	
	@Test
	public void testGetVariables() {
		// A constant, no variables.
		Monomial m = makeMonomial("2");
		Assert.assertEquals(0, m.getVariables().size());
		
		// If a zero coefficient, the monomial is automatically collapsed to 0
		m = makeMonomial("0*x^2");
		Assert.assertEquals(0, m.getVariables().size());
		
		// A single variable
		m = makeMonomial("x");
		Assert.assertEquals(1, m.getVariables().size());
		Assert.assertEquals(Collections.singleton(Expressions.parse("x")), m.getVariables());
		
		// A coefficient and a single variable
		m = makeMonomial("2*x");
		Assert.assertEquals(1, m.getVariables().size());
		Assert.assertEquals(Collections.singleton(Expressions.parse("x")), m.getVariables());
		
		// Two variables
		m = makeMonomial("y^2*x^3");
		Assert.assertEquals(2, m.getVariables().size());
		Assert.assertEquals(new HashSet<>(Arrays.asList(Expressions.parse("x"), Expressions.parse("y"))), m.getVariables());
		
		// A coefficient and two variables
		m = makeMonomial("4*y^2*x^3");
		Assert.assertEquals(2, m.getVariables().size());
		Assert.assertEquals(new HashSet<>(Arrays.asList(Expressions.parse("x"), Expressions.parse("y"))), m.getVariables());
	}
	
	@Test
	public void testGetVariablesLexicographicallyOrdered() {
// TODO		
	}
	
	@Test
	public void testGetPowersOfLexicographicallyOrderedVariables() {
// TODO		
	}
	
	@Test
	public void testGetPowerOfVariable() {
// TODO		
	}
	
	@Test
	public void testGetSignature() {
// TODO		
	}
	
	@Test
	public void testHaveLikeTerms() {
// TODO		
	}
	
	@Test
	public void testDegree() {
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
	public void testUnionVariablesLexicographically() {
// TODO		
	}
	
	//
	// FunctionApplication API related tests	
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
	public void testClone() {
// TODO		
	}
	
	//
	// Additional Test
	@Test
	public void testToString() {
// TODO		
	}
	
	@Test
	public void testEqualsEquivalentProductExpression() {
// TODO		
	}
	
	//
	// PRIVATE
	//
	private static Monomial makeMonomial(String monomial) {
		Monomial result = DefaultMonomial.make(Expressions.parse(monomial));
		return result;
	}
}
