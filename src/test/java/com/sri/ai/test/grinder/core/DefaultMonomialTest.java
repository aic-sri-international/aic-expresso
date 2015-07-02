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
		
		// Test edge case where the coefficient is represented as a power
		Assert.assertEquals(new Rational(8), makeMonomial("2^3*x^2").getCoefficient());
		
		// Test edge case where the coefficient is represented as a power and a separate constant
		Assert.assertEquals(new Rational(24), makeMonomial("2^3*x^2*3").getCoefficient());
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
		Assert.assertEquals(new HashSet<>(Expressions.parse("(x, y)").getArguments()), m.getVariables());
		
		// A coefficient and two variables
		m = makeMonomial("4*y^2*x^3");
		Assert.assertEquals(2, m.getVariables().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(x, y)").getArguments()), m.getVariables());
		
		// Ensure duplicates are correctly collapse
		m = makeMonomial("4*y^2*y^3");
		Assert.assertEquals(1, m.getVariables().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("tuple(y)").getArguments()), m.getVariables());		
		
		//
		// General variables examples
		m = makeMonomial("10 * |Dogs|^2 * |People|^3 * f(y) * x^2");
		Assert.assertEquals(4, m.getVariables().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("(|Dogs|, |People|, f(y), x)").getArguments()), m.getVariables());
	
		//
		m = makeMonomial("10 * 2^a");
		Assert.assertEquals(1, m.getVariables().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("tuple(2^a)").getArguments()), m.getVariables());
		
		m = makeMonomial("10 * 2^1.1"); // treated as a variable as a non-integer exponent
		Assert.assertEquals(1, m.getVariables().size());
		Assert.assertEquals(new HashSet<>(Expressions.parse("tuple(2^1.1)").getArguments()), m.getVariables());
	}
	
	@Test
	public void testGetVariablesLexicographicallyOrdered() {
		Monomial m = makeMonomial("2");
		Assert.assertEquals(0, m.getVariablesLexicographicallyOrdered().size());	
		
		m = makeMonomial("2*x");
		Assert.assertEquals(1, m.getVariables().size());
		Assert.assertEquals(Expressions.parse("tuple(x)").getArguments(), m.getVariablesLexicographicallyOrdered());
		
		m = makeMonomial("2*x^3*y^7*z^11");
		Assert.assertEquals(3, m.getVariables().size());
		Assert.assertEquals(Expressions.parse("(x, y, z)").getArguments(), m.getVariablesLexicographicallyOrdered());
		
		m = makeMonomial("2*z^3*y^7*x^11");
		Assert.assertEquals(3, m.getVariables().size());
		Assert.assertEquals(Expressions.parse("(x, y, z)").getArguments(), m.getVariablesLexicographicallyOrdered());
		
		m = makeMonomial("2*z^3*x^7*y^11");
		Assert.assertEquals(3, m.getVariables().size());
		Assert.assertEquals(Expressions.parse("(x, y, z)").getArguments(), m.getVariablesLexicographicallyOrdered());
		
		m = makeMonomial("2*z^3*x^7*x^11");
		Assert.assertEquals(2, m.getVariables().size());
		Assert.assertEquals(Expressions.parse("(x, z)").getArguments(), m.getVariablesLexicographicallyOrdered());
	}
	
	@Test
	public void testGetPowersOfLexicographicallyOrderedVariables() {
		Monomial m = makeMonomial("2");
		Assert.assertEquals(0, m.getPowersOfLexicographicallyOrderedVariables().size());	
		
		m = makeMonomial("2*x");
		Assert.assertEquals(1, m.getVariables().size());
		Assert.assertEquals(Arrays.asList(new Rational(1)), m.getPowersOfLexicographicallyOrderedVariables());
		
		m = makeMonomial("2*x^3*y^7*z^11");
		Assert.assertEquals(3, m.getVariables().size());
		Assert.assertEquals(Arrays.asList(new Rational(3), new Rational(7), new Rational(11)), m.getPowersOfLexicographicallyOrderedVariables());
		
		m = makeMonomial("2*z^3*y^7*x^11");
		Assert.assertEquals(3, m.getVariables().size());
		Assert.assertEquals(Arrays.asList(new Rational(11), new Rational(7), new Rational(3)), m.getPowersOfLexicographicallyOrderedVariables());
		
		m = makeMonomial("2*z^3*x^7*y^11");
		Assert.assertEquals(3, m.getVariables().size());
		Assert.assertEquals(Arrays.asList(new Rational(7), new Rational(11), new Rational(3)), m.getPowersOfLexicographicallyOrderedVariables());
		
		m = makeMonomial("2*z^3*x^7*x^11");
		Assert.assertEquals(2, m.getVariables().size());
		Assert.assertEquals(Arrays.asList(new Rational(18), new Rational(3)), m.getPowersOfLexicographicallyOrderedVariables());	
	}
	
	@Test
	public void testGetPowerOfVariable() {
		// Implicit power of 1
		Monomial m = makeMonomial("2*x");
		Assert.assertEquals(1, m.getVariables().size());
		Assert.assertEquals(new Rational(1), m.getPowerOfVariable(Expressions.parse("x")));	
		
		m = makeMonomial("2*x^3*y^7*z^11");
		Assert.assertEquals(new Rational(3), m.getPowerOfVariable(Expressions.parse("x")));	
		Assert.assertEquals(new Rational(7), m.getPowerOfVariable(Expressions.parse("y")));
		Assert.assertEquals(new Rational(11), m.getPowerOfVariable(Expressions.parse("z")));	
		
		m = makeMonomial("2*z^3*y^7*x^11");
		Assert.assertEquals(new Rational(11), m.getPowerOfVariable(Expressions.parse("x")));	
		Assert.assertEquals(new Rational(7), m.getPowerOfVariable(Expressions.parse("y")));
		Assert.assertEquals(new Rational(3), m.getPowerOfVariable(Expressions.parse("z")));
		
		m = makeMonomial("2*z^3*x^7*y^11");
		Assert.assertEquals(new Rational(7), m.getPowerOfVariable(Expressions.parse("x")));	
		Assert.assertEquals(new Rational(11), m.getPowerOfVariable(Expressions.parse("y")));
		Assert.assertEquals(new Rational(3), m.getPowerOfVariable(Expressions.parse("z")));
		
		m = makeMonomial("2*z^3*x^7*x^11");
		Assert.assertEquals(new Rational(18), m.getPowerOfVariable(Expressions.parse("x")));	
		Assert.assertEquals(new Rational(0), m.getPowerOfVariable(Expressions.parse("y")));
		Assert.assertEquals(new Rational(3), m.getPowerOfVariable(Expressions.parse("z")));
		
		// If Variable not present in the monomial its power is 0.
		m = makeMonomial("2");
		Assert.assertEquals(new Rational(0), m.getPowerOfVariable(Expressions.parse("x")));	
	}
	
	@Test
	public void testGetSignature() {
		Monomial m = makeMonomial("2*x");
		Assert.assertEquals(Arrays.asList(new Rational(1)), m.getSignature(Expressions.parse("tuple(x)").getArguments()));	
		
		// Examples form Javadoc
		// signature(3 * y * x^2,  (x, y, z))  =  (2, 1, 0)
		m = makeMonomial("3 * y * x^2");
		Assert.assertEquals(Arrays.asList(new Rational(2), new Rational(1), new Rational(0)), m.getSignature(Expressions.parse("tuple(x, y, z)").getArguments()));	
		// signature(3, (x, y, z))  =  (0, 0, 0)
		m = makeMonomial("3");
		Assert.assertEquals(Arrays.asList(new Rational(0), new Rational(0), new Rational(0)), m.getSignature(Expressions.parse("tuple(x, y, z)").getArguments()));
	}
	
	@Test
	public void testHaveLikeTerms() {
		Monomial m1 = makeMonomial("2");
		Monomial m2 = makeMonomial("2");
		Assert.assertEquals(true, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("2");
		m2 = makeMonomial("3");
		Assert.assertEquals(true, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("x");
		m2 = makeMonomial("x");
		Assert.assertEquals(true, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("x");
		m2 = makeMonomial("y");
		Assert.assertEquals(false, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("2");
		m2 = makeMonomial("2*x");
		Assert.assertEquals(false, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("2*x");
		m2 = makeMonomial("2*x");
		Assert.assertEquals(true, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("2*x^1");
		m2 = makeMonomial("2*x");
		Assert.assertEquals(true, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("2*x");
		m2 = makeMonomial("2*x^2");
		Assert.assertEquals(false, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("2*y^1*x");
		m2 = makeMonomial("2*x^1*y");
		Assert.assertEquals(true, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("2*y^3*x^2");
		m2 = makeMonomial("2*x^2*y^7");
		Assert.assertEquals(false, m1.haveLikeTerms(m2));
		
		m1 = makeMonomial("2*y^1*x");
		m2 = makeMonomial("2*x^1*y*z^4");
		Assert.assertEquals(false, m1.haveLikeTerms(m2));
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
// TODO		
	}
	
	@Test
	public void testDivide() {
// TODO		
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
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void testIllegalExponeniateArgument() {
		makeMonomial("x^2").exponentiate(-1);
	}
	
	@Test
	public void testUnionVariablesLexicographically() {
		Monomial m1 = makeMonomial("0");
		Monomial m2 = makeMonomial("0");
		Assert.assertEquals(Collections.emptyList(), Monomial.unionVariablesLexicographically(m1, m2));
		
		m1 = makeMonomial("x");
		m2 = makeMonomial("x");
		Assert.assertEquals(Expressions.parse("tuple(x)").getArguments(), Monomial.unionVariablesLexicographically(m1, m2));
		
		m1 = makeMonomial("x^2*y^3");
		m2 = makeMonomial("y^2*x^3");
		Assert.assertEquals(Expressions.parse("tuple(x, y)").getArguments(), Monomial.unionVariablesLexicographically(m1, m2));
		
		m1 = makeMonomial("x^2*y^3");
		m2 = makeMonomial("z^4*y^2*x^3");
		Assert.assertEquals(Expressions.parse("tuple(x, y, z)").getArguments(), Monomial.unionVariablesLexicographically(m1, m2));

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
	
	@Test
	public void testCompareTo() {
// TODO		
	}
	
	//
	// Additional Test
	@Test
	public void testEquals() {
		
	}	
	
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
