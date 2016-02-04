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
package com.sri.ai.test.grinder.helper;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.helper.MonomialComparator;
import com.sri.ai.grinder.polynomial.api.Monomial;
import com.sri.ai.grinder.polynomial.core.DefaultMonomial;

public class MonomialComparatorTest {
	private MonomialComparator comparator = new MonomialComparator();
	
	@Test
	public void testCompareDefaultConstructor() {
		Assert.assertEquals(-1, comparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^1")));
		Assert.assertEquals(1, comparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		
		Assert.assertEquals(0, comparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, comparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
			
		Assert.assertEquals(1, comparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^6")));
		Assert.assertEquals(-1, comparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
	}
	
	@Test
	public void testCompareSignatureFactorConstructor() {
		MonomialComparator signatureFactorsComparator;
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(1)").getArguments());	
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(2)").getArguments());	
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(x)").getArguments());	
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(y)").getArguments());	
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(w)").getArguments());	
		Assert.assertEquals(1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(z)").getArguments());	
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(x, y)").getArguments());	
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(w, x)").getArguments());	
		Assert.assertEquals(1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(x, w)").getArguments());	
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
		
		signatureFactorsComparator = new MonomialComparator(Expressions.parse("tuple(x, z)").getArguments());	
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("w^5")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("0"), makeMonomial("7")));
		Assert.assertEquals(0, signatureFactorsComparator.compare(makeMonomial("2*x^2*y^3"), makeMonomial("4*x^2*y^3")));
		Assert.assertEquals(-1, signatureFactorsComparator.compare(makeMonomial("x^2*y^3"), makeMonomial("z^5")));
	}

	//
	// PRIVATE
	//
	private static Monomial makeMonomial(String monomial) {
		Monomial result = DefaultMonomial.make(Expressions.parse(monomial));
		return result;
	}
}
