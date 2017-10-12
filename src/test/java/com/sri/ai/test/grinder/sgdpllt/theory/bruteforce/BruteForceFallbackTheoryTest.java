/*
 * Copyright (c) 2013, SRI International
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
 * Neither the name of the aic-expresso nor the names of its
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
package com.sri.ai.test.grinder.sgdpllt.theory.bruteforce;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.println;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.theory.bruteforce.BruteForceFallbackTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;

@Beta
public class BruteForceFallbackTheoryTest {

	@Test
	public void test() {
		String expressionString;
		String expectedString;
		
		expressionString = "sum({{ (on X in 1..30000) X }})"; // does not need fallback
		expectedString = "450015000";
		runTest(expressionString, expectedString);
		
		expressionString = "sum({{ (on X in 1..3) 2^X }})";
		expectedString = "14";
		runTest(expressionString, expectedString);

		expressionString = "sum({{ (on X in 1..6) Y/X }})";
		expectedString = "2.45 * Y";
		runTest(expressionString, expectedString, "Y", "1..4");

		expressionString = "sum({{ (on X in 1..6) f(X) }})";
		expectedString = "f(1) + f(2) + f(3) + f(4) + f(5) + f(6)";
		runTest(expressionString, expectedString, "f", "1..6 -> 0..2");
	}

	private static void runTest(String expressionString, String expectedString, String... symbolsAndTypes) {
		Theory theory = new BruteForceFallbackTheory(new DifferenceArithmeticTheory(false, true));
		Context context = new TrueContext(theory);
		context = context.extendWithSymbolsAndTypes(symbolsAndTypes);
		Expression expression = parse(expressionString);
		Expression expected = parse(expectedString);
		Expression actual = theory.evaluate(expression, context);
		println(actual);
		assertEquals(expected, actual);
	}
}