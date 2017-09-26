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
package com.sri.ai.test.grinder.sgdpllt.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic.LinearRealArithmeticTheory;

@Beta
public class BasicTest {

	@Test
	public void isLiteralTests() {
		Expression expression;
		boolean expected;
		Context context = new TrueContext();
		context = context.makeCloneWithAdditionalRegisteredSymbolsAndTypes(
				map(
						parse("X"), parse("Real"),
						parse("Y"), parse("Real"),
						parse("Z"), parse("Real"),
						parse("W"), parse("Integer")
						));

		expression = parse("1 >= 0");
		expected = true;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("X >= 0");
		expected = true;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("-X >= 0");
		expected = true;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("X - Y >= 0");
		expected = true;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("X + Y >= 0");
		expected = true;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("X - Y + 3 >= 0");
		expected = true;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("X - Y + 3 + Z >= Z");
		expected = true;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("X - X + X - Y + Y - Z + 1 - 5 >= 0");
		expected = true;
		runIsLiteralTest(expression, expected, context);

	


		// Must be linear
		expression = parse("X^2 >= 0");
		expected = false;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("X*Y >= 0");
		expected = false;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("X*X - Y >= 0");
		expected = false;
		runIsLiteralTest(expression, expected, context);
		
		expression = parse("2*X -3*Y + 3 >= 0");
		expected = true;
		runIsLiteralTest(expression, expected, context);

	

		
		// Variables must be real
		expression = parse("2*X - 2*X + X - 2*W + 2*W - 2*Z + 1 - 5 >= -Z");
		expected = true;
		runIsLiteralTest(expression, expected, context);

		expression = parse("W - X >= 0");
		expected = false;
		runIsLiteralTest(expression, expected, context);
		
		
		// Sides should be polynomials
		expression = parse("f()*2 > 0");
		expected = false;
		runIsLiteralTest(expression, expected, context);
	}

	private void runIsLiteralTest(Expression expression, boolean expected, Context context) {
		LinearRealArithmeticTheory theory = new LinearRealArithmeticTheory(false, true);
		boolean isLiteral = theory.isLiteral(expression, context);
		assertEquals(expected, isLiteral);
	}
}