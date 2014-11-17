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
package com.sri.ai.test.grinder.library.equality.cardinality.plaindpll;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.SymbolEqualityTheory;

@Beta
public class SimplifyFormulaTest {

	@Test
	public void test() {
		Expression expression;
		Expression expected;
		
		expression = Expressions.parse("Y != a and (there exists X : X = Y and Y = Z)");
		expected   = Expressions.parse("Y != a and Y = Z");
		runTest(expression, expected);
		
		expression = Expressions.parse("Y != a and (for all X : (X != Y or X = Y) and Y = Z)");
		expected   = Expressions.parse("Y != a and Y = Z");
		runTest(expression, expected);
		
//		expression = Expressions.parse("Y != a and (for all X : (X != Y or X = Y) and foo(Y))");
//		expected   = Expressions.parse("Y != a and foo(Y)");
//		runTest(expression, expected);
		
		expression = Expressions.parse("p or true or p");
		expected   = Expressions.parse("true");
		runTest(expression, expected);
		
		expression = Expressions.parse("p and false and p");
		expected   = Expressions.parse("false");
		runTest(expression, expected);
		
		expression = Expressions.parse("p and p and p");
		expected   = Expressions.parse("p");
		runTest(expression, expected);
		
		expression = Expressions.parse("p or p or p");
		expected   = Expressions.parse("p");
		runTest(expression, expected);
		
		expression = Expressions.parse("false or false or false");
		expected   = Expressions.parse("false");
		runTest(expression, expected);
		
		expression = Expressions.parse("true and true and true");
		expected   = Expressions.parse("true");
		runTest(expression, expected);
		
		expression = Expressions.parse("not false");
		expected   = Expressions.parse("true");
		runTest(expression, expected);
		
		expression = Expressions.parse("not not p");
		expected   = Expressions.parse("p");
		runTest(expression, expected);
		
		expression = Expressions.parse("not not (p or true)");
		expected   = Expressions.parse("true");
		runTest(expression, expected);
		
		expression = Expressions.parse("a = b");
		expected   = Expressions.parse("false");
		runTest(expression, expected);
		
		expression = Expressions.parse("a != b");
		expected   = Expressions.parse("true");
		runTest(expression, expected);
		
		expression = Expressions.parse("a = a");
		expected   = Expressions.parse("true");
		runTest(expression, expected);
		
		expression = Expressions.parse("a != a");
		expected   = Expressions.parse("false");
		runTest(expression, expected);
		
		expression = Expressions.parse("X = X");
		expected   = Expressions.parse("true");
		runTest(expression, expected);
		
		expression = Expressions.parse("X != X");
		expected   = Expressions.parse("false");
		runTest(expression, expected);
		
		expression = Expressions.parse("X = Y");
		expected   = Expressions.parse("X = Y");
		runTest(expression, expected);
		
		expression = Expressions.parse("a = b = a");
		expected   = Expressions.parse("false");
		runTest(expression, expected);
		
		expression = Expressions.parse("a = a = a");
		expected   = Expressions.parse("true");
		runTest(expression, expected);
	}

	private void runTest(Expression expression, Expression expected) {
		Expression actual;
		actual = (new SymbolEqualityTheory()).simplify(expression, new DefaultRewritingProcess(expression, null));
		Assert.assertEquals(expected, actual);
	}

}
