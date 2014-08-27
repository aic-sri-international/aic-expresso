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
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.ExternalizeSimpleExpressionsWithPairwiseEqualities;

@Beta
public class ExternalizeSimpleExpressionsWithPairwiseEqualitiesTest {

	@Test
	public void test() {
		Expression expression;
		Expression expected;
		
		GrinderUtil.setMinimumOutputForProfiling();
		
		expression = Expressions.parse("if X6 = a2 then 100000 else 8888 + (if X6 = a1 then 90000 else (((if X6 = a6 then 0 else 90) + 810) + 8100))");
		expected   = Expressions.parse("if X6 = a2 then 100000 else (if X6 = a1 then 8888 + 90000 else (if X6 = a6 then 8888 + ((0 + 810) + 8100) else 8888 + ((90 + 810) + 8100)))");
		runTest(expression, expected);
		
		expression = Expressions.parse("true");
		expected   = Expressions.parse("true");
		runTest(expression, expected);
		
		expression = Expressions.parse("false");
		expected   = Expressions.parse("false");
		runTest(expression, expected);
		
		expression = Expressions.parse("1 + 2");
		expected   = Expressions.parse("1 + 2");
		runTest(expression, expected);
		
		expression = Expressions.parse("1 + if X = a then 2 else 3");
		expected   = Expressions.parse("if X = a then 1 + 2 else 1 + 3");
		runTest(expression, expected);
		
		expression = Expressions.parse("(if X = a then 2 else 3) + 1");
		expected   = Expressions.parse("if X = a then 2 + 1 else 3 + 1");
		runTest(expression, expected);
		
		expression = Expressions.parse("if Z = b then (if X = a then 2 else 3) + 1 else 0");
		expected   = Expressions.parse("if Z = b then if X = a then 2 + 1 else 3 + 1 else 0");
		runTest(expression, expected);
		
		expression = Expressions.parse("if Z = b then (if X = a then 2 else 3) + (if X = a then 4 else 5) else 0");
		expected   = Expressions.parse("if Z = b then if X = a then 2 + 4 else 3 + 5 else 0");
		runTest(expression, expected);
		
		expression = Expressions.parse("if Z = b then (if X = a then 2 else 3) + (if X = b then 4 else 5) else 0");
		expected   = Expressions.parse("if Z = b then if X = a then 2 + 5 else (if X = b then 3 + 4 else 3 + 5) else 0");
		runTest(expression, expected);
	}

	private void runTest(Expression expression, Expression expected) {
		DefaultRewritingProcess process = new DefaultRewritingProcess(expression, null);
		Expression actual = ExternalizeSimpleExpressionsWithPairwiseEqualities.externalize(expression, process);
		Assert.assertEquals(expected, actual);
	}
}
