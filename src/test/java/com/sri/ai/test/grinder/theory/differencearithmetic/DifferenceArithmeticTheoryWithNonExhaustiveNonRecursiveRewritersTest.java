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
package com.sri.ai.test.grinder.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters;
import com.sri.ai.util.explanation.logging.api.ExplanationConfiguration;
import com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger;

@Beta
public class DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewritersTest {

	@Test
	public void basicTest() {

		ExplanationConfiguration.WHETHER_EXPLANATION_LOGGERS_ARE_ACTIVE_BY_DEFAULT = true;

		ThreadExplanationLogger.explanationBlockToFile("explanations.txt", code( () -> {

			Expression expression;
			Expression actual;
			Expression expected;
			Theory theory =
					new DifferenceArithmeticTheoryWithNonExhaustiveNonRecursiveRewriters(true, true);
			Context context = new TrueContext(theory);
			context = context.makeCloneWithAdditionalRegisteredSymbolsAndTypes(
					map(
							parse("X"), parse("Integer")
							));

			//		expression = parse("X = 0");
			//		expected = parse("X = 0");
			//		actual = context.evaluate(expression);
			//		assertEquals(expected, actual);
			//		
			//		expression = parse("if X = 0 then X != 0 else true");
			//		expected = parse("if X = 0 then false else true");
			//		actual = context.evaluate(expression);
			//		assertEquals(expected, actual);

			expression = parse("sum({{ (on I in 1..2) I + 1 }})");
			expected = parse("5");
			actual = context.evaluate(expression);
			assertEquals(expected, actual);

		}));

		ExplanationConfiguration.WHETHER_EXPLANATION_LOGGERS_ARE_ACTIVE_BY_DEFAULT = false;

	}

}