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

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static org.junit.Assert.assertEquals;

import java.util.Collection;
import java.util.List;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.plaindpll.core.SGDPLLTParallelizer;
import com.sri.ai.grinder.plaindpll.problemtype.ModelCounting;
import com.sri.ai.grinder.plaindpll.theory.EqualityConstraintTheory;
import com.sri.ai.grinder.plaindpll.theory.term.SymbolTermTheory;

@Beta
public class SGDPLLTParallelizerTest {
	
	@Test
	public void test() {
		
		Expression expression;
		List<String> expected;
		Collection<String> indices;
		int level;
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		level = 3;
		expression = parse("X != a and Y != b");
		indices = list("X");
		expected = list("false, [X], X = a", "Y != b, [X], X != a");
		runTest(expression, expected, indices, level);	
		
		level = 3;
		expression = parse("X != a and X != b and X != c and Y != b");
		indices = list("X");
		expected = list(
				"false, [X], X = a",
				"false, [X], X = b",
				"false, [X], X = c",
				"Y != b, [X], (X != a) and (X != b) and (X != c)");
		runTest(expression, expected, indices, level);	
		
		level = 3;
		expression = parse("X != a and Y != b");
		indices = list("X", "Y");
		expected = list(
				"false, [X, Y], X = a",
				"false, [X, Y], (Y = b) and (X != a)",
				"true, [X, Y], (X != a) and (Y != b)");
		runTest(expression, expected, indices, level);	
		
		level = 3;
		expression = parse("X != a and X != b and X != c and Y != b");
		indices = list("X", "Y");
		expected = list(
				"false, [X, Y], X = a",
				"false, [X, Y], X = b",
				"false, [X, Y], X = c",
				"Y != b, [X, Y], (X != a) and (X != b) and (X != c)"); // does not split on Y because depth 3 has been reached
		runTest(expression, expected, indices, level);	
		
		level = 10;
		expression = parse("X != a and X != b and X != c and Y != b");
		indices = list("X", "Y");
		expected = list(
				"false, [X, Y], X = a",
				"false, [X, Y], X = b",
				"false, [X, Y], X = c",
				"false, [X, Y], (Y = b) and (X != a) and (X != b) and (X != c)",
				"true, [X, Y], (X != a) and (X != b) and (X != c) and (Y != b)"); // does split on Y because depth 10 has not been reached
		runTest(expression, expected, indices, level);	

		level = 10;
		expression = parse("X != a and X != b and X != c and Y != b");
		indices = list("Y"); // now X is not an index; X != a is first splitter candidate but is skipped even though regular algorithm would take it. 
		expected = list(
				"false, [Y], Y = b",
				"(X != a) and (X != b) and (X != c), [Y], Y != b");
		runTest(expression, expected, indices, level);	
}

	protected void runTest(Expression expression, List<String> expected, Collection<String> indices, int level) {
		List<String> collected = list();
		SGDPLLTParallelizer.Collector collector = (e, i, c, p) -> collected.add(e + ", " + i + ", " + c);
		SGDPLLTParallelizer parallelizer = new SGDPLLTParallelizer(new EqualityConstraintTheory(new SymbolTermTheory()), new ModelCounting(), collector, level);
		List<Expression> indexExpressions = mapIntoList(indices, Expressions::parse);
		parallelizer.solve(expression, indexExpressions, new DefaultRewritingProcess(null));
		// System.out.println("Collected:\n" + join(collected, "\n"));
		assertEquals(expected, collected);
	}
}
