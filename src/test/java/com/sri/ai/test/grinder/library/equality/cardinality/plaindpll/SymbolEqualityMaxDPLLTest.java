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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;

import java.util.Collection;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
import com.sri.ai.grinder.plaindpll.problemtype.Max;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory;
import com.sri.ai.grinder.plaindpll.theory.term.SymbolTermTheory;

@Beta
public class SymbolEqualityMaxDPLLTest extends AbstractSymbolicSymbolEqualityDPLLTest {
	
	@Override
	protected Expression makeProblem(Expression expression, IndexExpressionsSet indexExpressions) {
		Expression set = new DefaultIntensionalMultiSet(indexExpressions, expression, Expressions.TRUE);
		Expression problem = apply(FunctorConstants.MAX, set);
		return problem;
	}

	@Override
	protected Rewriter makeRewriter() {
		return new SGDPLLT(new EqualityTheory(new SymbolTermTheory()), new Max());
	}

	@Test
	public void test() {
		
		Expression expression;
		Expression expected;
		Collection<String> indices;
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		// Repeated for debugging purposes:
//		expression = parse("X != b and X = Y");
//		indices    = list("X");
//		expected   = parse("if Y = b then 0 else 1");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		
		// this example tests whether conditioning an index to a value considers previous disequalities on that index,
		// because X is split on b first, and then the algorithm attempts to condition on X = Y, but that requires Y to be != b.
		expression = parse("if X != b and X = Y then 2 else 3");
		indices    = list("X");
		expected   = parse("3");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		// tests elimination for quantified sub-expressions
		expression = parse("if for all Y : X = Y then 2 else 3");
		indices    = list("X");
		expected   = parse("if | type(Y) | - 1 = 0 then 2 else 3");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		// tests infinity
		expression = parse("if X = a then infinity else -infinity");
		indices    = list("X");
		expected   = parse("infinity");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		// tests infinity
		expression = parse("if X = a then if X != a then 2 else -infinity else -infinity");
		indices    = list("X");
		expected   = parse("-infinity");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
	}
}
