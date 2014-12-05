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
import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.ModelCounting;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.PropositionalTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.DPLLGeneralizedAndSymbolic;
import com.sri.ai.util.Util;

@Beta
public class PropositionalLogicModelCountingDPLLTest {
	
	protected Expression makeProblem(Expression expression, List<Expression> indexExpressions) {
		Expression set = new DefaultIntensionalMultiSet(indexExpressions, Expressions.ONE, expression);
		Expression problem = apply(FunctorConstants.CARDINALITY, set);
		return problem;
	}

	protected Rewriter makeRewriter() {
		return new DPLLGeneralizedAndSymbolic(new PropositionalTheory(), new ModelCounting());
	}

	@Test
	public void test() {
		
		Expression expression;
		Expression expected;
		List<String> indices;
		
		GrinderUtil.setMinimumOutputForProfiling();
		
		// DEBUG
		expression = parse("p or q");
		indices    = list();
		expected   = parse("if p then 1 else if q then 1 else 0");
		test(expression, expected, indices);

		expression = parse("true");
		indices    = list("p");
		expected   = parse("2");
		test(expression, expected, indices);
		
		expression = parse("false");
		indices    = list("p");
		expected   = parse("0");
		test(expression, expected, indices);
		
		expression = parse("p");
		indices    = list("p");
		expected   = parse("1");
		test(expression, expected, indices);
		
		expression = parse("true");
		indices    = list("p", "q");
		expected   = parse("4");
		test(expression, expected, indices);
		
		expression = parse("p and q");
		indices    = list("p");
		expected   = parse("if q then 1 else 0");
		test(expression, expected, indices);
		
		expression = parse("p or q");
		indices    = list();
		expected   = parse("if p then 1 else if q then 1 else 0");
		test(expression, expected, indices);
		
		expression = parse("p or q");
		indices    = list("p");
		expected   = parse("if q then 2 else 1");
		test(expression, expected, indices);
		
		expression = parse("p");
		indices    = list("p", "q");
		expected   = parse("2");
		test(expression, expected, indices);
		
		expression = parse("p or not p");
		indices    = list("p", "q");
		expected   = parse("4");
		test(expression, expected, indices);
		
		expression = parse("p and q and r or not p and q and not r");
		indices    = list("p");
		expected   = parse("if q then 1 else 0");
		test(expression, expected, indices);
		
		expression = parse("(p and r) or (not p and r)"); // this will involve the symbolic combination of two conditional (in r) solutions
		indices    = list("p");
		expected   = parse("if r then 2 else 0");
		test(expression, expected, indices);

		expression = parse("r"); // test early externalization on r and that the model count (conditional on r from the constraint) will be simplified by the context.
		indices    = list("p");
		expected   = parse("if r then 2 else 0");
		test(expression, expected, indices);
	}

	protected void test(Expression expression, Expression expected, List<String> indicesStrings) {
		Rewriter rewriter = makeRewriter();
		List<Expression> indexExpressions = Util.mapIntoList(indicesStrings, string -> Expressions.makeSymbol(string));
		Expression actual = rewriter.rewrite(makeProblem(expression, indexExpressions), new DefaultRewritingProcess(rewriter));
		assertEquals(expected, actual);
	}
}
