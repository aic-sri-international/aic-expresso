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
package com.sri.ai.test.grinder.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

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
import com.sri.ai.grinder.plaindpll.problemtype.ModelCounting;
import com.sri.ai.grinder.plaindpll.problemtype.Sum;
import com.sri.ai.grinder.plaindpll.theory.EqualityConstraintTheory;
import com.sri.ai.grinder.plaindpll.theory.term.FunctionalTermTheory;
import com.sri.ai.grinder.plaindpll.theory.term.SymbolTermTheory;
import com.sri.ai.util.Util;

@Beta
/**
 * Tests the avoidance of unnecessary conditions by using type size.
 * For example, if Friends = {bob, mary},
 * then the solution to
 * <code>sum_empty_indices if F = bob then 1 else if F = mary then 2 else 3</code>
 * should be
 * <code>if F = bob then 1 else 2</code>
 * since the fact that <code>F != bob</code>, together with <code>| Friends | = 2</code>,
 * already implies <code>F = mary</code>.
 * @author braz
 *
 */
public class ContradictionByTypeSizeTest extends AbstractSymbolicSymbolEqualityDPLLTest {
	
	@Override
	protected Expression makeProblem(Expression expression, IndexExpressionsSet indexExpressions) {
		Expression set = new DefaultIntensionalMultiSet(indexExpressions, Expressions.ONE, expression);
		Expression problem = apply(FunctorConstants.CARDINALITY, set);
		return problem;
	}

	@Override
	protected Rewriter makeRewriter() {
		return new SGDPLLT(new EqualityConstraintTheory(new FunctionalTermTheory()), new ModelCounting());
	}

	@Test
	public void test() {
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		Expression expression;
		Expression expected;
		Expression result;
		
		SGDPLLT solver = new SGDPLLT(new EqualityConstraintTheory(new SymbolTermTheory()), new Sum());
		
		expression = parse("if F = bob then 1 else 2");
		expected   = parse("1");
		result = solver.solve(expression, Util.list(), Util.map("F", "Friends"), Util.map("Friends", "1"));
		assertEquals(expected, result);

		expression = parse("if F = bob then 1 else if F = mary then 2 else 3");
		expected   = parse("if F = bob then 1 else 2");
		result = solver.solve(expression, Util.list(), Util.map("F", "Friends"), Util.map("Friends", "2"));
		assertEquals(expected, result);
		
		expression = parse("if F = bob then 1 else if F = mary then 2 else if F = john then 3 else if F = paul then 4 else 5");
		expected   = parse("if F = bob then 1 else 2");
		result = solver.solve(expression, Util.list(), Util.map("F", "Friends"), Util.map("Friends", "2"));
		assertEquals(expected, result);
		
		expression = parse("if F = bob then 1 else if F = mary then 2 else 3");
		expected   = expression;
		result = solver.solve(expression, Util.list(), Util.map("F", "Friends"), Util.map("Friends", "5"));
		assertEquals(expected, result);
	}
}
