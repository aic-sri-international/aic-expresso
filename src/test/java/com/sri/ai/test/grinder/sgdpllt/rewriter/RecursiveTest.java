/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.test.grinder.sgdpllt.rewriter;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;

import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver.ItDependsOn;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver.Solution;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.rewriter.api.Rewriter;
import com.sri.ai.grinder.rewriter.api.RewriterFromStepMaker;
import com.sri.ai.grinder.rewriter.core.Recursive;
import com.sri.ai.grinder.theory.base.ConstantExpressionStepSolver;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;


public class RecursiveTest {

	@Test
	public void testSimpleRecursiveRewriter() {
		RewriterFromStepMaker rewriter = 
				(Expression e, Context c) -> {
					if (Expressions.isNumber(e)) {
						return new Solution(DefaultSymbol.createSymbol(e.intValue() + 1));
					}
					return new Solution(e);
				};

				Expression initial;
				Expression expected;

				initial = parse("7");
				expected = parse("8");
				runTest(rewriter, initial, expected, map());

				initial = parse("f(9,g(8,7,6))");
				expected = parse("f(10,g(9,8,7))");
				runTest(rewriter, initial, expected, map());

				initial = parse("(6)(9,g(8,7,6))");
				expected = parse("(7)(10,g(9,8,7))");
				runTest(rewriter, initial, expected, map());
	}

	@Test
	public void testConditionalRecursiveRewriter() {
		Expression xIs0 = parse("XIs0");
		RewriterFromStepMaker rewriter = 
				(Expression e, Context c) -> {
					if (Expressions.isNumber(e)) {
						return new Solution(DefaultSymbol.createSymbol(e.intValue() + 1));
					}
					else if (e.equals(parse("X"))) {
						ContextSplitting splitting = new ContextSplitting(xIs0, c);
						switch (splitting.getResult()) {
						case LITERAL_IS_TRUE:
							return new Solution(ZERO);
						case LITERAL_IS_FALSE:
							return new Solution(ONE);
						case LITERAL_IS_UNDEFINED:
							return new ItDependsOn(
									xIs0, 
									splitting, 
									new ConstantExpressionStepSolver(ZERO), 
									new ConstantExpressionStepSolver(ONE));
						default:
							throw new Error("Unpredicted case.");
						}
					}
					return new Solution(e);
				};

				Expression initial;
				Expression expected;

				initial = parse("X");
				expected = parse("if " + xIs0 + " then 0 else 1");
				runTest(rewriter, initial, expected, map(xIs0, parse("Boolean")));

				initial = parse("f(9,g(X,7,6))");
				expected = parse("if " + xIs0 + " then f(10,g(0,8,7)) else f(10,g(1,8,7))");
				runTest(rewriter, initial, expected, map(xIs0, parse("Boolean")));

				initial = parse("X(9,g(h(1,2,X),X,7,6))");
				expected = parse("if " + xIs0 + " then 0(10,g(h(2,3,0),0,8,7)) else 1(10,g(h(2,3,1),1,8,7))");
				runTest(rewriter, initial, expected, map(xIs0, parse("Boolean")));
	}

	private void runTest(Rewriter rewriter, Expression initial, Expression expected, Map<Expression, Expression> symbolsAndTypes) {
		CompoundTheory theory = new CompoundTheory(new PropositionalTheory(), new DifferenceArithmeticTheory(false, true));
		Context context = new TrueContext(theory);
		context = context.makeCloneWithAdditionalRegisteredSymbolsAndTypes(symbolsAndTypes);
		Rewriter recursive = new Recursive(rewriter);
		Expression solution = recursive.apply(initial, context);
		System.out.println("Solution: " + solution);	
		assertEquals(expected, solution);
	}
}
