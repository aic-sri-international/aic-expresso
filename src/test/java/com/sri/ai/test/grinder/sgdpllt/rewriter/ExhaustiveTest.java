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

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;

import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver.Solution;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.core.constraint.ContextSplitting;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.RewriterFromStepMaker;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;


public class ExhaustiveTest {

	@Test
	public void testSimpleExhaustiveRewriter() {
		RewriterFromStepMaker rewriter = 
				(Expression e, Context c) -> {
					if (Expressions.isNumber(e) && e.intValue() < 10) {
						return new Solution(DefaultSymbol.createSymbol(e.intValue() + 1));
					}
					return new Solution(e);
				};
		Expression initial = parse("1");
		Expression expected = parse("10");

		runTest(rewriter, initial, expected, map());
	}

	@Test
	public void testSimpleExhaustiveConditionalRewriter() {
		class FunkyStepSolver implements ExpressionLiteralSplitterStepSolver {
			private Expression expression;
			public FunkyStepSolver(Expression expression) {
				this.expression = expression;
			}
			@Override
			public FunkyStepSolver clone() {
				FunkyStepSolver result = null;
				try {
					result = (FunkyStepSolver) super.clone();
				} catch (CloneNotSupportedException e) {
					e.printStackTrace();
				}
				return result;
			}

			@Override
			public Step step(Context context) {
				if (Expressions.isNumber(expression) && expression.intValue() % 10 != 0) {
					if (expression.intValue() == 5) {
						Expression literal = parse("JumpAt5");
						ContextSplitting splitting = new ContextSplitting(literal, context);
						switch (splitting.getResult()) {
						case LITERAL_IS_TRUE:
							return new Solution(parse("11"));
						case LITERAL_IS_FALSE:
							return new Solution(parse("6"));
						case LITERAL_IS_UNDEFINED:
							return new ItDependsOn(literal, splitting, this, this);
						default:
							throw new Error("Unpredicted case");
						}
					}
					else {
						return new Solution(DefaultSymbol.createSymbol(expression.intValue() + 1));
					}
				}
				else return new Solution(expression);
			}
		};
		
		Rewriter rewriter = (Expression e) -> new FunkyStepSolver(e);
		Expression initial = parse("1");
		Expression expected = parse("if JumpAt5 then 20 else 10");

		runTest(rewriter, initial, expected, map(parse("JumpAt5"), parse("Boolean")));
	}

	private void runTest(Rewriter rewriter, Expression initial, Expression expected, Map<Expression, Expression> symbolsAndTypes) {
		CompoundTheory theory = new CompoundTheory(new PropositionalTheory(), new DifferenceArithmeticTheory(false, true));
		Context context = new TrueContext(theory);
		context = context.makeNewRegistryWithRegisteredAdditionalSymbolsAndTypes(symbolsAndTypes);
		Rewriter exhaustive = new Exhaustive(rewriter);
		Expression solution = exhaustive.apply(initial, context);
		System.out.println("Solution: " + solution);	
		assertEquals(expected, solution);
	}
}
