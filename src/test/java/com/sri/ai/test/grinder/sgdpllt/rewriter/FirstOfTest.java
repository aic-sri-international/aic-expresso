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

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver.Solution;
import com.sri.ai.grinder.core.TrueContext;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.rewriter.api.Rewriter;
import com.sri.ai.grinder.rewriter.api.RewriterFromStepMaker;
import com.sri.ai.grinder.rewriter.core.FirstOf;
import com.sri.ai.grinder.theory.compound.CompoundTheory;
import com.sri.ai.grinder.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.theory.propositional.PropositionalTheory;
import com.sri.ai.util.Util;


public class FirstOfTest {

	@Test
	public void testSimpleFirstOfRewriter() {
		List<RewriterFromStepMaker> rewriters = Util.<RewriterFromStepMaker>list(
				
				(Expression e, Context c) -> {
					if (Expressions.isNumber(e) && e.intValue() == 10) {
						return new Solution(DefaultSymbol.createSymbol(e.intValue() - 1));
					}
					return new Solution(e);
				}
				,				
				(Expression e, Context c) -> {
					if (Expressions.isNumber(e) && e.intValue() == 9) {
						return new Solution(DefaultSymbol.createSymbol(e.intValue() - 1));
					}
					return new Solution(e);
				}
				,				
				(Expression e, Context c) -> {
					if (Expressions.isNumber(e) && e.intValue() == 8) {
						return new Solution(DefaultSymbol.createSymbol(e.intValue() - 1));
					}
					return new Solution(e);
				}
				,				
				(Expression e, Context c) -> {
					if (Expressions.isNumber(e) && e.intValue() == 7) {
						return new Solution(DefaultSymbol.createSymbol(e.intValue() - 1));
					}
					return new Solution(e);
				}
				,				
				(Expression e, Context c) -> {
					if (Expressions.isNumber(e) && e.intValue() == 6) {
						return new Solution(DefaultSymbol.createSymbol(e.intValue() - 1));
					}
					return new Solution(e);
				}
				);

		Expression initial = parse("8");
		Expression expected = parse("7");
		runTest(new LinkedList<Rewriter>(rewriters), initial, expected, map());

		initial = parse("7");
		expected = parse("6");
		runTest(new LinkedList<Rewriter>(rewriters), initial, expected, map());
	}

	@Test
	public void testSimpleFirstOfConditionalRewriter() {
		
		class JumperAtStepSolver implements ExpressionLiteralSplitterStepSolver {
			
			private Expression expression;
			private int jumpPoint;
			
			public JumperAtStepSolver(Expression expression, int jumpPoint) {
				this.expression = expression;
				this.jumpPoint = jumpPoint;
			}
			
			@Override
			public JumperAtStepSolver clone() {
				JumperAtStepSolver result = null;
				try {
					result = (JumperAtStepSolver) super.clone();
				} catch (CloneNotSupportedException e) {
					e.printStackTrace();
				}
				return result;
			}

			@Override
			public Step step(Context context) {
				if (Expressions.isNumber(expression) && expression.intValue() % 10 != 0) {
					if (expression.intValue() == jumpPoint) {
						Expression literal = parse("Jump5");
						ContextSplitting splitting = new ContextSplitting(literal, context);
						switch (splitting.getResult()) {
						case LITERAL_IS_TRUE:
							return new Solution(DefaultSymbol.createSymbol(jumpPoint + 5));
						case LITERAL_IS_FALSE:
							return new Solution(DefaultSymbol.createSymbol(jumpPoint + 1));
						case LITERAL_IS_UNDEFINED:
							return new ItDependsOn(literal, splitting, this, this);
						default:
							throw new Error("Unpredicted case");
						}
					}
					else {
						return new Solution(expression);
					}
				}
				else return new Solution(expression);
			}
		};
		
		List<Rewriter> rewriters = Util.<Rewriter>list(
				(Expression e) -> new JumperAtStepSolver(e, 5)
				,
				(Expression e) -> new JumperAtStepSolver(e, 8)
				);
		
		Expression initial;
		Expression expected;

		initial = parse("1");
		expected = parse("1"); // no jumps at 1
		runTest(rewriters, initial, expected, map(parse("Jump5"), parse("Boolean")));
		
		initial = parse("5");
		expected = parse("if Jump5 then 10 else 6");
		runTest(rewriters, initial, expected, map(parse("Jump5"), parse("Boolean")));
		
		initial = parse("8");
		expected = parse("if Jump5 then 13 else 9");
		runTest(rewriters, initial, expected, map(parse("Jump5"), parse("Boolean")));
	}

	private void runTest(List<Rewriter> rewriters, Expression initial, Expression expected, Map<Expression, Expression> symbolsAndTypes) {
		CompoundTheory theory = new CompoundTheory(new PropositionalTheory(), new DifferenceArithmeticTheory(false, true));
		Context context = new TrueContext(theory);
		context = context.makeCloneWithAdditionalRegisteredSymbolsAndTypes(symbolsAndTypes);
		Rewriter firstOf = new FirstOf(rewriters);
		Expression solution = firstOf.apply(initial, context);
		System.out.println("Solution: " + solution);	
		assertEquals(expected, solution);
	}
}
