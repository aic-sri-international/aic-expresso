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
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver.Solution;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.core.constraint.ContextSplitting;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.RewriterFromStepMaker;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Switch;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.DifferenceArithmeticTheory;
import com.sri.ai.grinder.sgdpllt.theory.propositional.PropositionalTheory;


public class SwitchTest {

	@Test
	public void testSimpleSwitchRewriter() {
		RewriterFromStepMaker baseRewriterSum = 
				(Expression e, Context c) -> {
					int argument1 = e.get(0).intValue();
					int argument2 = e.get(1).intValue();
					Symbol resultValue = DefaultSymbol.createSymbol(argument1 + argument2);
					return new Solution(resultValue);
				};

		RewriterFromStepMaker baseRewriterSubtraction = 
				(Expression e, Context c) -> {
					int argument1 = e.get(0).intValue();
					int argument2 = e.get(1).intValue();
					Symbol resultValue = DefaultSymbol.createSymbol(argument1 - argument2);
					return new Solution(resultValue);
				};

		Switch rewriter = 
				new Switch<String>(
						e -> e.getFunctor() != null? e.getFunctor().toString() : "", 
						map(
								"+", baseRewriterSum, 
								"-", baseRewriterSubtraction));

		Expression initial;
		Expression expected;

		initial = parse("7");
		expected = parse("7");
		runTest(rewriter, initial, expected, map());

		initial = parse("10 - 7");
		expected = parse("3");
		runTest(rewriter, initial, expected, map());

		initial = parse("10 + 3");
		expected = parse("13");
		runTest(rewriter, initial, expected, map());
	}

	private static Expression inverted = parse("Inverted");

	private static class SumStepSolver implements ExpressionLiteralSplitterStepSolver {

		private Expression expression;
		
		public SumStepSolver(Expression expression) {
			super();
			this.expression = expression;
		}

		@Override
		public SubtractionStepSolver clone() {
			SubtractionStepSolver result = null;
			try {
				result = (SubtractionStepSolver) super.clone();
			} catch (CloneNotSupportedException e) {
				e.printStackTrace();
			}
			return result;
		}

		@Override
		public Step step(Context context) {
			int argument1 = expression.get(0).intValue();
			int argument2 = expression.get(1).intValue();
			ContextSplitting splitting = new ContextSplitting(inverted, context);
			Step result;
			switch (splitting.getResult()) {
			case LITERAL_IS_FALSE:
				result = new Solution(DefaultSymbol.createSymbol(argument1 + argument2));
				break;
			case LITERAL_IS_TRUE:
				result = new Solution(DefaultSymbol.createSymbol(argument1 - argument2));
				break;
			case LITERAL_IS_UNDEFINED:
				result =  new ItDependsOn(inverted, splitting, this, this);
				break;
				default:
					throw new Error("Unpredicated case.");
			}
			return result;
		}
	};

	private static class SubtractionStepSolver implements ExpressionLiteralSplitterStepSolver {

		private Expression expression;
		
		public SubtractionStepSolver(Expression expression) {
			super();
			this.expression = expression;
		}

		@Override
		public SubtractionStepSolver clone() {
			SubtractionStepSolver result = null;
			try {
				result = (SubtractionStepSolver) super.clone();
			} catch (CloneNotSupportedException e) {
				e.printStackTrace();
			}
			return result;
		}

		@Override
		public Step step(Context context) {
			int argument1 = expression.get(0).intValue();
			int argument2 = expression.get(1).intValue();
			ContextSplitting splitting = new ContextSplitting(inverted, context);
			Step result;
			switch (splitting.getResult()) {
			case LITERAL_IS_FALSE:
				result = new Solution(DefaultSymbol.createSymbol(argument1 - argument2));
				break;
			case LITERAL_IS_TRUE:
				result = new Solution(DefaultSymbol.createSymbol(argument1 + argument2));
				break;
			case LITERAL_IS_UNDEFINED:
				result =  new ItDependsOn(inverted, splitting, this, this);
				break;
				default:
					throw new Error("Unpredicated case.");
			}
			return result;
		}
	};

	@Test
	public void testConditionalSwitchRewriter() {
		Rewriter baseRewriterSum         = e -> new SumStepSolver(e);
		Rewriter baseRewriterSubtraction = e -> new SubtractionStepSolver(e);

		Switch rewriter = 
				new Switch<String>(
						e -> e.getFunctor() != null? e.getFunctor().toString() : "", 
						map(
								"+", baseRewriterSum, 
								"-", baseRewriterSubtraction));

		Expression initial;
		Expression expected;

		initial = parse("7");
		expected = parse("7");
		runTest(rewriter, initial, expected, map());

		initial = parse("10 - 3");
		expected = parse("if Inverted then 13 else 7");
		runTest(rewriter, initial, expected, map(inverted, parse("Boolean")));

		initial = parse("10 + 3");
		expected = parse("if Inverted then 7 else 13");
		runTest(rewriter, initial, expected, map(inverted, parse("Boolean")));
	}

	private void runTest(Rewriter rewriter, Expression initial, Expression expected, Map<Expression, Expression> symbolsAndTypes) {
		CompoundTheory theory = new CompoundTheory(new PropositionalTheory(), new DifferenceArithmeticTheory(false, true));
		Context context = new TrueContext(theory);
		context = context.registerAdditionalSymbolsAndTypes(symbolsAndTypes);
		Rewriter recursive = new Recursive(rewriter);
		Expression solution = recursive.rewrite(initial, context);
		System.out.println("Solution: " + solution);	
		assertEquals(expected, solution);
	}
}