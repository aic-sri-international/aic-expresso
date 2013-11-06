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
package com.sri.ai.test.grinder.sequence;

import static org.junit.Assert.assertEquals;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.DefaultRewriterLookup;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.grinder.sequence.RewriterFunctionalSequence;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.util.Util;

public class RewriterFunctionalSequenceTest extends AbstractGrinderTest {

	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}

	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		return new DefaultRewritingProcess(topExpression, new Basic());
	}

	@Before
	public void setUp() {
		super.setUp();
// TODO-remove the need for this once the Refine API is in place.
		GrinderConfiguration.setProperty(GrinderConfiguration.KEY_REWRITING_PROCESS_CACHE_MAXIMUM_SIZE, "0");
	}
	
	@Test
	public void testRewriterFunctionalSequence() {
				
		/*
		 * This test is a sketch of anytime belief propagation.
		 * Instead of bounds, two numbers are passed from sub-rewriters to rewriters and the result is the half of their average.
		 * Therefore, as they become smaller, like bounds become smaller, so will the resulting numbers.
		 * Eventually, leaf sub-rewriters produce 0 (analogous to 0-width bounds) and the final result also converges to 0.
		 * Each rewriter has a level and its sub-rewriters has that level - 1. Leaf rewriters are those with level 0.
		 * In the console output it is possible to see how the computation is successively expanded by a leaf,
		 * the value of which then is used all the way to the root, and then another leaf is expanded and so on. 
		 * 
		 * We test two scenarios: first, one in which the arguments to sub-rewriters are always different (by using a unique id).
		 * This guarantees that the functional sequences based on them are distinct.
		 * In the second scenario, sub-rewriters share id arguments and share functional sequence objects.
		 * This means that, when the invocation of a sub-rewriter triggers the updating of its functional sequence,
		 * all parent rewriters using that same functional sequence get the updated values.
		 * This makes convergence faster.
		 * In anytime BP, this appears when a message is used to compute more than one other message: for example, in a graph of the form
		 * 
		 *  / B \
		 * A     D - E
		 *  \ C /
		 * 
		 * where message D <- E is used for both messages B <- D and C <- D.
		 */
		
		int numberOfLevels = 3;
		
		DefaultRewriterLookup rewriterLookup = new DefaultRewriterLookup();		
		Rewriter rewriter = new HalfTheAverageRewriter();
		rewriterLookup.put("R_HalfTheAverageRewriter", rewriter);
		
		DefaultRewritingProcess process = new DefaultRewritingProcess(rewriter, rewriterLookup);
		
		HalfTheAverageRewriterFunctionalSequence sequence =
				new HalfTheAverageRewriterFunctionalSequence("R_HalfTheAverageRewriter", 
						Expressions.apply("args", DefaultSymbol.createSymbol(numberOfLevels), Expressions.ZERO), 
						process);
		
		System.out.println("Starting functional sequence *without* shared sub-functional sequences: ");
		List<Expression> results = new LinkedList<Expression>();
		while (sequence.hasNext()) {
			Expression result = sequence.next();
			System.out.println("Next output: " + result);
			results.add(result);
		}
		assertEquals(Util.list(
				DefaultSymbol.createSymbol(1),
				DefaultSymbol.createSymbol(0.5),
				DefaultSymbol.createSymbol(0.375),
				DefaultSymbol.createSymbol(0.34375),
				DefaultSymbol.createSymbol(0.328125),
				DefaultSymbol.createSymbol(0.3125),
				DefaultSymbol.createSymbol(0.28125),
				DefaultSymbol.createSymbol(0.265625),
				DefaultSymbol.createSymbol(0.25),
				DefaultSymbol.createSymbol(0.125),
				DefaultSymbol.createSymbol(0.09375),
				DefaultSymbol.createSymbol(0.078125),
				DefaultSymbol.createSymbol(0.0625),
				DefaultSymbol.createSymbol(0.03125),
				DefaultSymbol.createSymbol(0.015625),
				DefaultSymbol.createSymbol(0)
				), results);
		
		rewriterLookup = new DefaultRewriterLookup();		
		rewriter = new HalfTheAverageRewriterWithSharedSubRewriterInvocations();
		rewriterLookup.put("R_HalfTheAverageRewriterWithSharedSubRewriterInvocations", rewriter);
		
		process = new DefaultRewritingProcess(rewriter, rewriterLookup);
		
		sequence = new HalfTheAverageRewriterFunctionalSequence("R_HalfTheAverageRewriterWithSharedSubRewriterInvocations", 
							Expressions.apply("args", DefaultSymbol.createSymbol(numberOfLevels), Expressions.ZERO), 
							process);
		
		System.out.println("\nStarting functional sequence *with* shared sub-functional sequences: ");
		results = new LinkedList<Expression>();
		while (sequence.hasNext()) {
			Expression result = sequence.next();
			System.out.println("Next output: " + result);
			results.add(result);
		}
		assertEquals(Util.list(
				DefaultSymbol.createSymbol(1),
				DefaultSymbol.createSymbol(0.5),
				DefaultSymbol.createSymbol(0.25),
				DefaultSymbol.createSymbol(0.125),
				DefaultSymbol.createSymbol(0)
				), results);
	}

	public static class HalfTheAverageRewriterFunctionalSequence extends RewriterFunctionalSequence {
		public HalfTheAverageRewriterFunctionalSequence(String rewriterName, Expression arguments, RewritingProcess process) {
			super(rewriterName, arguments, process);
		}
		
		protected HalfTheAverageRewriterFunctionalSequence(String rewriterName, Expression arguments, RewritingProcess process, Map<ChildRewriterInvocation, RewriterFunctionalSequence> sharedRewriterFunctionalSequences) {
			super(rewriterName, arguments, process, sharedRewriterFunctionalSequences);
		}
		
		protected RewriterFunctionalSequence newInstance(String childRewriterName, Expression childCallExpression, RewritingProcess childCallProcess) {
			RewriterFunctionalSequence result = new HalfTheAverageRewriterFunctionalSequence(
														childRewriterName,
														childCallExpression,
														childCallProcess,
														sharedRewriterFunctionalSequences);
			return result;
		}

		@Override
		protected Expression initialValue() {
			return Expressions.ONE; // for bounds, this would be the trivial bound with all messages in it.
		}

		@Override
		protected boolean isFinalValue(Expression value) {
			return value.equals(0); // once we children half average 0, it's over. For bounds, this would be finding an exact bound with a single message in it.
		}

		@Override
		public String toString() {
			return "HalfTheAverageRewriterFunctionalSequence on args " + arguments + " (" + System.identityHashCode(this) + ")";
		}
	};
	
	public static class HalfTheAverageRewriter extends AbstractRewriter {	
		public HalfTheAverageRewriter() {
		}
		
		@Override
		public String getName() {
			return "R_HalfTheAverageRewriter";
		}

		@Override
		public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
			Expression result;
			double     level = doubleValue(expression.get(0));
			Expression id    = expression.get(1);
			if (level > 0) {
				Expression levelMinusOne = DefaultSymbol.createSymbol(level - 1);
				Expression child1Id    = makeChildCallId(id, Expressions.ZERO);
				Expression child2Id    = makeChildCallId(id, Expressions.ONE);
				Expression child1Input = process.rewrite(getName(), Expressions.apply("args", levelMinusOne, child1Id));
				Expression child2Input = process.rewrite(getName(), Expressions.apply("args", levelMinusOne, child2Id));
				double halfTheAverage =	(doubleValue(child1Input) + doubleValue(child2Input)) / 2 / 2;
				result = DefaultSymbol.createSymbol(halfTheAverage);
				System.out.println("Half the average at level " + level + ", id " + expression.get(1) + ", of " + child1Input + " and " + child2Input + " is " + result);
			}
			else {
				result = Expressions.ZERO; // this corresponds to "end of the line" factors without further children, producing exact bounds
				System.out.println("At level " + level + ", result is 0.");
			}
			return result;
		}

		protected Expression makeChildCallId(Expression id, Expression child) {
			return Expressions.apply("id", id, child);
		}
		
		private double doubleValue(Expression numericSymbol) {
			return ((Number) numericSymbol.getValue()).doubleValue();
		}
		
		protected boolean isTraceInAndOutOfRewriter() {
			return true;
		}
	}

	/**
	 * Same as {@link HalfTheAverageRewriter}, but passing identical arguments to sub-rewriters at the same level,
	 * causing them to use shared functional sequences.
	 */
	public static class HalfTheAverageRewriterWithSharedSubRewriterInvocations extends HalfTheAverageRewriter {

		public HalfTheAverageRewriterWithSharedSubRewriterInvocations() {
			super();
		}

		@Override
		public String getName() {
			return "R_HalfTheAverageRewriterWithSharedSubRewriterInvocations";
		}

		@Override
		protected Expression makeChildCallId(Expression id, Expression child) {
			return id;
		}
	}
}
