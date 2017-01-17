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
package com.sri.ai.grinder.sgdpllt.core.constraint;

import static com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter.merge;
import static com.sri.ai.util.Util.camelCaseToSpacedString;
import static com.sri.ai.util.Util.list;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.solver.SGDPLLT;
import com.sri.ai.grinder.sgdpllt.core.solver.SGVET;
import com.sri.ai.grinder.sgdpllt.library.BindingTopSimplifier;
import com.sri.ai.grinder.sgdpllt.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAllRewriter;
import com.sri.ai.grinder.sgdpllt.library.boole.LiteralRewriter;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExistsRewriter;
import com.sri.ai.grinder.sgdpllt.library.equality.EqualitySimplifier;
import com.sri.ai.grinder.sgdpllt.library.inequality.InequalitySimplifier;
import com.sri.ai.grinder.sgdpllt.library.number.MaxRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.NumericSimplifier;
import com.sri.ai.grinder.sgdpllt.library.number.ProductRewriter;
import com.sri.ai.grinder.sgdpllt.library.number.SummationRewriter;
import com.sri.ai.grinder.sgdpllt.library.set.CardinalityOfSetConstantSimplifier;
import com.sri.ai.grinder.sgdpllt.library.set.CardinalityTopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.FirstOf;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleEqualityTopRewriter;

@Beta
/** 
 * Basic implementation of some methods of {@link Theory}.
 */
abstract public class AbstractTheory implements Theory {

	protected TopRewriter topRewriter;
	
	/**
	 * Initializes types for testing to be the collection of a single type,
	 * a {@link Categorical} {@link Type} named <code>SomeType</code>
	 * with domain size 5 and known constants <code>a, b, c, d</code>,
	 * variables for testing to <code>X, Y, Z</code> of type <code>SomeType</code>,
	 * of which <code>X</code> is the main testing variable on which testing literals are generated.
	 * @param topRewriter a source of elementary simplifiers for this theory
	 */
	public AbstractTheory() {
		super();
	}

	private Rewriter cachedRecursiveExhaustiveTopRewriter;
	
	/**
	 * Sets the theory's rewriter.
	 * @param topRewriter
	 */
	protected void setTopRewriter(TopRewriter topRewriter) {
		this.topRewriter = topRewriter;
		this.cachedRecursiveExhaustiveTopRewriter = new Recursive(new Exhaustive(topRewriter));
	}
	
	@Override
	public TopRewriter getTopRewriter() {
		if (topRewriter == null) {
			setTopRewriter(makeDefaultTopRewriter());
		}
		return topRewriter;
	}

	@Override
	public Rewriter getRewriter() {
		if (topRewriter == null) {
			setTopRewriter(makeDefaultTopRewriter());
		}
		return cachedRecursiveExhaustiveTopRewriter;
	}

	/**
	 * We keep a static cached version of the default top rewriter
	 * so that all theories share the same instances of top rewriters.
	 * This way, if they are all merged, their shared instances
	 * will be recognized as the same by {@link TopRewriter#merge}
	 * and be used only once.
	 */
	private static TopRewriter staticCachedDefaultTopRewriter = null;

	public TopRewriter makeDefaultTopRewriter() {
		if (staticCachedDefaultTopRewriter == null) {
			staticCachedDefaultTopRewriter = 
					merge(
							// basic simplifications
							new BindingTopSimplifier(),
							new EqualitySimplifier(),
							new InequalitySimplifier(),
							new TupleEqualityTopRewriter(),
							new BooleanSimplifier(),
							new NumericSimplifier(),
							new CardinalityOfSetConstantSimplifier(),

							new SummationRewriter(new SGVET())
							,
							new ProductRewriter(new SGDPLLT())
							,
							new MaxRewriter(new SGDPLLT())
							,
							new CardinalityTopRewriter(new SGDPLLT())
							,
							new ForAllRewriter(new SGDPLLT())
							,
							new ThereExistsRewriter(new SGDPLLT()));
		}
		return staticCachedDefaultTopRewriter;
	}
	
	@Override
	public Expression simplify(Expression expression, Context context) {
		Expression result = getRewriter().apply(expression, context);
		return result;
	}
	
	@Override
	public ExpressionLiteralSplitterStepSolver makeEvaluatorStepSolver(Expression expression) {

		Rewriter literalExternalizer = new LiteralRewriter(new Recursive(new Exhaustive(getTopRewriter())));

		Recursive completeEvaluator = new Recursive(
				new Exhaustive(
						new FirstOf(
								getTopRewriter(), 
								literalExternalizer)));
		// it is a good idea to leave the literal externalizer at the end,
		// since it is expensive due to duplicating the entire problem at every split
		
		ExpressionLiteralSplitterStepSolver result = completeEvaluator.makeStepSolver(expression);
		
		return result;
	}

	@Override
	public Collection<Type> getNativeTypes() {
		return list();
	}
	
	@Override
	public
	String toString() {
		return camelCaseToSpacedString(getClass().getSimpleName());
	}
	
	@Override
	public Theory clone() {
		Theory result;
		try {
			result = (Theory) super.clone();
		}
		catch (CloneNotSupportedException cnse) {
			throw new RuntimeException(cnse);
		}
		return result;
	}
}