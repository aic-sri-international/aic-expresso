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
import com.sri.ai.grinder.sgdpllt.core.solver.EvaluatorStepSolver;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;

@Beta
/** 
 * Basic implementation of some methods of {@link Theory}.
 */
abstract public class AbstractTheory implements Theory {

	protected TopRewriter rewriter;
	
	/**
	 * Initializes types for testing to be the collection of a single type,
	 * a {@link Categorical} {@link Type} named <code>SomeType</code>
	 * with domain size 5 and known constants <code>a, b, c, d</code>,
	 * variables for testing to <code>X, Y, Z</code> of type <code>SomeType</code>,
	 * of which <code>X</code> is the main testing variable on which testing literals are generated.
	 * @param topRewriter a source of elementary simplifiers for this theory
	 */
	public AbstractTheory(TopRewriter topRewriter) {
		super();
		setTopRewriter(topRewriter);
	}

	private Rewriter cachedRecursiveExhaustiveRewriter;
	
	/**
	 * Sets the theory's rewriter.
	 * @param topRewriter
	 */
	protected void setTopRewriter(TopRewriter topRewriter) {
		this.rewriter = topRewriter;
		this.cachedRecursiveExhaustiveRewriter = new Recursive(new Exhaustive(topRewriter));
	}
	
	@Override
	public Expression simplify(Expression expression, Context context) {
		Expression result = cachedRecursiveExhaustiveRewriter.apply(expression, context);
		return result;
	}
	
	@Override
	public ExpressionLiteralSplitterStepSolver makeEvaluatorStepSolver(Expression expression) {
		EvaluatorStepSolver result = new EvaluatorStepSolver(expression);
		return result;
	}

	@Override
	public Collection<Type> getNativeTypes() {
		return list();
	}
	
	@Override
	public TopRewriter getTopRewriter() {
		return rewriter;
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