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
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.commonrewriters.CommonSimplifiersAndSymbolicQuantifierEliminationRewritersTopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;
import com.sri.ai.grinder.sgdpllt.rewriter.help.CompleteRewriter;

@Beta
/** 
 * Basic implementation of some methods of {@link Theory}.
 * 
 * It establishes evaluation for theories to be based on a top rewriter
 * provided by extending classes in the implementation of {@link #makeTopRewriter()}.
 * 
 * It also sets native types to an empty list, and {@link #toString()} to
 * use the result of applying {@link Util#camelCaseToSpacedString(String)} to the class name.
 */
abstract public class AbstractTheory implements Theory {

	private TopRewriter topRewriter;
	private Rewriter rewriter;
	private CompleteRewriter completeRewriter;
	
	public AbstractTheory() {
		super();
	}

	private void setAllRewriters(TopRewriter topRewriter) {
		this.topRewriter = topRewriter;
		this.rewriter = new Recursive(new Exhaustive(topRewriter));
		this.completeRewriter = new CompleteRewriter(topRewriter);
	}
	
	@Override
	public TopRewriter getTopRewriter() {
		if (topRewriter == null) {
			setAllRewriters(makeTopRewriter());
		}
		return topRewriter;
	}

	private Rewriter getRewriter() {
		if (rewriter == null) {
			setAllRewriters(makeTopRewriter());
		}
		return rewriter;
	}
	
	private Rewriter getCompleteRewriter() {
		if (completeRewriter == null) {
			setAllRewriters(makeTopRewriter());
		}
		return completeRewriter;
	}
	
	protected TopRewriter makeTopRewriter() {
		return CommonSimplifiersAndSymbolicQuantifierEliminationRewritersTopRewriter.INSTANCE;
	}

	@Override
	public Expression simplify(Expression expression, Context context) {
		Expression result = getRewriter().apply(expression, context);
		return result;
	}
	
	@Override
	public ExpressionLiteralSplitterStepSolver makeEvaluatorStepSolver(Expression expression) {
		ExpressionLiteralSplitterStepSolver result = getCompleteRewriter().makeStepSolver(expression);
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
		catch (CloneNotSupportedException exception) {
			throw new RuntimeException(exception);
		}
		return result;
	}
}