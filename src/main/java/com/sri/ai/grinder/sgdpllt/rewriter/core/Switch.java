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
package com.sri.ai.grinder.sgdpllt.rewriter.core;

import java.util.Map;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.theory.base.ConstantExpressionStepSolver;

/**
 * A rewriter that stores a map from keys in some type to base {@link Rewriter}s
 * and a key maker function to {@link Expression}s to the key type.
 * When rewriting an expression, it computes a key for it and
 * acts as the key's corresponding base rewriter.
 * If the computed key has no corresponding base rewriter, the same expression instance is returned.
 * @author braz
 *
 */
public class Switch<T> implements Rewriter {

	private Function<Expression, T> keyMaker;
	private Map<T, Rewriter> fromKeyToRewriter;
	
	public Switch(Function <Expression, T> keyMaker, Map<T, Rewriter> fromKeyToRewriter) {
		super();
		this.keyMaker = keyMaker;
		this.fromKeyToRewriter = fromKeyToRewriter;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
		ExpressionLiteralSplitterStepSolver result;
		T key = keyMaker.apply(expression);
		Rewriter baseRewriter = fromKeyToRewriter.get(key);
		if (baseRewriter != null) {
			result = baseRewriter.makeStepSolver(expression);
		}
		else {
			result = new ConstantExpressionStepSolver(expression);
		}
		return result;
	}
}