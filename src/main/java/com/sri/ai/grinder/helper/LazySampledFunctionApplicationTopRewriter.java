/*
 * Copyright (c) 2017, SRI International
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
package com.sri.ai.grinder.helper;

import static com.sri.ai.util.Util.mapIntoList;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.rewriter.api.Rewriter;
import com.sri.ai.grinder.rewriter.api.Simplifier;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.grinder.rewriter.core.IfRewriter;

/**
 * A {@link TopRewriter} that
 * rewrites an expression to its value if it is
 * a {@link LazySampledFunction} application,
 * or delegates its rewriting to a given base {@link TopRewriter} otherwise.
 *
 * @author braz
 *
 */
public class LazySampledFunctionApplicationTopRewriter extends IfRewriter {
	
	/**
	 * Constructor receives
	 * a {@link Rewriter} that evaluates the arguments of {@link LazySampledFunction} applications,
	 * and uses it to evaluate such applications.
	 * It also receives a base {@link TopRewriter}, which processes all other types of expressions.
	 * @param argumentsEvaluator
	 * @param baseTopRewriter
	 */
	public LazySampledFunctionApplicationTopRewriter(Rewriter argumentsEvaluator, TopRewriter baseTopRewriter) {
		super(
				(Expression e) -> e.getFunctor() instanceof LazySampledFunction,

				(Simplifier) (e, c) -> evaluateArgumentsAndGetValueOfLazyFunctionApplication(e, argumentsEvaluator, c),
				
				baseTopRewriter
		);
	}

	private static Expression evaluateArgumentsAndGetValueOfLazyFunctionApplication(Expression lazySampledFunctionApplication, Rewriter evaluator, Context c) {
		LazySampledFunction lazySampledFunctor = (LazySampledFunction) lazySampledFunctionApplication.getFunctor();
		List<Expression> argumentValues = mapIntoList(lazySampledFunctionApplication.getArguments(), a -> evaluator.apply(a, c));
		Expression result = lazySampledFunctor.sampleApplication(argumentValues);
		return result;
	}
}