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
package com.sri.ai.grinder.sgdpllt.rewriter.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver.Solution;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver.Step;
import com.sri.ai.grinder.sgdpllt.theory.base.FunctionOnContextExpressionStepSolver;

/**
 * A Simplifier knows just enough about the symbols in a language to simplify it in a shallow way,
 * that is, to replace function applications by a simpler equivalent expression, if that expression is determined by their immediate arguments.
 * Shallow simplifications are required to take polynomial time in the size of expressions (preferably linear time).
 * <p>
 * Examples of shallow simplifications are <code>x + 0</code> to <code>x</code>, <code>x or true</code> to <code>true</code>, and <code>x + 1 + 3</code> to <code>x + 4</code>.
 * Simplifications that are <i>not</i> shallow include those requiring case analysis (inference), such as <code>(p and q) or (p and not q)</code>leading to <code>p</code>.
 * <p>
 * A simplifier can be used as a rewriter that always returns unconditional steps, that is, solutions.
 * @author braz
 *
 */
@FunctionalInterface
public interface Simplifier extends TopRewriter {
	
	/**
	 * We define this method to make this a functional interface again,
	 * so that simplifiers can be easily defined with lambda expressions.
	 * @param expression
	 * @param context
	 * @return
	 */
	Expression applySimplifier(Expression expression, Context context);
	
	default Expression apply(Expression expression, Context context) { // more efficient
		return applySimplifier(expression, context);
	}
	
	default ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
		return new FunctionOnContextExpressionStepSolver(c -> apply(expression, c));
	}
	
	default Step step(Expression expression, Context context) { // optimized version
		Expression simplifiedExpression = apply(expression, context);
		Solution result = new Solution(simplifiedExpression);
		return result;
	}
}