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
package com.sri.ai.grinder.theory.base;

import static com.sri.ai.grinder.theory.base.ConstantExpressionStepSolver.constantExpressionStepSolver;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.StepSolver;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver.ItDependsOn;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver.Solution;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver.Step;

/**
 * Provides a static method for generating {@link Expression}-valued solver steps based on a literal.
 *
 * @author braz
 *
 */
@Beta
public class ExpressionConditionedOnLiteralSolutionStep {

	/**
	 * Produces a solver step based on value of literal, with corresponding given solutions
	 * (if literal is not defined by context, a {@link ItDependsOn} step is returned).
	 * @param literal
	 * @param solutionIfTrue
	 * @param solutionIfFalse
	 * @param context
	 * @return
	 */
	public static Step
	stepDependingOnLiteral(
			Expression literal, 
			Expression solutionIfTrue, 
			Expression solutionIfFalse, 
			Context context) {
		
		Step result;
		LiteralStepSolver literalStepSolver = new LiteralStepSolver(literal);
		StepSolver.Step<Boolean> step = literalStepSolver.step(context);
		if (step.itDepends()) {
			result =
					new ItDependsOn(
							step.getSplitter(),
							step.getContextSplittingWhenSplitterIsLiteral(),
							constantExpressionStepSolver(solutionIfTrue),
							constantExpressionStepSolver(solutionIfFalse));
		}
		else {
			result = new Solution(step.getValue()? solutionIfTrue : solutionIfFalse);
		}
		return result;
	}
}