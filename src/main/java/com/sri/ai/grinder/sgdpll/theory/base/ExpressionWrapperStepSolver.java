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
package com.sri.ai.grinder.sgdpll.theory.base;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.api.ContextDependentProblemStepSolver;

/**
 * A context-dependent problem step solver adapting a non-Expression
 * step solver to one in which solutions are wrapped into one
 * by a user-specified function.
 *
 * @author braz
 *
 */
@Beta
public class ExpressionWrapperStepSolver<T> implements ContextDependentExpressionProblemStepSolver {

	private ContextDependentProblemStepSolver<T> base;
	private Function<T, Expression> wrapper;
	
	public ExpressionWrapperStepSolver(ContextDependentProblemStepSolver<T> base, Function<T, Expression> wrapper) {
		super();
		this.base = base;
		this.wrapper = wrapper;
	}

	/**
	 * A cloning method delegating to super.clone().
	 */
	@SuppressWarnings("unchecked")
	@Override
	public ExpressionWrapperStepSolver<T> clone() {
		try {
			return (ExpressionWrapperStepSolver<T>) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error("Trying to clone " + getClass() + " but cloning is not supported for this class.");
		}
	}

	@Override
	public SolutionStep step(Context contextualConstraint, Context context) {
		
		ContextDependentProblemStepSolver.SolutionStep<T> step =
				base.step(contextualConstraint, context);

		SolutionStep result;
		
		if (step.itDepends()) {
			ExpressionWrapperStepSolver<T> subIfTrue = clone();
			subIfTrue.base = step.getStepSolverForWhenLiteralIsTrue();
			
			ExpressionWrapperStepSolver<T> subIfFalse = clone();
			subIfFalse.base = step.getStepSolverForWhenLiteralIsFalse();

			result = new ItDependsOn(
					step.getLiteral(),
					step.getConstraintSplitting(),
					subIfTrue,
					subIfFalse);
		}
		else {
			Expression expression = wrapper.apply(step.getValue());
			result = new Solution(expression);
		}
		
		return result;
	}
}