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
package com.sri.ai.grinder.sgdpllt.api;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.core.solver.ContextDependentExpressionProblemSolver;

/**
 * A {@link StepSolver} specialized for problems with {@link Expression}-typed solutions.
 * <p>
 * Ideally, a {@link Solution} object returned by such a step solver should not contain literals
 * (if it does, then it depends on that literals and therefore that literals should have been returned in a {@link ItDependsOn} solver step.
 * However, this is not currently enforced (TODO: it may be in the future).
 * 
 * @author braz
 *
 */
@Beta
public interface ExpressionStepSolver extends StepSolver<Expression>, Cloneable {

	/**
	 * Convenience method invoking
	 * {@link ContextDependentExpressionProblemSolver#staticSolve(ExpressionStepSolver, Context)}
	 * on this step solver.
	 * @param context
	 * @return
	 */
	default Expression solve(Context context) {
		Expression result = ContextDependentExpressionProblemSolver.staticSolve(this, context);
		return result;
	}

	@Override
	ExpressionStepSolver clone();
	
	/**
	 * Returns a solver step for the problem: either the solution itself, if independent
	 * on the values for free variables, or a literal that, if used to split the context,
	 * will bring the problem closer to a solution.
	 * @param context
	 * @return
	 */
	@Override
	SolverStep<Expression> step(Context context);
}