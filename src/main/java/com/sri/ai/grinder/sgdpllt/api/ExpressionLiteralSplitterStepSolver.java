/*
 * Copyright (c) 2016, SRI International
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
import com.sri.ai.grinder.sgdpllt.core.constraint.ContextSplitting;

/**
 * An indicator interface for ExpressionStepSolvers that only split on the
 * context using literals (i.e. atomic formulas or their negations).
 * 
 * @author oreilly
 *
 */
@Beta
public interface ExpressionLiteralSplitterStepSolver extends ExpressionStepSolver {

	@Override
	ExpressionLiteralSplitterStepSolver clone();
	
	@Override
	ExpressionLiteralSplitterStepSolver.Step step(Context context);
	
	public static interface Step extends StepSolver.Step<Expression> {
		/**
		 * 
		 * @return the literal splitter.
		 */
		Expression getLiteral();
		 
		/**
		 * Returns a {@link ExpressionLiteralSplitterStepSolver} to be used for finding the final solution
		 * in case the literal is defined as true by the context.
		 * This is merely an optimization, and using the original step solver should still work,
		 * but will perform wasted working re-discovering that expressions is already true.
		 * @return
		 */
		@Override
		ExpressionLiteralSplitterStepSolver getStepSolverForWhenSplitterIsTrue();
		
		/**
		 * Same as {@link #getStepSolverForWhenSplitterIsTrue()} but for when literal is false.
		 * @return
		 */
		@Override
		ExpressionLiteralSplitterStepSolver getStepSolverForWhenSplitterIsFalse();
	}
	
	public static class ItDependsOn extends StepSolver.ItDependsOn<Expression> implements Step {

		public ItDependsOn(
				Expression literal,
				ContextSplitting contextSplitting,
				ExpressionLiteralSplitterStepSolver stepSolverIfExpressionIsTrue,
				ExpressionLiteralSplitterStepSolver stepSolverIfExpressionIsFalse) {
			super(literal, contextSplitting, stepSolverIfExpressionIsTrue, stepSolverIfExpressionIsFalse);
		}
		
		@Override
		public Expression getLiteral() {
			return getSplitter();
		}
		
		@Override
		public ExpressionLiteralSplitterStepSolver getStepSolverForWhenSplitterIsTrue() {
			return (ExpressionLiteralSplitterStepSolver) super.getStepSolverForWhenSplitterIsTrue();
		}
		
		@Override
		public ExpressionLiteralSplitterStepSolver getStepSolverForWhenSplitterIsFalse() {
			return (ExpressionLiteralSplitterStepSolver) super.getStepSolverForWhenSplitterIsFalse();
		}
	}
	
	public static class Solution extends StepSolver.Solution<Expression> implements Step {

		public Solution(Expression value) {
			super(value);
		}
		
		public Expression getLiteral() {
			throw new Error("Solution does not define getLiteral().");
		}
		
		@Override
		public ExpressionLiteralSplitterStepSolver getStepSolverForWhenSplitterIsTrue() {
			throw new Error("Solution has no sub-step solvers since it does not depend on any expression");
		}

		@Override
		public ExpressionLiteralSplitterStepSolver getStepSolverForWhenSplitterIsFalse() {
			throw new Error("Solution has no sub-step solvers since it does not depend on any expression");
		}
	}
}
