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
 * A specialization of the StepSolver interface that indicates splitting only
 * ever occurs using literals and not general formulas. 
 * 
 * @author oreilly
 *
 * @param <T>
 */
@Beta
public interface LiteralSplitterStepSolver<T> extends StepSolver<T> {
	public static interface Step<T> extends StepSolver.Step<T> {
		Expression getSplitterLiteral();
		
		@Override
		LiteralSplitterStepSolver<T> getStepSolverForWhenSplitterIsTrue();
		@Override
		LiteralSplitterStepSolver<T> getStepSolverForWhenSplitterIsFalse();
	}
	
	public static class ItDependsOn<T> extends StepSolver.ItDependsOn<T> implements Step<T> {
		
		public ItDependsOn(
				Expression splitterLiteral,
				ContextSplitting contextSplitting,
				LiteralSplitterStepSolver<T> stepSolverIfSplitterIsTrue,
				LiteralSplitterStepSolver<T> stepSolverIfSplitterIsFalse) {
			super(splitterLiteral, contextSplitting, stepSolverIfSplitterIsTrue, stepSolverIfSplitterIsFalse);
		}
		
		@Override
		public Expression getSplitterLiteral() {
			return getSplitter();
		}
		
		@Override
		public LiteralSplitterStepSolver<T> getStepSolverForWhenSplitterIsTrue() {
			return (LiteralSplitterStepSolver<T>) super.getStepSolverForWhenSplitterIsTrue();
		}
		
		@Override
		public LiteralSplitterStepSolver<T> getStepSolverForWhenSplitterIsFalse() {
			return (LiteralSplitterStepSolver<T>) super.getStepSolverForWhenSplitterIsFalse();
		}
	}
	
	public static class Solution<T> extends StepSolver.Solution<T> implements Step<T> {
		public Solution(T value) {
			super(value);
		}
		
		public Expression getSplitterLiteral() {
			throw new Error("Solution does not define getSplitterLiteral().");
		}
		
		@Override
		public LiteralSplitterStepSolver<T> getStepSolverForWhenSplitterIsTrue() {
			throw new Error("Solution has no sub-step solvers since it does not depend on any expression");
		}
		
		@Override
		public LiteralSplitterStepSolver<T> getStepSolverForWhenSplitterIsFalse() {
			throw new Error("Solution has no sub-step solvers since it does not depend on any expression");
		}
	}
}