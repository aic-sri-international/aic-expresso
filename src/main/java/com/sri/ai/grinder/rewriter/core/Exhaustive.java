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
package com.sri.ai.grinder.rewriter.core;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.rewriter.api.Rewriter;

/**
 * Applies a base rewriter exhaustively.
 * 
 * @author braz
 *
 */
public class Exhaustive implements Rewriter {

	private Rewriter baseRewriter;
	private String name;
	
	public Exhaustive(Rewriter baseRewriter) {
		super();
		this.baseRewriter = baseRewriter;
		this.name = "Exhaustive for " + baseRewriter;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
		ExhaustiveStepSolver stepSolver = 
				new ExhaustiveStepSolver(baseRewriter.makeStepSolver(expression), expression, this);
		return stepSolver;
	}

	@Override
	public String toString() {
		return name;
	}
	
	/**
	 * Implements a step solver for {@link Exhaustive} rewriter.
	 * 
	 * It stores one of the sequel step solvers from a previous application of the {@link Exhaustive}'s base rewriter,
	 * the current expression being rewritten,
	 * and the {@link Exhaustive} itself.
	 * When it runs, it runs the base rewriter's sequel step solver and see if it finds a solution this time.
	 * 
	 * If the base rewriter's sequel step solver does not find a solution,
	 * this {@link ExhaustiveStepSolver} returns a {@link ItDependsOn}
	 * step with the appropriate sequel step solvers.
	 * 
	 * If the base rewriter's sequel step solver does find a solution,
	 * checks to see if there has been a change.
	 * If so, re-applies given exhaustive rewriter to that solution, ensuring exhaustivity.
	 * If not, returns this final solution as the solution of the {@link Exhaustive} rewriter.
	 * 
	 * @author braz
	 *
	 */
	private static class ExhaustiveStepSolver implements ExpressionLiteralSplitterStepSolver {
		
		private ExpressionLiteralSplitterStepSolver baseStepSolver;
		private Expression currentExpression;
		private Rewriter exhaustiveRewriter;
		private String name;
		
		public ExhaustiveStepSolver(ExpressionLiteralSplitterStepSolver baseStepSolver, Expression currentExpression, Rewriter exhaustiveRewriter) {
			this.exhaustiveRewriter = exhaustiveRewriter;
			this.currentExpression = currentExpression;
			this.baseStepSolver = baseStepSolver;
			this.name = "Exhaustive step solver for " + baseStepSolver;
		}
		
		@Override
		public ExhaustiveStepSolver clone() {
			ExhaustiveStepSolver result = null;
			try {
				result = (ExhaustiveStepSolver) super.clone();
			} catch (CloneNotSupportedException e) {
				e.printStackTrace();
			}
			return result;
		}
		
		public Step step(Context context) {
			Step result; 
			Step baseStep = baseStepSolver.step(context);
			if (baseStep.itDepends()) {
				
				ExhaustiveStepSolver ifTrue = clone();
				ifTrue.baseStepSolver = baseStep.getStepSolverForWhenSplitterIsTrue();
				
				ExhaustiveStepSolver ifFalse = clone();
				ifFalse.baseStepSolver = baseStep.getStepSolverForWhenSplitterIsFalse();
				
				result = 
						new ItDependsOn(
								baseStep.getSplitter(),
								baseStep.getContextSplittingWhenSplitterIsLiteral(),
								ifTrue,
								ifFalse);
			}
			else if (baseStep.getValue() != currentExpression) {
				// found a solution for the current application of base rewriter, but it has not converged yet,
				// so we apply the {@link Exhaustive} rewriter again.
				result = exhaustiveRewriter.step(baseStep.getValue(), context);
			}
			else {
				// current expression has converged to final, exhaustive solution.
				result = new Solution(currentExpression);
			}
			
			return result;
		}
		
		@Override
		public String toString() {
			return name;
		}
	}
}