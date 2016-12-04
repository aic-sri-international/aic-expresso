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

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.util.collect.ImmutableStackedLinkedList;

/**
 * Applies a list of base rewriters until one of them modifies the original expression, returning this as the overall result.
 * If no rewriter modifies the expression, the same instance is provided as a solution.
 * If any rewriter splits (returns a conditional step),
 * this rewriter splits as well, providing the appropriate sequel step solvers to continue the sequential rewriting
 * under each branch.
 * 
 * @author braz
 *
 */
public class FirstOf implements Rewriter {
	
	private List<Rewriter> baseRewriters;
	
	public FirstOf(List<Rewriter> baseRewriters) {
		super();
		this.baseRewriters = baseRewriters;
	}

	@Override
	public FirstOfStepSolver makeStepSolver(Expression expression) {
		FirstOfStepSolver stepSolver = new FirstOfStepSolver(expression, baseRewriters);
		return stepSolver;
	}
	
	@Override
	public FirstOf clone() {
		FirstOf result = null;
		try {
			result = (FirstOf) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	/**
	 * Creates a new instance of {@link FirstOf} rewriter with new baseRewriter,
	 * if it is not already present in the current base rewriters,
	 * or this same instance otherwise.
	 * @param baseRewriter
	 * @return
	 */
	public FirstOf addBaseRewriter(Rewriter baseRewriter) {
		FirstOf result;
		if ( ! baseRewriters.contains(baseRewriter)) {
			result = clone();
			result.baseRewriters = new ImmutableStackedLinkedList<Rewriter>(baseRewriter, baseRewriters);
		}
		else {
			result = this;
		}
		return result;
	}

	/**
	 * Implements a step solver for {@link FirstOf} rewriter.
	 * 
	 * It stores the original expression, the base rewriters list, and the index of the 
	 * base rewriter being currently applied.
	 * 
	 * It takes a step by using the current base rewriter's step solver (it may be a sequel from a previous step).
	 * 
	 * If the current base rewriter's sequel step solver does not find a solution,
	 * this {@link FirstOfStepSolver} returns a {@link ItDependsOn}
	 * step with the appropriate sequel step solvers.
	 * 
	 * If the base rewriter's sequel step solver does find a solution,
	 * we checks it to see if there has been a change from the original expression.
	 * If so, we return this solution as the overall solution.
	 * If not, we proceed with the evaluation by the next base rewriter.
	 * 
	 * @author braz
	 *
	 */
	private static class FirstOfStepSolver implements ExpressionLiteralSplitterStepSolver {
		
		private Expression expression;
		private List<Rewriter> baseRewriters;
		private int currentBaseRewriterIndex;
		private ExpressionLiteralSplitterStepSolver currentBaseStepSolver;
		
		public FirstOfStepSolver(Expression expression, List<Rewriter> baseRewriters) {
			super();
			this.expression = expression;
			this.baseRewriters = baseRewriters;
			setCurrentBaseRewriterIndex(0);
		}

		private void setCurrentBaseRewriterIndex(int currentBaseRewriterIndex) {
			this.currentBaseRewriterIndex = currentBaseRewriterIndex;
			makeCurrentStepSolver(expression, baseRewriters);
		}

		private void makeCurrentStepSolver(Expression expression, List<Rewriter> baseRewriters) {
			Rewriter currentBaseRewriter = baseRewriters.get(currentBaseRewriterIndex);
			this.currentBaseStepSolver = currentBaseRewriter.makeStepSolver(expression);
		}

		@Override
		public FirstOfStepSolver clone() {
			FirstOfStepSolver result = null;
			try {
				result = (FirstOfStepSolver) super.clone();
			} catch (CloneNotSupportedException e) {
				e.printStackTrace();
			}
			return result;
		}
		
		public Step step(Context context) {
			Step result; 
			Step baseStep = currentBaseStepSolver.step(context);
			if (baseStep.itDepends()) {
				
				FirstOfStepSolver ifTrue = clone();
				ifTrue.currentBaseStepSolver = baseStep.getStepSolverForWhenSplitterIsTrue();
				
				FirstOfStepSolver ifFalse = clone();
				ifFalse.currentBaseStepSolver = baseStep.getStepSolverForWhenSplitterIsFalse();
				
				result = 
						new ItDependsOn(
								baseStep.getSplitter(),
								baseStep.getContextSplittingWhenSplitterIsLiteral(),
								ifTrue,
								ifFalse);
			}
			else if (baseStep.getValue() != expression) {
				// found a solution for the current application of base rewriter, and it is a modified one,
				// so return it as the overall solution for the {@link FirstOf} rewriter.
				result = new Solution(baseStep.getValue());
			}
			else {
				// current expression has not been modified by base rewriter,
				// so move on to next one if available,
				// or stop otherwise
				if (currentBaseRewriterIndex + 1 != baseRewriters.size()) {
					FirstOfStepSolver next = clone();
					next.setCurrentBaseRewriterIndex(currentBaseRewriterIndex + 1);
					result = next.step(context);
				}
				else { // ran out of base rewriters, so returns original expression
					result = new Solution(expression);
				}
			}
			
			return result;
		}
	}
}