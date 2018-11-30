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
package com.sri.ai.grinder.core.solver;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.ExpressionStepSolver;

/**
 * An adapter that allows ExpressionStepSolvers to be used in
 * situations where ExpressionLiteralSplitterStepSolvers are required (i.e.
 * splitting on literals is assumed).
 * 
 * @author oreilly
 *
 */
@Beta
public class ExpressionStepSolverToLiteralSplitterStepSolverAdapter implements ExpressionLiteralSplitterStepSolver {
	//
	private ExpressionStepSolver currentFormulaSplitterStepSolver;
	private ExpressionStepSolver.Step currentFormulaSplitterStepSolverStep;
	private ExpressionLiteralSplitterStepSolver currentFormulaSplitterEvaluatorStepSolver;

	public static ExpressionLiteralSplitterStepSolver toExpressionLiteralSplitterStepSolver(ExpressionStepSolver formulaSplitterStepSolver) {
		ExpressionLiteralSplitterStepSolver result;
		if (formulaSplitterStepSolver instanceof ExpressionLiteralSplitterStepSolver) {
			result = (ExpressionLiteralSplitterStepSolver) formulaSplitterStepSolver;
		}
		else {
			result = new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(formulaSplitterStepSolver);
		}
		return result;
	}
	
	private ExpressionStepSolverToLiteralSplitterStepSolverAdapter(
			ExpressionStepSolver formulaSplitterStepSolver) {
		if (formulaSplitterStepSolver instanceof ExpressionLiteralSplitterStepSolver) {
			throw new IllegalArgumentException("You should not pass an ExpressionLiteralSplitterStepSolver to " + ExpressionStepSolverToLiteralSplitterStepSolverAdapter.class);
		}
		this.currentFormulaSplitterStepSolver = formulaSplitterStepSolver;
	}

	@Override
	public ExpressionStepSolverToLiteralSplitterStepSolverAdapter clone() {

		ExpressionStepSolverToLiteralSplitterStepSolverAdapter result;
		try {
			result = (ExpressionStepSolverToLiteralSplitterStepSolverAdapter) super.clone();
		}
		catch (CloneNotSupportedException cnse) {
			throw new Error(cnse);
		}

		return result;
	}
	
	@Override
	public Step step(Context context) {
		Step result = null;
		// if the following is true it means this is the first step call or that we have
		// completed walking the literals within a splitter formula using an evaluator/literal 
		// step solver and we are ready to get the next solver step from the formula splitter. 
		if (currentFormulaSplitterEvaluatorStepSolver == null) {
			currentFormulaSplitterStepSolverStep = currentFormulaSplitterStepSolver.step(context);
			if (!currentFormulaSplitterStepSolverStep.itDepends()) { 
	        	// is a final solution, return it
	            result = new ExpressionLiteralSplitterStepSolver.Solution(currentFormulaSplitterStepSolverStep.getValue());
	        }
			else {
				// The currentFormulaSplitterStepSolverStep may return a literal splitter, in that case there is no need 
				// to introduce the overhead of using an Evaluator to work through the splitter formula one literal at a time.
				if (context.getTheory().isLiteralOrBooleanConstant(currentFormulaSplitterStepSolverStep.getSplitter(), context)) {		
					// We need to wrap the ItDepends result sub-solvers in adapters as well.
					result = new ExpressionLiteralSplitterStepSolver.ItDependsOn(currentFormulaSplitterStepSolverStep.getSplitter(),
							currentFormulaSplitterStepSolverStep.getContextSplittingWhenSplitterIsLiteral(),
								toExpressionLiteralSplitterStepSolver(currentFormulaSplitterStepSolverStep.getStepSolverForWhenSplitterIs(true)),
								toExpressionLiteralSplitterStepSolver(currentFormulaSplitterStepSolverStep.getStepSolverForWhenSplitterIs(false)));
				}
				else {	
					// We have a new non-literal splitter formula that we have obtained from the formula splitter step solver
					// over which we want to walk the individual literals in that formula splitter. This is done
					// by delegating to a evaluator/literal step solver while extracting these literals.
					currentFormulaSplitterEvaluatorStepSolver = context.getTheory().makeEvaluatorStepSolver(currentFormulaSplitterStepSolverStep.getSplitter());
				}
			}
		}
		
		// If we don't have a result at this point, it means we are currently walking the literals
		// within a splitter formula generated by formula splitter step solver.
		if (result == null) {
			// We are using the evaluator to deal with getting the literals from the formula splitter.
			Step splitterEvaluationStep = currentFormulaSplitterEvaluatorStepSolver.step(context);
            if (splitterEvaluationStep.itDepends()) {
            	ExpressionStepSolverToLiteralSplitterStepSolverAdapter ifTrue = this.clone();
            	ifTrue.currentFormulaSplitterEvaluatorStepSolver = splitterEvaluationStep.getStepSolverForWhenSplitterIs(true);
            	ExpressionStepSolverToLiteralSplitterStepSolverAdapter ifFalse = this.clone();
            	ifFalse.currentFormulaSplitterEvaluatorStepSolver = splitterEvaluationStep.getStepSolverForWhenSplitterIs(false);
                // note that cloning will preserve currentFssStep for the sequel step solvers
                // this matters because it contains the sequels for currentFss,
                // which we will need below when we finish evaluating the formula splitter
                result = new ItDependsOn(splitterEvaluationStep.getSplitterLiteral(), 
                		splitterEvaluationStep.getContextSplittingWhenSplitterIsLiteral(), 
                		ifTrue, ifFalse);
            }
            else {                          
            	// we have a value for the formula splitter under the current context
                if (splitterEvaluationStep.getValue().equals(Expressions.TRUE)) {
                    // now we know that the fss' formula splitter is true, so we use its continuation as the next current fss
                	currentFormulaSplitterStepSolver = currentFormulaSplitterStepSolverStep.getStepSolverForWhenSplitterIs(true);
                }
                else {
                	currentFormulaSplitterStepSolver = currentFormulaSplitterStepSolverStep.getStepSolverForWhenSplitterIs(false);
                }
                // Set to null as we are now processed all the literals in the last formula splitter
                // we received and we want to delegate back to the formula splitter step solver to
                // compute the next step.
                currentFormulaSplitterEvaluatorStepSolver = null;
                // Compute the next step now using the formula splitter step solver.
                result = step(context);
            }
		}
		
		return result;
	}
	
	@Override
	public String toString() {
		return "ExpressionStepSolverToLiteralSplitterStepSolverAdapter for " + this.currentFormulaSplitterStepSolver;
	}
}