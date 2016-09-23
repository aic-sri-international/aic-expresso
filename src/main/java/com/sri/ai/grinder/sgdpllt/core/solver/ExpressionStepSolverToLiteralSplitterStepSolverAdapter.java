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
package com.sri.ai.grinder.sgdpllt.core.solver;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;

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

	private static final boolean _supportGeneralFormulas = false;
	//
	private ExpressionStepSolver currentFormulaSplitterStepSolver;
	private EvaluatorStepSolver currentFormulaSplitterEvaluatorStepSolver;

	public ExpressionStepSolverToLiteralSplitterStepSolverAdapter(
			ExpressionStepSolver formulaSplitterStepSolver) {
		if (formulaSplitterStepSolver instanceof ExpressionLiteralSplitterStepSolver) {
			throw new IllegalArgumentException("You do not pass an ExpressionLiteralSplitterStepSolver to this adapter.");
		}
		this.currentFormulaSplitterStepSolver = formulaSplitterStepSolver;
	}

	@Override
	public ExpressionStepSolverToLiteralSplitterStepSolverAdapter clone() {

		ExpressionStepSolver currentFormulaSplitterStepSolverClone = currentFormulaSplitterStepSolver.clone();
		EvaluatorStepSolver currentFormulaSplitterEvaluatorStepSolverClone = currentFormulaSplitterEvaluatorStepSolver.clone();
		
		ExpressionStepSolverToLiteralSplitterStepSolverAdapter result = new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(
				currentFormulaSplitterStepSolverClone);
		result.currentFormulaSplitterEvaluatorStepSolver = currentFormulaSplitterEvaluatorStepSolverClone;

		return result;
	}
	
	@Override
	public Step step(Context context) {
		Step result;
		if (_supportGeneralFormulas) {
			result = stepSupportGeneralFormulaSplitters(context);
		}
		else {
			result = stepSupportOnlyLiteralSplitters(context);
		}
		return result;
	}
	
	//
	//
	protected Step stepSupportGeneralFormulaSplitters(Context context) {
		while (true) {
	        // so we need one:
			ExpressionStepSolver.Step currentFormulaSplitterStepSolverStep = currentFormulaSplitterStepSolver.step(context);
	        if (!currentFormulaSplitterStepSolverStep.itDepends()) { 
	        	// is a final solution, return it
	            return new ExpressionLiteralSplitterStepSolver.Solution(currentFormulaSplitterStepSolverStep.getValue());
	        }
	        else {
	        	// may not be a literal her
	            Expression formulaSplitter = currentFormulaSplitterStepSolverStep.getSplitter(); 
	            currentFormulaSplitterEvaluatorStepSolver = new EvaluatorStepSolver(formulaSplitter);

	            Step splitterEvaluationStep = currentFormulaSplitterEvaluatorStepSolver.step(context);
	            if (splitterEvaluationStep.itDepends()) {
	            	ExpressionStepSolverToLiteralSplitterStepSolverAdapter ifTrue = this.clone();
	            	ifTrue.currentFormulaSplitterEvaluatorStepSolver = (EvaluatorStepSolver) splitterEvaluationStep.getStepSolverForWhenSplitterIsTrue();
	            	ExpressionStepSolverToLiteralSplitterStepSolverAdapter ifFalse = this.clone();
	            	ifTrue.currentFormulaSplitterEvaluatorStepSolver = (EvaluatorStepSolver) splitterEvaluationStep.getStepSolverForWhenSplitterIsFalse();
	                // note that cloning will preserve currentFssStep for the sequel step solvers
	                // this matters because it contains the sequels for currentFss,
	                // which we will need below when we finish evaluating the formula splitter
	                return new ItDependsOn(splitterEvaluationStep.getSplitterLiteral(), splitterEvaluationStep.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
	            }
	            else {
	            	// we have a value for the formula splitter under the current context
	                if (splitterEvaluationStep.getValue().equals(Expressions.TRUE)) {
	                    // now we know that the fss' formula splitter is true, so we use its continuation as the next current fss
	                	currentFormulaSplitterStepSolver = currentFormulaSplitterStepSolverStep.getStepSolverForWhenSplitterIsTrue();
	                }
	                else {
	                	currentFormulaSplitterStepSolver = currentFormulaSplitterStepSolverStep.getStepSolverForWhenSplitterIsFalse();
	                }
	            }
	        }
	    }
	}
	
	protected Step stepSupportOnlyLiteralSplitters(Context context) {
		ExpressionLiteralSplitterStepSolver.Step result;
		ExpressionStepSolver.Step formulaSolverStep = currentFormulaSplitterStepSolver.step(context);
		if (formulaSolverStep.itDepends()) {
			if (context.getTheory().isLiteral(formulaSolverStep.getSplitter(), context)) {		
				// We need to wrap the ItDepends result sub-solvers in adapters as well.
				result = new ExpressionLiteralSplitterStepSolver.ItDependsOn(formulaSolverStep.getSplitter(),
							formulaSolverStep.getContextSplittingWhenSplitterIsLiteral(),
							new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(formulaSolverStep.getStepSolverForWhenSplitterIsTrue()),
							new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(formulaSolverStep.getStepSolverForWhenSplitterIsFalse()));
			}
			else {
				// The splitter is a complex formula (i.e. not a literal).	
				throw new UnsupportedOperationException("Complex splitter formulas are not supported: "+formulaSolverStep.getSplitter());
			}
		}
		else {
			result = new ExpressionLiteralSplitterStepSolver.Solution(formulaSolverStep.getValue());
		}
		return result;
	}
}