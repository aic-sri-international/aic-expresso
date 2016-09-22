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
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;

/**
 * An adapter that allows ExpressionFormulaSplitterStepSolvers to be used in
 * situations where ExpressionLiteralSplitterStepSolvers are required (i.e.
 * splitting on literals is assumed).
 * 
 * @author oreilly
 *
 */
@Beta
public class ExpressionStepSolverToLiteralSplitterStepSolverAdapter implements ExpressionLiteralSplitterStepSolver {

	private ExpressionStepSolver formulaSplitterStepSolver;

	public ExpressionStepSolverToLiteralSplitterStepSolverAdapter(
			ExpressionStepSolver formulaSplitterStepSolver) {
		if (formulaSplitterStepSolver instanceof ExpressionLiteralSplitterStepSolver) {
			throw new IllegalArgumentException("You do not pass an ExpressionLiteralSplitterStepSolver to this adapter, i.e. recursive calls will occur");
		}
		this.formulaSplitterStepSolver = formulaSplitterStepSolver;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver clone() {

		ExpressionStepSolver formulaSplitterStepSolverClone = formulaSplitterStepSolver.clone();

		ExpressionLiteralSplitterStepSolver result = new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(
				formulaSplitterStepSolverClone);

		return result;
	}

	@Override
	public Step step(Context context) {
		ExpressionLiteralSplitterStepSolver.Step result;
		ExpressionStepSolver.Step formulaSolverStep = formulaSplitterStepSolver.step(context);
		if (formulaSolverStep.itDepends()) {
// TODO - support formulas in addition to literals.			
			// We need to wrap the ItDepends result sub-solvers in adapters as well.
			result = new ExpressionLiteralSplitterStepSolver.ItDependsOn(formulaSolverStep.getSplitter(),
						formulaSolverStep.getContextSplittingWhenSplitterIsLiteral(),
						new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(formulaSolverStep.getStepSolverForWhenSplitterIsTrue()),
						new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(formulaSolverStep.getStepSolverForWhenSplitterIsFalse()));
		}
		else {
			result = new ExpressionLiteralSplitterStepSolver.Solution(formulaSolverStep.getValue());
		}
		return result;
	}
}