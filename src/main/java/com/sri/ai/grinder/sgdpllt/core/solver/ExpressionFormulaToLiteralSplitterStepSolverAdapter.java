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
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionFormulaSplitterStepSolver;
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
public class ExpressionFormulaToLiteralSplitterStepSolverAdapter implements ExpressionLiteralSplitterStepSolver {

	private ExpressionFormulaSplitterStepSolver formulaSplitterStepSolver;

	public ExpressionFormulaToLiteralSplitterStepSolverAdapter(
			ExpressionFormulaSplitterStepSolver formulaSplitterStepSolver) {
		this.formulaSplitterStepSolver = formulaSplitterStepSolver;
	}

	@Override
	public Expression solve(Context context) {
		Expression result = formulaSplitterStepSolver.solve(context);
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver clone() {

		ExpressionFormulaSplitterStepSolver formulaSplitterStepSolverClone = (ExpressionFormulaSplitterStepSolver) formulaSplitterStepSolver
				.clone();

		ExpressionLiteralSplitterStepSolver result = new ExpressionFormulaToLiteralSplitterStepSolverAdapter(
				formulaSplitterStepSolverClone);

		return result;
	}

	@Override
	public SolverStep step(Context context) {
		SolverStep result = formulaSplitterStepSolver.step(context);
		if (result.itDepends()) {
// TODO - support formulas in addition to literals.			
			// We need to wrap the ItDepends result sub-solvers in adapters as well.
			result = new ItDependsOn(result.getSplitter(),
						result.getContextSplitting(),
						new ExpressionFormulaToLiteralSplitterStepSolverAdapter((ExpressionFormulaSplitterStepSolver) result.getStepSolverForWhenSplitterIsTrue()),
						new ExpressionFormulaToLiteralSplitterStepSolverAdapter((ExpressionFormulaSplitterStepSolver) result.getStepSolverForWhenSplitterIsFalse()));
		}
		return result;
	}
}