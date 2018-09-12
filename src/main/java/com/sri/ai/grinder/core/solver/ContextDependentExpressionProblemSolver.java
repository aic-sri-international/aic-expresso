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
package com.sri.ai.grinder.core.solver;

import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

/**
 * Solves a {@link ExpressionLiteralSplitterStepSolver} by successively conditioning the context on provided splitters.
 * 
 * For those familiar with the "Probabilistic Inference Modulo Theories" paper on IJCAI-16,
 * this corresponds to repeatedly applying if-splitting to a problem
 * until it is solved.
 * Note that quantifier-splitting only happens for quantifier elimination,
 * and at the level of {@link ExpressionLiteralSplitterStepSolver}
 * there is not necessarily a quantifier involved.
 * This is currently done at the level of {@link SingleQuantifierEliminationStepSolver}.
 * 
 * @author braz
 *
 */
@Beta
public class ContextDependentExpressionProblemSolver {

	private boolean interrupted = false;
	
	public void interrupt() {
		interrupted = true;
	}
	
	/**
	 * Returns the solution for a problem using a step solver.
	 * @param stepSolver
	 * @param context
	 * @return
	 */
	public Expression solve(ExpressionLiteralSplitterStepSolver stepSolver, Context context) {
		
		return explanationBlock("Going to solve ", stepSolver, " under ", context, code(() -> {

			if (interrupted) {
				throw new Error("Solver interrupted.");
			}

			Expression result;
			ExpressionLiteralSplitterStepSolver.Step step = stepSolver.step(context);
			if (step.itDepends()) {
				result = solveSplittedProblem(step);
			}
			else {
				result = getDeterminedSolution(step);
			}

			return result;

		}),	"Solution is ", RESULT);
	}

	private Expression solveSplittedProblem(ExpressionLiteralSplitterStepSolver.Step step) {
		explain("Problem depends on ", step);
		Expression splitter = step.getSplitter();
		ContextSplitting split = (ContextSplitting) step.getContextSplittingWhenSplitterIsLiteral();
		myAssert(() -> split.isUndefined(), () -> "Context splitting is supposed to be conditional but result contradicts that: " + split.getResult());
		Expression subSolution1 = solve(step.getStepSolverForWhenSplitterIsTrue (), split.getConstraintAndLiteral());
		Expression subSolution2 = solve(step.getStepSolverForWhenSplitterIsFalse(), split.getConstraintAndLiteralNegation());
		Expression result = IfThenElse.make(splitter, subSolution1, subSolution2, true);
		return result;
	}
	
	private Expression getDeterminedSolution(ExpressionLiteralSplitterStepSolver.Step step) {
		explain("Problem's solution is already determined to be ", step);
		Expression result = step.getValue();
		return result;
	}

	public static Expression staticSolve(ExpressionLiteralSplitterStepSolver stepSolver, Context context) {
		ContextDependentExpressionProblemSolver solver = new ContextDependentExpressionProblemSolver();
		Expression result = solver.solve(stepSolver, context);
		return result;
	}
}