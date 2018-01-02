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
package com.sri.ai.grinder.sgdpllt.core.solver;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleQuantifierEliminationProblem;

/**
 * A {@link AbstractQuantifierEliminationStepSolver} for quantifiers based on a group, and with body in which the index occurs in literals only.
 * This step solver first provides all the literals in the body as {@link ExpressionStepSolver#ItDepends} steps.
 * If at any point the constraint becomes unsatisfiable, the group's identity element is returned
 * (the above is all done by {@link AbstractQuantifierEliminationStepSolver}.
 * If we reach a point in which there are no further undefined literals in the body and the constraint is satisfiable,
 * {@link SingleQuantifierEliminationForIndexFreeBody}.
 * 
 * @author braz
 *
 */
@Beta
public class QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver extends AbstractQuantifierEliminationStepSolver {

	public QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver(SingleQuantifierEliminationProblem problem) {
		super(problem);
	}

	@Override
	protected Step eliminateQuantifierForLiteralFreeBody(Expression literalFreeBody, Context context) {
		SingleQuantifierEliminationForIndexFreeBody solver = makeSolver(literalFreeBody, context);
		Expression result = solver.eliminateQuantifierForLiteralFreeBody();
		return new Solution(result);
	}

	private SingleQuantifierEliminationForIndexFreeBody makeSolver(Expression literalFreeBody, Context context) {
		SingleQuantifierEliminationProblem problemWithLiteralFreeBody = getProblem().makeWithNewBody(literalFreeBody);
		SingleQuantifierEliminationForIndexFreeBody solver = new SingleQuantifierEliminationForIndexFreeBody(problemWithLiteralFreeBody, context);
		return solver;
	}	
}