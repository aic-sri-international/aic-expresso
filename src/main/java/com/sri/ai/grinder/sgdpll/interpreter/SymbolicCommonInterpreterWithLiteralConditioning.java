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
package com.sri.ai.grinder.sgdpll.interpreter;

import static com.sri.ai.grinder.sgdpll.core.solver.AbstractQuantifierEliminationStepSolver.makeEvaluator;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.core.constraint.CompleteMultiVariableConstraint;

/**
 * An extension of {@link SymbolicCommonInterpreter} whose results
 * are decision trees on its literals.
 *
 * @author braz
 *
 */
@Beta
public class SymbolicCommonInterpreterWithLiteralConditioning extends SymbolicCommonInterpreter {

	/**
	 * Constructs {@link SymbolicCommonInterpreterWithLiteralConditioning} with a constraint theory,
	 * which conditions on literals but does <i>not</i> simplifying literals according to contextual constraint.
	 * @param constraintTheory
	 */
	public SymbolicCommonInterpreterWithLiteralConditioning(ConstraintTheory constraintTheory) {
		super(constraintTheory);
	}

	/**
	 * We override this interpreter to go the extra mile and condition on literals in the
	 * result of super's interpretation, since we expect a symbolic interpreter
	 * to condition on the literals in order to make the expression succinct.
	 */
	@Override public Expression apply(Expression expression, Context context) {
		Expression interpretationResult = super.apply(expression, context);
		Context trueConstraint = context.conjoin(new CompleteMultiVariableConstraint(getConstraintTheory()), context);
		SymbolicCommonInterpreter simplifier =
				new SymbolicCommonInterpreter(getConstraintTheory());
		// TODO: given that we are only using the top simplifier from the simplifier above,
		// we don't need the "simplify given constraint" setting.
		// Not touching it now because I am in the middle of something else,
		// but should set it off, or just remove the functionality altogether if it is not used anywhere else.
		ContextDependentExpressionProblemStepSolver evaluator
		= makeEvaluator(interpretationResult, simplifier.getTopSimplifier());
		Expression result = evaluator.solve(trueConstraint, context);
		return result;
	}
}