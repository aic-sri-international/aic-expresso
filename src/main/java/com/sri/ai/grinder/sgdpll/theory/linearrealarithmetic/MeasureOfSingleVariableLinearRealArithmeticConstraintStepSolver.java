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
package com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.arrayList;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.theory.numeric.AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver;

/**
 * A {@link AbstractSingleVariableLinearRealArithmeticConstraintFeasibilityRegionStepSolver}
 * computing the measure of the set of values for variable satisfying the constraints.
 * <p>
 * The solution is guaranteed to be either a numerical constant, or
 * a conditional of the form {@code if <satisfiability condition> then <measure> else 0}.
 * 
 * @author braz
 *
 */
@Beta
public class MeasureOfSingleVariableLinearRealArithmeticConstraintStepSolver extends AbstractSingleVariableLinearRealArithmeticConstraintFeasibilityRegionStepSolver {

	public MeasureOfSingleVariableLinearRealArithmeticConstraintStepSolver(SingleVariableLinearRealArithmeticConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public MeasureOfSingleVariableLinearRealArithmeticConstraintStepSolver clone() {
		return (MeasureOfSingleVariableLinearRealArithmeticConstraintStepSolver) super.clone();
	}

	@Override
	protected Expression getSolutionExpressionGivenContradiction() {
		return ZERO;
	}

	@Override
	protected SolutionStep getSolutionStepAfterBoundsAreCheckedForFeasibility(Expression lowerBound, Expression upperBound, AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver sequelBase, Context context) {
		Expression boundsDifference = applyAndSimplify(MINUS, arrayList(upperBound, lowerBound), context);
		return new Solution(boundsDifference);
	}

	@Override
	public boolean unboundedVariableProducesShortCircuitSolution() {
		return true;
	}

	@Override
	public Expression getSolutionExpressionForUnboundedVariables() {
		return INFINITY;
	}

	@Override
	public Expression getSolutionExpressionForBoundVariable() {
		return ZERO; // because this is actually an integral, binding the variable renders the integral equal to zero
	}
}