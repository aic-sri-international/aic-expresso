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
package com.sri.ai.grinder.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.theory.numeric.AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver;

/**
 * A {@link AbstractSingleVariableLinearRealArithmeticConstraintFeasibilityRegionStepSolver}
 * computing the number of values for variable satisfying the constraints.
 * <p>
 * The solution is guaranteed to be either a numerical constant, or
 * a conditional of the form {@code if <satisfiability condition> then <model count> else 0}.
 * 
 * @author braz
 *
 */
@Beta
public class ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver extends AbstractSingleVariableLinearRealArithmeticConstraintFeasibilityRegionStepSolver {

	public ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver(SingleVariableLinearRealArithmeticConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver clone() {
		return (ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver) super.clone();
	}

	@Override
	protected Expression getSolutionExpressionGivenContradiction() {
		return ZERO;
	}

	@Override
	protected Step getSolutionStepAfterBoundsAreCheckedForFeasibility(Expression lowerBound, Expression upperBound, AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver sequelBase, Context context) {
		return new Solution(INFINITY);
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
		return ONE;
	}
}