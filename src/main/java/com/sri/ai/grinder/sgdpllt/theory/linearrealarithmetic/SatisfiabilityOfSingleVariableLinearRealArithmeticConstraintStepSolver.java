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
package com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.sgdpllt.theory.base.ExpressionConditionedOnLiteralSolutionStep.stepDependingOnLiteral;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractBooleanWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver;

/**
 * A {@link AbstractBooleanWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver} for a {@link SingleVariableLinearRealArithmeticConstraint}.
 * <p>
 * The solution is guaranteed to be either a boolean constant or a difference arithmetic expression with 0 on the right-hand side.
 * 
 * @author braz
 *
 */
@Beta
public class SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver implements ExpressionLiteralSplitterStepSolver {

	private SingleVariableLinearRealArithmeticConstraint constraint;
	private ExpressionLiteralSplitterStepSolver modelCounting;
	
	public SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver(SingleVariableLinearRealArithmeticConstraint constraint) {
		this.constraint = constraint;
		this.modelCounting = new ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver(constraint);
	}
	
	public SingleVariableLinearRealArithmeticConstraint getConstraint() {
		return constraint;
	}

	@Override
	public SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver clone() {
		SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver result = null;
		try {
			result = (SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	@Override
	public SolverStep step(Context context) {
		SolverStep modelCountingStep = modelCounting.step(context);
		if (modelCountingStep == null) {
			return null;
		}
		else if (modelCountingStep.itDepends()) {
			// satisfiability depends on the same expression, but sub-step solvers must be satisfiability step solvers.
			SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver ifTrue = clone();
			ifTrue.modelCounting = modelCountingStep.getStepSolverForWhenSplitterIsTrue();
			SatisfiabilityOfSingleVariableLinearRealArithmeticConstraintStepSolver ifFalse = clone();
			ifFalse.modelCounting = modelCountingStep.getStepSolverForWhenSplitterIsFalse();
			return new ItDependsOn(modelCountingStep.getSplitter(), modelCountingStep.getContextSplitting(), ifTrue, ifFalse);
		}

		SolverStep result;
		
		Expression satisfiable;
		satisfiable = apply(GREATER_THAN, modelCountingStep.getValue(), ZERO);
		Expression simplifiedSatisfiable = constraint.getTheory().simplify(satisfiable, context);

		// result = new Solution(simplifiedSatisfiable); // used to be like this; not good, for if simplifiedSatisfiable is inconsistent with context, it is not equal to 'false', becoming incomplete even if the context is complete.

		result = stepDependingOnLiteral(simplifiedSatisfiable, TRUE, FALSE, context);

		return result;
	}
}