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
package com.sri.ai.grinder.sgdpll2.theory.inequality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;
import com.sri.ai.grinder.sgdpll2.core.solver.AbstractBooleanProblemWithPropagatedAndDefiningLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver;

/**
 * A {@link AbstractBooleanProblemWithPropagatedAndDefiningLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver} for a {@link SingleVariableInequalityConstraint}.
 * <p>
 * The solution is guaranteed to be either a boolean constant or a difference arithmetic expression with 0 on the right-hand side.
 * 
 * @author braz
 *
 */
@Beta
public class SatisfiabilityOfSingleVariableInequalityConstraintStepSolver implements ContextDependentExpressionProblemStepSolver {

	private Constraint2 constraint;
	private ContextDependentExpressionProblemStepSolver modelCounting;
	private boolean solutionIsTrue = false;
	private boolean solutionIsFalse = false;
	
	public SatisfiabilityOfSingleVariableInequalityConstraintStepSolver(SingleVariableInequalityConstraint constraint) {
		this.constraint = constraint;
		this.modelCounting = new ModelCountingOfSingleVariableInequalityConstraintStepSolver(constraint);
	}

	@Override
	public SatisfiabilityOfSingleVariableInequalityConstraintStepSolver clone() {
		SatisfiabilityOfSingleVariableInequalityConstraintStepSolver result = null;
		try {
			result = (SatisfiabilityOfSingleVariableInequalityConstraintStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {
		if (solutionIsTrue) {
			return new Solution(TRUE);
		}
		else if (solutionIsFalse) {
			return new Solution(FALSE);
		}
		
		SolutionStep modelCountingStep = modelCounting.step(contextualConstraint, process);
		if (modelCountingStep == null) {
			return null;
		}
		else if (modelCountingStep.itDepends()) {
			// satisfiability depends on the same expression, but sub-step solvers must be satisfiability step solvers.
			return new ItDependsOn(modelCountingStep.getLiteral(), modelCountingStep.getConstraintSplitting(), this, this);
		}

		SolutionStep result;
		
		Expression satisfiable;
		satisfiable = apply(GREATER_THAN, modelCountingStep.getValue(), ZERO);
		Expression simplifiedSatisfiable = constraint.getConstraintTheory().simplify(satisfiable, process);

		// result = new Solution(simplifiedSatisfiable); // used to be like this; not good, for if simplifiedSatisfiable is inconsistent with contextual constraint, it is not equal to 'false', becoming incomplete even if the contextual constraint is complete.

		ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, simplifiedSatisfiable, process);
		if (split.getResult() == ConstraintSplitting.Result.CONSTRAINT_IS_CONTRADICTORY) {
			return null;
		}

		switch (split.getResult()) {
		case LITERAL_IS_TRUE:
			result = new Solution(TRUE);
			break;
		case LITERAL_IS_FALSE:
			result = new Solution(FALSE);
			break;
		case LITERAL_IS_UNDEFINED:
			SatisfiabilityOfSingleVariableInequalityConstraintStepSolver ifTrue = clone();
			ifTrue.solutionIsTrue = true;
			SatisfiabilityOfSingleVariableInequalityConstraintStepSolver ifFalse = clone();
			ifFalse.solutionIsFalse = true;
			result = new ItDependsOn(simplifiedSatisfiable, split, ifTrue, ifFalse);
			break;
		default:
			throw new Error("Unrecognized result");
		}

		return result;
	}
}