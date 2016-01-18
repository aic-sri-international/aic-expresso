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
package com.sri.ai.grinder.sgdpll2.core.solver;

import static com.sri.ai.expresso.helper.Expressions.ZERO;

import java.util.ArrayList;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;

/**
 * An abstract step solver for model counting step solvers.
 * <p>
 * It extends {@link AbstractNumericalProblemWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver}
 * and provides propagated literals and propagated CNF identical to the ones provided by the satisfiability
 * step solver, which is obtained through the constraint theory's
 * {@link ConstraintTheory#getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint, RewritingProcess)}.
 * However, it is important that the satisfiability step solver provided by the constraint theory
 * be an instance of {@link AbstractContextDependentProblemWithPropagatedLiteralsStepSolver},
 * because {@link #getPropagatedCNF(RewritingProcess)} is delegated to the satisfiability step solver's
 * {@link AbstractContextDependentProblemWithPropagatedLiteralsStepSolver#getPropagatedCNF(RewritingProcess)}
 * method.
 * An error is thrown if this assumption is violated.
 * <p>
 * This class already defines that the model count is zero if the constraint is not satisfiable according to the
 * satisfiability step solver due to propagated literals and propagated CNF not being satisfied by the contextual constraint.
 * Extensions still need to provide methods
 * to compute the final solution when propagated literals and propagated CNF are satisfied.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver extends AbstractNumericalProblemWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver {

	public AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver(Constraint2 constraint) {
		super(constraint);
	}
	
	@Override
	protected Expression solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied() {
		return ZERO;
	}

	@Override
	public SingleVariableConstraint getConstraint() {
		return (SingleVariableConstraint) super.getConstraint();
	}
	
	@Override
	protected boolean usingDefaultImplementationOfMakePropagatedCNF() {
		return false;
	}

	/**
	 * This is overridden to re-use satisfiability's propagated CNF.
	 * It overrides caching done by overridden method, but that's fine since satisfiability should be doing that already.
	 */
	@Override
	protected ArrayList<ArrayList<Expression>> getPropagatedCNF(RewritingProcess process) {
		ConstraintTheory constraintTheory = getConstraint().getConstraintTheory();
		ContextDependentExpressionProblemStepSolver satisfiability =
				constraintTheory.getSingleVariableConstraintSatisfiabilityStepSolver(getConstraint(), process);
		AbstractContextDependentProblemWithPropagatedLiteralsStepSolver satisfiabilityWithPropagatedLiterals;
		try {
			satisfiabilityWithPropagatedLiterals =
					(AbstractContextDependentProblemWithPropagatedLiteralsStepSolver) satisfiability;
		} catch (ClassCastException e) {
			throw new Error(this.getClass() + 
					" can only be used with theories providing satisfiability context-dependent step solvers"
					+ " that are extensions of " + AbstractContextDependentProblemWithPropagatedLiteralsStepSolver.class
					+ ", but theory " + constraintTheory.getClass() + " provided instead an instance of"
					+ satisfiability.getClass());
		}
		return satisfiabilityWithPropagatedLiterals.getPropagatedCNF(process);
	}

	/**
	 * Throws an Error since this method should not be used due to the fact that {@link #getPropagatedCNF}
	 * delegates to satisfiability step solver.
	 */
	@Override
	final protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(RewritingProcess process) {
		throw new Error("Should not be invoked because this class redirects computation to propagated CNF to an internal satisfiability problem");
	}
}