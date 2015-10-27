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

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.theory.equality.SingleVariableEqualityConstraint;

/**
 * An abstract {@link AbstractModelCountingOfConstraintStepSolver} for a {@link SingleVariableEqualityConstraint}
 * that assumes the only propagated CNF will be those for satisfiability.
 * <p>
 * This class relies on the constraint theory providing a {@link ContextDependentProblemStepSolver} for satisfiability
 * (through {@link ConstraintTheory#getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint, RewritingProcess)})
 * that is an extension of {@link AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver},
 * because it delegates the generation of the propagated CNF to
 * {@link AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver#getPropagatedCNF(RewritingProcess)}.
 * An error is thrown if this assumption is violated.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractModelCountingOfSingleVariableWithPropagatedAndDefiningLiteralsConstraintStepSolver extends AbstractModelCountingWithPropagatedAndDefiningLiteralsStepSolver {

	public AbstractModelCountingOfSingleVariableWithPropagatedAndDefiningLiteralsConstraintStepSolver(SingleVariableConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public SingleVariableConstraint getConstraint() {
		return (SingleVariableConstraint) super.getConstraint();
	}
	
	@Override
	protected boolean indicateWhetherGetPropagatedCNFWillBeOverridden() {
		return true;
	}

	@Override
	public Iterable<Iterable<Expression>> getPropagatedCNF(RewritingProcess process) {
		ConstraintTheory constraintTheory = getConstraint().getConstraintTheory();
		ContextDependentProblemStepSolver satisfiability =
				constraintTheory.getSingleVariableConstraintSatisfiabilityStepSolver(getConstraint(), process);
		AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver satisfiabilityWithPropagatedLiterals;
		try {
			satisfiabilityWithPropagatedLiterals =
					(AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver) satisfiability;
		} catch (ClassCastException e) {
			throw new Error(this.getClass() + 
					" can only be used with theories providing satisfiability context-dependent step solvers"
					+ " that are extensions of " + AbstractContextDependentProblemWithPropagatedAndDefiningLiteralsStepSolver.class
					+ ", but theory " + constraintTheory.getClass() + " provided instead an instance of"
					+ satisfiability.getClass());
		}
		return satisfiabilityWithPropagatedLiterals.getPropagatedCNF(process);
	}
}