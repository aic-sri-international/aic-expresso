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
package com.sri.ai.grinder.sgdpllt.theory.propositional;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TWO;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver;

/**
 * A {@link AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver} for
 * a {@link SingleVariablePropositionalConstraint}.
 * As such, it is required to provide a way of computing a solution when all propagated literals and propagated CNF are satisfied by the context.
 * <p>
 * if the propagated literals and propagated CNF are defined by the context, then a solution is already defined.
 * The solution is simply 1 if the variable is constrained to be true or false, or 2 otherwise.
 * 
 * @author braz
 *
 */
@Beta
public class ModelCountingOfSingleVariablePropositionalConstraintStepSolver extends AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver {

	public ModelCountingOfSingleVariablePropositionalConstraintStepSolver(SingleVariablePropositionalConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public SingleVariablePropositionalConstraint getConstraint() {
		return (SingleVariablePropositionalConstraint) super.getConstraint();
	}

	@Override
	protected SolverStep solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(
			Context context) {
		
		boolean variableIsNotConstrained = 
				getConstraint().getPositiveNormalizedAtoms().isEmpty() && getConstraint().getNegativeNormalizedAtoms().isEmpty();
		
		Expression solutionExpression = variableIsNotConstrained? TWO : ONE;
		
		return new Solution(solutionExpression);
	}
}