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

import static com.sri.ai.expresso.helper.Expressions.ZERO;

import java.util.ArrayList;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Constraint;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;

/**
 * An abstract step solver for model counting step solvers.
 * <p>
 * It extends {@link AbstractExpressionWithPropagatedLiteralsStepSolver}
 * and provides propagated literals and propagated CNF identical to the ones provided by the satisfiability
 * step solver, which is obtained through the theory's
 * {@link Theory#getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint, Context)}.
 * However, it is important that the satisfiability step solver provided by the theory
 * be an instance of {@link AbstractExpressionWithPropagatedLiteralsStepSolver},
 * because {@link #getPropagatedCNF(Context)} is delegated to the satisfiability step solver's
 * {@link AbstractExpressionWithPropagatedLiteralsStepSolver#getPropagatedCNF(Context)}
 * method.
 * An error is thrown if this assumption is violated.
 * <p>
 * This class already defines that the model count is zero if the constraint is not satisfiable according to the
 * satisfiability step solver due to propagated literals and propagated CNF not being satisfied by the context.
 * Extensions still need to provide methods
 * to compute the final solution when propagated literals and propagated CNF are satisfied.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver extends AbstractExpressionWithPropagatedLiteralsStepSolver {

	public AbstractModelCountingWithPropagatedLiteralsImportedFromSatisfiabilityStepSolver(Constraint constraint) {
		super(constraint);
	}
	
	@Override
	protected Expression getSolutionExpressionGivenContradiction() {
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
	protected ArrayList<ArrayList<Expression>> getPropagatedCNF(Context context) {
		Theory theory = getConstraint().getTheory();
		ExpressionStepSolver satisfiability =
				theory.getSingleVariableConstraintSatisfiabilityStepSolver(getConstraint(), context);
		if (satisfiability == null) {
			throw new Error("No solver present for solving satisfiability of " + getConstraint().getVariable());
		}
		AbstractExpressionWithPropagatedLiteralsStepSolver satisfiabilityWithPropagatedLiterals;
		try {
			satisfiabilityWithPropagatedLiterals =
					(AbstractExpressionWithPropagatedLiteralsStepSolver) satisfiability;
		} catch (ClassCastException e) {
			throw new Error(this.getClass() + 
					" can only be used with theories providing satisfiability context-dependent step solvers"
					+ " that are extensions of " + AbstractExpressionWithPropagatedLiteralsStepSolver.class
					+ ", but theory " + theory.getClass() + " provided instead an instance of"
					+ satisfiability.getClass());
		}
		return satisfiabilityWithPropagatedLiterals.getPropagatedCNF(context);
	}

	/**
	 * Throws an Error since this method should not be used due to the fact that {@link #getPropagatedCNF}
	 * delegates to satisfiability step solver.
	 */
	@Override
	final protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(Context context) {
		throw new Error("Should not be invoked because this class redirects computation to propagated CNF to an internal satisfiability problem");
	}
}