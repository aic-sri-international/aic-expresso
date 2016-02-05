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
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;

/**
 * A {@link AbstractQuantifierEliminationStepSolver} for quantifiers based on a group, and with body in which the index occurs in literals only.
 * This step solver first provides all the literals in the body as {@link ContextDependentExpressionProblemStepSolver#ItDepends} steps.
 * If at any point the constraint becomes unsatisfiable, the group's identity element is returned
 * (the above is all done by {@link AbstractQuantifierEliminationStepSolver}.
 * If we reach a point in which there are no further undefined literals in the body and the constraint is satisfiable,
 * one of two things happens:
 * <ul>
 * <li> if the group is idempotent, simplify group's identity.
 * <li> if the group is not idempotent, 
 * applies {@link AssociativeCommutativeGroup#addNTimes(Expression, Expression, RewritingProcess)} to
 * the literal-free body and {@link SingleVariableConstraint#modelCount(Constraint2, RewritingProcess)},
 * followed by {@link ConstraintTheory#simplify(Expression, RewritingProcess)}.
 * No if then else externalization is performed.
 * </ul>
 * 
 * 
 * @author braz
 *
 */
@Beta
public class QuantifierEliminationOnBodyWithIndexInLiteralsOnlyStepSolver extends AbstractQuantifierEliminationStepSolver {

	public QuantifierEliminationOnBodyWithIndexInLiteralsOnlyStepSolver(AssociativeCommutativeGroup group, Simplifier simplifier, SingleVariableConstraint indexConstraint, Expression body) {
		super(group, simplifier, indexConstraint, body);
	}

	@Override
	protected AbstractQuantifierEliminationStepSolver makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint) {
		AbstractQuantifierEliminationStepSolver result = clone();
		result.indexConstraint = newIndexConstraint;
		return result;
	}

	@Override
	protected SolutionStep eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(
			Constraint2 contextualConstraint, SingleVariableConstraint indexConstraint, Expression literalFreeBody, RewritingProcess process) {
		
		Expression result;
		if (getGroup().isIdempotent()) {
			Expression conditionForSatisfiability = indexConstraint.satisfiability(contextualConstraint, process);
			result = IfThenElse.makeWithoutConditionalCondition(conditionForSatisfiability, literalFreeBody, getGroup().additiveIdentityElement());
		}
		else {
			result = getGroup().addNTimes(literalFreeBody, indexConstraint.modelCount(contextualConstraint, process), process);
			result = getConstraintTheory().simplify(result, process);
		}
		
		return new Solution(result);
	}
}