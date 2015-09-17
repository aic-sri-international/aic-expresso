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
package com.sri.ai.grinder.sgdpll2.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.getFirstOrNull;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.MultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.solver.ContextDependentProblemSolver;

/**
 * An {@link Constraint} on multiple variables,
 * with the ability to ensure all single-variable constraints that are part of it
 * have a property determined by a {@link ContextDependentProblemStepSolver}.
 * 
 * @author braz
 *
 */
@Beta
public class MultiVariableConstraintWithCheckedProperty extends AbstractExpressionWrapper implements MultiVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private ConstraintTheory constraintTheory;
	private MultiVariableConstraintWithCheckedProperty contextualConstraint;
	private SingleVariableConstraint singleVariableConstraint;
	private Function<SingleVariableConstraint, ContextDependentProblemStepSolver> contextDependentProblemMaker;
	
	public MultiVariableConstraintWithCheckedProperty(
			ConstraintTheory constraintTheory, Function<SingleVariableConstraint, ContextDependentProblemStepSolver> contextDependentProblemMaker) {
		this.constraintTheory = constraintTheory;
		this.contextDependentProblemMaker = contextDependentProblemMaker;
		this.contextualConstraint = null;
		this.singleVariableConstraint = null;
	}
	
	private MultiVariableConstraintWithCheckedProperty(
			MultiVariableConstraintWithCheckedProperty contextualConstraint, SingleVariableConstraint singleVariableConstraint) {
		this.constraintTheory = contextualConstraint.getConstraintTheory();
		this.contextDependentProblemMaker = contextualConstraint.contextDependentProblemMaker;
		this.contextualConstraint = contextualConstraint;
		this.singleVariableConstraint = singleVariableConstraint;
	}

	@Override
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}

	@Override
	public MultiVariableConstraintWithCheckedProperty conjoin(Expression literal, RewritingProcess process) {
		MultiVariableConstraintWithCheckedProperty result;
		
		if (literal.equals(TRUE)) {
			result = this;
		}
		else if (literal.equals(FALSE)) {
			result = null;
		}
		else {
			Collection<Expression> variables = constraintTheory.getVariablesIn(literal, process);
			if (singleVariableConstraint != null) {
				SingleVariableConstraint newSingleVariableConstraint;
				MultiVariableConstraintWithCheckedProperty newContextualConstraint;
				if (variables.contains(singleVariableConstraint.getVariable())) {
					newSingleVariableConstraint = singleVariableConstraint.conjoin(literal, process);
					newContextualConstraint = contextualConstraint;
				}
				else {
					newSingleVariableConstraint = singleVariableConstraint;
					newContextualConstraint = contextualConstraint.conjoin(literal, process);
				}

				result = makeAndCheck(newSingleVariableConstraint, newContextualConstraint, process);
			}
			else {
				Expression firstVariable = getFirstOrNull(variables);
				SingleVariableConstraint newSingleVariableConstraint = constraintTheory.makeSingleVariableConstraint(firstVariable);
				newSingleVariableConstraint = newSingleVariableConstraint.conjoin(literal, process);
				result = new MultiVariableConstraintWithCheckedProperty(this, newSingleVariableConstraint);
			}
		}
		return result;
	}

	private MultiVariableConstraintWithCheckedProperty makeAndCheck(
			SingleVariableConstraint newSingleVariableConstraint, MultiVariableConstraintWithCheckedProperty newContextualConstraint, RewritingProcess process) {

		MultiVariableConstraintWithCheckedProperty result;
		if (newSingleVariableConstraint == null || newContextualConstraint == null) {
			result = null;
		}
		else {
			result = new MultiVariableConstraintWithCheckedProperty(newContextualConstraint, newSingleVariableConstraint);
			result = result.check(process);
		}
		
		return result;
	}

	private MultiVariableConstraintWithCheckedProperty check(RewritingProcess process) {
		MultiVariableConstraintWithCheckedProperty result;
		ContextDependentProblemStepSolver problem = contextDependentProblemMaker.apply(singleVariableConstraint);
		Expression solution = ContextDependentProblemSolver.solve(problem, contextualConstraint, process);
		if (solution.equals(FALSE)) { // the single-variable constraint is unsatisfiable in all contexts, so it is unsatisfiable.
			result = null;
		}
		else {
			result = this;
		}
		return result;
	}
	
	@Override
	protected Expression computeInnerExpression() {
		Expression result;
		if (contextualConstraint == null) {
			if (singleVariableConstraint == null) {
				result = TRUE;
			}
			else {
				result = singleVariableConstraint;
			}
		}
		else {
			if (singleVariableConstraint == null) {
				result = contextualConstraint;
			}
			else {
				result = And.make(contextualConstraint, singleVariableConstraint);
			}
		}
		return result;
	}
}