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
package com.sri.ai.grinder.sgdpll2.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.core.solver.ContextDependentProblemSolver;

/**
 * An {@link Expression} with efficient internal representation for incrementally deciding satisfiability of a boolean formulas on literals in a certain theory.
 * 
 * @author braz
 *
 */
public interface SingleVariableConstraint extends Expression, Constraint2 {

	/**
	 * @return the variable term constrained by this constraint.
	 */
	Expression getVariable();
	
	/**
	 * Returns a {@link SingleVariableConstraint} on a given variable, according to a given constraint theory,
	 * equivalent to given formula (or null if formula is inconsistent)
	 * -- the formula object itself is returned if it happens to be a
	 * {@link SingleVariableConstraint} on same variable and theory.
	 * @param variable
	 * @param formula
	 * @return
	 */
	public static SingleVariableConstraint make(
			ConstraintTheory constraintTheory,
			Expression variable,
			Expression formula,
			RewritingProcess process) {
		
		SingleVariableConstraint result = null;
		if (formula instanceof SingleVariableConstraint) {
			SingleVariableConstraint formulasAsConstraint = (SingleVariableConstraint) formula;
			if (formulasAsConstraint.getVariable().equals(variable) &&
					formulasAsConstraint.getConstraintTheory().equals(constraintTheory)) {
				result = formulasAsConstraint;
			}
		}
		
		// if formula is not appropriate constraint, create a new one and conjoin with formula
		if (result == null) {
			result = constraintTheory.makeSingleVariableConstraint(variable).conjoin(formula, process);
		}
		
		return result;
	}
	
	@Override
	default SingleVariableConstraint conjoin(Expression formula, RewritingProcess process) {
		return (SingleVariableConstraint) Constraint2.super.conjoin(formula, process);
	}
	
	/**
	 * Returns the satisfiability of this single-variable constraint under a contextual constraint and process.
	 * @param contextualConstraint
	 * @param process
	 * @return
	 */
	default Expression satisfiability(Constraint2 contextualConstraint, RewritingProcess process) {
		ContextDependentProblemStepSolver satisfiabilityStepSolver = getConstraintTheory().getSingleVariableConstraintSatisfiabilityStepSolver(this);
		Expression satisfiability = ContextDependentProblemSolver.solve(satisfiabilityStepSolver, contextualConstraint, process);
		return satisfiability;
	}
	
	/**
	 * Returns the model count of this single-variable constraint under a contextual constraint and process.
	 * @param contextualConstraint
	 * @param process
	 * @return
	 */
	default Expression modelCount(Constraint2 contextualConstraint, RewritingProcess process) {
		ContextDependentProblemStepSolver modelCountingStepSolver = getConstraintTheory().getSingleVariableConstraintModelCountingStepSolver(this);
		Expression modelCount = ContextDependentProblemSolver.solve(modelCountingStepSolver, contextualConstraint, process);
		return modelCount;
	}
}