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
package com.sri.ai.grinder.api;

import java.util.List;

import com.sri.ai.expresso.api.Expression;

/**
 * An {@link Expression} with efficient internal representation for incrementally deciding satisfiability of a boolean formulas on literals in a certain theory.
 * 
 * @author braz
 *
 */
public interface SingleVariableConstraint extends Constraint {

	/**
	 * @return the variable term constrained by this constraint.
	 */
	Expression getVariable();
	
	/**
	 * A single-variable constraint has the ability to temporarily hold literals not on its variable.
	 * This returns such literals.
	 * This works as a buffer for literals propagated (deduced) at the point in which the constraint
	 * is formed (by conjunction with another), so that they don't need to be deduced every time
	 * we need to check if there are literals to be propagated.
	 * @return
	 */
	List<Expression> getExternalLiterals();
	
	/**
	 * Creates a simplification of this constraint
	 * that is equal to it but for the absence of external literals.
	 * However, it is important to notice that,
	 * because the result is considered a simplification,
	 * it can only be used in contexts in which the original
	 * constraint (and therefore the external literals) hold,
	 * because it may still assume them to hold.
	 * While this may at first seem like an unnecessary restriction,
	 * it allows implementations to count on the external literal information
	 * to keep whatever internal bookkeeping that depends on the original
	 * constraint, avoiding the need to repeated computation.
	 * <p>
	 * For example, equality constraints keep track of how many
	 * disequalities against uniquely named constants have been observed
	 * on the constraint's variable so far (even the disequalities that
	 * have been discarded since then).
	 * Copying this information to the new constraint is only justified
	 * if we know it is going to be used as a simplification of the original
	 * constraint, since it may not follow from the normalized atoms and external literals
	 * alone.
	 * @return
	 */
	SingleVariableConstraint makeSimplificationWithoutExternalLiterals();
	
	/**
	 * Returns a {@link SingleVariableConstraint} on a given variable, according to a given theory,
	 * equivalent to given formula
	 * -- the formula object itself is returned if it happens to be a
	 * {@link SingleVariableConstraint} on same variable and theory.
	 * @param variable
	 * @param formula
	 * @return
	 */
	public static SingleVariableConstraint make(
			Theory theory,
			Expression variable,
			Expression formula,
			Context context) {
		
		SingleVariableConstraint result = null;
		result = useFormulaIfItIsASingleVariableConstraintForTheRightTheory(variable, formula, theory);
		
		if (result == null) {
			result = makeSingleVariableConstraintFromFormula(variable, formula, context);
		}
		
		return result;
	}

	public static SingleVariableConstraint useFormulaIfItIsASingleVariableConstraintForTheRightTheory(
			Expression variable, 
			Expression formula,
			Theory theory) {
		
		SingleVariableConstraint result = null;
		
		if (formula instanceof SingleVariableConstraint) {
			SingleVariableConstraint formulasAsConstraint = (SingleVariableConstraint) formula;
			if (formulasAsConstraint.getVariable().equals(variable) &&
					formulasAsConstraint.getTheory().equals(theory)) {
				result = formulasAsConstraint;
			}
		}
		return result;
	}

	public static SingleVariableConstraint makeSingleVariableConstraintFromFormula(Expression variable, Expression formula, Context context) throws Error {
		SingleVariableConstraint result;
		SingleVariableConstraint singleVariableConstraint = context.getTheory().makeSingleVariableConstraint(variable, context);
		if (singleVariableConstraint == null) {
			throw new Error("The current theory does not know how to manipulate constraints on " + variable + " (type " + context.getTypeOfRegisteredSymbol(variable) + ").");
		}
		result = singleVariableConstraint.conjoin(formula, context);
		return result;
	}
	
	@Override
	default SingleVariableConstraint conjoin(Expression formula, Context context) {
		return (SingleVariableConstraint) Constraint.super.conjoin(formula, context);
	}
	
	/**
	 * Returns the satisfiability of this single-variable constraint under a context and context,
	 * or null if there is no appropriate solver.
	 * @param context
	 * @return
	 */
	default Expression satisfiability(Context context) {
		ExpressionLiteralSplitterStepSolver satisfiabilityStepSolver = getTheory().getSingleVariableConstraintSatisfiabilityStepSolver(this, context);
		Expression satisfiability;
		if (satisfiabilityStepSolver != null) {
			satisfiability = satisfiabilityStepSolver.solve(context);
		}
		else {
			satisfiability = null;
		}
		return satisfiability;
	}
	
	/**
	 * Returns the model count of this single-variable constraint under a context and context
	 * or null if there is no appropriate solver.
	 * @param context
	 * @return
	 */
	default Expression modelCount(Context context) {
		ExpressionLiteralSplitterStepSolver modelCountingStepSolver = 
				getTheory()
				.getSingleVariableConstraintModelCountingStepSolver(this, context);
		Expression modelCount;
		if (modelCountingStepSolver == null) {
			modelCount = null;
		}
		else {
			modelCount = modelCountingStepSolver.solve(context);
		}
		return modelCount;
	}
	
	/**
	 * Returns an expression to which the constraint's variable is bound to
	 * under this constraint, if there is such a value and it can be determined by the implementation.
	 * @return
	 */
	Expression binding();
	
	@Override
	default Expression binding(Expression variable) {
		Expression result;
		if ( ! isContradiction() && variable.equals(getVariable())) {
			result = binding();
		}
		else {
			result = null;
		}
		return result;
	}
	
	SingleVariableConstraint clone();
}