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
import static com.sri.ai.expresso.helper.Expressions.contains;
import static com.sri.ai.util.Util.getFirstOrNull;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.MultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

/**
 * An {@link Constraint2} on multiple variables,
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
	private Constraint2 contextualConstraint;
	private SingleVariableConstraint singleVariableConstraint;
	
	/**
	 * A {@link BinaryFunction} making a {@link SingleVariableConstraint} for a given
	 * variable and current rewriting process.
	 * @author braz
	 *
	 */
	public static
	interface ContextDependentProblemStepSolverMaker
	extends BinaryFunction<SingleVariableConstraint, RewritingProcess, ContextDependentProblemStepSolver> {}
	
	ContextDependentProblemStepSolverMaker contextDependentProblemStepSolverMaker;
	
	/**
	 * Creates a new {@link MultiVariableConstraintWithCheckedProperty} from a {@link SingleVariableConstraint} and a {@link Constraint2},
	 * returning null if either is null.
	 * @param newContextualConstraint
	 * @param newSingleVariableConstraint
	 * @param process
	 * @return
	 */
	public static MultiVariableConstraintWithCheckedProperty makeAndCheck(
			Constraint2 newContextualConstraint,
			SingleVariableConstraint newSingleVariableConstraint,
			ContextDependentProblemStepSolverMaker contextDependentProblemStepSolverMaker,
			RewritingProcess process) {
	
		MultiVariableConstraintWithCheckedProperty result;
		if (newSingleVariableConstraint == null || newContextualConstraint == null) {
			result = null;
		}
		else {
			result = new MultiVariableConstraintWithCheckedProperty(newContextualConstraint, newSingleVariableConstraint, contextDependentProblemStepSolverMaker);
			result = result.check(process);
		}
		
		return result;
	}

	public MultiVariableConstraintWithCheckedProperty(
			ConstraintTheory constraintTheory, ContextDependentProblemStepSolverMaker contextDependentProblemMaker) {
		this.constraintTheory = constraintTheory;
		this.contextDependentProblemStepSolverMaker = contextDependentProblemMaker;
		this.contextualConstraint = null;
		this.singleVariableConstraint = null;
	}
	
	/**
	 * Constructs a {@link MultiVariableConstraintWithCheckedProperty} from a contextual constraint
	 * and a {@link SingleVariableConstraint},
	 * which is only correct if the {@link SingleVariableConstraint}'s variable does not appear
	 * in the contextual constraint.
	 * Note also that this does not check the checked property.
	 * Because of these issues, the constructor is private.
	 * @param contextualConstraint
	 * @param singleVariableConstraint
	 */
	private MultiVariableConstraintWithCheckedProperty(
			Constraint2 contextualConstraint,
			SingleVariableConstraint singleVariableConstraint,
			ContextDependentProblemStepSolverMaker contextDependentProblemMaker) {
		this.constraintTheory = contextualConstraint.getConstraintTheory();
		this.contextDependentProblemStepSolverMaker = contextDependentProblemMaker;
		this.contextualConstraint = contextualConstraint;
		this.singleVariableConstraint = singleVariableConstraint;
	}

	@Override
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}
	
	@Override
	public MultiVariableConstraintWithCheckedProperty conjoin(Expression formula, RewritingProcess process) {
		MultiVariableConstraintWithCheckedProperty result;
		
		Pair<Boolean, MultiVariableConstraintWithCheckedProperty> specializedResult
		= conjoinSpecializedForConstraintsIfApplicable(formula, process);
		
		if (specializedResult.first) {
			result = specializedResult.second;
		}
		else { // fall back to default implementation
			result = (MultiVariableConstraintWithCheckedProperty) MultiVariableConstraint.super.conjoin(formula, process);
		}
		
		return result;
	}

	/**
	 * Returns a pair indicating whether specialized conjoin for constraints applies to this case and,
	 * if so, provides the result of this conjoining.
	 * @param formula
	 * @param process
	 * @return
	 */
	private Pair<Boolean, MultiVariableConstraintWithCheckedProperty> conjoinSpecializedForConstraintsIfApplicable(Expression formula, RewritingProcess process) {
		Pair<Boolean, MultiVariableConstraintWithCheckedProperty> result = new Pair<>(false, null);
		
		if (formula instanceof SingleVariableConstraint) {
			SingleVariableConstraint formulaAsSingleVariableConstraint = (SingleVariableConstraint) formula;
			if ( ! contains(this, formulaAsSingleVariableConstraint.getVariable(), process)) {
				result = Pair.make(true, makeAndCheck(this, formulaAsSingleVariableConstraint, contextDependentProblemStepSolverMaker, process));
				// if the variable is new to this constraint, we can simply tack on its constraint under it. 
			}
		}
		else if (formula instanceof MultiVariableConstraintWithCheckedProperty) {
			MultiVariableConstraintWithCheckedProperty formulaAsMultiVariableConstraint = (MultiVariableConstraintWithCheckedProperty) formula;
			MultiVariableConstraintWithCheckedProperty conjunction = this;
			if (formulaAsMultiVariableConstraint.contextualConstraint != null) {
				conjunction = conjunction.conjoin(formulaAsMultiVariableConstraint.contextualConstraint, process);
			}
			if (formulaAsMultiVariableConstraint.singleVariableConstraint != null) {
				conjunction = conjunction.conjoin(formulaAsMultiVariableConstraint.singleVariableConstraint, process);
			}
			result = Pair.make(true, conjunction);
		}
		
		return result;
	}

	@Override
	public MultiVariableConstraintWithCheckedProperty conjoinWithLiteral(Expression literal, RewritingProcess process) {
		MultiVariableConstraintWithCheckedProperty result;
		if (literal.equals(TRUE)) {
			result = this;
		}
		else if (literal.equals(FALSE)) {
			result = null;
		}
		else {
			Collection<Expression> variablesInLiteral = constraintTheory.getVariablesIn(literal, process);
			if (variablesInLiteral.isEmpty()) {
				Expression literalSimplifiedToConstant = constraintTheory.simplify(literal, process);
//				System.out.println("constraint: " + this);	
//				System.out.println("literal: " + literal);	
//				System.out.println("literal simplified to constant: " + literalSimplifiedToConstant);	
				result = conjoinWithLiteral(literalSimplifiedToConstant, process);
			}
			else if (singleVariableConstraint != null) {
				SingleVariableConstraint newSingleVariableConstraint;
				Constraint2 newContextualConstraint;
				if (variablesInLiteral.contains(singleVariableConstraint.getVariable())) {
					newSingleVariableConstraint = singleVariableConstraint.conjoin(literal, process);
					newContextualConstraint = contextualConstraint;
				}
				else {
					newSingleVariableConstraint = singleVariableConstraint;
					newContextualConstraint = contextualConstraint.conjoin(literal, process);
				}
		
				result = makeAndCheck(newContextualConstraint, newSingleVariableConstraint, contextDependentProblemStepSolverMaker, process);
			}
			else {
				Expression firstVariable = getFirstOrNull(variablesInLiteral);
				SingleVariableConstraint newSingleVariableConstraint = constraintTheory.makeSingleVariableConstraint(firstVariable, constraintTheory, process);
				newSingleVariableConstraint = newSingleVariableConstraint.conjoin(literal, process);
				if (newSingleVariableConstraint != null) {
					result = new MultiVariableConstraintWithCheckedProperty(this, newSingleVariableConstraint, contextDependentProblemStepSolverMaker);
					// the use of 'this' here does not mean that *this* constraint has to be provided as the contextual constraint.
					// any empty multi-variable constraint would do.
					// It is just that at this point we know 'this' to be an empty constraint and use it as a conveniently already available one.
				}
				else {
					result = null;
				}
			}
		}
		return result;
	}

	private MultiVariableConstraintWithCheckedProperty check(RewritingProcess process) {
		MultiVariableConstraintWithCheckedProperty result;
		ContextDependentProblemStepSolver problem = contextDependentProblemStepSolverMaker.apply(singleVariableConstraint, process);
		Expression solution = problem.solve(contextualConstraint, process);
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