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
package com.sri.ai.grinder.sgdpll.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.contains;
import static com.sri.ai.util.Util.getFirstOrNull;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.api.MultiVariableConstraint;
import com.sri.ai.grinder.sgdpll.api.SingleVariableConstraint;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

/**
 * An {@link Constraint} on multiple variables,
 * with the ability to ensure all single-variable constraints that are part of it
 * have a property determined by a {@link ContextDependentExpressionProblemStepSolver},
 * or otherwise the total constraint is deemed unsatisfiable.
 * 
 * @author braz
 *
 */
@Beta
public class MultiVariableConstraintWithCheckedProperty extends AbstractConstraint implements MultiVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private SingleVariableConstraint head; // constraint on last variable
	private Constraint tail; // constraint on variables but the last one; works as contextual constraint for head when checking property
	private boolean checked;
	
	/**
	 * A {@link BinaryFunction} making a {@link SingleVariableConstraint} for a given
	 * variable and current context.
	 * @author braz
	 *
	 */
	public static
	interface ContextDependentProblemStepSolverMaker
	extends BinaryFunction<SingleVariableConstraint, Context, ContextDependentExpressionProblemStepSolver> {}
	
	ContextDependentProblemStepSolverMaker contextDependentProblemStepSolverMaker;
	
	/**
	 * Creates a new {@link MultiVariableConstraintWithCheckedProperty} from a {@link SingleVariableConstraint} and a {@link Constraint},
	 * returning null if either is null.
	 * @param newTail
	 * @param newHead
	 * @param constraintTheory TODO
	 * @param context
	 * @return
	 */
	public static MultiVariableConstraintWithCheckedProperty makeAndCheck(
			Constraint newTail,
			SingleVariableConstraint newHead,
			ConstraintTheory constraintTheory,
			ContextDependentProblemStepSolverMaker contextDependentProblemStepSolverMaker, 
			Context context) {
	
		MultiVariableConstraintWithCheckedProperty result;
		if (newHead.isContradiction() || newTail.isContradiction()) {
			MultiVariableConstraintWithCheckedProperty newMultiVariableConstraintWithCheckedProperty = 
					new MultiVariableConstraintWithCheckedProperty(constraintTheory, contextDependentProblemStepSolverMaker);
			result = newMultiVariableConstraintWithCheckedProperty.makeContradiction();
			// TODO: perhaps this should be cached
		}
		else {
			result = 
					new MultiVariableConstraintWithCheckedProperty(
							newTail.getConstraintTheory(), 
							newTail, 
							newHead, 
							contextDependentProblemStepSolverMaker);
			result = result.check(context);
		}
		
		return result;
	}

	public MultiVariableConstraintWithCheckedProperty(
			ConstraintTheory constraintTheory, ContextDependentProblemStepSolverMaker contextDependentProblemMaker) {
		this(
				constraintTheory, 
				null,
				null, 
				contextDependentProblemMaker);
	}
	
	/**
	 * Constructs a {@link MultiVariableConstraintWithCheckedProperty} from a head and a tail constraints,
	 * which is only correct if the {@link SingleVariableConstraint}'s variable does not appear
	 * in the tail constraint.
	 * Note also that this does not check the checked property.
	 * Because of these issues, the constructor is private.
	 * @param tail
	 * @param head
	 */
	private MultiVariableConstraintWithCheckedProperty(
			ConstraintTheory constraintTheory,
			Constraint tail,
			SingleVariableConstraint head,
			ContextDependentProblemStepSolverMaker contextDependentProblemMaker) {
		super(constraintTheory);
		this.tail = tail;
		this.head = head;
		this.checked = false;
		this.contextDependentProblemStepSolverMaker = contextDependentProblemMaker;
	}

	@Override
	public MultiVariableConstraintWithCheckedProperty conjoin(Expression formula, Context context) {
		MultiVariableConstraintWithCheckedProperty result;
		
		Pair<Boolean, MultiVariableConstraintWithCheckedProperty> specializedResult
		= conjoinSpecializedForConstraintsIfApplicable(formula, context);
		
		if (specializedResult.first) {
			result = specializedResult.second;
		}
		else { // fall back to default implementation
			result = (MultiVariableConstraintWithCheckedProperty) MultiVariableConstraint.super.conjoin(formula, context);
		}
		
		return result;
	}

	/**
	 * Returns a pair indicating whether specialized conjoin for constraints applies to this case and,
	 * if so, provides the result of this conjoining.
	 * @param formula
	 * @param context
	 * @return
	 */
	private Pair<Boolean, MultiVariableConstraintWithCheckedProperty> conjoinSpecializedForConstraintsIfApplicable(Expression formula, Context context) {
		Pair<Boolean, MultiVariableConstraintWithCheckedProperty> result = new Pair<>(false, null);
		
		if (formula instanceof SingleVariableConstraint) {
			SingleVariableConstraint formulaAsSingleVariableConstraint = (SingleVariableConstraint) formula;
			if ( ! contains(this, formulaAsSingleVariableConstraint.getVariable(), context)) { // TODO: using contains here is overkill
				result = Pair.make(true, makeAndCheck(this, formulaAsSingleVariableConstraint, getConstraintTheory(), contextDependentProblemStepSolverMaker, context));
				// if the variable is new to this constraint, we can simply tack on its constraint under it. 
			}
		}
		else if (formula instanceof MultiVariableConstraintWithCheckedProperty) {
			MultiVariableConstraintWithCheckedProperty formulaAsMultiVariableConstraint = (MultiVariableConstraintWithCheckedProperty) formula;
			MultiVariableConstraintWithCheckedProperty conjunction = this;
			if (formulaAsMultiVariableConstraint.tail != null) {
				conjunction = conjunction.conjoin(formulaAsMultiVariableConstraint.tail, context);
			}
			if (formulaAsMultiVariableConstraint.head != null) {
				conjunction = conjunction.conjoin(formulaAsMultiVariableConstraint.head, context);
			}
			result = Pair.make(true, conjunction);
		}
		
		return result;
	}

	@Override
	public MultiVariableConstraintWithCheckedProperty conjoinWithLiteral(Expression literal, Context context) {
		MultiVariableConstraintWithCheckedProperty result;
		if (literal.equals(TRUE)) {
			result = this;
		}
		else if (literal.equals(FALSE)) {
			result = makeContradiction();
		}
		else {
			Collection<Expression> variablesInLiteral = getConstraintTheory().getVariablesIn(literal, context);
			if (variablesInLiteral.isEmpty()) {
				Expression literalSimplifiedToConstant = getConstraintTheory().simplify(literal, context);
//				System.out.println("constraint: " + this);	
//				System.out.println("literal: " + literal);	
//				System.out.println("literal simplified to constant: " + literalSimplifiedToConstant);	
				result = conjoinWithLiteral(literalSimplifiedToConstant, context);
			}
			else if (head != null) {
				SingleVariableConstraint newHead;
				Constraint newTail;
				if (variablesInLiteral.contains(head.getVariable())) {
					newHead = head.conjoin(literal, context);
					newTail = tail;
				}
				else {
					newHead = head;
					newTail = tail.conjoin(literal, context);
				}
		
				// optional, but good:
				// we propagate external literals from head
				// up the chain so they are integrated and simplified in the corresponding single-variable constraints
				if ( ! newHead.isContradiction()) {
					for (Expression externalLiteral : newHead.getExternalLiterals()) {
						if ( ! newTail.isContradiction()) {
							newTail = newTail.conjoin(externalLiteral, context);
						}
					}
					newHead = newHead.makeSimplificationWithoutExternalLiterals();
				}

				if (newHead == head && newTail == tail) { // in case nothing changed
					result = this;
				}
				else {
					result = makeAndCheck(newTail, newHead, getConstraintTheory(), contextDependentProblemStepSolverMaker, context);
				}
			}
			else {
				Expression firstVariable = getFirstOrNull(variablesInLiteral);
				SingleVariableConstraint newSingleVariableConstraint = getConstraintTheory().makeSingleVariableConstraint(firstVariable, getConstraintTheory(), context);
				newSingleVariableConstraint = newSingleVariableConstraint.conjoin(literal, context);
				if ( ! newSingleVariableConstraint.isContradiction()) {
					result = new MultiVariableConstraintWithCheckedProperty(
							getConstraintTheory(),
							this, 
							newSingleVariableConstraint, 
							contextDependentProblemStepSolverMaker);
					result = result.check(context);
				}
				else {
					result = makeContradiction();
				}
			}
		}
		return result;
	}

	/**
	 * @return
	 */
	@Override
	public MultiVariableConstraintWithCheckedProperty makeContradiction() {
		return (MultiVariableConstraintWithCheckedProperty) super.makeContradiction();
	}

	private MultiVariableConstraintWithCheckedProperty check(Context context) {
		MultiVariableConstraintWithCheckedProperty result;
		if (checked) {
			result = this;
		}
		else {
			ContextDependentExpressionProblemStepSolver problem = contextDependentProblemStepSolverMaker.apply(head, context);
			Context tailAsContext = context.conjoin(tail, context);
			Expression solution = problem.solve(tailAsContext, context);
			if (solution == null) { // tail is found to be inconsistent with given context
				result = makeContradiction();
			}
			else if (solution.equals(FALSE)) { // the head constraint does not exhibit the property in all contexts, so the total constraint does not either.
				result = makeContradiction();
			}
			else {
				this.checked = true;
				result = this;
			}
		}
		return result;
	}
	
	@Override
	protected Expression computeInnerExpressionIfNotContradiction() {
		Expression result;
		if (tail == null) {
			if (head == null) {
				result = TRUE;
			}
			else {
				result = head;
			}
		}
		else {
			if (head == null) {
				result = tail;
			}
			else {
				result = And.make(tail, head);
			}
		}
		return result;
	}

	@Override
	public Expression binding(Expression variable) {
		Expression result;
		if ( ! head.isContradiction() && head.getVariable().equals(variable)) {
			result = head.binding();
		}
		else {
			result = tail.binding(variable);
		}
		return result;
	}
}