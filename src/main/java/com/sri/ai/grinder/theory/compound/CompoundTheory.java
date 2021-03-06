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
package com.sri.ai.grinder.theory.compound;

import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.thereExists;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.constraint.AbstractTheory;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.util.Util;

/** 
 * A {@link Theory} formed by the union of other constraint theories.
 */
@Beta
public class CompoundTheory extends AbstractTheory {

	private List<Theory> subTheories;
	
	public CompoundTheory(Theory... subTheoriesArray) {
		super();
		this.subTheories = list(subTheoriesArray);
		Util.myAssert(() -> subTheories.size() != 0, () -> getClass() + " needs to receive at least one sub-theory but got none.");
	}

	@Override
	public TopRewriter makeTopRewriter() {
		List<TopRewriter> subRewriters = mapIntoList(subTheories, t -> t.getTopRewriter());
		TopRewriter topRewriter = TopRewriter.makeTopRewriterFromTopRewritersThatAreEitherFirstOfOrSwitches(subRewriters);
		return topRewriter;
	}

	@Override
	public Collection<Type> getNativeTypes() {
		Collection<Type> result = new LinkedHashSet<Type>();
		for (Theory subTheory : getSubTheories()) {
			result.addAll(subTheory.getNativeTypes());
		}
		return result;
	}

	@Override
	public boolean isSuitableFor(Type type) {
		boolean result = thereExists(getSubTheories(), t -> t.isSuitableFor(type));
		return result;
	}
	
	public Collection<Theory> getSubTheories() {
		return subTheories;
	}

	// NOTE: package protected so TestingSupport can utilize.
	Theory getTheory(Expression variable, Context context) {
		Type type = GrinderUtil.getTypeOfExpression(variable, context);
		Theory result = getTheory(variable, type);
		return result;
	}

	Theory getTheory(Expression variable, Type variableType) {		
		if (variableType == null) {
			throw new IllegalArgumentException("Cannot decide which theory to use for variable " + variable + " because it does not have a registered type.");
		}
		
		Theory result =
				getFirstSatisfyingPredicateOrNull(
						getSubTheories(),
						t -> t.isSuitableFor(variableType));

		// Used to enforce presence of theory here,
		// but we are now more flexible,
		// allowing to have quantifiers indexed by variables
		// for which we have no theory,
		// and leaving them alone in that case.
		// When the theory is used for more essential tasks,
		// such as checking the context's satisfiability,
		// an error will be thrown at that point.
		// check(() -> result != null, () -> "There is no sub-theory suitable for " + variable + ", which has type " + variableType);
		
		if (result == null) {
			throw new IllegalArgumentException("No theory for " + variable + " of type " + variableType);
		}
		
		return result;
	}
	
	@Override
	public boolean isAtom(Expression expression, Context context) {
		boolean result = thereExists(getSubTheories(), t -> t.isAtom(expression, context));
		return result;
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraintAfterBookkeeping(Expression variable, Context context) {
		Theory theoryForVariable = getTheory(variable, context);
		SingleVariableConstraint result;
		if (theoryForVariable != null) {
			result = theoryForVariable.makeSingleVariableConstraintAfterBookkeeping(variable, context);
		}
		else {
			result = null;
		}
		return result;
	}

	private int cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable = -1; // int so we can know whether it is 'undefined'
	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		if (cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable == -1) {
			boolean trueForAllTheories =
					forAll(
							getSubTheories(),
							Theory::singleVariableConstraintIsCompleteWithRespectToItsVariable);
			cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable = trueForAllTheories ? 1 : 0;
		}
		return cachedSingleVariableConstraintIsCompleteWithRespectToItsVariable == 1;
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression) {
		boolean result =
				thereExists(
						getSubTheories(),
						t -> t.isInterpretedInThisTheoryBesidesBooleanConnectives(expression));
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context) {
		Theory theory = getTheory(constraint.getVariable(), context);
		ExpressionLiteralSplitterStepSolver result;
		if (theory != null) {
			result = theory.getSingleVariableConstraintSatisfiabilityStepSolver(constraint, context);
		}
		else {
			result = null;
		}
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context) {
		Theory theory = getTheory(constraint.getVariable(), context);
		ExpressionLiteralSplitterStepSolver result;
		if (theory != null) {
			result = theory.getSingleVariableConstraintModelCountingStepSolver(constraint, context);
		}
		else {
			result = null;
		}
		return result;
	}

	@Override
	public 	ExpressionLiteralSplitterStepSolver getSingleQuantifierEliminatorStepSolver(SingleQuantifierEliminationProblem problem, Context context) {
		Theory theory = getTheory(problem.getIndex(), context);
		ExpressionLiteralSplitterStepSolver result = theory.getSingleQuantifierEliminatorStepSolver(problem, context);
		return result;
	}

	@Override
	public Expression getAtomNegation(Expression atom, Context context) {
		Theory theory =
				getFirstSatisfyingPredicateOrNull(getSubTheories(), t -> t.isLiteralOrBooleanConstant(atom, context));
		Expression result;
		if (theory == null) {
			result = Not.make(atom);
		}
		else {
			result = theory.getAtomNegation(atom, context);
		}
		return result;
	}

	@Override
	public String toString() {
		return "Compound theory (" + join(getSubTheories()) + ")";
	}

}