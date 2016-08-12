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
package com.sri.ai.grinder.sgdpll.interpreter;

import static com.sri.ai.grinder.helper.GrinderUtil.makeIndexExpressionsForIndicesInListAndTypesInContext;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.getIndex;
import static com.sri.ai.grinder.sgdpll.core.solver.AbstractQuantifierEliminationStepSolver.makeEvaluator;
import static com.sri.ai.util.Util.getLast;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.api.QuantifierEliminator;
import com.sri.ai.grinder.sgdpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.api.Theory;
import com.sri.ai.grinder.sgdpll.core.solver.AbstractQuantifierEliminator;
import com.sri.ai.grinder.sgdpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll.simplifier.api.TopSimplifier;
import com.sri.ai.util.base.Pair;

/**
 * A {@link QuantifierEliminator} implementing the SGDPLL(T) algorithm.
 *
 * @author braz
 *
 */
@Beta
public class SGDPLLT extends AbstractQuantifierEliminator {

	private TopSimplifier topSimplifier;
	private AssociativeCommutativeGroup group;
	
	public SGDPLLT(AssociativeCommutativeGroup group, TopSimplifier topSimplifier) {
		super();
		this.topSimplifier = topSimplifier;
		this.group = group;
	}

	@Override
	public Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, Context context) {
		ExtensionalIndexExpressionsSet indexExpressionsSet = makeIndexExpressionsForIndicesInListAndTypesInContext(indices, context);
		Expression result = solve(group, topSimplifier, indexExpressionsSet, constraint, body, context);
		return result;
	}

	/**
	 * Solves an aggregate operation based on a group operation.
	 * This is defined as the operation applied to the instantiations of a
	 * quantifier-free body for each assignment to a set of indices satisfying a given condition,
	 * under a given context. 
	 * @param group
	 * @param indicesCondition
	 * @param body
	 * @param body
	 * @param context
	 * @return
	 */
	private static Expression solve(
			AssociativeCommutativeGroup group,
			TopSimplifier topSimplifier,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression indicesCondition,
			Expression body,
			Context context) {
		
		Theory theory = context.getTheory();
		
		Expression currentBody = body;
		
		int numberOfIndices = indexExpressions.getList().size();
		
		if (numberOfIndices != 0) {
			// Re-use {@link SingleVariableConstraint} if condition is one.
			// TODO: eventually we want the algorithm to work so that it splitters may be entire constraints,
			// if they are found. Then this encoding would become superfluous,
			// and the condition could always be safely encoded in the body, since it would then be picked and re-used.
			// This would also re-use body if it happens to be a constraint.
			Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint =
					SGDPLLT.encodeConditionAsLastIndexConstraintIfPossibleOrInBodyOtherwise(
							group, indexExpressions, indicesCondition, body, theory, context);
			currentBody = bodyAndLastIndexConstraint.first;
			SingleVariableConstraint lastIndexConstraint = bodyAndLastIndexConstraint.second;

			if (lastIndexConstraint.isContradiction()) {
				return group.additiveIdentityElement();
			}

			for (int i = numberOfIndices - 1; i >= 0; i--) { // evaluate from inside out; this may change in the future
				Expression indexExpression = indexExpressions.getList().get(i);
				Expression index = getIndex(indexExpression);
				SingleVariableConstraint constraintForThisIndex =
						i == numberOfIndices - 1?
								lastIndexConstraint
								: theory.makeSingleVariableConstraint(index, theory, context);
				currentBody =
						theory
						.getSingleVariableConstraintQuantifierEliminatorStepSolver(
								group, constraintForThisIndex, currentBody, topSimplifier, context)
						.solve(context);
			}
		}
		else {
			currentBody = IfThenElse.make(indicesCondition, currentBody, group.additiveIdentityElement());
		}
		
		// Normalize final result.
		ContextDependentExpressionProblemStepSolver evaluator
		= makeEvaluator(currentBody, topSimplifier);
		currentBody = evaluator.solve(context);
		
		return currentBody;
	}

	/**
	 * If condition is an instance of {@link SingleVariableConstraint} or is representable as one
	 * (that is, it is a conjunctive clause),
	 * then make it the constraint to be used for last index, and use original given body.
	 * Otherwise, encode the condition as part of the body,
	 * in the form <code>if condition then body else identityElement</code>.
	 * This is particularly useful when the condition is already a constraint of {@link SingleVariableConstraint},
	 * since it re-used to work put on building it in the first place.
	 * @param group
	 * @param indexExpressions
	 * @param quantifierFreeIndicesCondition
	 * @param quantifierFreeBody
	 * @param context
	 * @return
	 */
	private static Pair<Expression, SingleVariableConstraint>
	encodeConditionAsLastIndexConstraintIfPossibleOrInBodyOtherwise(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression quantifierFreeIndicesCondition,
			Expression quantifierFreeBody,
			Theory theory,
			Context context) {
		
		Expression body;
		SingleVariableConstraint lastIndexConstraint = null;
		Expression lastIndex = getIndex(getLast(indexExpressions.getList()));
		try {
			body = quantifierFreeBody;
			lastIndexConstraint = SingleVariableConstraint.make(theory, lastIndex, quantifierFreeIndicesCondition, context);
			return Pair.make(body, lastIndexConstraint);
		} catch (Error e) { /* proceed to default case below */ }
		
		// did not work out because condition is not SingleVariableConstraint on last index
		body = IfThenElse.make(quantifierFreeIndicesCondition, quantifierFreeBody, group.additiveIdentityElement());
		lastIndexConstraint = theory.makeSingleVariableConstraint(lastIndex, theory, context);
		Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint = Pair.make(body, lastIndexConstraint);
		
		return bodyAndLastIndexConstraint;
	}
}