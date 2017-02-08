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

import static com.sri.ai.util.Util.getLast;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.MultiIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.util.base.Pair;

/**
 * A {@link MultiIndexQuantifierEliminator} implementing the SGDPLL(T) algorithm.
 *
 * @author braz
 *
 */
@Beta
public class SGDPLLT extends AbstractMultiIndexQuantifierEliminator {

	public SGDPLLT() {
		super();
	}

	/**
	 * @param group
	 * @param indices
	 * @param indicesCondition
	 * @param body
	 * @param context
	 * @return
	 */
	@Override
	public Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context) {
		Theory theory = context.getTheory();
		
		Expression currentBody = body;
		
		int numberOfIndices = indices.size();
		
		if (numberOfIndices != 0) {
			// Re-use {@link SingleVariableConstraint} if condition is one.
			// TODO: eventually we want the algorithm to work so that it splitters may be entire constraints,
			// if they are found. Then this encoding would become superfluous,
			// and the condition could always be safely encoded in the body, since it would then be picked and re-used.
			// This would also re-use body if it happens to be a constraint.
			Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint =
					SGDPLLT.encodeConditionAsLastIndexConstraintIfPossibleOrInBodyOtherwise(
							group, indices, indicesCondition, body, theory, context);
			currentBody = bodyAndLastIndexConstraint.first;
			SingleVariableConstraint lastIndexConstraint = bodyAndLastIndexConstraint.second;

			if (lastIndexConstraint.isContradiction()) {
				return group.additiveIdentityElement();
			}

			for (int i = numberOfIndices - 1; i >= 0; i--) { // evaluate from inside out; this may change in the future
				Expression index = indices.get(i);
				SingleVariableConstraint constraintForThisIndex =
						i == numberOfIndices - 1?
								lastIndexConstraint
								: theory.makeSingleVariableConstraint(index, theory, context);
				ExpressionLiteralSplitterStepSolver quantifierEliminatorStepSolver = 
						theory.getSingleVariableConstraintQuantifierEliminatorStepSolver(
								group, constraintForThisIndex, currentBody, context);
				if (quantifierEliminatorStepSolver != null) {
					currentBody = quantifierEliminatorStepSolver.solve(context);
				}
				else {
					// cannot eliminate this level, so reconstruct original expression up to this index
					throw new Error("Reconstruction of quantifier not yet eliminable not yet implemented.");
					// once implemented, must return  
				}
			}
		}
		else {
			currentBody = IfThenElse.make(indicesCondition, currentBody, group.additiveIdentityElement());
		}
		
		// Normalize final result.
		ExpressionLiteralSplitterStepSolver evaluator = theory.makeEvaluatorStepSolver(currentBody);
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
			List<Expression> indices,
			Expression quantifierFreeIndicesCondition,
			Expression quantifierFreeBody,
			Theory theory,
			Context context) {
		
		Expression body;
		SingleVariableConstraint lastIndexConstraint = null;
		Expression lastIndex = getLast(indices);
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
