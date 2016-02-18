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
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.api.GroupProblemType;
import com.sri.ai.grinder.sgdpll.api.OldStyleQuantifierEliminator;
import com.sri.ai.grinder.sgdpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.AbstractOldStyleQuantifierEliminator;
import com.sri.ai.grinder.sgdpll.core.constraint.CompleteMultiVariableContext;
import com.sri.ai.grinder.sgdpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.grinder.sgdpll.simplifier.api.TopSimplifier;
import com.sri.ai.grinder.sgdpll.simplifier.core.Recursive;
import com.sri.ai.grinder.sgdpll.simplifier.core.TopExhaustive;
import com.sri.ai.util.base.Pair;

/**
 * A {@link OldStyleQuantifierEliminator} implementing the SGDPLL(T) algorithm.
 *
 * @author braz
 *
 */
@Beta
public class SGDPLLT extends AbstractOldStyleQuantifierEliminator {

	private TopSimplifier topSimplifier;
	private Simplifier simplifier;
	private GroupProblemType problemType;
	private ConstraintTheory constraintTheory;
	
	public SGDPLLT(TopSimplifier topSimplifier, GroupProblemType problemType, ConstraintTheory constraintTheory) {
		super();
		this.topSimplifier = topSimplifier;
		this.simplifier = new Recursive(new TopExhaustive(topSimplifier));
		this.problemType = problemType;
		this.constraintTheory = constraintTheory;
	}

	@Override
	public Context makeTrueConstraint(Collection<Expression> indices, Context context) {
		return new CompleteMultiVariableContext(constraintTheory, context);
	}

	@Override
	public Context makeProcess(Map<String, String> mapFromSymbolNameToTypeName, Map<String, String> mapFromCategoricalTypeNameToSizeString, Collection<Type> additionalTypes, Predicate<Expression> isUniquelyNamedConstantPredicate, ConstraintTheory constraintTheory) {
		return GrinderUtil.makeProcess(
				mapFromSymbolNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isUniquelyNamedConstantPredicate, constraintTheory) ;
	}

	@Override
	public Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, Context context) {
		ExtensionalIndexExpressionsSet indexExpressionsSet = makeIndexExpressionsForIndicesInListAndTypesInContext(indices, context);
		Context trueContextualConstraint = makeTrueConstraint(indices, context);
		Expression quantifierFreeConstraint = simplifier.apply(constraint, context);
		Expression quantifierFreeBody = simplifier.apply(body, context);
		Expression result = solve(problemType, topSimplifier, indexExpressionsSet, quantifierFreeConstraint, quantifierFreeBody, trueContextualConstraint, context);
		return result;
	}

	/**
	 * Solves an aggregate operation based on a group operation.
	 * This is defined as the operation applied to the instantiations of a
	 * quantifier-free body for each assignment to a set of indices satisfying a given condition,
	 * under a given contextual constraint. 
	 * @param group
	 * @param indexExpressions
	 * @param indicesCondition
	 * @param body
	 * @param contextualConstraint
	 * @param context
	 * @return
	 */
	public static Expression solve(
			AssociativeCommutativeGroup group,
			TopSimplifier topSimplifier,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression quantifierFreeIndicesCondition,
			Expression quantifierFreeBody,
			Context contextualConstraint,
			Context context) {
		
		Simplifier simplifier = new Recursive(new TopExhaustive(topSimplifier));
		
		ConstraintTheory constraintTheory = contextualConstraint.getConstraintTheory();
		
		Expression currentBody = quantifierFreeBody;
		
		int numberOfIndices = indexExpressions.getList().size();
		
		if (numberOfIndices != 0) {
			// Re-use {@link SingleVariableConstraint} if condition is one.
			// TODO: eventually we want the algorithm to work so that it splitters may be entire constraints,
			// if they are found. Then this encoding would become superfluous,
			// and the condition could always be safely encoded in the body, since it would then be picked and re-used.
			// This would also re-use body if it happens to be a constraint.
			Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint =
					SGDPLLT.encodeConditionAsLastIndexConstraintIfPossibleOrInBodyOtherwise(
							group, indexExpressions, quantifierFreeIndicesCondition, quantifierFreeBody, constraintTheory, context);
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
								: constraintTheory.makeSingleVariableConstraint(index, constraintTheory, context);
				currentBody =
						constraintTheory.getSingleVariableConstraintQuantifierEliminatorStepSolver(
								group, constraintForThisIndex, currentBody, simplifier, context).
						solve(contextualConstraint);
			}
		}
		else {
			currentBody = IfThenElse.make(quantifierFreeIndicesCondition, currentBody, group.additiveIdentityElement());
		}
		
		// Normalize final result.
		ContextDependentExpressionProblemStepSolver evaluator
		= makeEvaluator(currentBody, topSimplifier);
		currentBody = evaluator.solve(contextualConstraint);
		
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
			ConstraintTheory constraintTheory,
			Context context) {
		
		Expression body;
		SingleVariableConstraint lastIndexConstraint = null;
		Expression lastIndex = getIndex(getLast(indexExpressions.getList()));
		try {
			body = quantifierFreeBody;
			lastIndexConstraint = SingleVariableConstraint.make(constraintTheory, lastIndex, quantifierFreeIndicesCondition, context);
			return Pair.make(body, lastIndexConstraint);
		} catch (Error e) { /* proceed to default case below */ }
		
		// did not work out because condition is not SingleVariableConstraint on last index
		body = IfThenElse.make(quantifierFreeIndicesCondition, quantifierFreeBody, group.additiveIdentityElement());
		lastIndexConstraint = constraintTheory.makeSingleVariableConstraint(lastIndex, constraintTheory, context);
		Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint = Pair.make(body, lastIndexConstraint);
		
		return bodyAndLastIndexConstraint;
	}
}