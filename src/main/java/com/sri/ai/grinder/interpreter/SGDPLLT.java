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
package com.sri.ai.grinder.interpreter;

import static com.sri.ai.grinder.helper.GrinderUtil.extendContextualSymbolsWithIndexExpressions;
import static com.sri.ai.grinder.helper.GrinderUtil.makeIndexExpressionsForIndicesInListAndTypesInContext;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.getIndex;
import static com.sri.ai.grinder.sgdpll2.core.solver.AbstractQuantifierEliminationStepSolver.makeEvaluator;
import static com.sri.ai.util.Util.getLast;

import java.util.Collection;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.QuantifierEliminatorWithSetup;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.SimplifierUnderContextualConstraint;
import com.sri.ai.grinder.core.AbstractQuantifierEliminatorWithSetup;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.api.GroupProblemType;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.util.base.Pair;

/**
 * A {@link QuantifierEliminatorWithSetup} implementing the SGDPLL(T) algorithm.
 *
 * @author braz
 *
 */
@Beta
public class SGDPLLT extends AbstractQuantifierEliminatorWithSetup {

	private SimplifierUnderContextualConstraint simplifierUnderContextualConstraint;
	private GroupProblemType problemType;
	private ConstraintTheory constraintTheory;
	
	public SGDPLLT(SimplifierUnderContextualConstraint simplifier, GroupProblemType problemType, ConstraintTheory constraintTheory) {
		super();
		this.simplifierUnderContextualConstraint = simplifier;
		this.problemType = problemType;
		this.constraintTheory = constraintTheory;
	}

	@Override
	public Constraint makeTrueConstraint(Collection<Expression> indices) {
		return new CompleteMultiVariableConstraint(constraintTheory);
	}

	@Override
	public Expression simplify(Expression expression, RewritingProcess process) {
		return simplifierUnderContextualConstraint.apply(expression, process);
	}

	@Override
	public Expression getAdditiveIdentityElement() {
		return problemType.additiveIdentityElement();
	}

	@Override
	public RewritingProcess makeProcess(
			Constraint constraint, 
			Map<String, String> mapFromSymbolNameToTypeName,
			Map<String, String> mapFromCategoricalTypeNameToSizeString,
			Collection<Type> additionalTypes,
			Predicate<Expression> isUniquelyNamedConstantPredicate) {
		
		return DPLLUtil.makeProcess(
				mapFromSymbolNameToTypeName,
				mapFromCategoricalTypeNameToSizeString,
				additionalTypes,
				isUniquelyNamedConstantPredicate) ;
	}

	
	@Override
	public Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, RewritingProcess process) {
		ExtensionalIndexExpressionsSet indexExpressionsSet = makeIndexExpressionsForIndicesInListAndTypesInContext(indices, process);
		Constraint2 trueContextualConstraint = (Constraint2) makeTrueConstraint(indices);
		Expression quantifierFreeConstraint = simplifierUnderContextualConstraint.apply(constraint, process);
		Expression quantifierFreeBody = simplifierUnderContextualConstraint.apply(body, process);
		Expression result = solve(problemType, simplifierUnderContextualConstraint, indexExpressionsSet, quantifierFreeConstraint, quantifierFreeBody, trueContextualConstraint, process);
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
	 * @param process
	 * @return
	 */
	public static Expression solve(
			AssociativeCommutativeGroup group,
			SimplifierUnderContextualConstraint simplifierUnderContextualConstraint,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression quantifierFreeIndicesCondition,
			Expression quantifierFreeBody,
			Constraint2 contextualConstraint,
			RewritingProcess process) {
		
		ConstraintTheory constraintTheory = contextualConstraint.getConstraintTheory();
		
		process = extendContextualSymbolsWithIndexExpressions(indexExpressions, process);
		
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
							group, indexExpressions, quantifierFreeIndicesCondition, quantifierFreeBody, constraintTheory, process);
			currentBody = bodyAndLastIndexConstraint.first;
			SingleVariableConstraint lastIndexConstraint = bodyAndLastIndexConstraint.second;

			if (lastIndexConstraint == null) {
				return group.additiveIdentityElement();
			}

			for (int i = numberOfIndices - 1; i >= 0; i--) { // evaluate from inside out; this may change in the future
				Expression indexExpression = indexExpressions.getList().get(i);
				Expression index = getIndex(indexExpression);
				SingleVariableConstraint constraintForThisIndex =
						i == numberOfIndices - 1?
								lastIndexConstraint
								: constraintTheory.makeSingleVariableConstraint(index, constraintTheory, process);
				currentBody =
						constraintTheory.getSingleVariableConstraintQuantifierEliminatorStepSolver(
								group, constraintForThisIndex, currentBody, simplifierUnderContextualConstraint, process).
						solve(contextualConstraint, process);
			}
		}
		else {
			currentBody = IfThenElse.make(quantifierFreeIndicesCondition, currentBody, group.additiveIdentityElement());
		}
		
		// Normalize final result.
		ContextDependentExpressionProblemStepSolver evaluator
		= makeEvaluator(currentBody, simplifierUnderContextualConstraint, constraintTheory);
		currentBody = evaluator.solve(contextualConstraint, process);
		
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
	 * @param process
	 * @return
	 */
	private static Pair<Expression, SingleVariableConstraint>
	encodeConditionAsLastIndexConstraintIfPossibleOrInBodyOtherwise(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression quantifierFreeIndicesCondition,
			Expression quantifierFreeBody,
			ConstraintTheory constraintTheory,
			RewritingProcess process) {
		
		Expression body;
		SingleVariableConstraint lastIndexConstraint = null;
		Expression lastIndex = getIndex(getLast(indexExpressions.getList()));
		try {
			body = quantifierFreeBody;
			lastIndexConstraint = SingleVariableConstraint.make(constraintTheory, lastIndex, quantifierFreeIndicesCondition, process);
			return Pair.make(body, lastIndexConstraint);
		} catch (Error e) { /* proceed to default case below */ }
		
		// did not work out because condition is not SingleVariableConstraint on last index
		body = IfThenElse.make(quantifierFreeIndicesCondition, quantifierFreeBody, group.additiveIdentityElement());
		lastIndexConstraint = constraintTheory.makeSingleVariableConstraint(lastIndex, constraintTheory, process);
		Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint = Pair.make(body, lastIndexConstraint);
		
		return bodyAndLastIndexConstraint;
	}
}