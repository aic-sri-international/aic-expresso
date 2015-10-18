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

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.extendContextualSymbolsWithIndexExpressions;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.getIndex;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.getLast;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.CommonSimplifier;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.solver.ContextDependentProblemSolver;
import com.sri.ai.grinder.sgdpll2.core.solver.QuantifierOnBodyWithIndexInLiteralsOnlyStepSolver;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.util.base.Pair;

/**
 * An extension of {@link AbstractInterpreter} re-using {@link CommonSimplifier}
 * (provided by {@link #makeAnotherMapBasedSimplifier()},
 * and augmented with symbolic solvers for
 * summations, and universal and existentially quantified formulas.
 *
 * @author braz
 *
 */
@Beta
public class SymbolicCommonInterpreter extends AbstractCommonInterpreter {

	private ConstraintTheory constraintTheory;
	
	/**
	 * Constructs {@link SymbolicCommonInterpreter} with a constraint theory and
	 * <i>not</i> simplifying literals according to contextual constraint.
	 * @param constraintTheory
	 */
	public SymbolicCommonInterpreter(ConstraintTheory constraintTheory) {
		this(constraintTheory, false);
	}

	/**
	 * Constructs {@link SymbolicCommonInterpreter} with a constraint theory and
	 * setting it to simplify literals according to contextual constraint stored in
	 * <code>process</code>'s global object under {@link #INTERPRETER_CONTEXTUAL_CONSTRAINT}.
	 * @param constraintTheory
	 * @param simplifyGivenConstraint
	 */
	public SymbolicCommonInterpreter(ConstraintTheory constraintTheory, boolean simplifyGivenConstraint) {
		super(simplifyGivenConstraint);
		this.constraintTheory = constraintTheory;
	}
	
	@Override
	protected Expression evaluateAggregateOperation(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression indicesCondition,
			Expression body,
			RewritingProcess process) throws Error {

		Constraint contextualConstraint = (Constraint) process.getGlobalObject(INTERPRETER_CONTEXTUAL_CONSTRAINT);
		process = extendContextualSymbolsWithIndexExpressions(indexExpressions, process);
		Expression quantifierFreeBody = apply(body, process);
		Expression quantifierFreeIndicesCondition = apply(indicesCondition, process);
		int numberOfIndices = indexExpressions.getList().size();
		
		// Re-use {@link SingleVariableConstraint} if condition is one.
		// TODO: eventually we want the algorithm to work so that it splitters may be entire constraints,
		// if they are found. Then this encoding would become superfluous,
		// and the condition could always be safely encoded in the body, since it would then be picked and re-used.
		// This would also re-use body if it happens to be a constraint.
		Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint =
				encodeConditionAsLastIndexConstraintIfPossibleOrInBodyOtherwise(
						group, indexExpressions, quantifierFreeIndicesCondition, quantifierFreeBody, process);
		Expression currentBody = bodyAndLastIndexConstraint.first;
		SingleVariableConstraint lastIndexConstraint = bodyAndLastIndexConstraint.second;
		
		
		for (int i = numberOfIndices - 1; i >= 0; i--) { // evaluate from inside out; this may change in the future
			Expression indexExpression = indexExpressions.getList().get(i);
			Expression index = getIndex(indexExpression);
			SingleVariableConstraint constraintForThisIndex =
					i == numberOfIndices - 1?
							lastIndexConstraint
							: constraintTheory.makeSingleVariableConstraint(index);
			ContextDependentProblemStepSolver solver =
					new QuantifierOnBodyWithIndexInLiteralsOnlyStepSolver(
							group, constraintForThisIndex, currentBody);
			currentBody = ContextDependentProblemSolver.solve(solver, contextualConstraint, process);
		}
		
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
	private Pair<Expression, SingleVariableConstraint>
	encodeConditionAsLastIndexConstraintIfPossibleOrInBodyOtherwise(
			AssociativeCommutativeGroup group,
			ExtensionalIndexExpressionsSet indexExpressions,
			Expression quantifierFreeIndicesCondition,
			Expression quantifierFreeBody,
			RewritingProcess process) {
		
		Expression body;
		SingleVariableConstraint lastIndexConstraint = null;
		Expression lastIndex = getIndex(getLast(indexExpressions.getList()));
		try {
			body = quantifierFreeBody;
			lastIndexConstraint = SingleVariableConstraint.make(constraintTheory, lastIndex, quantifierFreeIndicesCondition, process);
			return Pair.make(body, lastIndexConstraint);
		} catch (Error e) { /* proceed to default case before */ }
		
		// did not work out because condition is not SingleVariableConstraint on last index
		body = IfThenElse.make(quantifierFreeIndicesCondition, quantifierFreeBody, group.additiveIdentityElement());
		lastIndexConstraint = constraintTheory.makeSingleVariableConstraint(lastIndex);
		Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint = Pair.make(body, lastIndexConstraint);
		
		return bodyAndLastIndexConstraint;
	}

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		AbstractInterpreter interpreter = new SymbolicCommonInterpreter(new EqualityConstraintTheory(), true);
		RewritingProcess process = new DefaultRewritingProcess(null);
		Constraint contextualConstraint = new CompleteMultiVariableConstraint(new EqualityConstraintTheory());
		contextualConstraint = contextualConstraint.conjoin(parse("W != 3"), process);
		process.putGlobalObject(INTERPRETER_CONTEXTUAL_CONSTRAINT, contextualConstraint);
		process = process.put(new Categorical("Population", 500000, arrayList(parse("tom")))); // two pitfalls: immutable process and need for arrayList rather than just list
		process = process.put(new Categorical("Numbers", 3, arrayList(parse("1"), parse("2"), parse("3"))));
//		Expression expression = parse("there exists X in Numbers : X = 3 and X + 1 = 1 + X");
//		Expression expression = parse("if for all X in Population : (there exists Y in Population : Y != X and W != 3) then Hurrah else not Hurrah");
//		Expression expression = parse("there exists Y in Population : Y != tom and W != 3");
		Expression expression = parse("sum({{(on Y in Population) 2 | for all X in Population : (X = tom) => Y != X and W != 3 and Z != 2}})");
//		Expression expression = parse("sum({{(on Y in Population, Z in Numbers) 2 | Y != tom and Z != 2}})");
//		Expression expression = parse("sum({{(on Y in Population, Z in Numbers) 2 | (for all X in Population : (X = tom) => Y != X) and W != 3 and Z != 2}})");
//		Expression expression = parse("product({{(on Y in Population) 2 | Y != tom and W != 3}})");
//		Expression expression = parse("max({{(on Y in Population) 2 | for all X in Population : (X = tom) => Y != X and W != 3 and Z != 2}})");
		Expression result = interpreter.apply(expression, process);
		System.out.println("result: " + result);
	}
}