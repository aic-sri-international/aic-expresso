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
package com.sri.ai.grinder.sgdpll2.theory.base;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;
import com.sri.ai.util.base.OrderedPairsOfIntegersIterator;
import com.sri.ai.util.base.PairOf;

/**
 * A context-dependent problem step solver 
 * making decisions for literals based on all pairs of expressions in a given list
 * <p>
 * The pairs of expressions considered are those of expressions whose indices are ordered.
 * For example, if the list is <code>a, b, c</code>, then
 * <code>(a, b)</code>, <code>(a, c)</code> and <code>(b, c)</code>
 * will be considered.
 * <p>
 * The list is randomly accessed, and therefore it is preferable that it offers constant-time random access.
 * <p> 
 * The way the literal is formed is specified by abstract method {@link #makeLiteral(int, int)}.
 * <p>
 * Implementing classes must provide implementations for methods
 * {@link #makeSubStepSolverForWhenLiteralIsTrue(OrderedPairsOfIntegersIterator)}
 * and 
 * {@link #makeSubStepSolverForWhenLiteralIsFalse(OrderedPairsOfIntegersIterator)}
 * which must be step solvers for the same problem, but which are
 * allowed to assume the literal to be defined as true and false in the contextual constraint, respectively.
 *
 * @author braz
 *
 */
@Beta
public abstract class AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver implements ContextDependentExpressionProblemStepSolver {

	private List<Expression> expressions;
	private OrderedPairsOfIntegersIterator initialIndices;
	// indices ranges over pairs (i, j) of indices of 'expressions'
	// all expressions before the one indexed by i have been checked to be distinct or not from all others in front of it
	// all expressions after the one indexed by i and before the one indexed by j have been checked to be distinct from the one indexed by i
	private int numberOfElementsExaminedSoFar; // number of expressions before the one indexed by i that are equal to some element after it (that is, they don't count towards the number of distinct disequals
	
	/**
	 * Provides solution step after going over all ordered pairs.
	 * @return
	 */
	abstract public Solution makeSolutionStepAfterGoingOverAllPairs();

	/**
	 * Creates literal for splitting based on ordered pair (i, j).
	 * @param i
	 * @param j
	 * @return
	 */
	abstract public Expression makeLiteral(int i, int j);

	/**
	 * @param indices the indices of the literal, which can, in the sub-step solver, be assumed to be false.
	 * @return
	 */
	abstract public AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver makeSubStepSolverForWhenLiteralIsFalse(OrderedPairsOfIntegersIterator indices);

	/**
	 * @param indices the indices of the literal, which can, in the sub-step solver, be assumed to be true.
	 * @return
	 */
	abstract public AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver makeSubStepSolverForWhenLiteralIsTrue(OrderedPairsOfIntegersIterator indices);

	public AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver(ArrayList<Expression> expressions) {
		this(expressions, 0, 1);
	}

	public AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver(List<Expression> expressions, int i, int j) {
		super();
		this.expressions = expressions;
		this.initialIndices = new OrderedPairsOfIntegersIterator(expressions.size(), i, j);
		this.numberOfElementsExaminedSoFar = i;
	}

	protected AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver(List<Expression> expressions, OrderedPairsOfIntegersIterator initialIndices, int numberOfElementsExaminedSoFar) {
		super();
		this.expressions = expressions;
		this.initialIndices = initialIndices;
		this.numberOfElementsExaminedSoFar = numberOfElementsExaminedSoFar;
	}

	@Override
	public ContextDependentExpressionProblemStepSolver clone() {
		try {
			return (ContextDependentExpressionProblemStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}
	
	/**
	 * Returns the list of expressions.
	 * @return
	 */
	public List<Expression> getExpressions() {
		return Collections.unmodifiableList(expressions);
	}

	/**
	 * Number of values for <code>i</code> such that all pairs with indices <code>(i, j)</code>
	 * have already been examined.
	 * In other words, the first component of the first pair analysed by this step solver
	 * has index {@link #getNumberOfElementsExaminedSoFar()}
	 * (this method does not say anything about the index of the second component of the first pair).
	 * @return
	 */
	public int getNumberOfElementsExaminedSoFar() {
		return numberOfElementsExaminedSoFar;
	}

	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {

		OrderedPairsOfIntegersIterator indices = initialIndices.clone();
		
		if (indices.hasNext()) {
			PairOf<Integer> pair = indices.next();
			int i = pair.first;
			int j = pair.second;

			numberOfElementsExaminedSoFar = i;
			
			Expression unsimplifiedLiteral = makeLiteral(i, j);
			Expression literal = contextualConstraint.getConstraintTheory().simplify(unsimplifiedLiteral, process);

			AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver stepSolverForWhenLiteralIsTrue  = null; // this null is never used, just making compiler happy
			AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver stepSolverForWhenLiteralIsFalse = null; // this null is never used, just making compiler happy

			ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, literal, process);
			if (split.getResult().equals(ConstraintSplitting.Result.CONSTRAINT_IS_CONTRADICTORY)) {
				return null;
			}
			
			boolean literalIsTrue  =  split.getResult() == ConstraintSplitting.Result.LITERAL_IS_TRUE;
			boolean literalIsFalse = !literalIsTrue && split.getResult() == ConstraintSplitting.Result.LITERAL_IS_FALSE;
			boolean undefined = !literalIsTrue && !literalIsFalse;
			boolean needSubStepSolverForWhenLiteralIsTrue  = literalIsTrue  || undefined;
			boolean needSubStepSolverForWhenLiteralIsFalse = literalIsFalse || undefined;
			
			if (needSubStepSolverForWhenLiteralIsTrue) {
				stepSolverForWhenLiteralIsTrue = makeSubStepSolverForWhenLiteralIsTrue(indices);
			}
			
			if (needSubStepSolverForWhenLiteralIsFalse) {
				stepSolverForWhenLiteralIsFalse = makeSubStepSolverForWhenLiteralIsFalse(indices);
			}
			
			if (literalIsTrue) {
				return stepSolverForWhenLiteralIsTrue.step(split.getConstraintAndLiteral(), process);
			}
			else if (literalIsFalse) {
				return stepSolverForWhenLiteralIsFalse.step(split.getConstraintAndLiteralNegation(), process);
			}
			else {
				return new ItDependsOn(literal, split, stepSolverForWhenLiteralIsTrue, stepSolverForWhenLiteralIsFalse);
			}
		}
		// went over all pairs
		numberOfElementsExaminedSoFar = expressions.size();
		Solution result = makeSolutionStepAfterGoingOverAllPairs();
		return result;
	}
}