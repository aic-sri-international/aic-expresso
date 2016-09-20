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
package com.sri.ai.grinder.sgdpllt.theory.base;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.core.constraint.ContextSplitting;
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
 * The way the literal is formed is specified by abstract method {@link #makeLiteral()}.
 * <p>
 * Implementing classes must provide implementations for methods
 * {@link #makeSubStepSolverForWhenLiteralIsTrue()}
 * and 
 * {@link #makeSubStepSolverForWhenLiteralIsFalse()}
 * which must be step solvers for the same problem, but which are
 * allowed to assume the literal to be defined as true and false in the context, respectively.
 *
 * @author braz
 *
 */
@Beta
public abstract class AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver implements ExpressionStepSolver {

	private List<Expression> expressions;
	private OrderedPairsOfIntegersIterator initialIndices;
	// indices ranges over pairs (i, j) of indices of 'expressions'
	// all expressions before the one indexed by i have been checked to be distinct or not from all others in front of it
	// all expressions after the one indexed by i and before the one indexed by j have been checked to be distinct from the one indexed by i
	
	private boolean setFieldsForCaseInWhichThereArePairs = false;
	private boolean hasPair;
	protected PairOf<Integer> pair;
	protected OrderedPairsOfIntegersIterator nextIndices;
	protected int i;
	protected int j;
	protected Expression iThExpression;
	protected Expression jThExpression;

	/**
	 * Provides solver step for cases in which there are no, or only one, expressions.
	 * @return
	 */
	abstract public Solution makeSolutionStepWhenThereAreNoPairs();

	/**
	 * Provides solver step after going over all ordered pairs;
	 * note that fields {@link #i}, {@link #j},
	 * {@link #iThExpression}, {@link #jThExpression} and {@link #nextIndices}
	 * are <i>not</i> set at this point, when we ran out of pairs,
	 * even if there has been pairs up to here.
	 * @return
	 */
	abstract public Solution makeSolutionStepAfterGoingOverAllPairs();

	/**
	 * Creates literal for splitting based on ordered pair (i, j).
	 * @return
	 */
	abstract public Expression makeLiteral();

	/**
	 * Makes sub-step solver for when literal is true.
	 * @return
	 */
	abstract public AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver makeSubStepSolverForWhenLiteralIsTrue();

	/**
	 * Makes sub-step solver for when literal is false.
	 * @return
	 */
	abstract public AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver makeSubStepSolverForWhenLiteralIsFalse();

	public AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver(ArrayList<Expression> expressions) {
		this(expressions, 0, 1);
	}

	public AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver(List<Expression> expressions, int i, int j) {
		this(expressions, new OrderedPairsOfIntegersIterator(expressions.size(), i, j));
	}

	protected AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver(List<Expression> expressions, OrderedPairsOfIntegersIterator initialIndices) {
		super();
		this.expressions = expressions;
		this.initialIndices = initialIndices;
		if (expressions.size() > 1) {
			setFieldsForCaseInWhichThereArePairs();
		}
	}

	@Override
	public ExpressionStepSolver clone() {
		try {
			return (ExpressionStepSolver) super.clone();
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

	public boolean hasPair() {
		return hasPair;
	}

	private void setFieldsForCaseInWhichThereArePairs() {
		if ( ! setFieldsForCaseInWhichThereArePairs) {
			nextIndices = initialIndices.clone();
			hasPair = nextIndices.hasNext();
			if (hasPair) {
				pair = nextIndices.next();
				i = pair.first;
				j = pair.second;
				iThExpression = getExpressions().get(i);
				jThExpression = getExpressions().get(j);
			}
			setFieldsForCaseInWhichThereArePairs = true;
		}
	}

	/**
	 * Return how many elements have been checked against all its successors in the sequence of expressions.
	 * @return
	 */
	public int numberOfElementsAlreadyExamined() {
		if (expressions.size() < 2) {
			return expressions.size(); // no pairs, so no examination needed, so all are considered examined.
		}
		else {
			return i;
		}
	}

	@Override
	public SolverStep step(Context context) {
		
		if (expressions.size() < 2) {
			return makeSolutionStepWhenThereAreNoPairs();
		}

		if (hasPair()) {

			Expression unsimplifiedLiteral = makeLiteral();
			Expression literal = context.getTheory().simplify(unsimplifiedLiteral, context);

			AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver stepSolverForWhenLiteralIsTrue  = null; // this null is never used, just making compiler happy
			AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver stepSolverForWhenLiteralIsFalse = null; // this null is never used, just making compiler happy

			ContextSplitting split = new ContextSplitting(literal, context);
			if (split.getResult().equals(ContextSplitting.Result.CONSTRAINT_IS_CONTRADICTORY)) {
				return null;
			}
			
			boolean literalIsTrue  =  split.getResult() == ContextSplitting.Result.LITERAL_IS_TRUE;
			boolean literalIsFalse = !literalIsTrue && split.getResult() == ContextSplitting.Result.LITERAL_IS_FALSE;
			boolean undefined = !literalIsTrue && !literalIsFalse;
			boolean needSubStepSolverForWhenLiteralIsTrue  = literalIsTrue  || undefined;
			boolean needSubStepSolverForWhenLiteralIsFalse = literalIsFalse || undefined;
			
			if (needSubStepSolverForWhenLiteralIsTrue) {
				stepSolverForWhenLiteralIsTrue = makeSubStepSolverForWhenLiteralIsTrue();
			}
			
			if (needSubStepSolverForWhenLiteralIsFalse) {
				stepSolverForWhenLiteralIsFalse = makeSubStepSolverForWhenLiteralIsFalse();
			}
			
			if (literalIsTrue) {
				return stepSolverForWhenLiteralIsTrue.step(split.getConstraintAndLiteral());
			}
			else if (literalIsFalse) {
				return stepSolverForWhenLiteralIsFalse.step(split.getConstraintAndLiteralNegation());
			}
			else {
				return new ItDependsOn(literal, split, stepSolverForWhenLiteralIsTrue, stepSolverForWhenLiteralIsFalse);
			}
		}
		// went over all pairs
		Solution result = makeSolutionStepAfterGoingOverAllPairs();
		return result;
	}
}