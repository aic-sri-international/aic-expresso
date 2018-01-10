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
package com.sri.ai.grinder.helper;

import static com.sri.ai.expresso.helper.Expressions.apply;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.StepSolver;
import com.sri.ai.grinder.theory.base.AbstractExpressionsSequenceStepSolver;

/**
 * A context-dependent problem step solver deciding which in a set of expressions is the maximum one
 * given an order.
 * <p>
 * Note that it would be intuitive to have this class implement {@link ExpressionStepSolver},
 * but this class delegates to its super class, whose {@link #step(Context)} method returns a {@link ContextDependentProblemStepSolver#SolverStep<Expression>},
 * and we do not want to have to construct a new {@link ExpressionStepSolver#SolverStep}
 * object every time just to conform to the interface.
 *
 * @author braz
 *
 */
@Beta
public class MaximumExpressionStepSolver extends AbstractExpressionsSequenceStepSolver<Expression> implements ExpressionLiteralSplitterStepSolver {

	private Expression order;
	private Expression orderMaximum;
	private Expression maximumSoFar;

	@Override
	public MaximumExpressionStepSolver clone() {
		MaximumExpressionStepSolver clone = (MaximumExpressionStepSolver) super.clone();
		return clone;
	}
	
	/**
	 * Makes step solver
	 * @param expressions the expressions being compared
	 * @param order the operator for "less than" in this order
	 * @param orderMinimum the minimum value in the order (not the type! For example, the minimum in the < order is -infinity, and the minimum in the > order is infinity).
	 * @param orderMaximum the maximum value in the type (not the type! For example, the maximum in the < order is infinity, and the maximum in the > order is -infinity).
	 */
	public MaximumExpressionStepSolver(List<Expression> expressions, Expression order, Expression orderMinimum, Expression orderMaximum) {
		this(expressions, order, orderMaximum, orderMinimum, 0);
	}

	/**
	 * Makes step solver
	 * @param expressions the expressions being compared
	 * @param order the operator for "less than" in this order
	 * @param orderMinimum the minimum value in the type
	 * @param orderMinimum the minimum value in the order (not the type! For example, the minimum in the < order is -infinity, and the minimum in the > order is infinity).
	 * @param orderMaximum the maximum value in the type (not the type! For example, the maximum in the < order is infinity, and the maximum in the > order is -infinity).
	 */
	private MaximumExpressionStepSolver(List<Expression> expressions, Expression order, Expression orderMaximum, Expression maximumSoFar, int current) {
		super(expressions, current);
		this.order = order;
		this.orderMaximum = orderMaximum;
		this.maximumSoFar = maximumSoFar;
	}
	
	@Override
	public ExpressionLiteralSplitterStepSolver.Step step(Context context) {
		ExpressionLiteralSplitterStepSolver.Step result;
		if (maximumSoFar.equals(orderMaximum)) { // short-circuiting if maximum already found
			result = new ExpressionLiteralSplitterStepSolver.Solution(orderMaximum);
		}
		else {
			StepSolver.Step<Expression> innerResult = super.step(context);
			if (innerResult.itDepends()) {
				result = new ExpressionLiteralSplitterStepSolver.ItDependsOn(innerResult.getSplitter(), 
								innerResult.getContextSplittingWhenSplitterIsLiteral(), 
								(ExpressionLiteralSplitterStepSolver) innerResult.getStepSolverForWhenSplitterIsTrue(), 
								(ExpressionLiteralSplitterStepSolver) innerResult.getStepSolverForWhenSplitterIsFalse());
			}
			else {
				result = new ExpressionLiteralSplitterStepSolver.Solution(innerResult.getValue());
			}
		}
		return result;
	}

	@Override
	protected Expression makeLiteralBasedOn(Expression currentExpression) {
		Expression result = apply(order, maximumSoFar, currentExpression);
		return result;
	}

	@Override
	protected ExpressionLiteralSplitterStepSolver makeSubStepSolverWhenLiteralIsTrue() {
		return new MaximumExpressionStepSolver(getExpressions(), order, orderMaximum, getCurrentExpression(), getCurrent() + 1);
	}

	@Override
	protected ExpressionLiteralSplitterStepSolver makeSubStepSolverWhenLiteralIsFalse() {
		return new MaximumExpressionStepSolver(getExpressions(), order, orderMaximum, maximumSoFar, getCurrent() + 1);
	}

	@Override
	protected ExpressionLiteralSplitterStepSolver.Step makeSolutionWhenAllElementsHaveBeenChecked() {
		ExpressionLiteralSplitterStepSolver.Solution result = new ExpressionLiteralSplitterStepSolver.Solution(maximumSoFar);
		return result;
	}
}