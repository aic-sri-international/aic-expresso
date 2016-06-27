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
package com.sri.ai.grinder.sgdpll.theory.base;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;

/**
 * An extension of {@link AbstractLinearStepSolver}
 * that obtains the literals to be tested from a given list of expressions.
 * <p>
 * More specifically, it implements {@link #makeLiteral()} to pick the next expression
 * <code>e</code> from a given list of expressions,
 * and invokes a new method {@link #makeLiteralBasedOn(Expression)}
 * on <code>e</code> to obtain the next literal to be tested.
 * <p>
 * Like other extensions of {@link AbstractLinearStepSolver},
 * implementations of this class must still define
 * {@link #makeSubStepSolverWhenLiteralIsTrue()},
 * {@link #makeSubStepSolverWhenLiteralIsFalse()}, and
 * {@link #makeSolutionWhenAllElementsHaveBeenChecked()}.
 *
 * @author braz
 *
 */
@Beta
public abstract class AbstractExpressionsSequenceStepSolver<T> extends AbstractLinearStepSolver<T> {

	private List<Expression> expressions;

	/**
	 * Makes the decision literal based on a given expression.
	 * @param expression
	 * @return
	 */
	protected abstract Expression makeLiteralBasedOn(Expression expression);
	
	public AbstractExpressionsSequenceStepSolver(List<Expression> expressions) {
		this(expressions, 0);
	}

	protected AbstractExpressionsSequenceStepSolver(List<Expression> expressions, int current) {
		super(expressions.size(), current);
		this.expressions = expressions;
	}

	public List<Expression> getExpressions() {
		return expressions;
	}
	
	@Override
	protected Expression makeLiteral() {
		Expression result = makeLiteralBasedOn(getCurrentExpression());
		return result;
	}

	protected Expression getCurrentExpression() {
		Expression result = expressions.get(getCurrent());
		return result;
	}
}