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
package com.sri.ai.grinder.sgdpllt.helper;

import static com.sri.ai.expresso.helper.Expressions.apply;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.base.AbstractExpressionsSequenceStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.base.AbstractSelectionStepSolver;

/**
 * A context-dependent problem step solver deciding which in a set of expressions that satisfy
 * a comparison according to a given functor and bound.
 * <p>
 * Unlike many step solvers finding Expression-typed solutions, this one
 * is not an extension of {@link ContextDependentExpressionProblemStepSolver}
 * because it extends {@link AbstractExpressionsSequenceStepSolver},
 * which does not necessarily have Expression-typed solutions.
 * This has the disadvantage of requiring the use of
 * generic {@link ContextDependentProblemStepSolver#SolutionStep},
 * instead of its more common specialization
 * {@link ContextDependentExpressionProblemStepSolver#SolutionStep}.
 *
 * @author braz
 *
 */
@Beta
public class SelectExpressionsSatisfyingComparisonStepSolver extends AbstractSelectionStepSolver {

	private String functor;
	private Expression bound;
	
	public SelectExpressionsSatisfyingComparisonStepSolver(List<Expression> expressions, String functor, Expression bound) {
		super(expressions);
		this.functor = functor;
		this.bound = bound;
	}
	
	@Override
	protected Expression makeLiteralBasedOn(Expression currentExpression) {
		return apply(functor, currentExpression, bound);
	}
}