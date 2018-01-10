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
package com.sri.ai.grinder.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;

/**
 * A context-dependent problem step solver deciding whether the number of unique given expression is smaller than a given limit
 *
 * @author braz
 *
 */
@Beta
public class NumberOfDistinctExpressionsIsLessThanStepSolver implements ExpressionLiteralSplitterStepSolver {

	private int limit;
	private List<Expression> expressions;
	private DistinctExpressionsStepSolver distinctExpressionsStepSolver;
	
	public NumberOfDistinctExpressionsIsLessThanStepSolver(int limit, ArrayList<Expression> expressions) {
		super();
		this.limit = limit;
		this.expressions = expressions;
		this.distinctExpressionsStepSolver = new DistinctExpressionsStepSolver(expressions);
	}

	@Override
	public NumberOfDistinctExpressionsIsLessThanStepSolver clone() {
		try {
			return (NumberOfDistinctExpressionsIsLessThanStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}

	/**
	 * Returns the underlying {@link DistinctExpressionsStepSolver}.
	 * This is useful if one wants to re-use the work done for computing distinct expressions
	 * without a limit on their number.
	 * @return
	 */
	public DistinctExpressionsStepSolver getDistinctExpressionsStepSolver() {
		return distinctExpressionsStepSolver;
	}
	
	@Override
	public Step step(Context context) {
		if (distinctExpressionsStepSolver.getUniqueValuesWhenStepSolverWasConstructed().size() >= limit) {
			return new Solution(FALSE);
		}
		else if (distinctExpressionsStepSolver.getUniqueValuesWhenStepSolverWasConstructed().size() + maximumPossibleNumberOfRemainingUniqueExpressions() < limit) {
			// we already know the limit will never be reached
			return new Solution(TRUE);
		}

		Step step = distinctExpressionsStepSolver.step(context);
		if (step.itDepends()) {
			NumberOfDistinctExpressionsIsLessThanStepSolver subStepSolverWhenFormulaIsTrue = clone();
			subStepSolverWhenFormulaIsTrue.distinctExpressionsStepSolver = (DistinctExpressionsStepSolver) step.getStepSolverForWhenSplitterIsTrue();

			NumberOfDistinctExpressionsIsLessThanStepSolver subStepSolverWhenFormulaIsFalse = clone();
			subStepSolverWhenFormulaIsFalse.distinctExpressionsStepSolver = (DistinctExpressionsStepSolver) step.getStepSolverForWhenSplitterIsFalse();

			return new ItDependsOn(step.getSplitterLiteral(), step.getContextSplittingWhenSplitterIsLiteral(), subStepSolverWhenFormulaIsTrue, subStepSolverWhenFormulaIsFalse);
		}
		else {
			return new Solution(makeSymbol(step.getValue().numberOfArguments() < limit));
		}
	}
	
	private int maximumPossibleNumberOfRemainingUniqueExpressions() {
		return expressions.size() - distinctExpressionsStepSolver.numberOfElementsAlreadyExamined();
	}
}