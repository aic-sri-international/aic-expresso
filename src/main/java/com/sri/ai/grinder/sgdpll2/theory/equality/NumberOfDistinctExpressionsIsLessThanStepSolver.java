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
package com.sri.ai.grinder.sgdpll2.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;

/**
 * A context-dependent problem step solver deciding whether the number of unique given expression is smaller than a given limit
 *
 * @author braz
 *
 */
@Beta
public class NumberOfDistinctExpressionsIsLessThanStepSolver implements ContextDependentProblemStepSolver {

	private int limit;
	private List<Expression> expressions;
	private NumberOfDistinctExpressionsStepSolver counterStepSolver;
	
	public NumberOfDistinctExpressionsIsLessThanStepSolver(int limit, ArrayList<Expression> expressions) {
		this(limit, expressions, 0, 1, 0);
	}

	public NumberOfDistinctExpressionsIsLessThanStepSolver(int limit, List<Expression> expressions, int i, int j, int numberOfNonUniqueExpressionsSoFar) {
		super();
		this.limit = limit;
		this.expressions = expressions;
		this.counterStepSolver = new NumberOfDistinctExpressionsStepSolver(expressions, i, j, numberOfNonUniqueExpressionsSoFar);
	}

	private NumberOfDistinctExpressionsIsLessThanStepSolver(int limit, List<Expression> expressions, NumberOfDistinctExpressionsStepSolver counterStepSolver) {
		this.limit = limit;
		this.expressions = expressions;
		this.counterStepSolver = counterStepSolver;
	}

	@Override
	public ContextDependentProblemStepSolver clone() {
		try {
			return (ContextDependentProblemStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}

	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {
		if (counterStepSolver.numberOfUniqueExpressionsSoFar() >= limit) {
			return new Solution(FALSE);
		}
		else if (counterStepSolver.numberOfUniqueExpressionsSoFar() + maximumPossibleNumberOfRemainingUniqueExpressions() < limit) {
			// we already know the limit will never be reached
			return new Solution(TRUE);
		}

		SolutionStep step = counterStepSolver.step(contextualConstraint, process);
		if (step.itDepends()) {
			NumberOfDistinctExpressionsIsLessThanStepSolver subStepSolverWhenFormulaIsTrue
			= new NumberOfDistinctExpressionsIsLessThanStepSolver(limit, expressions, (NumberOfDistinctExpressionsStepSolver) step.getStepSolverForWhenExpressionIsTrue());

			NumberOfDistinctExpressionsIsLessThanStepSolver subStepSolverWhenFormulaIsFalse
			= new NumberOfDistinctExpressionsIsLessThanStepSolver(limit, expressions, (NumberOfDistinctExpressionsStepSolver) step.getStepSolverForWhenExpressionIsFalse());

			return new ItDependsOn(step.getExpression(), step.getConstraintSplitting(), subStepSolverWhenFormulaIsTrue, subStepSolverWhenFormulaIsFalse);
		}
		else {
			return new Solution(makeSymbol(step.getExpression().intValue() < limit));
		}
	}
	
	private int maximumPossibleNumberOfRemainingUniqueExpressions() {
		return expressions.size() - counterStepSolver.getNumberOfElementsExaminedSoFar();
	}
}