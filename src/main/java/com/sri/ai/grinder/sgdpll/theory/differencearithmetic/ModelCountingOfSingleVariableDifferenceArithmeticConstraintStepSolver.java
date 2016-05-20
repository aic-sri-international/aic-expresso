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
package com.sri.ai.grinder.sgdpll.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.arrayList;

import java.util.ArrayList;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

/**
 * A {@link AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver}
 * computing the number of values for the variable satisfying the constraints.
 * <p>
 * The solution is guaranteed to be either a numerical constant, or
 * a conditional of the form {@code if <satisfiability condition> then <model count> else 0}.
 * 
 * @author braz
 *
 */
@Beta
public class ModelCountingOfSingleVariableDifferenceArithmeticConstraintStepSolver extends AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver {

	public ModelCountingOfSingleVariableDifferenceArithmeticConstraintStepSolver(SingleVariableDifferenceArithmeticConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public ModelCountingOfSingleVariableDifferenceArithmeticConstraintStepSolver clone() {
		return (ModelCountingOfSingleVariableDifferenceArithmeticConstraintStepSolver) super.clone();
	}

	@Override
	public SingleVariableDifferenceArithmeticConstraint getConstraint() {
		return (SingleVariableDifferenceArithmeticConstraint) super.getConstraint();
	}
	
	@Override
	public boolean unboundedVariableProducesShortCircuitSolution() {
		return true;
	}

	@Override
	protected Expression getSolutionExpressionGivenContradiction() {
		return ZERO;
	}

	@Override
	public Expression getSolutionExpressionForUnboundedVariables() {
		return INFINITY;
	}

	@Override
	public Expression getSolutionExpressionForBoundVariable() {
		return ONE;
	}
	
	@Override
	public Expression getSolutionExpressionGivenBoundsAndDistinctDisequals(Expression greatestStrictLowerBound, Expression leastNonStrictUpperBound, Expression boundsDifference, Expression distinctDisequalsSet, Context context) {
		// at this point, the context establishes that one of the strict lower bounds L is greater than all the others,
		// that one of the non-strict upper bounds U is less than all the others, and that
		// all disequals are in ]L, U], and are disequal from each other.
		// Therefore, the constraint is satisfiable if and only if U - L > D
		// where D is the number of disequals.
		Expression solutionExpression;
		Expression numberOfDistinctDisequals = makeSymbol(distinctDisequalsSet.numberOfArguments());
		ArrayList<Expression> boundsDifferenceAndNumberOfDisequals = arrayList(boundsDifference, numberOfDistinctDisequals);
		solutionExpression = applyAndSimplify(MINUS, boundsDifferenceAndNumberOfDisequals, context);
		return solutionExpression;
	}
}