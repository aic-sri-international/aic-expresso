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
package com.sri.ai.grinder.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.ZERO;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.core.solver.AbstractSingleQuantifierEliminationStepSolver;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.grinder.polynomial.core.DefaultPolynomial;
import com.sri.ai.grinder.polynomial.core.PolynomialIntegration;
import com.sri.ai.grinder.polynomial.core.PolynomialSummation;

/**
 * A step solver for a summation with an real index constrained by linear real arithmetic literals,
 * over a polynomial.
 * It works by evaluating the body until there are no literals on it,
 * computing the index satisfying values for the index,
 * and using {@link PolynomialSummation}.
 * 
 * @author braz
 *
 */
@Beta
public class SummationOnLinearRealArithmeticAndPolynomialStepSolver extends AbstractSingleQuantifierEliminationStepSolver {

	// TODO: before making changes to this class,
	// it would be better to abstract its common aspects (lots of them)
	// with SummationOnDifferenceArithmeticAndPolynomialStepSolver
	// (the changes will probably need to be reflected there as well,
	// so it's better to unify first).
	
	private IntervalWithMeasureEquivalentToSingleVariableLinearRealArithmeticConstraintStepSolver valuesOfSingleVariableLinearRealArithmeticConstraintStepSolver;
	
	public SummationOnLinearRealArithmeticAndPolynomialStepSolver(SingleQuantifierEliminationProblem problem) {
		super(problem);
		valuesOfSingleVariableLinearRealArithmeticConstraintStepSolver =
				new IntervalWithMeasureEquivalentToSingleVariableLinearRealArithmeticConstraintStepSolver(
						(SingleVariableLinearRealArithmeticConstraint) getIndexConstraint());
	}

	@Override
	public SummationOnLinearRealArithmeticAndPolynomialStepSolver clone() {
		return (SummationOnLinearRealArithmeticAndPolynomialStepSolver) super.clone();
	}
	
	@Override
	protected SummationOnLinearRealArithmeticAndPolynomialStepSolver makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint) {
		SingleQuantifierEliminationProblem newProblem = getProblem().makeWithNewIndexConstraint(newIndexConstraint);
		SummationOnLinearRealArithmeticAndPolynomialStepSolver result = new SummationOnLinearRealArithmeticAndPolynomialStepSolver(newProblem);
		return result;
	}

	@Override
	protected Step eliminateQuantifierForLiteralFreeBody(
			Expression literalFreeBody,
			Context context) {

		ExpressionLiteralSplitterStepSolver.Step step = 
				valuesOfSingleVariableLinearRealArithmeticConstraintStepSolver.step(context);
		if (step == null) {
			return null;
		}
		if (step.itDepends()) {
			SummationOnLinearRealArithmeticAndPolynomialStepSolver ifTrue = clone();
			ifTrue.valuesOfSingleVariableLinearRealArithmeticConstraintStepSolver =
					(IntervalWithMeasureEquivalentToSingleVariableLinearRealArithmeticConstraintStepSolver)
					step.getStepSolverForWhenSplitterIsTrue();
			SummationOnLinearRealArithmeticAndPolynomialStepSolver ifFalse = clone();
			ifFalse.valuesOfSingleVariableLinearRealArithmeticConstraintStepSolver =
					(IntervalWithMeasureEquivalentToSingleVariableLinearRealArithmeticConstraintStepSolver)
					step.getStepSolverForWhenSplitterIsFalse();
			return new ItDependsOn(step.getSplitterLiteral(), step.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
		}
		Expression values = step.getValue();
		
		Expression result = computeSummationGivenValues(literalFreeBody, values, context);
		return new Solution(result);
	}

	private Expression computeSummationGivenValues(
			Expression literalFreeBody,
			Expression values,
			Context context) {
		
		Expression result;
		if (values.equals(Sets.EMPTY_SET) || Sets.isExtensionalSet(values)) {
			result = ZERO;
		}
		else {
			Expression lowerBound = values.get(0);
			Expression upperBound = values.get(1);
			Polynomial bodyPolynomial = DefaultPolynomial.make(literalFreeBody);
			Predicate<Expression> isVariable = new IsVariable(context.getIsUniquelyNamedConstantPredicate(), context.getTypes(), context.getTheory());
			result = PolynomialIntegration.definiteIntegral(bodyPolynomial, getIndex(), lowerBound, upperBound, isVariable);
		}
		return result;
	}
}