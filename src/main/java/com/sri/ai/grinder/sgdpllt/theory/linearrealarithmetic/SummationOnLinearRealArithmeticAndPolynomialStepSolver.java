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
package com.sri.ai.grinder.sgdpllt.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.grinder.polynomial.api.Polynomial.makeRandomPolynomial;

import java.util.ArrayList;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.grinder.polynomial.core.DefaultPolynomial;
import com.sri.ai.grinder.polynomial.core.PolynomialIntegration;
import com.sri.ai.grinder.polynomial.core.PolynomialSummation;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractQuantifierEliminationStepSolver;
import com.sri.ai.grinder.sgdpllt.group.Sum;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;

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
public class SummationOnLinearRealArithmeticAndPolynomialStepSolver extends AbstractQuantifierEliminationStepSolver {

	// TODO: before making changes to this class,
	// it would be better to abstract its common aspects (lots of them)
	// with SummationOnDifferenceArithmeticAndPolynomialStepSolver
	// (the changes will probably need to be reflected there as well,
	// so it's better to unify first).
	
	private MeasureEquivalentIntervalOfSingleVariableLinearRealArithmeticConstraintStepSolver valuesOfSingleVariableLinearRealArithmeticConstraintStepSolver;
	
	public SummationOnLinearRealArithmeticAndPolynomialStepSolver(SingleVariableConstraint indexConstraint, Expression body) {
		super(new Sum(), indexConstraint, body);
		valuesOfSingleVariableLinearRealArithmeticConstraintStepSolver =
				new MeasureEquivalentIntervalOfSingleVariableLinearRealArithmeticConstraintStepSolver(
						(SingleVariableLinearRealArithmeticConstraint) indexConstraint);
	}

	@Override
	public SummationOnLinearRealArithmeticAndPolynomialStepSolver clone() {
		return (SummationOnLinearRealArithmeticAndPolynomialStepSolver) super.clone();
	}
	
	@Override
	protected AbstractQuantifierEliminationStepSolver makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint) {
		AbstractQuantifierEliminationStepSolver result = 
				new SummationOnLinearRealArithmeticAndPolynomialStepSolver(
						newIndexConstraint, body);
		return result;
	}

	@Override
	protected Step eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(
			SingleVariableConstraint indexConstraint,
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
					(MeasureEquivalentIntervalOfSingleVariableLinearRealArithmeticConstraintStepSolver)
					step.getStepSolverForWhenSplitterIsTrue();
			SummationOnLinearRealArithmeticAndPolynomialStepSolver ifFalse = clone();
			ifFalse.valuesOfSingleVariableLinearRealArithmeticConstraintStepSolver =
					(MeasureEquivalentIntervalOfSingleVariableLinearRealArithmeticConstraintStepSolver)
					step.getStepSolverForWhenSplitterIsFalse();
			return new ItDependsOn(step.getSplitterLiteral(), step.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
		}
		Expression values = step.getValue();
		
		Expression result = computeSummationGivenValues(indexConstraint.getVariable(), literalFreeBody, values, context);
		return new Solution(result);
	}

	private Expression computeSummationGivenValues(
			Expression variable,
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
			result = PolynomialIntegration.definiteIntegral(bodyPolynomial, variable, lowerBound, upperBound);
		}
		return result;
	}

	@Override
	public Expression makeRandomUnconditionalBody(Random random) {
		// unconditional body class is polynomials
		ArrayList<Expression> freeVariables = TheoryTestingSupport.make(getTheory()).getVariablesForTesting();
		int degree = random.nextInt(3);
		int maximumNumberOfFreeVariablesInEach = 2;
		int maximumConstant = 10;
		Expression result =
				makeRandomPolynomial(
						random, 
						getIndex(), 
						degree, 
						freeVariables, 
						maximumNumberOfFreeVariablesInEach,
						maximumConstant);
		return result;
	}
}