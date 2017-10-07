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
package com.sri.ai.grinder.sgdpllt.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PLUS;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.polynomial.api.Polynomial;
import com.sri.ai.grinder.polynomial.core.DefaultPolynomial;
import com.sri.ai.grinder.polynomial.core.PolynomialSummation;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.QuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractQuantifierEliminationStepSolver;

/**
 * A step solver for a summation with an integer index constrained by difference arithmetic literals,
 * over a polynomial on integers.
 * It works by evaluating the body until there are no literals on it,
 * computing the index satisfying values for the index,
 * and using {@link PolynomialSummation}.
 * 
 * @author braz
 *
 */
@Beta
public class SummationOnDifferenceArithmeticAndPolynomialStepSolver extends AbstractQuantifierEliminationStepSolver {

	// TODO: before making changes to this class,
	// it would be better to abstract its common aspects (lots of them)
	// with SummationOnLinearRealArithmeticAndPolynomialStepSolver
	// (the changes will probably need to be reflected there as well,
	// so it's better to unify first).
	
	private ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver valuesOfSingleVariableDifferenceArithmeticConstraintStepSolver;
	
	public SummationOnDifferenceArithmeticAndPolynomialStepSolver(QuantifierEliminationProblem problem) {
		super(problem);
		valuesOfSingleVariableDifferenceArithmeticConstraintStepSolver =
				new ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver(
						(SingleVariableDifferenceArithmeticConstraint) getIndexConstraint());
	}

	@Override
	public SummationOnDifferenceArithmeticAndPolynomialStepSolver clone() {
		return (SummationOnDifferenceArithmeticAndPolynomialStepSolver) super.clone();
	}
	
	@Override
	protected SummationOnDifferenceArithmeticAndPolynomialStepSolver makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint) {
		SummationOnDifferenceArithmeticAndPolynomialStepSolver result = 
				new SummationOnDifferenceArithmeticAndPolynomialStepSolver(
						problem.makeWithNewIndexConstraint(newIndexConstraint));
		return result;
	}

	@Override
	protected Step eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(
			SingleVariableConstraint indexConstraint,
			Expression literalFreeBody,
			Context context) {

		ExpressionLiteralSplitterStepSolver.Step step = 
				valuesOfSingleVariableDifferenceArithmeticConstraintStepSolver.step(context);
		if (step == null) {
			return null;
		}
		if (step.itDepends()) {
			SummationOnDifferenceArithmeticAndPolynomialStepSolver ifTrue = clone();
			ifTrue.valuesOfSingleVariableDifferenceArithmeticConstraintStepSolver =
					(ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver)
					step.getStepSolverForWhenSplitterIsTrue();
			SummationOnDifferenceArithmeticAndPolynomialStepSolver ifFalse = clone();
			ifFalse.valuesOfSingleVariableDifferenceArithmeticConstraintStepSolver =
					(ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver)
					step.getStepSolverForWhenSplitterIsFalse();
			return new ItDependsOn(step.getSplitterLiteral(), step.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
		}
		RangeAndExceptionsSet values = (RangeAndExceptionsSet) step.getValue();
		
		Expression result = computeSummationGivenValues(indexConstraint.getVariable(), literalFreeBody, values, context);
		return new Solution(result);
	}

	private Expression computeSummationGivenValues(
			Expression variable,
			Expression literalFreeBody,
			RangeAndExceptionsSet values,
			Context context) {
		
		Expression result;
		if (values.equals(RangeAndExceptionsSet.EMPTY)) {
			result = ZERO;
		}
		else {
			Theory theory = context.getTheory();
			if (values instanceof RangeAndExceptionsSet.Singleton) {
				Expression value = ((RangeAndExceptionsSet.Singleton)values).getSingleValue();
				Expression valueAtPoint = 
						DefaultPolynomial.make(getValueAtGivenPoint(literalFreeBody, variable, value, theory, context));
				result = valueAtPoint;
			}
			else {
				Expression interval;
				List<Expression> disequals;
				if (values.hasFunctor(MINUS)) {
					interval = values.get(0);
					disequals = values.get(1).getArguments();
				}
				else {
					interval = values;
					disequals = list();
				}
				Expression strictLowerBound = interval.get(0);
				Expression nonStrictUpperBound = interval.get(1);
				Polynomial bodyPolynomial = DefaultPolynomial.make(literalFreeBody);
				Expression intervalSummation =
						PolynomialSummation.sum(
								variable,
								strictLowerBound,
								nonStrictUpperBound,
								bodyPolynomial);
				
				ArrayList<Expression> argumentsForSubtraction =
						new ArrayList<>(1 + disequals.size());
				argumentsForSubtraction.add(intervalSummation);
				for (Expression disequal : disequals) {
					Expression valueAtDisequal =
							getValueAtGivenPoint(
									literalFreeBody,
									variable,
									disequal,
									theory,
									context);
					argumentsForSubtraction.add(apply(MINUS, valueAtDisequal));
				}
				Expression intervalSummationMinusValuesAtDisequals =
						apply(PLUS, argumentsForSubtraction);
				result = DefaultPolynomial.make(theory.simplify(intervalSummationMinusValuesAtDisequals, context));
			}
		}
		return result;
	}

	private Expression getValueAtGivenPoint(Expression literalFreeBody, Expression variable, Expression value, Theory theory, Context context) {
		Expression newBody = literalFreeBody.replaceAllOccurrences(variable, value, context);
		Expression valueAtPoint = theory.simplify(newBody, context);
		return valueAtPoint;
	}
}