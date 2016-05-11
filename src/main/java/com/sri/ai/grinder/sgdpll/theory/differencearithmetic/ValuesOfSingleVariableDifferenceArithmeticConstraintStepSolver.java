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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.sgdpll.theory.differencearithmetic.RangeAndExceptionsSet.EMPTY;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.getFirst;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll.helper.MaximumExpressionStepSolver;
import com.sri.ai.grinder.sgdpll.helper.SelectExpressionsSatisfyingComparisonStepSolver;
import com.sri.ai.grinder.sgdpll.theory.base.ConstantExpressionStepSolver;
import com.sri.ai.grinder.sgdpll.theory.base.ConstantStepSolver;
import com.sri.ai.grinder.sgdpll.theory.base.LiteralStepSolver;
import com.sri.ai.grinder.sgdpll.theory.equality.DistinctExpressionsStepSolver;
import com.sri.ai.grinder.sgdpll.theory.equality.NumberOfDistinctExpressionsIsLessThanStepSolver;

/**
 * A step solver computing the possible values of the variable of
 * a single-variable difference arithmetic constraint.
 * @author braz
 *
 */
@Beta
public class ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver extends AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver {

	private ContextDependentProblemStepSolver<Expression> initialMaximumStrictLowerBoundStepSolver;

	private ContextDependentProblemStepSolver<Expression> initialMinimumNonStrictUpperBoundStepSolver;

	private ContextDependentProblemStepSolver<List<Expression>> initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver;

	private ContextDependentProblemStepSolver<List<Expression>> initialDisequalsWithinBoundsStepSolver;

	private ContextDependentProblemStepSolver<Boolean> initialLowerBoundIsLessThanUpperBoundStepSolver;

	private ContextDependentProblemStepSolver<Expression> initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;

	private DistinctExpressionsStepSolver initialDistinctDisequalsStepSolver;
	

	public ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver(SingleVariableDifferenceArithmeticConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver clone() {
		return (ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver) super.clone();
	}

	@Override
	public SingleVariableDifferenceArithmeticConstraint getConstraint() {
		return (SingleVariableDifferenceArithmeticConstraint) super.getConstraint();
	}
	
	@Override
	protected SolutionStep solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Context context) {
		// at this point, the context establishes that one of the strict lower bounds L is greater than all the others,
		// that one of the non-strict upper bounds U is less than all the others, and that
		// all disequals are in ]L, U], and are disequal from each other.
		// Therefore, the constraint is satisfiable if and only if U - L > D
		// where D is the number of disequals.

		Expression solutionExpression;
		
		// successor keeps track of updates to non-splitting inner step solvers so far.
		// When a splitting inner step solver is found, it is used as a basis
		// for the sub-step solvers.
		ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver successor = clone();

		if (getConstraint().getPropagateAllLiteralsWhenVariableIsBound() && ! getEquals().isEmpty()) {
			Expression value = getFirst(getEquals());
			solutionExpression = new RangeAndExceptionsSet.Singleton(value);
		}
		else {
			ContextDependentProblemStepSolver<Expression> maximumStrictLowerBoundStepSolver;
			if (initialMaximumStrictLowerBoundStepSolver == null) {
				maximumStrictLowerBoundStepSolver
				= new MaximumExpressionStepSolver(
						getStrictLowerBoundsIncludingImplicitOnes(context),
						LESS_THAN_SYMBOL, // use total order <
						MINUS_INFINITY,
						INFINITY); // at first, I placed the type minimum and maximum strict lower bounds here. This is incorrect because if the type maximum is, say, 4, and I have "X > 3 and X > I" (3 is the maximum strict lower bounds for values in the type), the step solver short-circuits and returns 3, without ever even looking at I. Looking at I is needed because if I is greater than 3 than this constraint is unsatisfiable.
			}
			else {
				maximumStrictLowerBoundStepSolver = initialMaximumStrictLowerBoundStepSolver;
			}
			ContextDependentProblemStepSolver.SolutionStep<Expression> maximumStrictLowerBoundStep = maximumStrictLowerBoundStepSolver.step(context);
			if (maximumStrictLowerBoundStep.itDepends()) {
				ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
				ifTrue.initialMaximumStrictLowerBoundStepSolver = maximumStrictLowerBoundStep.getStepSolverForWhenLiteralIsTrue();
				ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
				ifFalse.initialMaximumStrictLowerBoundStepSolver = maximumStrictLowerBoundStep.getStepSolverForWhenLiteralIsFalse();
				ItDependsOn result = new ItDependsOn(maximumStrictLowerBoundStep.getLiteral(), maximumStrictLowerBoundStep.getContextSplitting(), ifTrue, ifFalse);
				return result;
			}
			Expression greatestStrictLowerBound = maximumStrictLowerBoundStep.getValue();
			successor.initialMaximumStrictLowerBoundStepSolver = new ConstantExpressionStepSolver(greatestStrictLowerBound);
			
			ContextDependentProblemStepSolver<Expression> minimumNonStrictUpperBoundStepSolver;
			if (initialMinimumNonStrictUpperBoundStepSolver == null) {
				minimumNonStrictUpperBoundStepSolver
				= new MaximumExpressionStepSolver(
						getNonStrictUpperBoundsIncludingImplicitOnes(context),
						GREATER_THAN_SYMBOL, // use total order > since "minimum" is maximum under it
						INFINITY, // "minimum" is maximum value because we are operating on the inverse order
						MINUS_INFINITY); // "maximum" is minimum value because we are operating on the inverse order
			}
			else {
				minimumNonStrictUpperBoundStepSolver = initialMinimumNonStrictUpperBoundStepSolver;
			}
			ContextDependentProblemStepSolver.SolutionStep<Expression> minimumNonStrictUpperBoundStep = minimumNonStrictUpperBoundStepSolver.step(context);
			if (minimumNonStrictUpperBoundStep.itDepends()) {
				ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
				ifTrue.initialMinimumNonStrictUpperBoundStepSolver = (MaximumExpressionStepSolver) minimumNonStrictUpperBoundStep.getStepSolverForWhenLiteralIsTrue();
				ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
				ifFalse.initialMinimumNonStrictUpperBoundStepSolver = (MaximumExpressionStepSolver) minimumNonStrictUpperBoundStep.getStepSolverForWhenLiteralIsFalse();
				ItDependsOn result = new ItDependsOn(minimumNonStrictUpperBoundStep.getLiteral(), minimumNonStrictUpperBoundStep.getContextSplitting(), ifTrue, ifFalse);
				return result;
			}
			Expression leastNonStrictUpperBound = minimumNonStrictUpperBoundStep.getValue();
			successor.initialMinimumNonStrictUpperBoundStepSolver = new ConstantExpressionStepSolver(leastNonStrictUpperBound);
			
			if (greatestStrictLowerBound.equals(MINUS_INFINITY) || leastNonStrictUpperBound.equals(INFINITY)) {
				solutionExpression = new RangeAndExceptionsSet.DefaultRangeAndExceptionsSet(MINUS_INFINITY, INFINITY);
			}
			else {
				ContextDependentProblemStepSolver<Boolean> lowerBoundIsLessThanUpperBoundStepSolver;
				if (initialLowerBoundIsLessThanUpperBoundStepSolver == null) {
					Expression lowerBoundIsLessThanUpperBound = applyAndSimplify(LESS_THAN, arrayList(greatestStrictLowerBound, leastNonStrictUpperBound), context);
					lowerBoundIsLessThanUpperBoundStepSolver = new LiteralStepSolver(lowerBoundIsLessThanUpperBound);
				}
				else {
					lowerBoundIsLessThanUpperBoundStepSolver = initialLowerBoundIsLessThanUpperBoundStepSolver;
				}
				ContextDependentProblemStepSolver.SolutionStep<Boolean> lowerBoundIsLessThanUpperBoundStep = lowerBoundIsLessThanUpperBoundStepSolver.step(context);
				if (lowerBoundIsLessThanUpperBoundStep.itDepends()) {
					ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
					ifTrue.initialLowerBoundIsLessThanUpperBoundStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenLiteralIsTrue();
					ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
					ifFalse.initialLowerBoundIsLessThanUpperBoundStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenLiteralIsFalse();
					ItDependsOn result = new ItDependsOn(lowerBoundIsLessThanUpperBoundStep.getLiteral(), lowerBoundIsLessThanUpperBoundStep.getContextSplitting(), ifTrue, ifFalse);
					return result;
				}
				if ( ! lowerBoundIsLessThanUpperBoundStep.getValue()) {
					return new Solution(EMPTY);
				}
				// else, bounds difference is positive and we can move on
				successor.initialLowerBoundIsLessThanUpperBoundStepSolver = new ConstantStepSolver<Boolean>(true);
				
				ContextDependentProblemStepSolver<List<Expression>> disequalsGreaterThanGreatestStrictLowerBoundStepSolver;
				if (initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver == null) {
					disequalsGreaterThanGreatestStrictLowerBoundStepSolver
					= new SelectExpressionsSatisfyingComparisonStepSolver(getDisequals(), GREATER_THAN, greatestStrictLowerBound);
				}
				else {
					disequalsGreaterThanGreatestStrictLowerBoundStepSolver = initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver;
				}
				ContextDependentProblemStepSolver.SolutionStep<List<Expression>> step
				= disequalsGreaterThanGreatestStrictLowerBoundStepSolver.step(context);
				if (step.itDepends()) {
					ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
					ifTrue.initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step.getStepSolverForWhenLiteralIsTrue();
					ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
					ifFalse.initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step.getStepSolverForWhenLiteralIsFalse();
					ItDependsOn result = new ItDependsOn(step.getLiteral(), step.getContextSplitting(), ifTrue, ifFalse);
					return result;
				}
				List<Expression> disequalsGreaterThanGreatestStrictLowerBound = step.getValue();
				successor.initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver = new ConstantStepSolver<List<Expression>>(disequalsGreaterThanGreatestStrictLowerBound);

				ContextDependentProblemStepSolver<List<Expression>> disequalsWithinBoundsStepSolver;
				if (initialDisequalsWithinBoundsStepSolver == null) {
					disequalsWithinBoundsStepSolver
					= new SelectExpressionsSatisfyingComparisonStepSolver(
							disequalsGreaterThanGreatestStrictLowerBound,
							LESS_THAN_OR_EQUAL_TO, leastNonStrictUpperBound);
				}
				else {
					disequalsWithinBoundsStepSolver = initialDisequalsWithinBoundsStepSolver;
				}
				ContextDependentProblemStepSolver.SolutionStep<List<Expression>> step2
				= disequalsWithinBoundsStepSolver.step(context);
				if (step2.itDepends()) {
					ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
					ifTrue.initialDisequalsWithinBoundsStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step2.getStepSolverForWhenLiteralIsTrue();
					ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
					ifFalse.initialDisequalsWithinBoundsStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step2.getStepSolverForWhenLiteralIsFalse();
					ItDependsOn result = new ItDependsOn(step2.getLiteral(), step2.getContextSplitting(), ifTrue, ifFalse);
					return result;
				}
				ArrayList<Expression> disequalsWithinBounds = new ArrayList<>(step2.getValue());
				successor.initialDisequalsWithinBoundsStepSolver = new ConstantStepSolver<List<Expression>>(disequalsWithinBounds);

				Expression boundsDifference = applyAndSimplify(MINUS, arrayList(leastNonStrictUpperBound, greatestStrictLowerBound), context);

				// the goal of the upcoming 'if' is to define the values for these two next declared variables:
				
				boolean weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds;
				// if true, number of distinct disequals exceeds number of values within bounds;
				// if false, that may be true or false, we don't know.
				
				DistinctExpressionsStepSolver distinctExpressionsStepSolver;
				
				if (isNumber(boundsDifference)) {
					ContextDependentProblemStepSolver<Expression> numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;
					if (initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver == null) {
						numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver
						= new NumberOfDistinctExpressionsIsLessThanStepSolver(boundsDifference.intValue(), disequalsWithinBounds);
					}
					else {
						numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;
					}
					ContextDependentProblemStepSolver.SolutionStep<Expression> numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver.step(context);

					if (numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.itDepends()) {
						ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
						ifTrue.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = (NumberOfDistinctExpressionsIsLessThanStepSolver) numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getStepSolverForWhenLiteralIsTrue();
						ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
						ifFalse.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = (NumberOfDistinctExpressionsIsLessThanStepSolver) numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getStepSolverForWhenLiteralIsFalse();
						ItDependsOn result = new ItDependsOn(numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getLiteral(), numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getContextSplitting(), ifTrue, ifFalse);
						return result;
					}
					Expression numberOfDistinctDisequalsIsLessThanBoundsDifference = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getValue();
					successor.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = new ConstantExpressionStepSolver(numberOfDistinctDisequalsIsLessThanBoundsDifference);

					weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds = numberOfDistinctDisequalsIsLessThanBoundsDifference.equals(FALSE);
		
					if (initialDistinctDisequalsStepSolver == null) {
						// if initialDistinctDisequalsStepSolver has not been set yet, it is because the predecessor of this step solver did not get to the point of using distinctExpressionsStepSolver; this means numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver is not a ConstantExpressionStepSolver (if it were, then the predecessor would have proceeded to use distinctExpressionsStepSolver), so it must be a NumberOfDistinctExpressionsIsLessThanStepSolver.
						distinctExpressionsStepSolver = 
								((NumberOfDistinctExpressionsIsLessThanStepSolver)
								numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver).getDistinctExpressionsStepSolver();
					}
					else {
						distinctExpressionsStepSolver = initialDistinctDisequalsStepSolver;
					}
				}
				else {
					weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds = false;
					if (initialDistinctDisequalsStepSolver == null) {
						distinctExpressionsStepSolver = new DistinctExpressionsStepSolver(disequalsWithinBounds);
					}
					else {
						distinctExpressionsStepSolver = initialDistinctDisequalsStepSolver;
					}
				}

				if (weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds) {
					solutionExpression = RangeAndExceptionsSet.EMPTY; // there are no available values left
				}
				else if ( ! getEquals().isEmpty()) { // if bound to a value
					solutionExpression = new RangeAndExceptionsSet.Singleton(getFirst(getEquals()));
				}
				else {
					SolutionStep distinctDisequalsStep = distinctExpressionsStepSolver.step(context);
					if (distinctDisequalsStep.itDepends()) {
						ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
						ifTrue.initialDistinctDisequalsStepSolver = (DistinctExpressionsStepSolver) distinctDisequalsStep.getStepSolverForWhenLiteralIsTrue();
						ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
						ifFalse.initialDistinctDisequalsStepSolver = (DistinctExpressionsStepSolver) distinctDisequalsStep.getStepSolverForWhenLiteralIsFalse();
						ItDependsOn result = new ItDependsOn(distinctDisequalsStep.getLiteral(), distinctDisequalsStep.getContextSplitting(), ifTrue, ifFalse);
						return result;
					}
					Expression distinctDisequals = distinctDisequalsStep.getValue();
					solutionExpression = 
							new RangeAndExceptionsSet.DefaultRangeAndExceptionsSet(
									greatestStrictLowerBound,
									leastNonStrictUpperBound,
									distinctDisequals.getArguments()
									);
				}
			}
		}

		return new Solution(solutionExpression);
	}

	@Override
	protected Expression solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied() {
		return RangeAndExceptionsSet.EMPTY;
	}

	private ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver makeBasisForSubStepSolver(ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver successor) {
		return successor.clone();
	}
}