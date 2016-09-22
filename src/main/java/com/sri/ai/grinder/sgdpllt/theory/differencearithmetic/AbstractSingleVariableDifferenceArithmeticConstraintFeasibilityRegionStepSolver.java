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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.base.Pair.pair;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.StepSolver;
import com.sri.ai.grinder.sgdpllt.helper.SelectExpressionsSatisfyingComparisonStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.base.ConstantExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.base.ConstantStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.equality.DistinctExpressionsStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.equality.NumberOfDistinctExpressionsIsLessThanStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.numeric.AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver;
import com.sri.ai.util.base.Pair;

/**
 * A {@link AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver}
 * for a {@link SingleVariableLinearRealArithmeticConstraint}.
 * In particular, its implementation of method
 * {@link #getSolutionStepAfterBoundsAreCheckedForFeasibility(
 * Expression maximumLowerBound, 
 * Expression minimumUpperBound, 
 * AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver sequelBaseAsNumericStepSolver, 
 * Context context)}
 * performs checks of disequalities, determining which ones are relevant and how many
 * distinct disequalities there are, indicating a contradiction if there are more distinct disequals
 * than the number of values within the bounds.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver extends AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver {

	/**
	 * Concrete extensions must implement this method to provide a solution
	 * given the already computed strict maximum lower bound, non-strict minimum upper bound,
	 * and an extensional uni-set of distinct disequals
	 * (terms from which the variable is constrained to be disequal).
	 * <p>
	 * 
	 * @param strictMaximumLowerBound
	 * @param nonStrictMinimumUpperBound
	 * @param boundsDifference
	 * @param distinctDisequalsSet an extensional uni-set of distinct disequals within the bounds.
	 * @param context
	 * @return
	 */
	abstract public Expression getSolutionExpressionGivenBoundsAndDistinctDisequals(Expression strictMaximumLowerBound, Expression nonStrictMinimumUpperBound, Expression boundsDifference, Expression distinctDisequalsSet, Context context);

	public AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver(SingleVariableDifferenceArithmeticConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver clone() {
		return (AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver) super.clone();
	}

	@Override
	public SingleVariableDifferenceArithmeticConstraint getConstraint() {
		return (SingleVariableDifferenceArithmeticConstraint) super.getConstraint();
	}

	protected
	AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver 
	makeSequelStepSolver(
			AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver 
			sequelBase) {
		return 
				(AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver) 
				super.makeSequelStepSolver(sequelBase);
	}

	private StepSolver<List<Expression>> initialDisequalsGreaterThanMaximumLowerBoundStepSolver;

	private StepSolver<List<Expression>> initialDisequalsWithinBoundsStepSolver;

	private StepSolver<Expression> initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;

	private DistinctExpressionsStepSolver initialDistinctDisequalsStepSolver;
	
	@Override
	protected Step getSolutionStepAfterBoundsAreCheckedForFeasibility(
			Expression maximumLowerBound,
			Expression minimumUpperBound,
			AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver sequelBaseAsNumericStepSolver,
			Context context) {
		
		AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver sequelBase
		= (AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver) sequelBaseAsNumericStepSolver;
		
		StepSolver<List<Expression>> disequalsGreaterThanMaximumLowerBoundStepSolver;
		if (initialDisequalsGreaterThanMaximumLowerBoundStepSolver == null) {
			disequalsGreaterThanMaximumLowerBoundStepSolver
			= new SelectExpressionsSatisfyingComparisonStepSolver(
					getDisequals(),
					GREATER_THAN, maximumLowerBound); // relies on this class's enforcing of all lower bounds being strict
		}
		else {
			disequalsGreaterThanMaximumLowerBoundStepSolver = initialDisequalsGreaterThanMaximumLowerBoundStepSolver;
		}
		StepSolver.Step<List<Expression>> disequalsGreaterThanGreatestStrictLowerBoundStep
		= disequalsGreaterThanMaximumLowerBoundStepSolver.step(context);
		if (disequalsGreaterThanGreatestStrictLowerBoundStep.itDepends()) {
			AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = makeSequelStepSolver(sequelBase);
			ifTrue.initialDisequalsGreaterThanMaximumLowerBoundStepSolver = disequalsGreaterThanGreatestStrictLowerBoundStep.getStepSolverForWhenSplitterIsTrue();
			AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = makeSequelStepSolver(sequelBase);
			ifFalse.initialDisequalsGreaterThanMaximumLowerBoundStepSolver = disequalsGreaterThanGreatestStrictLowerBoundStep.getStepSolverForWhenSplitterIsFalse();
			ItDependsOn result = new ItDependsOn(disequalsGreaterThanGreatestStrictLowerBoundStep.getSplitter(), disequalsGreaterThanGreatestStrictLowerBoundStep.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
			return result;
		}
		List<Expression> disequalsGreaterThanGreatestStrictLowerBound = disequalsGreaterThanGreatestStrictLowerBoundStep.getValue();
		sequelBase.initialDisequalsGreaterThanMaximumLowerBoundStepSolver = new ConstantStepSolver<List<Expression>>(disequalsGreaterThanGreatestStrictLowerBound);

		StepSolver<List<Expression>> disequalsWithinBoundsStepSolver;
		if (initialDisequalsWithinBoundsStepSolver == null) {
			disequalsWithinBoundsStepSolver
			= new SelectExpressionsSatisfyingComparisonStepSolver(
					disequalsGreaterThanGreatestStrictLowerBound,
					LESS_THAN_OR_EQUAL_TO, minimumUpperBound); // relies on this class's enforcing of all upper bounds being non-strict
		}
		else {
			disequalsWithinBoundsStepSolver = initialDisequalsWithinBoundsStepSolver;
		}
		StepSolver.Step<List<Expression>> disequalsWithinBoundsStep = disequalsWithinBoundsStepSolver.step(context);
		if (disequalsWithinBoundsStep.itDepends()) {
			AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = makeSequelStepSolver(sequelBase);
			ifTrue.initialDisequalsWithinBoundsStepSolver = disequalsWithinBoundsStep.getStepSolverForWhenSplitterIsTrue();
			AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = makeSequelStepSolver(sequelBase);
			ifFalse.initialDisequalsWithinBoundsStepSolver = disequalsWithinBoundsStep.getStepSolverForWhenSplitterIsFalse();
			ItDependsOn result = new ItDependsOn(disequalsWithinBoundsStep.getSplitter(), disequalsWithinBoundsStep.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
			return result;
		}
		ArrayList<Expression> disequalsWithinBounds = new ArrayList<>(disequalsWithinBoundsStep.getValue());
		sequelBase.initialDisequalsWithinBoundsStepSolver = new ConstantStepSolver<List<Expression>>(disequalsWithinBounds);

		Expression boundsDifference = applyAndSimplify(MINUS, arrayList(minimumUpperBound, maximumLowerBound), context);

		// the goal of the upcoming 'if' is to define the values for these two next declared variables:
		
		boolean weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds;
		// if true, number of distinct disequals exceeds number of values within bounds;
		// if false, that may be true or false, we don't know.
		
		DistinctExpressionsStepSolver distinctExpressionsStepSolver;
		
		if (isNumber(boundsDifference)) {
			StepSolver<Expression> numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;
			if (initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver == null) {
				numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver
				= new NumberOfDistinctExpressionsIsLessThanStepSolver(boundsDifference.intValue(), disequalsWithinBounds);
			}
			else {
				numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;
			}
			StepSolver.Step<Expression> numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver.step(context);

			if (numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.itDepends()) {
				AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = makeSequelStepSolver(sequelBase);
				ifTrue.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getStepSolverForWhenSplitterIsTrue();
				AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = makeSequelStepSolver(sequelBase);
				ifFalse.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getStepSolverForWhenSplitterIsFalse();
				ItDependsOn result = new ItDependsOn(numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getSplitter(), numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
				return result;
			}
			Expression numberOfDistinctDisequalsIsLessThanBoundsDifference = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getValue();
			sequelBase.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = new ConstantExpressionStepSolver(numberOfDistinctDisequalsIsLessThanBoundsDifference);

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

		Expression solutionExpression;
		
		if (weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds) {
			solutionExpression = getSolutionExpressionGivenContradiction(); // there are no available values left
		}
		else if ( ! getEquals().isEmpty()) { // if bound to a value
			solutionExpression = getSolutionExpressionForBoundVariable();
		}
		else {
			Step distinctDisequalsStep = distinctExpressionsStepSolver.step(context);
			if (distinctDisequalsStep.itDepends()) {
				AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue = makeSequelStepSolver(sequelBase);
				ifTrue.initialDistinctDisequalsStepSolver = (DistinctExpressionsStepSolver) distinctDisequalsStep.getStepSolverForWhenSplitterIsTrue();
				AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = makeSequelStepSolver(sequelBase);
				ifFalse.initialDistinctDisequalsStepSolver = (DistinctExpressionsStepSolver) distinctDisequalsStep.getStepSolverForWhenSplitterIsFalse();
				ItDependsOn result = new ItDependsOn(distinctDisequalsStep.getSplitter(), distinctDisequalsStep.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
				return result;
			}
			Expression distinctDisequalsExtensionalUniSet = distinctDisequalsStep.getValue();
			solutionExpression = 
					getSolutionExpressionGivenBoundsAndDistinctDisequals(
							maximumLowerBound, 
							minimumUpperBound, 
							boundsDifference, 
							distinctDisequalsExtensionalUniSet, 
							context);
		}
		
		return new Solution(solutionExpression);
	}
	
	@Override
	protected Expression makeLiteralCheckingWhetherThereAreAnyValuesWithinBounds(Expression lowerBound, Expression upperBound, Context context) {
		Expression result = applyAndSimplifyWithoutConsideringContextualConstraint(LESS_THAN, arrayList(lowerBound, upperBound), context);
		// relies on lower bounds being strict, and upper bounds being non-strict
		return result;
	}

	@Override
	protected Pair<Expression, Boolean> processExplicitLowerBoundAndStrictnessPair(Expression lowerBound, boolean strictness, Context context) {
		Pair<Expression, Boolean> result;
		if (!strictness) {
			result = pair(applyAndSimplifyWithoutConsideringContextualConstraint(MINUS, arrayList(lowerBound, ONE), context), true /* now it is strict */);
		}
		else {
			result = pair(lowerBound, strictness);
		}
		return result;
	}
	
	@Override
	protected Pair<Expression, Boolean> processExplicitUpperBoundAndStrictnessPair(Expression upperBound, boolean strictness, Context context) {
		Pair<Expression, Boolean> result;
		if (strictness) {
			result = pair(applyAndSimplifyWithoutConsideringContextualConstraint(MINUS, arrayList(upperBound, ONE), context), false /* now it is non-strict */);
		}
		else {
			result = pair(upperBound, strictness);
		}
		return result;
	}

	protected IntegerInterval getType(Context context) {
		return getConstraint().getType(context);
	}
	
	private Pair<Expression, Boolean> typeLowerBoundAndStrictness;
	
	protected Pair<Expression, Boolean> getTypeLowerBoundAndStrictness(Context context) {
		if (typeLowerBoundAndStrictness == null) {
			IntegerInterval type = getType(context);
			Expression nonStrictLowerBound = type.getNonStrictLowerBound();
			if (Expressions.isNumber(nonStrictLowerBound)) {
				typeLowerBoundAndStrictness = pair(makeSymbol(nonStrictLowerBound.intValue() - 1), true /* strict */);
			}
			else { // has to be -infinity
				typeLowerBoundAndStrictness = pair(MINUS_INFINITY, true /* strict */);
			}
		}
		return typeLowerBoundAndStrictness;
	}

	private Pair<Expression, Boolean> typeUpperBoundAndStrictess;
	
	protected Pair<Expression, Boolean> getTypeUpperBoundAndStrictness(Context context) {
		if (typeUpperBoundAndStrictess == null) {
			IntegerInterval type = getType(context);
			Expression bound = type.getNonStrictUpperBound();
			typeUpperBoundAndStrictess = pair(bound, false);
		}
		return typeUpperBoundAndStrictess;
	}
}