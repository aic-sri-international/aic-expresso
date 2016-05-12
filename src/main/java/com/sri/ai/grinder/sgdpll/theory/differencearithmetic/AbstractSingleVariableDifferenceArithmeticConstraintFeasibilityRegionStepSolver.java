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
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.isNumber;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.base.PairOf.makePairOf;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.sgdpll.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll.core.solver.AbstractContextDependentProblemWithPropagatedLiteralsStepSolver;
import com.sri.ai.grinder.sgdpll.helper.MaximumExpressionStepSolver;
import com.sri.ai.grinder.sgdpll.helper.SelectExpressionsSatisfyingComparisonStepSolver;
import com.sri.ai.grinder.sgdpll.theory.base.ConstantExpressionStepSolver;
import com.sri.ai.grinder.sgdpll.theory.base.ConstantStepSolver;
import com.sri.ai.grinder.sgdpll.theory.base.LiteralStepSolver;
import com.sri.ai.grinder.sgdpll.theory.equality.DistinctExpressionsStepSolver;
import com.sri.ai.grinder.sgdpll.theory.equality.NumberOfDistinctExpressionsIsLessThanStepSolver;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.SingleVariableLinearRealArithmeticConstraint;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.PairOfElementsInListIterator;

/**
 * A {@link AbstractContextDependentProblemWithPropagatedLiteralsStepSolver}
 * for a {@link SingleVariableLinearRealArithmeticConstraint}
 * with basic methods for determining the feasibility region of the variable,
 * but leaving the determination of the final solution to {@link #solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Context)}.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver extends AbstractContextDependentProblemWithPropagatedLiteralsStepSolver {

	protected static final Symbol GREATER_THAN_SYMBOL = makeSymbol(GREATER_THAN);

	protected static final Symbol LESS_THAN_SYMBOL = makeSymbol(LESS_THAN);

	protected ArrayList<Expression> equals;

	protected ArrayList<Expression> disequals;

	protected ArrayList<Expression> nonEqualityComparisons;

	protected ArrayList<Expression> strictLowerBoundsIncludingImplicitOnes;

	protected ArrayList<Expression> nonStrictUpperBoundsIncludingImplicitOnes;

	protected ArrayList<PairOf<Expression>> pairsOfEquals;
	
	
	protected ContextDependentProblemStepSolver<Expression> initialMaximumStrictLowerBoundStepSolver;

	protected ContextDependentProblemStepSolver<Expression> initialMinimumNonStrictUpperBoundStepSolver;

	protected ContextDependentProblemStepSolver<List<Expression>> initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver;

	protected ContextDependentProblemStepSolver<List<Expression>> initialDisequalsWithinBoundsStepSolver;

	protected ContextDependentProblemStepSolver<Boolean> initialLowerBoundIsLessThanUpperBoundStepSolver;

	protected ContextDependentProblemStepSolver<Expression> initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;

	protected DistinctExpressionsStepSolver initialDistinctDisequalsStepSolver;
	

	public abstract boolean unboundedVariableProducesShortCircuitSolution();

	public abstract Expression getSolutionExpressionForUnboundedVariables();

	public abstract Expression getSolutionExpressionForBoundVariable();

	public abstract Expression getSolutionExpressionGivenBoundsAndDistinctDisequals(Expression greatestStrictLowerBound, Expression leastNonStrictUpperBound, Expression boundsDifference, SolutionStep distinctDisequalsStep, Context context);

	
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
	
	@Override
	protected boolean usingDefaultImplementationOfMakePropagatedCNF() {
		return true;
	}

	@Override
	protected Iterable<Expression> getPropagatedLiterals(Context context) {
		
		// System.out.println("getPropagatedLiterals:");
		// System.out.println("constraint: " + constraint);
		// System.out.println("strict lower bounds: " + join(getStrictLowerBounds(context)));
		// System.out.println("non-strict upper bounds: " + join(getNonStrictUpperBounds(context)));
		// System.out.println("pairs of equals to variable: " + join(pairsOfEquals()));
		// System.out.println("equals to variable: " + join(getEquals()));
		// System.out.println("non-equality comparisons: " + join(getNonEqualityComparisons(context)));
		
		Iterator<Expression> propagatedEqualities;
		if (getConstraint().getPropagateAllLiteralsWhenVariableIsBound()) {
			propagatedEqualities = iterator(); // the literals below must have already been propagated
		}
		else {
			// if X = Y and X = Z, then Y = Z
			Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator = pairsOfEquals().iterator();
			propagatedEqualities =
					functionIterator(
							pairsOfEqualsToVariableIterator,
							p -> {
								Expression result = Equality.makeWithConstantSimplification(p.first, p.second, context);
								// System.out.println("Unsimplified equality of equals: " + p.first + " = " + p.second);	
								// System.out.println("constraint is: " + constraint);	
								// System.out.println("Simplified to: " + result);	
								return result;
							});
			// Note: the above could be lumped together with the propagated comparisons below, if
			// they were modified to include equalities instead of just non-equality comparisons
			// However, that would go over all pairs of terms equal to the variable, which is unnecessary since equality is symmetrical
			// The above only goes over pairs that are sorted by position in normalized atoms.
		}
		
		
		// if X = Y and X op Z, then Y op Z, for op any atom functor other than equality (which is already covered above).
		// TODO: the single-variable constraint should be changed so that when X = Y all other constraints are placed on Y
		// instead and put on external literals

		Iterator<Expression> propagatedComparisons;
		if (getConstraint().getPropagateAllLiteralsWhenVariableIsBound()) {
			propagatedComparisons = iterator();
		}
		else {
			propagatedComparisons =
					functionIterator(
							new CartesianProductIterator<Expression>(
									() -> getEquals().iterator(),
									() -> getNonEqualityComparisons(context).iterator()
									),
									equalAndNonEqualityComparison -> {
										Expression equal = equalAndNonEqualityComparison.get(0);
										Expression nonEqualityComparison = equalAndNonEqualityComparison.get(1);
										Expression termBeingCompared = nonEqualityComparison.get(1);
										Expression unsimplifiedAtom = apply(nonEqualityComparison.getFunctor(), equal, termBeingCompared);
										Expression result = constraint.getConstraintTheory().simplify(unsimplifiedAtom, context);
										// System.out.println("Unsimplified comparison of equal and term in non-equality comparison: " + unsimplifiedAtom);	
										// System.out.println("Non-equality comparison was: " + nonEqualityComparison);	
										// System.out.println("constraint is: " + constraint);	
										// System.out.println("Simplified to: " + result);	
										return result;
									});
		}

		// provide external literals first
		Iterator<Expression> propagatedLiteralsIterator =
				new NestedIterator<>(
						getConstraint().getExternalLiterals(),
						propagatedEqualities,
						propagatedComparisons);
		// TODO: have super class take care of external literals, so extensions don't need to think about them
		
		Iterable<Expression> result = in(propagatedLiteralsIterator);
		
		return result;
	}

	protected ArrayList<Expression> getStrictLowerBoundsIncludingImplicitOnes(Context context) {
		if (strictLowerBoundsIncludingImplicitOnes == null) {
			SingleVariableDifferenceArithmeticConstraint differenceArithmeticConstraint = (SingleVariableDifferenceArithmeticConstraint) constraint;
			
			FunctionIterator<Expression, Expression> strictLowerBoundsFromPositiveNormalizedAtomsIterator
			= functionIterator(
					predicateIterator(
							differenceArithmeticConstraint.getPositiveNormalizedAtoms(),
							e -> e.hasFunctor(GREATER_THAN) // X > Y, so Y is a strict lower bound
					), 
					e -> e.get(1));
			
			FunctionIterator<Expression, Expression> strictLowerBoundsFromNegativeNormalizedAtomsIterator
			= functionIterator(
					predicateIterator(
							differenceArithmeticConstraint.getNegativeNormalizedAtoms(),
							e -> e.hasFunctor(LESS_THAN)
					), 
					e -> apply(MINUS, e.get(1), ONE)); // atom is (not (X < Y)), e.g., X >= Y, so X > Y - 1 and Y - 1 is a strict lower bound
			
			Expression typeStrictLowerBound = getTypeStrictLowerBound(context);
			
			Iterator<Expression> strictLowerBoundsIterator = new NestedIterator<>(
					strictLowerBoundsFromPositiveNormalizedAtomsIterator,
					strictLowerBoundsFromNegativeNormalizedAtomsIterator,
					typeStrictLowerBound);
			
			strictLowerBoundsIncludingImplicitOnes = arrayListFrom(strictLowerBoundsIterator);
		}
		return strictLowerBoundsIncludingImplicitOnes;
	}

	protected ArrayList<Expression> getNonStrictUpperBoundsIncludingImplicitOnes(Context context) {
		if (nonStrictUpperBoundsIncludingImplicitOnes == null) {
			SingleVariableDifferenceArithmeticConstraint differenceArithmeticConstraint = (SingleVariableDifferenceArithmeticConstraint) constraint;
			
			FunctionIterator<Expression, Expression> nonStrictUpperBoundsFromPositiveNormalizedAtomsIterator
			= functionIterator(
					predicateIterator(
							differenceArithmeticConstraint.getPositiveNormalizedAtoms(),
							e -> e.hasFunctor(LESS_THAN)
					),
					e -> Minus.make(e.get(1), ONE)); // atom is X < Y, so X <= Y - 1, so Y - 1 is a non-strict upper bound
			
			FunctionIterator<Expression, Expression> nonStrictUpperBoundsFromNegativeNormalizedAtomsIterator
			= functionIterator(
					predicateIterator(
							differenceArithmeticConstraint.getNegativeNormalizedAtoms(),
							e -> e.hasFunctor(GREATER_THAN) // not (X > Y) <=> X <= Y, so Y is a non-strict upper bound
					), 
					e -> e.get(1));
			
			Expression typeNonStrictUpperBound = getTypeNonStrictUpperBound(context);
			
			Iterator<Expression> nonStrictUpperBoundsIterator = new NestedIterator<>(
					nonStrictUpperBoundsFromPositiveNormalizedAtomsIterator,
					nonStrictUpperBoundsFromNegativeNormalizedAtomsIterator,
					typeNonStrictUpperBound);
			
			nonStrictUpperBoundsIncludingImplicitOnes = arrayListFrom(nonStrictUpperBoundsIterator);
		}
		return nonStrictUpperBoundsIncludingImplicitOnes;
	}

	protected ArrayList<Expression> getEquals() {
		if (equals == null) {
			SingleVariableDifferenceArithmeticConstraint differenceArithmeticConstraint
			= (SingleVariableDifferenceArithmeticConstraint) constraint;
			
			Iterator<Expression> equalsIterator =
					functionIterator(
							predicateIterator(
									differenceArithmeticConstraint.getPositiveNormalizedAtoms(),
									e -> e.hasFunctor(EQUALITY)
									), 
									e -> e.get(1));
			
			equals = arrayListFrom(equalsIterator);
		}
		return equals;
	}

	protected ArrayList<Expression> getNonEqualityComparisons(Context context) {
		if (nonEqualityComparisons == null) {
			SingleVariableDifferenceArithmeticConstraint differenceArithmeticConstraint = (SingleVariableDifferenceArithmeticConstraint) constraint;

			Iterator<Expression> fromPositiveNormalizedAtoms =
					predicateIterator(
							differenceArithmeticConstraint.getPositiveNormalizedAtoms(),
							e -> ! e.hasFunctor(FunctorConstants.EQUALITY)
							);

			Iterator<Expression> fromNegativeNormalizedAtoms =
					functionIterator(
							differenceArithmeticConstraint.getNegativeNormalizedAtoms(), // negative normalized atom is never an equality
							e -> differenceArithmeticConstraint.getConstraintTheory().getLiteralNegation(e, context)
							);

			Expression variableIsGreaterThanTypeStrictLowerBound =
					apply(GREATER_THAN, getConstraint().getVariable(), getTypeStrictLowerBound(context));

			Expression variableIsLessThanOrEqualToTypeNonStrictUpperBound =
					apply(LESS_THAN_OR_EQUAL_TO, getConstraint().getVariable(), getTypeNonStrictUpperBound(context));

			Iterator<Expression> all =
					new NestedIterator<Expression>(
							fromPositiveNormalizedAtoms,
							fromNegativeNormalizedAtoms,
							variableIsGreaterThanTypeStrictLowerBound,
							variableIsLessThanOrEqualToTypeNonStrictUpperBound);
			
			nonEqualityComparisons = arrayListFrom(all);
		}
		
		return nonEqualityComparisons;
	}

	protected ArrayList<Expression> getDisequals() {
		if (disequals == null) {
			SingleVariableDifferenceArithmeticConstraint differenceArithmeticConstraint = (SingleVariableDifferenceArithmeticConstraint) constraint;
			Iterator<Expression> disequalsIterator =
					functionIterator(
							predicateIterator(
									differenceArithmeticConstraint.getNegativeNormalizedAtoms(),
									e -> e.hasFunctor(FunctorConstants.EQUALITY) // negative equality is disequality
									), 
									e -> e.get(1));
			disequals = Util.arrayListFrom(disequalsIterator);
		}
		return disequals;
	}

	protected ArrayList<PairOf<Expression>> pairsOfEquals() {
		if (pairsOfEquals == null) {
			ArrayList<Expression> equalities = Util.collectToArrayList(getConstraint().getPositiveNormalizedAtoms(), e -> e.hasFunctor(EQUALITY));

			PairOfElementsInListIterator<Expression> pairsOfEqualitiesIterator = 
					new PairOfElementsInListIterator<>(equalities);

			//		Function<PairOf<Expression>, PairOf<Expression>> makePairOfSecondArguments = p -> makePairOf(p.first.get(1), p.second.get(1));
			// above lambda somehow not working at Ciaran's environment, replacing with seemingly identical anonymous class object below		
			Function<PairOf<Expression>, PairOf<Expression>> makePairOfSecondArguments = new Function<PairOf<Expression>, PairOf<Expression>>() {
				@Override
				public PairOf<Expression> apply(PairOf<Expression> p) {
					return makePairOf(p.first.get(1), p.second.get(1));
				}
			};
			Iterator<PairOf<Expression>> pairsOfEqualsIterator = functionIterator(pairsOfEqualitiesIterator, makePairOfSecondArguments);
			
			pairsOfEquals = arrayListFrom(pairsOfEqualsIterator);
		}

		return pairsOfEquals;
	}

	@Override
	protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(Context context) {
		return list();
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
		AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver successor = clone();

		if (getConstraint().getPropagateAllLiteralsWhenVariableIsBound() && ! getEquals().isEmpty()) {
			solutionExpression = getSolutionExpressionForBoundVariable();
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
				AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = successor.clone();
				ifTrue.initialMaximumStrictLowerBoundStepSolver = maximumStrictLowerBoundStep.getStepSolverForWhenLiteralIsTrue();
				AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = successor.clone();
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
				AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = successor.clone();
				ifTrue.initialMinimumNonStrictUpperBoundStepSolver = minimumNonStrictUpperBoundStep.getStepSolverForWhenLiteralIsTrue();
				AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = successor.clone();
				ifFalse.initialMinimumNonStrictUpperBoundStepSolver = minimumNonStrictUpperBoundStep.getStepSolverForWhenLiteralIsFalse();
				ItDependsOn result = new ItDependsOn(minimumNonStrictUpperBoundStep.getLiteral(), minimumNonStrictUpperBoundStep.getContextSplitting(), ifTrue, ifFalse);
				return result;
			}
			Expression leastNonStrictUpperBound = minimumNonStrictUpperBoundStep.getValue();
			successor.initialMinimumNonStrictUpperBoundStepSolver = new ConstantExpressionStepSolver(leastNonStrictUpperBound);
			
			if (unboundedVariableProducesShortCircuitSolution() && (greatestStrictLowerBound.equals(MINUS_INFINITY) || leastNonStrictUpperBound.equals(getSolutionExpressionForUnboundedVariables()))) {
				solutionExpression = getSolutionExpressionForUnboundedVariables();
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
					AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = successor.clone();
					ifTrue.initialLowerBoundIsLessThanUpperBoundStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenLiteralIsTrue();
					AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = successor.clone();
					ifFalse.initialLowerBoundIsLessThanUpperBoundStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenLiteralIsFalse();
					ItDependsOn result = new ItDependsOn(lowerBoundIsLessThanUpperBoundStep.getLiteral(), lowerBoundIsLessThanUpperBoundStep.getContextSplitting(), ifTrue, ifFalse);
					return result;
				}
				if ( ! lowerBoundIsLessThanUpperBoundStep.getValue()) {
					return new Solution(getSolutionExpressionGivenContradiction());
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
					AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = successor.clone();
					ifTrue.initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step.getStepSolverForWhenLiteralIsTrue();
					AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = successor.clone();
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
					AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = successor.clone();
					ifTrue.initialDisequalsWithinBoundsStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step2.getStepSolverForWhenLiteralIsTrue();
					AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = successor.clone();
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
						AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = successor.clone();
						ifTrue.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = (NumberOfDistinctExpressionsIsLessThanStepSolver) numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getStepSolverForWhenLiteralIsTrue();
						AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = successor.clone();
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
					solutionExpression = getSolutionExpressionGivenContradiction(); // there are no available values left
				}
				else if ( ! getEquals().isEmpty()) { // if bound to a value
					solutionExpression = getSolutionExpressionForBoundVariable();
				}
				else {
					SolutionStep distinctDisequalsStep = distinctExpressionsStepSolver.step(context);
					if (distinctDisequalsStep.itDepends()) {
						AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifTrue  = successor.clone();
						ifTrue.initialDistinctDisequalsStepSolver = (DistinctExpressionsStepSolver) distinctDisequalsStep.getStepSolverForWhenLiteralIsTrue();
						AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver ifFalse = successor.clone();
						ifFalse.initialDistinctDisequalsStepSolver = (DistinctExpressionsStepSolver) distinctDisequalsStep.getStepSolverForWhenLiteralIsFalse();
						ItDependsOn result = new ItDependsOn(distinctDisequalsStep.getLiteral(), distinctDisequalsStep.getContextSplitting(), ifTrue, ifFalse);
						return result;
					}
					solutionExpression = getSolutionExpressionGivenBoundsAndDistinctDisequals(greatestStrictLowerBound, leastNonStrictUpperBound, boundsDifference, distinctDisequalsStep, context);
				}
			}
		}

		return new Solution(solutionExpression);
	}

	protected AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver makeBasisForSubStepSolver(AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver successor) {
		return successor.clone();
	}

	protected IntegerInterval getType(Context context) {
		return getConstraint().getType(context);
	}
	
	private Expression typeStrictLowerBound;
	
	protected Expression getTypeStrictLowerBound(Context context) {
		if (typeStrictLowerBound == null) {
			IntegerInterval type = getType(context);
			Expression nonStrictLowerBound = type.getNonStrictLowerBound();
			if (Expressions.isNumber(nonStrictLowerBound)) {
				typeStrictLowerBound = makeSymbol(nonStrictLowerBound.intValue() - 1);
			}
			else { // has to be -infinity
				typeStrictLowerBound = MINUS_INFINITY;
			}
		}
		return typeStrictLowerBound;
	}

	protected Expression getTypeNonStrictUpperBound(Context context) {
		IntegerInterval type = getType(context);
		Expression result = type.getNonStrictUpperBound();
		return result;
	}

	protected Expression applyAndSimplify(String comparison, ArrayList<Expression> arguments, Context context) {
		Expression unsimplifiedAtom = apply(comparison, arguments);
		Expression result = constraint.getConstraintTheory().simplify(unsimplifiedAtom, context);
		return result;
	}
}