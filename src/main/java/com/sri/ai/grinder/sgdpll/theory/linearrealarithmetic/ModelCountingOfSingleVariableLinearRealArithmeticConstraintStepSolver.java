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
package com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
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
import com.sri.ai.util.Util;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.PairOfElementsInListIterator;

/**
 * A {@link AbstractContextDependentProblemWithPropagatedLiteralsStepSolver} for a {@link SingleVariableLinearRealArithmeticConstraint}.
 * <p>
 * The solution is guaranteed to be either a numerical constant, or
 * a conditional of the form {@code if <satisfiability condition> then <model count> else 0}.
 * 
 * @author braz
 *
 */
@Beta
public class ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver extends AbstractContextDependentProblemWithPropagatedLiteralsStepSolver {

	// NOTE: this class is essentially a copy of ValuesOfSingleVariableLinearRealArithmeticConstraintStepSolver
	// with short-circuiting optimization.
	// They should be kept in tune to each other.
	
	private static final Symbol GREATER_THAN_SYMBOL = makeSymbol(GREATER_THAN);

	private static final Symbol LESS_THAN_SYMBOL = makeSymbol(LESS_THAN);

	private ArrayList<Expression> equals;

	private ArrayList<Expression> disequals;

	private ArrayList<Expression> nonEqualityComparisons;

	private ArrayList<Expression> strictLowerBoundsIncludingImplicitOnes;

	private ArrayList<Expression> nonStrictUpperBoundsIncludingImplicitOnes;

	private ArrayList<PairOf<Expression>> pairsOfEquals;
	
	

	private ContextDependentProblemStepSolver<Expression> initialMaximumStrictLowerBoundStepSolver;

	private ContextDependentProblemStepSolver<Expression> initialMinimumNonStrictUpperBoundStepSolver;

	private ContextDependentProblemStepSolver<List<Expression>> initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver;

	private ContextDependentProblemStepSolver<List<Expression>> initialDisequalsWithinBoundsStepSolver;

	private ContextDependentProblemStepSolver<Boolean> initialLowerBoundIsLessThanUpperBoundStepSolver;

	private NumberOfDistinctExpressionsIsLessThanStepSolver initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;

	private DistinctExpressionsStepSolver initialDistinctDisequalsStepSolver;
	
	

	public ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver(SingleVariableLinearRealArithmeticConstraint constraint) {
		super(constraint);
	}
	
	/**
	 * @return
	 */
	@Override
	public ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver clone() {
		return (ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver) super.clone();
	}

	@Override
	public SingleVariableLinearRealArithmeticConstraint getConstraint() {
		return (SingleVariableLinearRealArithmeticConstraint) super.getConstraint();
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

	private ArrayList<Expression> getStrictLowerBoundsIncludingImplicitOnes(Context context) {
		if (strictLowerBoundsIncludingImplicitOnes == null) {
			SingleVariableLinearRealArithmeticConstraint differenceArithmeticConstraint = (SingleVariableLinearRealArithmeticConstraint) constraint;
			
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

	private ArrayList<Expression> getNonStrictUpperBoundsIncludingImplicitOnes(Context context) {
		if (nonStrictUpperBoundsIncludingImplicitOnes == null) {
			SingleVariableLinearRealArithmeticConstraint differenceArithmeticConstraint = (SingleVariableLinearRealArithmeticConstraint) constraint;
			
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

	private ArrayList<Expression> getEquals() {
		if (equals == null) {
			SingleVariableLinearRealArithmeticConstraint differenceArithmeticConstraint
			= (SingleVariableLinearRealArithmeticConstraint) constraint;
			
			Iterator<Expression> equalsIterator =
					functionIterator(
							predicateIterator(
									differenceArithmeticConstraint.getPositiveNormalizedAtoms(),
									e -> e.hasFunctor(FunctorConstants.EQUALITY)
									), 
									e -> e.get(1));
			
			equals = arrayListFrom(equalsIterator);
		}
		return equals;
	}

	private ArrayList<Expression> getNonEqualityComparisons(Context context) {
		if (nonEqualityComparisons == null) {
			SingleVariableLinearRealArithmeticConstraint differenceArithmeticConstraint = (SingleVariableLinearRealArithmeticConstraint) constraint;

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

	private ArrayList<Expression> getDisequals() {
		if (disequals == null) {
			SingleVariableLinearRealArithmeticConstraint differenceArithmeticConstraint = (SingleVariableLinearRealArithmeticConstraint) constraint;
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

	private ArrayList<PairOf<Expression>> pairsOfEquals() {
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
		ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver successor = clone();

		if (getConstraint().getPropagateAllLiteralsWhenVariableIsBound() && ! getEquals().isEmpty()) {
			solutionExpression = ONE;
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
				ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
				ifTrue.initialMaximumStrictLowerBoundStepSolver = (MaximumExpressionStepSolver) maximumStrictLowerBoundStep.getStepSolverForWhenLiteralIsTrue();
				ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
				ifFalse.initialMaximumStrictLowerBoundStepSolver = (MaximumExpressionStepSolver) maximumStrictLowerBoundStep.getStepSolverForWhenLiteralIsFalse();
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
				ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
				ifTrue.initialMinimumNonStrictUpperBoundStepSolver = (MaximumExpressionStepSolver) minimumNonStrictUpperBoundStep.getStepSolverForWhenLiteralIsTrue();
				ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
				ifFalse.initialMinimumNonStrictUpperBoundStepSolver = (MaximumExpressionStepSolver) minimumNonStrictUpperBoundStep.getStepSolverForWhenLiteralIsFalse();
				ItDependsOn result = new ItDependsOn(minimumNonStrictUpperBoundStep.getLiteral(), minimumNonStrictUpperBoundStep.getContextSplitting(), ifTrue, ifFalse);
				return result;
			}
			Expression leastNonStrictUpperBound = minimumNonStrictUpperBoundStep.getValue();
			successor.initialMinimumNonStrictUpperBoundStepSolver = new ConstantExpressionStepSolver(leastNonStrictUpperBound);
			
			if (greatestStrictLowerBound.equals(MINUS_INFINITY) || leastNonStrictUpperBound.equals(INFINITY)) {
				solutionExpression = INFINITY;
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
					ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
					ifTrue.initialLowerBoundIsLessThanUpperBoundStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenLiteralIsTrue();
					ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
					ifFalse.initialLowerBoundIsLessThanUpperBoundStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenLiteralIsFalse();
					ItDependsOn result = new ItDependsOn(lowerBoundIsLessThanUpperBoundStep.getLiteral(), lowerBoundIsLessThanUpperBoundStep.getContextSplitting(), ifTrue, ifFalse);
					return result;
				}
				if ( ! lowerBoundIsLessThanUpperBoundStep.getValue()) {
					return new Solution(ZERO);
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
					ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
					ifTrue.initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step.getStepSolverForWhenLiteralIsTrue();
					ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
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
					ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
					ifTrue.initialDisequalsWithinBoundsStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step2.getStepSolverForWhenLiteralIsTrue();
					ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
					ifFalse.initialDisequalsWithinBoundsStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step2.getStepSolverForWhenLiteralIsFalse();
					ItDependsOn result = new ItDependsOn(step2.getLiteral(), step2.getContextSplitting(), ifTrue, ifFalse);
					return result;
				}
				ArrayList<Expression> disequalsWithinBounds = new ArrayList<>(step2.getValue());
				successor.initialDisequalsWithinBoundsStepSolver = new ConstantStepSolver<List<Expression>>(disequalsWithinBounds);

				Expression boundsDifference = applyAndSimplify(MINUS, arrayList(leastNonStrictUpperBound, greatestStrictLowerBound), context);

				boolean weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds;
				DistinctExpressionsStepSolver distinctExpressionsStepSolver;
				
				if (isNumber(boundsDifference)) {
					NumberOfDistinctExpressionsIsLessThanStepSolver numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;
					if (initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver == null) {
						numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver
						= new NumberOfDistinctExpressionsIsLessThanStepSolver(boundsDifference.intValue(), disequalsWithinBounds);
					}
					else {
						numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;
					}
					SolutionStep numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver.step(context);

					if (numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.itDepends()) {
						ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
						ifTrue.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = (NumberOfDistinctExpressionsIsLessThanStepSolver) numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getStepSolverForWhenLiteralIsTrue();
						ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
						ifFalse.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = (NumberOfDistinctExpressionsIsLessThanStepSolver) numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getStepSolverForWhenLiteralIsFalse();
						ItDependsOn result = new ItDependsOn(numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getLiteral(), numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getContextSplitting(), ifTrue, ifFalse);
						return result;
					}
					successor.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;
					
					weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getValue().equals(FALSE);
					distinctExpressionsStepSolver = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver.getDistinctExpressionsStepSolver();
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
					solutionExpression = ZERO; // there are no available values left
				}
				else if ( ! getEquals().isEmpty()) { // if bound to a value
					solutionExpression = ONE;
				}
				else {
					SolutionStep distinctDisequalsStep = distinctExpressionsStepSolver.step(context);
					if (distinctDisequalsStep.itDepends()) {
						ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifTrue  = makeBasisForSubStepSolver(successor);
						ifTrue.initialDistinctDisequalsStepSolver = (DistinctExpressionsStepSolver) distinctDisequalsStep.getStepSolverForWhenLiteralIsTrue();
						ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver ifFalse = makeBasisForSubStepSolver(successor);
						ifFalse.initialDistinctDisequalsStepSolver = (DistinctExpressionsStepSolver) distinctDisequalsStep.getStepSolverForWhenLiteralIsFalse();
						ItDependsOn result = new ItDependsOn(distinctDisequalsStep.getLiteral(), distinctDisequalsStep.getContextSplitting(), ifTrue, ifFalse);
						return result;
					}
					Expression numberOfDistinctDisequals = makeSymbol(distinctDisequalsStep.getValue().numberOfArguments());
					ArrayList<Expression> boundsDifferenceAndNumberOfDisequals = arrayList(boundsDifference, numberOfDistinctDisequals);
					solutionExpression = applyAndSimplify(MINUS, boundsDifferenceAndNumberOfDisequals, context);
				}
			}
		}

		return new Solution(solutionExpression);
	}

	private ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver makeBasisForSubStepSolver(ModelCountingOfSingleVariableLinearRealArithmeticConstraintStepSolver successor) {
		return successor.clone();
	}

	private IntegerInterval getType(Context context) {
		return getConstraint().getType(context);
	}
	
	private Expression typeStrictLowerBound;
	
	private Expression getTypeStrictLowerBound(Context context) {
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

	private Expression getTypeNonStrictUpperBound(Context context) {
		IntegerInterval type = getType(context);
		Expression result = type.getNonStrictUpperBound();
		return result;
	}

	private Expression applyAndSimplify(String comparison, ArrayList<Expression> arguments, Context context) {
		Expression unsimplifiedAtom = apply(comparison, arguments);
		Expression result = constraint.getConstraintTheory().simplify(unsimplifiedAtom, context);
		return result;
	}
	
	@Override
	protected Expression getSolutionExpressionGivenContradiction() {
		return ZERO;
	}
}