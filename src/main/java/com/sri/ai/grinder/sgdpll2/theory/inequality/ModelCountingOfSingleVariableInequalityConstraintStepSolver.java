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
package com.sri.ai.grinder.sgdpll2.theory.inequality;

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
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.solver.AbstractNumericalProblemWithPropagatedAndDefiningLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver;
import com.sri.ai.grinder.sgdpll2.theory.base.LiteralStepSolver;
import com.sri.ai.grinder.sgdpll2.theory.equality.NumberOfDistinctExpressionsIsLessThanStepSolver;
import com.sri.ai.grinder.sgdpll2.theory.equality.NumberOfDistinctExpressionsStepSolver;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.PairOfElementsInListIterator;

/**
 * A {@link AbstractNumericalProblemWithPropagatedAndDefiningLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver} for a {@link SingleVariableInequalityConstraint}.
 * <p>
 * The solution is guaranteed to be either a numerical constant, or
 * a conditional of the form {@code if <satisfiability condition> then <model count> else 0}.
 * 
 * @author braz
 *
 */
@Beta
public class ModelCountingOfSingleVariableInequalityConstraintStepSolver extends AbstractNumericalProblemWithPropagatedAndDefiningLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver {

	private static final Symbol GREATER_THAN_SYMBOL = makeSymbol(FunctorConstants.GREATER_THAN);

	private static final Symbol LESS_THAN_SYMBOL = makeSymbol(LESS_THAN);

	private ArrayList<Expression> equals;

	private ArrayList<Expression> disequals;

	private ArrayList<Expression> nonEqualityComparisons;

	private ArrayList<Expression> strictLowerBoundsIncludingImplicitOnes;

	private ArrayList<Expression> nonStrictUpperBoundsIncludingImplicitOnes;

	private ArrayList<PairOf<Expression>> pairsOfEquals;
	
	

	private MaximumExpressionStepSolver initialMaximumStrictLowerBoundStepSolver;

	private MaximumExpressionStepSolver initialMinimumNonStrictUpperBoundStepSolver;

	private SelectExpressionsSatisfyingComparisonStepSolver initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver;

	private SelectExpressionsSatisfyingComparisonStepSolver initialDisequalsWithinBoundsStepSolver;

	private ContextDependentProblemStepSolver<Boolean> initialLowerBoundIsLessThanUpperBoundStepSolver;

	private NumberOfDistinctExpressionsIsLessThanStepSolver initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;

	private NumberOfDistinctExpressionsStepSolver initialNumberOfDistinctDisequalsStepSolver;
	
	

	public ModelCountingOfSingleVariableInequalityConstraintStepSolver(SingleVariableInequalityConstraint constraint) {
		super(constraint);
	}
	
	/**
	 * @return
	 */
	public ModelCountingOfSingleVariableInequalityConstraintStepSolver clone() {
		return (ModelCountingOfSingleVariableInequalityConstraintStepSolver) super.clone();
	}

	@Override
	public SingleVariableInequalityConstraint getConstraint() {
		return (SingleVariableInequalityConstraint) super.getConstraint();
	}
	
	@Override
	protected boolean usingDefaultImplementationOfMakePropagatedCNF() {
		return true;
	}

	@Override
	protected Iterable<Expression> getPropagatedLiterals(RewritingProcess process) {
		
		// System.out.println("getPropagatedLiterals:");
		// System.out.println("constraint: " + constraint);
		// System.out.println("strict lower bounds: " + join(getStrictLowerBounds(process)));
		// System.out.println("non-strict upper bounds: " + join(getNonStrictUpperBounds(process)));
		// System.out.println("pairs of equals to variable: " + join(pairsOfEquals()));
		// System.out.println("equals to variable: " + join(getEquals()));
		// System.out.println("non-equality comparisons: " + join(getNonEqualityComparisons(process)));
		
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
								Expression result = Equality.makeWithConstantSimplification(p.first, p.second, process);
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
									() -> getNonEqualityComparisons(process).iterator()
									),
									equalAndNonEqualityComparison -> {
										Expression equal = equalAndNonEqualityComparison.get(0);
										Expression nonEqualityComparison = equalAndNonEqualityComparison.get(1);
										Expression termBeingCompared = nonEqualityComparison.get(1);
										Expression unsimplifiedAtom = apply(nonEqualityComparison.getFunctor(), equal, termBeingCompared);
										Expression result = constraint.getConstraintTheory().simplify(unsimplifiedAtom, process);
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

	private ArrayList<Expression> getStrictLowerBoundsIncludingImplicitOnes(RewritingProcess process) {
		if (strictLowerBoundsIncludingImplicitOnes == null) {
			SingleVariableInequalityConstraint inequalitiesConstraint = (SingleVariableInequalityConstraint) constraint;
			
			FunctionIterator<Expression, Expression> strictLowerBoundsFromPositiveNormalizedAtomsIterator
			= functionIterator(
					predicateIterator(
							inequalitiesConstraint.getPositiveNormalizedAtoms(),
							e -> e.hasFunctor(GREATER_THAN) // X > Y, so Y is a strict lower bound
					), 
					e -> e.get(1));
			
			FunctionIterator<Expression, Expression> strictLowerBoundsFromNegativeNormalizedAtomsIterator
			= functionIterator(
					predicateIterator(
							inequalitiesConstraint.getNegativeNormalizedAtoms(),
							e -> e.hasFunctor(LESS_THAN)
					), 
					e -> apply(MINUS, e.get(1), ONE)); // atom is (not (X < Y)), e.g., X >= Y, so X > Y - 1 and Y - 1 is a strict lower bound
			
			Expression typeStrictLowerBound = getTypeStrictLowerBound(process);
			
			Iterator<Expression> strictLowerBoundsIterator = new NestedIterator<>(
					strictLowerBoundsFromPositiveNormalizedAtomsIterator,
					strictLowerBoundsFromNegativeNormalizedAtomsIterator,
					typeStrictLowerBound);
			
			strictLowerBoundsIncludingImplicitOnes = arrayListFrom(strictLowerBoundsIterator);
		}
		return strictLowerBoundsIncludingImplicitOnes;
	}

	private ArrayList<Expression> getNonStrictUpperBoundsIncludingImplicitOnes(RewritingProcess process) {
		if (nonStrictUpperBoundsIncludingImplicitOnes == null) {
			SingleVariableInequalityConstraint inequalitiesConstraint = (SingleVariableInequalityConstraint) constraint;
			
			FunctionIterator<Expression, Expression> nonStrictUpperBoundsFromPositiveNormalizedAtomsIterator
			= functionIterator(
					predicateIterator(
							inequalitiesConstraint.getPositiveNormalizedAtoms(),
							e -> e.hasFunctor(LESS_THAN)
					),
					e -> Minus.make(e.get(1), ONE)); // atom is X < Y, so X <= Y - 1, so Y - 1 is a non-strict upper bound
			
			FunctionIterator<Expression, Expression> nonStrictUpperBoundsFromNegativeNormalizedAtomsIterator
			= functionIterator(
					predicateIterator(
							inequalitiesConstraint.getNegativeNormalizedAtoms(),
							e -> e.hasFunctor(GREATER_THAN) // not (X > Y) <=> X <= Y, so Y is a non-strict upper bound
					), 
					e -> e.get(1));
			
			Expression typeNonStrictUpperBound = getTypeNonStrictUpperBound(process);
			
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
			SingleVariableInequalityConstraint inequalitiesConstraint
			= (SingleVariableInequalityConstraint) constraint;
			
			Iterator<Expression> equalsIterator =
					functionIterator(
							predicateIterator(
									inequalitiesConstraint.getPositiveNormalizedAtoms(),
									e -> e.hasFunctor(FunctorConstants.EQUALITY)
									), 
									e -> e.get(1));
			
			equals = arrayListFrom(equalsIterator);
		}
		return equals;
	}

	private ArrayList<Expression> getNonEqualityComparisons(RewritingProcess process) {
		if (nonEqualityComparisons == null) {
			SingleVariableInequalityConstraint inequalitiesConstraint = (SingleVariableInequalityConstraint) constraint;

			Iterator<Expression> fromPositiveNormalizedAtoms =
					predicateIterator(
							inequalitiesConstraint.getPositiveNormalizedAtoms(),
							e -> ! e.hasFunctor(FunctorConstants.EQUALITY)
							);

			Iterator<Expression> fromNegativeNormalizedAtoms =
					functionIterator(
							inequalitiesConstraint.getNegativeNormalizedAtoms(), // negative normalized atom is never an equality
							e -> inequalitiesConstraint.getConstraintTheory().getLiteralNegation(e, process)
							);

			Expression variableIsGreaterThanTypeStrictLowerBound =
					apply(GREATER_THAN, getConstraint().getVariable(), getTypeStrictLowerBound(process));

			Expression variableIsLessThanOrEqualToTypeNonStrictUpperBound =
					apply(LESS_THAN_OR_EQUAL_TO, getConstraint().getVariable(), getTypeNonStrictUpperBound(process));

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
			SingleVariableInequalityConstraint inequalitiesConstraint = (SingleVariableInequalityConstraint) constraint;
			Iterator<Expression> disequalsIterator =
					functionIterator(
							predicateIterator(
									inequalitiesConstraint.getNegativeNormalizedAtoms(),
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
	protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(RewritingProcess process) {
		return list();
	}
	
	@Override
	protected Iterable<Expression> getDefiningLiterals(Constraint2 contextualConstraint, RewritingProcess process) {
		return list();
	}

	@Override
	protected SolutionStep solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfiedAndDefiningLiteralsAreDefined(Constraint2 contextualConstraint, RewritingProcess process) {
		// at this point, the context establishes that one of the strict lower bounds L is greater than all the others,
		// that one of the non-strict upper bounds U is less than all the others, and that
		// all disequals are in ]L, U], and are disequal from each other.
		// Therefore, the constraint is satisfiable if and only if U - L > D
		// where D is the number of disequals.

		Expression solutionExpression;

		if (getConstraint().getPropagateAllLiteralsWhenVariableIsBound() && ! getEquals().isEmpty()) {
			solutionExpression = ONE;
		}
		else {
			MaximumExpressionStepSolver maximumStrictLowerBoundStepSolver;
			if (initialMaximumStrictLowerBoundStepSolver == null) {
				maximumStrictLowerBoundStepSolver
				= new MaximumExpressionStepSolver(
						getStrictLowerBoundsIncludingImplicitOnes(process),
						LESS_THAN_SYMBOL, // use total order <
						MINUS_INFINITY,
						INFINITY); // at first, I placed the type minimum and maximum strict lower bounds here. This is incorrect because if the type maximum is, say, 4, and I have "X > 3 and X > I" (3 is the maximum strict lower bounds for values in the type), the step solver short-circuits and returns 3, without ever even looking at I. Looking at I is needed because if I is greater than 3 than this constraint is unsatisfiable.
			}
			else {
				maximumStrictLowerBoundStepSolver = initialMaximumStrictLowerBoundStepSolver;
			}
			ContextDependentProblemStepSolver.SolutionStep<Expression> maximumStrictLowerBoundStep = maximumStrictLowerBoundStepSolver.step(contextualConstraint, process);
			if (maximumStrictLowerBoundStep.itDepends()) {
				ModelCountingOfSingleVariableInequalityConstraintStepSolver ifTrue  = clone();
				ifTrue.initialMaximumStrictLowerBoundStepSolver = (MaximumExpressionStepSolver) maximumStrictLowerBoundStep.getStepSolverForWhenLiteralIsTrue();
				ModelCountingOfSingleVariableInequalityConstraintStepSolver ifFalse = clone();
				ifFalse.initialMaximumStrictLowerBoundStepSolver = (MaximumExpressionStepSolver) maximumStrictLowerBoundStep.getStepSolverForWhenLiteralIsFalse();
				ItDependsOn result = new ItDependsOn(maximumStrictLowerBoundStep.getLiteral(), maximumStrictLowerBoundStep.getConstraintSplitting(), ifTrue, ifFalse);
				return result;
			}
			Expression greatestStrictLowerBound = maximumStrictLowerBoundStep.getValue();

			MaximumExpressionStepSolver minimumNonStrictUpperBoundStepSolver;
			if (initialMinimumNonStrictUpperBoundStepSolver == null) {
				minimumNonStrictUpperBoundStepSolver
				= new MaximumExpressionStepSolver(
						getNonStrictUpperBoundsIncludingImplicitOnes(process),
						GREATER_THAN_SYMBOL, // use total order > since "minimum" is maximum under it
						INFINITY, // "minimum" is maximum value because we are operating on the inverse order
						MINUS_INFINITY); // "maximum" is minimum value because we are operating on the inverse order
			}
			else {
				minimumNonStrictUpperBoundStepSolver = initialMinimumNonStrictUpperBoundStepSolver;
			}
			ContextDependentProblemStepSolver.SolutionStep<Expression> minimumNonStrictUpperBoundStep = minimumNonStrictUpperBoundStepSolver.step(contextualConstraint, process);
			if (minimumNonStrictUpperBoundStep.itDepends()) {
				ModelCountingOfSingleVariableInequalityConstraintStepSolver ifTrue  = clone();
				ifTrue.initialMinimumNonStrictUpperBoundStepSolver = (MaximumExpressionStepSolver) minimumNonStrictUpperBoundStep.getStepSolverForWhenLiteralIsTrue();
				ModelCountingOfSingleVariableInequalityConstraintStepSolver ifFalse = clone();
				ifFalse.initialMinimumNonStrictUpperBoundStepSolver = (MaximumExpressionStepSolver) minimumNonStrictUpperBoundStep.getStepSolverForWhenLiteralIsFalse();
				ItDependsOn result = new ItDependsOn(minimumNonStrictUpperBoundStep.getLiteral(), minimumNonStrictUpperBoundStep.getConstraintSplitting(), ifTrue, ifFalse);
				return result;
			}
			Expression leastNonStrictUpperBound = minimumNonStrictUpperBoundStep.getValue();

			if (greatestStrictLowerBound.equals(MINUS_INFINITY) || leastNonStrictUpperBound.equals(INFINITY)) {
				solutionExpression = INFINITY;
			}
			else {
				ContextDependentProblemStepSolver<Boolean> lowerBoundIsLessThanUpperBoundStepSolver;
				if (initialLowerBoundIsLessThanUpperBoundStepSolver == null) {
					Expression lowerBoundIsLessThanUpperBound = applyAndSimplify(LESS_THAN, arrayList(greatestStrictLowerBound, leastNonStrictUpperBound), process);
					lowerBoundIsLessThanUpperBoundStepSolver = new LiteralStepSolver(lowerBoundIsLessThanUpperBound);
				}
				else {
					lowerBoundIsLessThanUpperBoundStepSolver = initialLowerBoundIsLessThanUpperBoundStepSolver;
				}
				ContextDependentProblemStepSolver.SolutionStep<Boolean> lowerBoundIsLessThanUpperBoundStep = lowerBoundIsLessThanUpperBoundStepSolver.step(contextualConstraint, process);
				if (lowerBoundIsLessThanUpperBoundStep.itDepends()) {
					ModelCountingOfSingleVariableInequalityConstraintStepSolver ifTrue  = clone();
					ifTrue.initialLowerBoundIsLessThanUpperBoundStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenLiteralIsTrue();
					ModelCountingOfSingleVariableInequalityConstraintStepSolver ifFalse = clone();
					ifFalse.initialLowerBoundIsLessThanUpperBoundStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenLiteralIsFalse();
					ItDependsOn result = new ItDependsOn(lowerBoundIsLessThanUpperBoundStep.getLiteral(), lowerBoundIsLessThanUpperBoundStep.getConstraintSplitting(), ifTrue, ifFalse);
					return result;
				}
				if ( ! lowerBoundIsLessThanUpperBoundStep.getValue()) {
					return new Solution(ZERO);
				}
				// else, bounds difference is positive and we can move on
				
				SelectExpressionsSatisfyingComparisonStepSolver disequalsGreaterThanGreatestStrictLowerBoundStepSolver;
				if (initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver == null) {
					disequalsGreaterThanGreatestStrictLowerBoundStepSolver
					= new SelectExpressionsSatisfyingComparisonStepSolver(getDisequals(), GREATER_THAN, greatestStrictLowerBound);
				}
				else {
					disequalsGreaterThanGreatestStrictLowerBoundStepSolver = initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver;
				}
				ContextDependentProblemStepSolver.SolutionStep<List<Expression>> step
				= disequalsGreaterThanGreatestStrictLowerBoundStepSolver.step(contextualConstraint, process);
				if (step.itDepends()) {
					ModelCountingOfSingleVariableInequalityConstraintStepSolver ifTrue  = clone();
					ifTrue.initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step.getStepSolverForWhenLiteralIsTrue();
					ModelCountingOfSingleVariableInequalityConstraintStepSolver ifFalse = clone();
					ifFalse.initialDisequalsGreaterThanGreatestStrictLowerBoundStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step.getStepSolverForWhenLiteralIsFalse();
					ItDependsOn result = new ItDependsOn(step.getLiteral(), step.getConstraintSplitting(), ifTrue, ifFalse);
					return result;
				}
				List<Expression> disequalsGreaterThanGreatestStrictLowerBound = step.getValue();

				SelectExpressionsSatisfyingComparisonStepSolver disequalsWithinBoundsStepSolver;
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
				= disequalsWithinBoundsStepSolver.step(contextualConstraint, process);
				if (step2.itDepends()) {
					ModelCountingOfSingleVariableInequalityConstraintStepSolver ifTrue  = clone();
					ifTrue.initialDisequalsWithinBoundsStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step2.getStepSolverForWhenLiteralIsTrue();
					ModelCountingOfSingleVariableInequalityConstraintStepSolver ifFalse = clone();
					ifFalse.initialDisequalsWithinBoundsStepSolver = (SelectExpressionsSatisfyingComparisonStepSolver) step2.getStepSolverForWhenLiteralIsFalse();
					ItDependsOn result = new ItDependsOn(step2.getLiteral(), step2.getConstraintSplitting(), ifTrue, ifFalse);
					return result;
				}
				ArrayList<Expression> disequalsWithinBounds = new ArrayList<>(step2.getValue());

				Expression boundsDifference = applyAndSimplify(MINUS, arrayList(leastNonStrictUpperBound, greatestStrictLowerBound), process);

				boolean weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds;
				NumberOfDistinctExpressionsStepSolver numberOfDistinctExpressionsStepSolver;
				
				if (isNumber(boundsDifference)) {
					NumberOfDistinctExpressionsIsLessThanStepSolver numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;
					if (initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver == null) {
						numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver
						= new NumberOfDistinctExpressionsIsLessThanStepSolver(boundsDifference.intValue(), disequalsWithinBounds);
					}
					else {
						numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver;
					}
					SolutionStep numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver.step(contextualConstraint, process);

					if (numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.itDepends()) {
						ModelCountingOfSingleVariableInequalityConstraintStepSolver ifTrue  = clone();
						ifTrue.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = (NumberOfDistinctExpressionsIsLessThanStepSolver) numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getStepSolverForWhenLiteralIsTrue();
						ModelCountingOfSingleVariableInequalityConstraintStepSolver ifFalse = clone();
						ifFalse.initialNumberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver = (NumberOfDistinctExpressionsIsLessThanStepSolver) numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getStepSolverForWhenLiteralIsFalse();
						ItDependsOn result = new ItDependsOn(numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getLiteral(), numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getConstraintSplitting(), ifTrue, ifFalse);
						return result;
					}

					weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStep.getValue().equals(FALSE);
					numberOfDistinctExpressionsStepSolver = numberOfDistinctDisequalsIsLessThanBoundsDifferenceStepSolver.getNumberOfDistinctExpressionsStepSolver();
				}
				else {
					weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds = false;
					if (initialNumberOfDistinctDisequalsStepSolver == null) {
						numberOfDistinctExpressionsStepSolver = new NumberOfDistinctExpressionsStepSolver(disequalsWithinBounds);
					}
					else {
						numberOfDistinctExpressionsStepSolver = initialNumberOfDistinctDisequalsStepSolver;
					}
				}

				if (weKnowThatNumberOfDistinctDisequalsExceedsNumberOfValuesWithinBounds) {
					solutionExpression = ZERO; // there are no available values left
				}
				else if ( ! getEquals().isEmpty()) { // if bound to a value
					solutionExpression = ONE;
				}
				else {
					SolutionStep numberOfDistinctDisequalsStep = numberOfDistinctExpressionsStepSolver.step(contextualConstraint, process);
					if (numberOfDistinctDisequalsStep.itDepends()) {
						ModelCountingOfSingleVariableInequalityConstraintStepSolver ifTrue  = clone();
						ifTrue.initialNumberOfDistinctDisequalsStepSolver = (NumberOfDistinctExpressionsStepSolver) numberOfDistinctDisequalsStep.getStepSolverForWhenLiteralIsTrue();
						ModelCountingOfSingleVariableInequalityConstraintStepSolver ifFalse = clone();
						ifFalse.initialNumberOfDistinctDisequalsStepSolver = (NumberOfDistinctExpressionsStepSolver) numberOfDistinctDisequalsStep.getStepSolverForWhenLiteralIsFalse();
						ItDependsOn result = new ItDependsOn(numberOfDistinctDisequalsStep.getLiteral(), numberOfDistinctDisequalsStep.getConstraintSplitting(), ifTrue, ifFalse);
						return result;
					}
					Expression numberOfDistinctDisequals = numberOfDistinctDisequalsStep.getValue();
					ArrayList<Expression> boundsDifferenceAndNumberOfDisequals = arrayList(boundsDifference, numberOfDistinctDisequals);
					solutionExpression = applyAndSimplify(MINUS, boundsDifferenceAndNumberOfDisequals, process);
				}
			}
		}

		return new Solution(solutionExpression);
	}

	private IntegerInterval getType(RewritingProcess process) {
		return getConstraint().getType(process);
	}
	
	private Expression typeStrictLowerBound;
	
	private Expression getTypeStrictLowerBound(RewritingProcess process) {
		if (typeStrictLowerBound == null) {
			IntegerInterval type = getType(process);
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

	private Expression getTypeNonStrictUpperBound(RewritingProcess process) {
		IntegerInterval type = getType(process);
		Expression result = type.getNonStrictUpperBound();
		return result;
	}

	private Expression applyAndSimplify(String comparison, ArrayList<Expression> arguments, RewritingProcess process) {
		Expression unsimplifiedAtom = apply(comparison, arguments);
		Expression result = constraint.getConstraintTheory().simplify(unsimplifiedAtom, process);
		return result;
	}
}