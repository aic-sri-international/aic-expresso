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

import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.base.PairOf.makePairOf;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;

import java.util.ArrayList;
import java.util.Iterator;

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
import com.sri.ai.grinder.sgdpll.core.solver.AbstractNumericalProblemWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.SingleVariableLinearRealArithmeticConstraint;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.PairOfElementsInListIterator;

/**
 * A {@link AbstractNumericalProblemWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver}
 * for a {@link SingleVariableLinearRealArithmeticConstraint}
 * with basic methods for determining the feasibility region of the variable,
 * but leaving the determination of the final solution to {@link #solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Context)}.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver extends AbstractNumericalProblemWithPropagatedLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver {

	protected static final Symbol GREATER_THAN_SYMBOL = makeSymbol(GREATER_THAN);

	protected static final Symbol LESS_THAN_SYMBOL = makeSymbol(LESS_THAN);

	protected ArrayList<Expression> equals;

	protected ArrayList<Expression> disequals;

	protected ArrayList<Expression> nonEqualityComparisons;

	protected ArrayList<Expression> strictLowerBoundsIncludingImplicitOnes;

	protected ArrayList<Expression> nonStrictUpperBoundsIncludingImplicitOnes;

	protected ArrayList<PairOf<Expression>> pairsOfEquals;
	
	
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