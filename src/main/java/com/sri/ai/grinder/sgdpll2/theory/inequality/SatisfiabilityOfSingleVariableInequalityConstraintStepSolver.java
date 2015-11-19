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

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.base.PairOf.makePairOf;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;

import java.util.ArrayList;
import java.util.Iterator;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.core.solver.AbstractBooleanProblemWithPropagatedAndDefiningLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.PairOfElementsInListIterator;

/**
 * A {@link AbstractBooleanProblemWithPropagatedAndDefiningLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver} for a {@link SingleVariableInequalityConstraint}.
 * 
 * @author braz
 *
 */
@Beta
public class SatisfiabilityOfSingleVariableInequalityConstraintStepSolver extends AbstractBooleanProblemWithPropagatedAndDefiningLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver {

	public SatisfiabilityOfSingleVariableInequalityConstraintStepSolver(SingleVariableInequalityConstraint constraint) {
		super(constraint);
	}
	
	@Override
	public SingleVariableInequalityConstraint getConstraint() {
		return (SingleVariableInequalityConstraint) super.getConstraint();
	}
	
	@Override
	protected boolean usingDefaultImplementationOfGetPropagatedCNF() {
		return true;
	}

	@Override
	protected Iterable<Expression> getPropagatedLiterals(RewritingProcess process) {
		
		// each strict lower bound must be strictly less than every non-strict upper bound:
		// X > Y and X <= Z => Y < Z
		Iterator<Expression> comparisonsBetweenStrictLowerAndNonStrictUpperBounds
		= getIteratorOverComparisonsOfStrictLowerAndNonStrictUpperBounds(process);

		// if X = Y and X = Z, then Y = Z
		Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator = pairsOfEquals();
		Iterator<Expression> propagatedEqualities = functionIterator(pairsOfEqualsToVariableIterator, p -> Equality.makeWithConstantSimplification(p.first, p.second, process));
		// Note: the above could be lumped together with the propagated comparisons below, if
		// they were modified to include equalities instead of just non-equality comparisons
		// However, that would go over all pairs of terms equal to the variable, which is unnecessary since equality is symmetrical
		// The above only goes over pairs that are sorted by position in normalized atoms.
		
		// if X = Y and X op Z, then Y op Z, for op any atom functor other than equality (which is already covered above).
		// TODO: the single-variable constraint should be changed so that when X = Y all other constraints are placed on Y
		// instead and put on external literals

		Iterator<Expression> propagatedComparisons =
				functionIterator(
						new CartesianProductIterator<Expression>(
								() -> getEquals(),
								() -> getNonEqualityComparisons(process)
						),
						equalAndNonEqualityComparison -> {
							Expression equal = equalAndNonEqualityComparison.get(0);
							Expression nonEqualityComparison = equalAndNonEqualityComparison.get(1);
							Expression termBeingCompared = nonEqualityComparison.get(1);
							Expression unsimplifiedAtom = apply(nonEqualityComparison.getFunctor(), equal, termBeingCompared);
							Expression result = constraint.getConstraintTheory().simplify(unsimplifiedAtom, process);
							return result;
						});

		// provide external literals first
		Iterator<Expression> propagatedLiteralsIterator =
				new NestedIterator<>(
						getConstraint().getExternalLiterals(),
						comparisonsBetweenStrictLowerAndNonStrictUpperBounds,
						propagatedEqualities,
						propagatedComparisons);
		// TODO: have super class take care of external literals, so extensions don't need to think about them
		
		Iterable<Expression> result = in(propagatedLiteralsIterator);
		
		return result;
	}

	/**
	 * @param process TODO
	 * @return
	 */
	private Iterator<Expression> getIteratorOverComparisonsOfStrictLowerAndNonStrictUpperBounds(RewritingProcess process) {
		// determine pairs of such bounds
		Iterator<ArrayList<Expression>> pairsOfStrictLowerAndNonStricUpperBounds =
				new CartesianProductIterator<Expression>(
						() -> getStrictLowerBounds(),
						() -> getNonStrictUpperBounds());
		
		// make iterator over their comparisons
		Iterator<Expression> comparisonsBetweenStrictLowerAndNonStrictUpperBounds
		= functionIterator(
				pairsOfStrictLowerAndNonStricUpperBounds,
				pair -> constraint.getConstraintTheory().simplify(apply(LESS_THAN, pair.get(0), pair.get(1)), process));
		return comparisonsBetweenStrictLowerAndNonStrictUpperBounds;
	}

	private Iterator<Expression> getStrictLowerBounds() {
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
		
		Iterator<Expression> result = new NestedIterator<>(
				strictLowerBoundsFromPositiveNormalizedAtomsIterator, strictLowerBoundsFromNegativeNormalizedAtomsIterator);
		
		return result;
	}

	private Iterator<Expression> getNonStrictUpperBounds() {
		SingleVariableInequalityConstraint inequalitiesConstraint = (SingleVariableInequalityConstraint) constraint;
		
		FunctionIterator<Expression, Expression> nonStrictUpperBoundsFromPositiveNormalizedAtomsIterator
		= functionIterator(
				predicateIterator(
						inequalitiesConstraint.getPositiveNormalizedAtoms(),
						e -> e.hasFunctor(LESS_THAN)
				), 
				e -> Plus.make(arrayList(e.get(1), ONE))); // atom is X < Y, so X <= Y + 1, so Y + 1 is a non-strict upper bound
		
		FunctionIterator<Expression, Expression> nonStrictUpperBoundsFromNegativeNormalizedAtomsIterator
		= functionIterator(
				predicateIterator(
						inequalitiesConstraint.getNegativeNormalizedAtoms(),
						e -> e.hasFunctor(GREATER_THAN) // not (X > Y) <=> X <= Y, so Y is a non-strict upper bound
				), 
				e -> e.get(1));
		
		Iterator<Expression> result = new NestedIterator<>(
				nonStrictUpperBoundsFromPositiveNormalizedAtomsIterator, nonStrictUpperBoundsFromNegativeNormalizedAtomsIterator);
		
		return result;
	}

	private Iterator<Expression> getEquals() {
		SingleVariableInequalityConstraint inequalitiesConstraint = (SingleVariableInequalityConstraint) constraint;
		
		FunctionIterator<Expression, Expression> result
		= functionIterator(
				predicateIterator(
						inequalitiesConstraint.getPositiveNormalizedAtoms(),
						e -> e.hasFunctor(FunctorConstants.EQUALITY)
				), 
				e -> e.get(1));
		
		return result;
	}

	private Iterator<Expression> getNonEqualityComparisons(RewritingProcess process) {
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

		Iterator<Expression> result =
				new NestedIterator<Expression>(fromPositiveNormalizedAtoms, fromNegativeNormalizedAtoms);
		
		return result;
	}

	private Iterator<Expression> getDisequals(RewritingProcess process) {
		SingleVariableInequalityConstraint inequalitiesConstraint = (SingleVariableInequalityConstraint) constraint;
		
		FunctionIterator<Expression, Expression> result
		= functionIterator(
				predicateIterator(
						inequalitiesConstraint.getNegativeNormalizedAtoms(),
						e -> e.hasFunctor(FunctorConstants.EQUALITY) // negative equality is disequality
				), 
				e -> e.get(1));
		
		return result;
	}

	private Iterator<PairOf<Expression>> pairsOfEquals() {
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
		Iterator<PairOf<Expression>> result = functionIterator(pairsOfEqualitiesIterator, makePairOfSecondArguments);
		
		return result;
	}

	private Iterator<PairOf<Expression>> pairsOfDisequals() {
		ArrayList<Expression> equalities = Util.collectToArrayList(getConstraint().getNegativeNormalizedAtoms(), e -> e.hasFunctor(EQUALITY));
		
		PairOfElementsInListIterator<Expression> pairsOfDisequalitiesIterator = 
				new PairOfElementsInListIterator<>(equalities);

//		Function<PairOf<Expression>, PairOf<Expression>> makePairOfSecondArguments = p -> makePairOf(p.first.get(1), p.second.get(1));
// above lambda somehow not working at Ciaran's environment, replacing with seemingly identical anonymous class object below		
		Function<PairOf<Expression>, PairOf<Expression>> makePairOfSecondArguments = new Function<PairOf<Expression>, PairOf<Expression>>() {
			@Override
			public PairOf<Expression> apply(PairOf<Expression> p) {
				return makePairOf(p.first.get(1), p.second.get(1));
			}
		};
		Iterator<PairOf<Expression>> result = functionIterator(pairsOfDisequalitiesIterator, makePairOfSecondArguments);
		
		return result;
	}

	@Override
	protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(RewritingProcess process) {
		return list();
	}
	
	@Override
	protected Iterable<Expression> getDefiningLiterals(RewritingProcess process) {

		// before we can reach a decision, we need to know whether each disequality is within or without bounds:
		
		// if X > Y and X != W, is W <= Y (making it irrelevant) or is W > Y?
		Iterator<Expression> strictLowerBoundsAndDisequals =
				functionIterator(
						new CartesianProductIterator<Expression>(
								() -> getStrictLowerBounds(),
								() -> getDisequals(process)
						),
						strictLowerBoundAndDisequal -> {
							Expression strictLowerBound = strictLowerBoundAndDisequal.get(0);
							Expression disequal = strictLowerBoundAndDisequal.get(1);
							Expression unsimplifiedAtom = apply(LESS_THAN_OR_EQUAL_TO, strictLowerBound, disequal);
							Expression result = constraint.getConstraintTheory().simplify(unsimplifiedAtom, process);
							return result;
						});
		
		// if X <= Z and X != W, is W > Z (making it irrelevant) or is W <= Z?
		Iterator<Expression> nonStrictUpperBoundsAndDisequals =
				functionIterator(
						new CartesianProductIterator<Expression>(
								() -> getNonStrictUpperBounds(),
								() -> getDisequals(process)
						),
						nonStrictUpperBoundAndDisequal -> {
							Expression nonStrictUpperBound = nonStrictUpperBoundAndDisequal.get(0);
							Expression disequal = nonStrictUpperBoundAndDisequal.get(1);
							Expression unsimplifiedAtom = apply(GREATER_THAN, disequal, nonStrictUpperBound);
							Expression result = constraint.getConstraintTheory().simplify(unsimplifiedAtom, process);
							return result;
						});
		
		// we also need to know how many distinct disequals there are
		Iterator<Expression> disequalsComparisons =
				functionIterator(
						pairsOfDisequals(), pair -> {
							Expression unsimplifiedAtom = apply(EQUALITY, pair.first, pair.second);
							return constraint.getConstraintTheory().simplify(unsimplifiedAtom, process);	
						});

		Iterator<Expression> result = new NestedIterator<Expression>(
				strictLowerBoundsAndDisequals, nonStrictUpperBoundsAndDisequals, disequalsComparisons);
		
		return in(result);
	}

	@Override
	protected Expression solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfiedAndDefiningLiteralsAreDefined(Constraint2 contextualConstraint, RewritingProcess process) {
		return TRUE;
	}
}