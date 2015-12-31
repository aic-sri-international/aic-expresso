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

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.set;
import static com.sri.ai.util.base.PairOf.makePairOf;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.collect.NestedIterator.nestedIterator;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

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
import com.sri.ai.grinder.sgdpll2.core.solver.AbstractNumericalProblemWithPropagatedAndDefiningLiteralsRequiringPropagatedLiteralsAndCNFToBeSatisfiedStepSolver;
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

	private ArrayList<Expression> strictLowerBounds;

	private ArrayList<Expression> nonStrictUpperBounds;

	private IntegerInterval type;
	
	private ArrayList<PairOf<Expression>> pairsOfDisequals;

	public ModelCountingOfSingleVariableInequalityConstraintStepSolver(SingleVariableInequalityConstraint constraint) {
		super(constraint);
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
		
		// each strict lower bound must be strictly less than every non-strict upper bound:
		// X > Y and X <= Z => Y < Z
		Iterator<Expression> comparisonsBetweenStrictLowerAndNonStrictUpperBounds
		= getComparisonsOfStrictLowerAndNonStrictUpperBoundsIterator(process);

		Iterator<Expression> propagatedEqualities;
		if (getConstraint().getPropagateAllLiteralsWhenVariableIsBound()) {
			propagatedEqualities = iterator(); // the literals below must have already been propagated
		}
		else {
			// if X = Y and X = Z, then Y = Z
			Iterator<PairOf<Expression>> pairsOfEqualsToVariableIterator = pairsOfEqualsIterator();
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
									() -> getNonEqualityComparisonsIterator(process)
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
						comparisonsBetweenStrictLowerAndNonStrictUpperBounds,
						propagatedEqualities,
						propagatedComparisons);
		// TODO: have super class take care of external literals, so extensions don't need to think about them
		
		Iterable<Expression> result = in(propagatedLiteralsIterator);
		
		return result;
	}

	private Iterator<Expression> getComparisonsOfStrictLowerAndNonStrictUpperBoundsIterator(RewritingProcess process) {
		// determine pairs of such bounds
		Iterator<ArrayList<Expression>> pairsOfStrictLowerAndNonStricUpperBounds =
				new CartesianProductIterator<Expression>(
						() -> getStrictLowerBoundsIncludingImplicitOnes(process).iterator(),
						() -> getNonStrictUpperBoundsIncludingImplicitOnes(process).iterator());
		
		// make iterator over their comparisons
		Iterator<Expression> comparisonsBetweenStrictLowerAndNonStrictUpperBounds
		= functionIterator(
				pairsOfStrictLowerAndNonStricUpperBounds,
				pair -> {
					Expression unsimplifiedLiteral = apply(LESS_THAN, pair.get(0), pair.get(1));
					Expression result = constraint.getConstraintTheory().simplify(unsimplifiedLiteral, process);
					// System.out.println("Unsimplified comparison of strict lower and non-strict upper bounds: " + unsimplifiedLiteral);	
					// System.out.println("constraint is: " + constraint);	
					// System.out.println("Simplified to: " + result);	
					return result;
				});
		return comparisonsBetweenStrictLowerAndNonStrictUpperBounds;
	}

	private ArrayList<Expression> getStrictLowerBoundsIncludingImplicitOnes(RewritingProcess process) {
		if (strictLowerBounds == null) {
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
			
			strictLowerBounds = arrayListFrom(strictLowerBoundsIterator);
		}
		return strictLowerBounds;
	}

	private ArrayList<Expression> getNonStrictUpperBoundsIncludingImplicitOnes(RewritingProcess process) {
		if (nonStrictUpperBounds == null) {
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
			
			nonStrictUpperBounds = arrayListFrom(nonStrictUpperBoundsIterator);
		}
		return nonStrictUpperBounds;
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

	private Iterator<Expression> getNonEqualityComparisonsIterator(RewritingProcess process) {
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
		
		Iterator<Expression> result =
				new NestedIterator<Expression>(
						fromPositiveNormalizedAtoms,
						fromNegativeNormalizedAtoms,
						variableIsGreaterThanTypeStrictLowerBound,
						variableIsLessThanOrEqualToTypeNonStrictUpperBound);
		
		return result;
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

	private Iterator<PairOf<Expression>> pairsOfEqualsIterator() {
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

	private ArrayList<PairOf<Expression>> getPairsOfDisequals() {
		if (pairsOfDisequals == null) {
			ArrayList<Expression> equalities = Util.collectToArrayList(getConstraint().getNegativeNormalizedAtoms(), e -> e.hasFunctor(EQUALITY));
			
			PairOfElementsInListIterator<Expression> pairsOfDisequalitiesIterator = 
					new PairOfElementsInListIterator<>(equalities);

//			Function<PairOf<Expression>, PairOf<Expression>> makePairOfSecondArguments = p -> makePairOf(p.first.get(1), p.second.get(1));
	// above lambda somehow not working at Ciaran's environment, replacing with seemingly identical anonymous class object below		
			Function<PairOf<Expression>, PairOf<Expression>> makePairOfSecondArguments = new Function<PairOf<Expression>, PairOf<Expression>>() {
				@Override
				public PairOf<Expression> apply(PairOf<Expression> p) {
					return makePairOf(p.first.get(1), p.second.get(1));
				}
			};
			Iterator<PairOf<Expression>> result = functionIterator(pairsOfDisequalitiesIterator, makePairOfSecondArguments);

			pairsOfDisequals = arrayListFrom(result);
		}
		return pairsOfDisequals;
	}

	@Override
	protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(RewritingProcess process) {
		return list();
	}
	
	@Override
	protected Iterable<Expression> getDefiningLiterals(Constraint2 contextualConstraint, RewritingProcess process) {

		// System.out.println("getDefiningLiterals:");
		// System.out.println("constraint: " + constraint);
		// System.out.println("disequals: " + join(getDisequals(process)));
		
		// before we can reach a decision, we need to know whether each value being compared to the variable is within or without bounds:
		
		// if X > Y and X op W (for op in >, !=) is W <= Y (making it irrelevant) or is W > Y?
		// op does not need to be <= because in this case we would already know that W > Y because that would be a propagated literal
		Iterator<Expression> strictLowerBoundsAndDisequalsAndOtherStrictLowerBounds =
				functionIterator(
						new CartesianProductIterator<Expression>(
								() -> nestedIterator(getDisequals(), getStrictLowerBoundsIncludingImplicitOnes(process)),
								() -> getStrictLowerBoundsIncludingImplicitOnes(process).iterator()
						),
						anotherAndStrictLowerBound -> {
							Expression result = applyAndSimplify(LESS_THAN_OR_EQUAL_TO, anotherAndStrictLowerBound, process);
							// System.out.println("comparison of disequal/lower bound to lower bound: " + apply(LESS_THAN_OR_EQUAL_TO, anotherAndStrictLowerBound));	
							// System.out.println("simplified to: " + result);	
							return result;
						}
						);
		
		// if X <= Z and X op W (for op in !=, <=), is W > Z (making it irrelevant) or is W <= Z?
		// op does not need to be > because in this case we would already know that W < Z because that would be a propagated literal
		Iterator<Expression> nonStrictUpperBoundsAndDisequalsAndOtherNonStrictUpperBounds =
				functionIterator(
						new CartesianProductIterator<Expression>(
								() -> nestedIterator(getDisequals(), getNonStrictUpperBoundsIncludingImplicitOnes(process)),
								() -> getNonStrictUpperBoundsIncludingImplicitOnes(process).iterator()
						),
						anotherAndNonStrictUpperBound -> {
							Expression result = applyAndSimplify(GREATER_THAN, anotherAndNonStrictUpperBound, process);
							// System.out.println("comparison of disequal/upper bound to upper bound: " + apply(GREATER_THAN, anotherAndNonStrictUpperBound));	
							// System.out.println("simplified to: " + result);	
							return result;
						}
						);
		
		// we also need to know how many distinct disequals there are
		Iterator<Expression> disequalsComparisons =
				functionIterator(
						getPairsOfDisequals(), pair -> {
							Expression unsimplifiedAtom = apply(EQUALITY, pair.first, pair.second);
							return constraint.getConstraintTheory().simplify(unsimplifiedAtom, process);	
						});

		Iterator<Expression> numberOfDistinctDisequalsIsLessThanNumberOfValuesAllowedByBounds =
				definingLiteralsForCheckingIfNumberOfDistinctDisequalsDoesNotExceedNumberOfValuesAllowedByBounds(contextualConstraint, process);

		Iterator<Expression> result = new NestedIterator<Expression>(
				strictLowerBoundsAndDisequalsAndOtherStrictLowerBounds,
				nonStrictUpperBoundsAndDisequalsAndOtherNonStrictUpperBounds,
				disequalsComparisons,
				numberOfDistinctDisequalsIsLessThanNumberOfValuesAllowedByBounds
				);
		
		return in(result);
	}

	private Iterator<Expression> definingLiteralsForCheckingIfNumberOfDistinctDisequalsDoesNotExceedNumberOfValuesAllowedByBounds(Constraint2 contextualConstraint, RewritingProcess process) {
		// at this point, the context establishes that one of the strict lower bounds L is greater than all the others,
		// that one of the non-strict upper bounds U is less than all the others, and that
		// all disequals are in ]L, U], and are disequal from each other.
		// Therefore, the constraint is satisfiable if and only if U - L > D
		// where D is the number of disequals.
		
		Expression greatestStrictLowerBound = computeGreatestStrictLowerBound(contextualConstraint, process);
		
		Expression leastNonStrictUpperBound = computeLeastNonStrictUpperBound(contextualConstraint, process);
		
		Iterator<Expression> result;
		if (greatestStrictLowerBound.equals(MINUS_INFINITY) || leastNonStrictUpperBound.equals(INFINITY)) {
			result = iterator(); // result is infinity, so no need for further defining literals
		}
		else {
			int numberOfDisequals = computeNumberOfDistinctDisequalsWithinBounds(greatestStrictLowerBound, leastNonStrictUpperBound, contextualConstraint, process);
			Expression boundsDifference = apply(MINUS, leastNonStrictUpperBound, greatestStrictLowerBound);
			ArrayList<Expression> boundsDifferenceAndNumberOfDisequals = arrayList(boundsDifference, makeSymbol(numberOfDisequals));
			Expression numberOfDistinctDisequalsIsLessThanNumberOfValuedAllowedByBounds = applyAndSimplify(GREATER_THAN, boundsDifferenceAndNumberOfDisequals, process);
			result = iterator(numberOfDistinctDisequalsIsLessThanNumberOfValuedAllowedByBounds);
			// we need to know this before providing a final solution
		}
	
		return result;
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
			
			MaximumExpressionStepSolver maximumStrictLowerBoundStepSolver
			= new MaximumExpressionStepSolver(
					getStrictLowerBoundsIncludingImplicitOnes(process),
					LESS_THAN_SYMBOL, // use total order <
					getTypeStrictLowerBound(process), // type minimum
					getTypeStrictUpperBound(process)); // type maximum
			SolutionStep maximumStrictLowerBoundStep = maximumStrictLowerBoundStepSolver.step(contextualConstraint, process);
			if (maximumStrictLowerBoundStep.itDepends()) {
				throw new Error("Should not be here at this point");	
//				ItDependsOn result = new ItDependsOn(maximumStrictLowerBoundStep.getExpression(), maximumStrictLowerBoundStep.getConstraintSplitting(), clone(), clone());
//				return result;
			}
			Expression greatestStrictLowerBound = maximumStrictLowerBoundStep.getExpression();
					
			MaximumExpressionStepSolver minimumNonStrictUpperBoundStepSolver
			= new MaximumExpressionStepSolver(
					getNonStrictUpperBoundsIncludingImplicitOnes(process),
					GREATER_THAN_SYMBOL, // use total order > since "minimum" is maximum under it
					getTypeNonStrictUpperBound(process), // type "minimum" in inverted direction is the upper bound
					getTypeNonStrictLowerBound(process)); // type "maximum" in inverted direction is the lower bound
			SolutionStep minimumNonStrictUpperBoundStep = minimumNonStrictUpperBoundStepSolver.step(contextualConstraint, process);
			if (minimumNonStrictUpperBoundStep.itDepends()) {
				throw new Error("Should not be here at this point");	
//				ItDependsOn result = new ItDependsOn(minimumNonStrictUpperBoundStep.getExpression(), minimumNonStrictUpperBoundStep.getConstraintSplitting(), clone(), clone());
//				return result;
			}
			
//			if ( ! leastNonStrictUpperBound.equals(minimumNonStrictUpperBoundStep.getExpression())) {
//				System.out.println("Constraint: " + getConstraint());
//				System.out.println("Contextual constraint: " + contextualConstraint);
//				System.out.println("type: " + getType(process));
//				System.out.println("Non-strict upper bounds including implicit ones: " + getNonStrictUpperBoundsIncludingImplicitOnes(process));
//				System.out.println("Least non-strict upper bound per old method: " + leastNonStrictUpperBound);
//				System.out.println("Least non-strict upper bound per new method: " + minimumNonStrictUpperBoundStep.getExpression());
//				System.exit(-1);
//			}
			
			Expression leastNonStrictUpperBound = minimumNonStrictUpperBoundStep.getExpression();
					
			if (greatestStrictLowerBound.equals(MINUS_INFINITY) || leastNonStrictUpperBound.equals(INFINITY)) {
				solutionExpression = INFINITY;
			}
			else {
				int numberOfDisequals = computeNumberOfDistinctDisequalsWithinBounds(greatestStrictLowerBound, leastNonStrictUpperBound, contextualConstraint, process);
				Expression boundsDifference = apply(MINUS, leastNonStrictUpperBound, greatestStrictLowerBound);
				ArrayList<Expression> boundsDifferenceAndNumberOfDisequals = arrayList(boundsDifference, makeSymbol(numberOfDisequals));
				Expression numberOfDistinctDisequalsIsLessThanNumberOfValuedAllowedByBounds = applyAndSimplify(GREATER_THAN, boundsDifferenceAndNumberOfDisequals, process);
				if ( ! contextualConstraint.implies(numberOfDistinctDisequalsIsLessThanNumberOfValuedAllowedByBounds, process)) {
					solutionExpression = ZERO; // there are no available values left
				}
				else if ( ! getEquals().isEmpty()) { // if bound to a value
					solutionExpression = ONE;
				}
				else {
					solutionExpression = applyAndSimplify(MINUS, boundsDifferenceAndNumberOfDisequals, process);
				}
			}
		}

		return new Solution(solutionExpression);
	}

	private Expression computeLeastNonStrictUpperBound(Constraint2 contextualConstraint, RewritingProcess process) {
		Expression leastNonStrictUpperBound = getTypeNonStrictUpperBound(process);
		for (Expression nonStrictUpperBound : getNonStrictUpperBoundsIncludingImplicitOnes(process)) {
			Expression thisIsLessThanCurrentLeast = applyAndSimplify(LESS_THAN, arrayList(nonStrictUpperBound, leastNonStrictUpperBound), process);
			if (contextualConstraint.implies(thisIsLessThanCurrentLeast, process)) { // by now, this is either implied or not, because previous defining literals decide it
				leastNonStrictUpperBound = nonStrictUpperBound;
			}
		}
		return leastNonStrictUpperBound;
	}

	private Expression computeGreatestStrictLowerBound(Constraint2 contextualConstraint, RewritingProcess process) {
		Expression greatestStrictLowerBound = getTypeStrictLowerBound(process);
		for (Expression strictLowerBound : getStrictLowerBoundsIncludingImplicitOnes(process)) {
			Expression thisIsGreaterThanPreviousGreatest = applyAndSimplify(GREATER_THAN, arrayList(strictLowerBound, greatestStrictLowerBound), process);
			if (contextualConstraint.implies(thisIsGreaterThanPreviousGreatest, process)) { // by now, this is either implied or not, because previous defining literals decide it
				greatestStrictLowerBound = strictLowerBound;
			}
		}
		return greatestStrictLowerBound;
	}

	private int computeNumberOfDistinctDisequalsWithinBounds(
			Expression greatestStrictLowerBound, Expression leastNonStrictUpperBound, Constraint2 contextualConstraint, RewritingProcess process) {
		
		Function<Expression, Boolean> implied = a -> {  // abbreviation of "implied by contextual constraint"
			Expression simplifiedAtom = getConstraint().getConstraintTheory().simplify(a, process);
			boolean result = contextualConstraint.implies(simplifiedAtom, process);
			return result;
		};

		Set<Expression> distinctDisequalsWithinBounds = set();
		for (Expression disequal : getDisequals()) {
			boolean greaterThanGreatestStrictLowerBound = implied.apply(apply(GREATER_THAN, disequal, greatestStrictLowerBound));
			// Note that the two instances of 'apply' above are very different and only coincidentally named the same; one is the application of Function<Expression, Boolean> whereas the other is the construction of a function application Expression
			if (greaterThanGreatestStrictLowerBound) {
				boolean lessThanOrEqualToLeastNonStrictUpperBound = implied.apply(apply(LESS_THAN_OR_EQUAL_TO, disequal, leastNonStrictUpperBound));
				if (lessThanOrEqualToLeastNonStrictUpperBound) {
					boolean distinctFromAllPreviousDistincts =
							forAll(distinctDisequalsWithinBounds, d -> implied.apply(apply(DISEQUALITY, d, disequal)));
					if (distinctFromAllPreviousDistincts) {
						distinctDisequalsWithinBounds.add(disequal);
					}
				}
			}
		}
		
		int result = distinctDisequalsWithinBounds.size();
		return result;
	}

	private IntegerInterval getType(RewritingProcess process) {
		if (type == null) {
			type = (IntegerInterval) process.getType(getConstraint().getVariableTypeExpression(process));
		}
		return type;
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

	private Expression typeStrictUpperBound;
	
	private Expression getTypeStrictUpperBound(RewritingProcess process) {
		if (typeStrictUpperBound == null) {
			IntegerInterval type = getType(process);
			Expression nonStrictUpperBound = type.getNonStrictUpperBound();
			if (Expressions.isNumber(nonStrictUpperBound)) {
				typeStrictUpperBound = makeSymbol(nonStrictUpperBound.intValue() + 1);
			}
			else { // has to be infinity
				typeStrictUpperBound = INFINITY;
			}
		}
		return typeStrictUpperBound;
	}

	private Expression getTypeNonStrictLowerBound(RewritingProcess process) {
		IntegerInterval type = getType(process);
		Expression result = type.getNonStrictLowerBound();
		return result;
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