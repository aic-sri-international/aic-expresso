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
package com.sri.ai.grinder.sgdpllt.theory.numeric;

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.arrayListFrom;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.base.Pair.pair;
import static com.sri.ai.util.base.PairOf.makePairOf;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;
import static com.sri.ai.util.collect.PredicateIterator.predicateIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.StepSolver;
import com.sri.ai.grinder.sgdpllt.core.TrueContext;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractSingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.core.solver.AbstractExpressionWithPropagatedLiteralsStepSolver;
import com.sri.ai.grinder.sgdpllt.helper.MaximumExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.theory.base.ConstantExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.base.ConstantStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.base.LiteralStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.differencearithmetic.AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.base.PairOf;
import com.sri.ai.util.collect.CartesianProductIterator;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.collect.PairOfElementsInListIterator;

/**
 * A {@link AbstractExpressionWithPropagatedLiteralsStepSolver}
 * for a {@link AbstractSingleVariableNumericConstraint}
 * with basic methods for determining the feasibility region of the variable,
 * but leaving the determination of the final solution to {@link #solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Context)}.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver extends AbstractExpressionWithPropagatedLiteralsStepSolver {

	/**
	 * SUBTLETY NOTES:
	 * 
	 * A subtle bug that came up points to the need for care using the context in these step solvers.
	 * 
	 * A step solver is supposed to be reusable for multiple contexts.
	 * At the same time, it is very advantageous to cache intermediary information such as propagated CNF and lower and upper bounds,
	 * which remain the same across multiple contexts and are expensive to compute.
	 * 
	 * Cached information, however, must be independent of the context; otherwise, its reuse under a different context will cause trouble.
	 * This is a little tricky because sometimes we must simplify the cached expressions, and simplification usually depends on the context.
	 * For example, if the index of a summation is I and the constraint is I = J and I < J + 1,
	 * one of the propagated literals is J < J + 1, which can be simplified, regardless of the context, to true.
	 * To uncover that, however, we must use simplification, which depends on the context.
	 * In the above example, the result is always true regardless of the context, so simplifying it with the context does not cause any problems.
	 * However, if the constraint were I = J and I < 4, then the propagated literal would be J < 4, which
	 * *will* be simplified to different expressions depending on the context (if the context says that J = 0, for example, it will be true).
	 * So we must simplify using the method applyAndSimplifyWithoutConsideringContextualConstraint, which simplifies according to variable types
	 * and the operators in the current theory, but does not use the context constraint.
	 * 
	 * One might then be tempted to conclude that we should always simplify in this manner, but there is a last subtle point,
	 * which is that step solvers are not supposed to return conditional steps that are implied to be true or false according to the context
	 * under which the step is computed. Therefore, we must *at some point* use simplification that takes the contextual constraint into account.
	 * The crucial point is that this should never be done to compute information that will be *cached* for use under multiple contexts.
	 */

	/**
	 * This method is invoked after the bounds are checked for the constraint's feasibility.
	 * It must then do whatever work needs to be done to reach the solution to the problem being
	 * solved (the specific problem, and they way to solve it, are left to the extensions to define and know about).
	 *
	 * <p>
	 * Bounds may be strict (<code><, ></code>) or non-strict (<code><=, >=</code>).
	 * Whether a bound is strict can be checked with {@link #getMapFromLowerBoundsToStrictness(Context)}
	 * and {@link #getMapFromUpperBoundsToStrictness(Context)}.
	 * <p>
	 * This method also receives a "sequel base", which 
	 * keeps cached information computed so far by the step solver.
	 * It can (and should, for efficiency and re-use of already computed information)
	 * be used as a base for sequel step solvers if this method is to return a ItDepends step.
	 * Typically, this base will be cloned twice to construct two sequel step solvers,
	 * one for the case in which the splitter literal is true, and another for when its false,
	 * and some sub-step solver field inside each of them will be set according to which side of the split the sequel will be.
	 * Please refer to the code in {@link AbstractSingleVariableDifferenceArithmeticConstraintFeasibilityRegionStepSolver#getSolutionStepAfterBoundsAreCheckedForFeasibility(
			Expression maximumLowerBound,
			Expression minimumUpperBound,
			AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver sequelBaseNumeric,
			Context context)} for a concrete example.
	 * {@link #solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Context)} also provides a concrete example of using
	 * {@link #makeSequelStepSolver(AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver)}.	
	 * 
	 * @param maximumLowerBound
	 * @param minimumUpperBound
	 * @param sequelBase
	 * @param context
	 * @return
	 */
	abstract protected Step getSolutionStepAfterBoundsAreCheckedForFeasibility(
			Expression maximumLowerBound,
			Expression minimumUpperBound,
			AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver sequelBase,
			Context context);	
	
	/**
	 * Must provide a literal deciding whether the given bounds admit any values for the variable.
	 * <p>
	 * Bounds may be strict (<code><, ></code>) or non-strict (<code><=, >=</code>).
	 * Whether a bound is strict can be checked with {@link #getMapFromLowerBoundsToStrictness(Context)}
	 * and {@link #getMapFromUpperBoundsToStrictness(Context)}.
	 * @param lowerBound
	 * @param upperBound
	 * @param context
	 * @return
	 */
	abstract protected Expression makeLiteralCheckingWhetherThereAreAnyValuesWithinBounds(Expression lowerBound, Expression upperBound, Context context);

	/**
	 * Implementations of this method must indicate the lower bound of the constraint's variable implicit in its
	 * type, along with whether it is a strict bound or not.
	 * @param context
	 * @return
	 */
	abstract protected Pair<Expression, Boolean> getTypeLowerBoundAndStrictness(Context context);

	/**
	 * Implementations of this method must indicate the upper bound of the constraint's variable implicit in its
	 * type, along with whether it is a strict bound or not.
	 * @param context
	 * @return
	 */
	abstract protected Pair<Expression, Boolean> getTypeUpperBoundAndStrictness(Context context);

	/**
	 * Indicates whether the fact that the variable is unbounded
	 * (that is, has either lower or upper bound equal to
	 * {@link Expressions#MINUS_INFINITY} or {@link Expressions#INFINITY})
	 * means that we can immediate return the solution provided
	 * by {@link #getSolutionExpressionForUnboundedVariables()}.
	 * @return
	 */
	abstract public boolean unboundedVariableProducesShortCircuitSolution();

	/**
	 * Provides the solution when variable is unbounded
	 * (that is, has either lower or upper bound equal to
	 * {@link Expressions#MINUS_INFINITY} or {@link Expressions#INFINITY})
	 * and {@link #unboundedVariableProducesShortCircuitSolution()}
	 * returns <code>true</code>.
	 * @return
	 */
	abstract public Expression getSolutionExpressionForUnboundedVariables();

	/**
	 * Provides the solution when the variable is bound to a value
	 * and it has already been decided that the constraint is satisfiable.
	 * <p>
	 * Here are two illustrations of this method's use.
	 * If the implementation is deciding model counting, for example,
	 * this will be the expression <code>1</code>,
	 * whereas if the implementation is computing the set of satisfying values,
	 * this will be one of the elements provided by the method {@link #getEquals()}.
	 * @return
	 */
	abstract public Expression getSolutionExpressionForBoundVariable();

	/**
	 * This method is given each detected (lower bound, strictness) pair
	 * coming from an explicit literal (as opposed to an implicit bound from the variable's type)
	 * and has the chance to provide an equivalent (lower bound, strictness) pair.
	 * The default is just to return the same bound and strictness.
	 * This can be useful for implementations manipulating integers and enforcing all lower bounds to be,
	 * say, strict, by subtracting 1 from non-strict ones.
	 * @param lowerBound
	 * @param strictness
	 * @param context
	 * @return
	 */
	protected Pair<Expression, Boolean> processExplicitLowerBoundAndStrictnessPair(Expression lowerBound, boolean strictness, Context context) {
		return pair(lowerBound, strictness);
	}
	
	/**
	 * This method is given each detected (upper bound, strictness) pair
	 * coming from an explicit literal (as opposed to an implicit bound from the variable's type)
	 * and has the chance to provide an equivalent (upper bound, strictness) pair.
	 * The default is just to return the same bound and strictness.
	 * This can be useful for implementations manipulating integers and enforcing all upper bounds to be,
	 * say, non-strict, by subtracting 1 from strict ones.
	 * @param strictness
	 * @param context
	 * @param lowerBound
	 * @return
	 */
	protected Pair<Expression, Boolean> processExplicitUpperBoundAndStrictnessPair(Expression upperBound, boolean strictness, Context context) {
		return pair(upperBound, strictness);
	}

	protected static final Symbol GREATER_THAN_SYMBOL = makeSymbol(GREATER_THAN);

	protected static final Symbol LESS_THAN_SYMBOL = makeSymbol(LESS_THAN);

	private ArrayList<Expression> equals;

	private ArrayList<Expression> disequals;

	private ArrayList<Expression> nonEqualityComparisons;

	protected ArrayList<Expression> lowerBoundsIncludingImplicitOnes;
	protected Map<Expression, Boolean> fromLowerBoundsIncludingImplicitOnesToStrictness;

	protected ArrayList<Expression>    upperBoundsIncludingImplicitOnes;
	protected Map<Expression, Boolean> fromUpperBoundsIncludingImplicitOnesToStrictness;

	protected ArrayList<PairOf<Expression>> pairsOfEquals;
	
	/**
	 * The initial step solver to use to decide which lower bound is the maximum one.
	 * It may have been set to the sequel of a step solver of the same problem,
	 * during the execution of a prequel step solver.
	 */
	private ExpressionLiteralSplitterStepSolver initialMaximumLowerBoundStepSolver;

	/**
	 * The initial step solver to use to decide which upper bound is the minimum one.
	 * It may have been set to the sequel of a step solver of the same problem,
	 * during the execution of a prequel step solver.
	 */
	private ExpressionLiteralSplitterStepSolver initialMinimumUpperBoundStepSolver;

	/**
	 * The initial step solver to use to decide whether the lower bound is less than the upper bound.
	 * It may have been set to the sequel of a step solver of the same problem,
	 * during the execution of a prequel step solver.
	 */
	private StepSolver<Boolean> initialBoundedSpaceIsNotEmptyStepSolver;


	public 
	AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver(
			AbstractSingleVariableNumericConstraint constraint) {
		
		super(constraint);
	}
	
	@Override
	public AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver clone() {
		return (AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver) super.clone();
	}

	@Override
	public AbstractSingleVariableNumericConstraint getConstraint() {
		return (AbstractSingleVariableNumericConstraint) super.getConstraint();
	}
	
	@Override
	protected boolean usingDefaultImplementationOfMakePropagatedCNF() {
		return true;
	}

	@Override
	protected Iterable<Expression> getPropagatedLiterals(Context context) {
		
		// System.out.println("getPropagatedLiterals:");
		// System.out.println("constraint: " + constraint);
		// System.out.println("lower bounds: " + join(getLowerBounds(context)));
		// System.out.println("upper bounds: " + join(getUpperBounds(context)));
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
		// instead and put on external literals. TODO: this is done, remove previous TODO when convenient (debugging right now, don't want to change line positions)

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
										Expression result = 
												applyAndSimplifyWithoutConsideringContextualConstraint(
														nonEqualityComparison.getFunctor().toString(), 
														arrayList(equal, termBeingCompared), 
														context);
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

	protected ArrayList<Expression> getLowerBoundsIncludingImplicitOnes(Context context) {
		if (lowerBoundsIncludingImplicitOnes == null) {
			makeLowerBoundsAndStrictness(context);
		}
		return lowerBoundsIncludingImplicitOnes;
	}

	protected Map<Expression, Boolean> getMapFromLowerBoundsToStrictness(Context context) {
		if (fromLowerBoundsIncludingImplicitOnesToStrictness == null) {
			makeLowerBoundsAndStrictness(context);
		}
		return fromLowerBoundsIncludingImplicitOnesToStrictness;
	}
	
	/** 
	 * A method setting {@link #lowerBoundsIncludingImplicitOnes} and {@link #fromLowerBoundsIncludingImplicitOnesToStrictness}
	 * from constraint and variable's type.
	 * @param context
	 */
	protected void makeLowerBoundsAndStrictness(Context context) {
		AbstractSingleVariableConstraint abstractSingleVariableConstraint = (AbstractSingleVariableConstraint) constraint;
		
		FunctionIterator<Expression, Pair<Expression, Boolean>> lowerBoundsAndStrictnessFromPositiveNormalizedAtomsIterator
		= functionIterator(
				predicateIterator(
						abstractSingleVariableConstraint.getPositiveNormalizedAtoms(),
						e -> e.hasFunctor(GREATER_THAN) // X > Y, so Y is a strict lower bound
				), 
				e -> processExplicitLowerBoundAndStrictnessPair(e.get(1), true, context)); // bound is strict
		
		FunctionIterator<Expression, Pair<Expression, Boolean>> lowerBoundsAndStrictnessFromNegativeNormalizedAtomsIterator
		= functionIterator(
				predicateIterator(
						abstractSingleVariableConstraint.getNegativeNormalizedAtoms(),
						e -> e.hasFunctor(LESS_THAN)
				), 
				e -> processExplicitLowerBoundAndStrictnessPair(e.get(1), false, context)); // not (X < Y) <=> X >= Y, so bound is non-strict
		
		Pair<Expression, Boolean> typeLowerBoundAndStrictness = getTypeLowerBoundAndStrictness(context);
		
		Iterator<Pair<Expression, Boolean>> lowerBoundsAndStrictnessIterator = new NestedIterator<>(
				lowerBoundsAndStrictnessFromPositiveNormalizedAtomsIterator,
				lowerBoundsAndStrictnessFromNegativeNormalizedAtomsIterator,
				typeLowerBoundAndStrictness);
		
		lowerBoundsIncludingImplicitOnes = arrayList();
		fromLowerBoundsIncludingImplicitOnesToStrictness = map();
		for (Pair<Expression, Boolean> boundAndStrictness : in(lowerBoundsAndStrictnessIterator)) {
			Expression bound = boundAndStrictness.first;
			lowerBoundsIncludingImplicitOnes.add(bound);
			Boolean strictness = boundAndStrictness.second;
			Boolean previousStrictness = fromLowerBoundsIncludingImplicitOnesToStrictness.get(bound);
			if (previousStrictness == null || (!previousStrictness && strictness) ) {
				// if no strictness information so far, store current one; otherwise, only need to change it if previous occurrences were non-strict and this one is strict
				fromLowerBoundsIncludingImplicitOnesToStrictness.put(bound, strictness);
			}
		}
	}

	protected ArrayList<Expression> getUpperBoundsIncludingImplicitOnes(Context context) {
		if (upperBoundsIncludingImplicitOnes == null) {
			makeUpperBoundsAndStrictness(context);
		}
		return upperBoundsIncludingImplicitOnes;
	}

	protected Map<Expression, Boolean> getMapFromUpperBoundsToStrictness(Context context) {
		if (fromUpperBoundsIncludingImplicitOnesToStrictness == null) {
			makeLowerBoundsAndStrictness(context);
		}
		return fromUpperBoundsIncludingImplicitOnesToStrictness;
	}
	
	/** 
	 * A method setting {@link #upperBoundsIncludingImplicitOnes} and {@link #fromUpperBoundsIncludingImplicitOnesToStrictness}
	 * from constraint and variable's type.
	 * @param context
	 */
	protected void makeUpperBoundsAndStrictness(Context context) {
		AbstractSingleVariableConstraint abstractSingleVariableConstraint = (AbstractSingleVariableConstraint) constraint;
		
		FunctionIterator<Expression, Pair<Expression, Boolean>> upperBoundsFromPositiveNormalizedAtomsIterator
		= functionIterator(
				predicateIterator(
						abstractSingleVariableConstraint.getPositiveNormalizedAtoms(),
						e -> e.hasFunctor(LESS_THAN)
				),
				e -> processExplicitUpperBoundAndStrictnessPair(e.get(1), true, context)); // strict
		
		FunctionIterator<Expression, Pair<Expression, Boolean>> upperBoundsFromNegativeNormalizedAtomsIterator
		= functionIterator(
				predicateIterator(
						abstractSingleVariableConstraint.getNegativeNormalizedAtoms(),
						e -> e.hasFunctor(GREATER_THAN) // not (X > Y) <=> X <= Y, so Y is a non-strict upper bound
				),
				e -> processExplicitUpperBoundAndStrictnessPair(e.get(1), false, context)); // non-strict
		
		Pair<Expression, Boolean> typeUpperBound = getTypeUpperBoundAndStrictness(context);
		
		Iterator<Pair<Expression, Boolean>> upperBoundsAndStrictnessIterator = new NestedIterator<>(
				upperBoundsFromPositiveNormalizedAtomsIterator,
				upperBoundsFromNegativeNormalizedAtomsIterator,
				typeUpperBound);
		
		upperBoundsIncludingImplicitOnes = arrayList();
		fromUpperBoundsIncludingImplicitOnesToStrictness = map();
		for (Pair<Expression, Boolean> boundAndStrictness : in(upperBoundsAndStrictnessIterator)) {
			Expression bound = boundAndStrictness.first;
			upperBoundsIncludingImplicitOnes.add(bound);
			Boolean strictness = boundAndStrictness.second;
			Boolean previousStrictness = fromUpperBoundsIncludingImplicitOnesToStrictness.get(bound);
			if (previousStrictness == null || (!previousStrictness && strictness) ) {
				// if no strictness information so far, store current one; otherwise, only need to change it if previous occurrences were non-strict and this one is strict
				fromUpperBoundsIncludingImplicitOnesToStrictness.put(bound, strictness);
			}
		}
	}

	protected ArrayList<Expression> getEquals() {
		if (equals == null) {
			AbstractSingleVariableConstraint abstractSingleVariableConstraint
			= (AbstractSingleVariableConstraint) constraint;
			
			Iterator<Expression> equalsIterator =
					functionIterator(
							predicateIterator(
									abstractSingleVariableConstraint.getPositiveNormalizedAtoms(),
									e -> e.hasFunctor(EQUALITY)
									), 
									e -> e.get(1));
			
			equals = arrayListFrom(equalsIterator);
		}
		return equals;
	}

	protected ArrayList<Expression> getNonEqualityComparisons(Context context) {
		if (nonEqualityComparisons == null) {
			AbstractSingleVariableConstraint abstractSingleVariableConstraint = (AbstractSingleVariableConstraint) constraint;

			Iterator<Expression> fromPositiveNormalizedAtoms =
					predicateIterator(
							abstractSingleVariableConstraint.getPositiveNormalizedAtoms(),
							e -> ! e.hasFunctor(FunctorConstants.EQUALITY)
							);

			Iterator<Expression> fromNegativeNormalizedAtoms =
					functionIterator(
							abstractSingleVariableConstraint.getNegativeNormalizedAtoms(), // negative normalized atom is never an equality
							e -> abstractSingleVariableConstraint.getTheory().getLiteralNegation(e, context)
							);

			Pair<Expression, Boolean> typeLowerBoundAndStrictness = getTypeLowerBoundAndStrictness(context);
			Expression typeLowerBound = typeLowerBoundAndStrictness.first;
			boolean typeLowerBoundIsStrict = typeLowerBoundAndStrictness.second;
			String greaterThanOperator = typeLowerBoundIsStrict? GREATER_THAN : GREATER_THAN_OR_EQUAL_TO;
			Expression variableIsGreaterThanTypeLowerBound =
					apply(greaterThanOperator, getConstraint().getVariable(), typeLowerBound);

			Pair<Expression, Boolean> typeUpperBoundAndStrictness = getTypeUpperBoundAndStrictness(context);
			Expression typeUpperBound = typeUpperBoundAndStrictness.first;
			boolean typeUpperBoundIsStrict = typeUpperBoundAndStrictness.second;
			String lessThanOperator = typeUpperBoundIsStrict? LESS_THAN : LESS_THAN_OR_EQUAL_TO;
			Expression variableIsLessThanOrEqualToTypeUpperBound =
					apply(lessThanOperator, getConstraint().getVariable(), typeUpperBound);

			Iterator<Expression> all =
					new NestedIterator<Expression>(
							fromPositiveNormalizedAtoms,
							fromNegativeNormalizedAtoms,
							variableIsGreaterThanTypeLowerBound,
							variableIsLessThanOrEqualToTypeUpperBound);
			
			nonEqualityComparisons = arrayListFrom(all);
		}
		
		return nonEqualityComparisons;
	}

	protected ArrayList<Expression> getDisequals() {
		if (disequals == null) {
			AbstractSingleVariableConstraint abstractSingleVariableConstraint = (AbstractSingleVariableConstraint) constraint;
			Iterator<Expression> disequalsIterator =
					functionIterator(
							predicateIterator(
									abstractSingleVariableConstraint.getNegativeNormalizedAtoms(),
									e -> e.hasFunctor(FunctorConstants.EQUALITY) // negative equality is disequality
									), 
									e -> e.get(1));
			disequals = arrayListFrom(disequalsIterator);
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

	/**
	 * Method used to create a sequel step solver based on a given step solver used to keep track of updates sub-step solvers.
	 * This consists of cloning the given step solver plus copying cached fields from "this", if any
	 * (they will always be useful to the sequel step solver since cached fields will always refer to the encoded problem,
	 * and the problem must remain fixed from step solver to its sequels).
	 * @param sequelBase
	 * @return
	 */
	protected
	AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver 
	makeSequelStepSolver(
			AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver 
			sequelBase) {
	
		// copy the sub-step solvers for the sequel step solver set by the "user"
		AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver result = sequelBase.clone();
	
		// if cached fields are already computed, re-use them:
	
		if (equals != null) {
			result.equals = equals;
		}
	
		if (disequals != null) {
			result.disequals = disequals;
		}
	
		if (nonEqualityComparisons != null) {
			result.nonEqualityComparisons = nonEqualityComparisons;
		}
	
		if (lowerBoundsIncludingImplicitOnes != null) {
			result.lowerBoundsIncludingImplicitOnes = lowerBoundsIncludingImplicitOnes;
		}
	
		if (fromLowerBoundsIncludingImplicitOnesToStrictness != null) {
			result.fromLowerBoundsIncludingImplicitOnesToStrictness = fromLowerBoundsIncludingImplicitOnesToStrictness;
		}
	
		if (upperBoundsIncludingImplicitOnes != null) {
			result.upperBoundsIncludingImplicitOnes = upperBoundsIncludingImplicitOnes;
		}
	
		if (fromUpperBoundsIncludingImplicitOnesToStrictness != null) {
			result.fromUpperBoundsIncludingImplicitOnesToStrictness = fromUpperBoundsIncludingImplicitOnesToStrictness;
		}
	
		if (pairsOfEquals != null) {
			result.pairsOfEquals = pairsOfEquals;
		}
	
		return result;
	}

	@Override
	protected Iterable<Iterable<Expression>> getPropagatedCNFBesidesPropagatedLiterals(Context context) {
		return list();
	}
	
	@Override
	protected Step solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied(Context context) {

		Expression solutionExpression;
		
		// sequelBase keeps track of updates to non-splitting sub-step solvers so far.
		// When a splitting sub-step solver is found, it is used as a basis
		// for the sequel step solvers.
		// The reason we keep this clone, that is itself cloned later,
		// as opposed to updating and cloning "this" every time,
		// is that step solvers must not be modified by their method "step",
		// unless they are caching context-independent information.
		// sequelBase serves as a blackboard for all the updates learned while executing this method,
		// which then don't need to be kept by "this".
		// These updates are then cloned into the sequel step solvers.

		AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver sequelBase = clone();

		if (getConstraint().getPropagateAllLiteralsWhenVariableIsBound() && ! getEquals().isEmpty()) {
			solutionExpression = getSolutionExpressionForBoundVariable();
		}
		else {
			ExpressionLiteralSplitterStepSolver maximumLowerBoundStepSolver;
			if (initialMaximumLowerBoundStepSolver == null) {
				maximumLowerBoundStepSolver
				= new MaximumExpressionStepSolver(
						getLowerBoundsIncludingImplicitOnes(context),
						LESS_THAN_SYMBOL, // use total order <
						MINUS_INFINITY,
						INFINITY); // at first, I placed the type minimum and maximum strict lower bounds here. This is incorrect because if the type maximum is, say, 4, and I have "X > 3 and X > I" (3 is the maximum strict lower bounds for values in the type), the step solver short-circuits and returns 3, without ever even looking at I. Looking at I is needed because if I is greater than 3 than this constraint is unsatisfiable.
			}
			else {
				maximumLowerBoundStepSolver = initialMaximumLowerBoundStepSolver;
			}
			ExpressionLiteralSplitterStepSolver.Step maximumLowerBoundStep = maximumLowerBoundStepSolver.step(context);
			if (maximumLowerBoundStep.itDepends()) {
				AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver ifTrue  = makeSequelStepSolver(sequelBase);
				ifTrue.initialMaximumLowerBoundStepSolver = maximumLowerBoundStep.getStepSolverForWhenSplitterIsTrue();
				AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver ifFalse = makeSequelStepSolver(sequelBase);
				ifFalse.initialMaximumLowerBoundStepSolver = maximumLowerBoundStep.getStepSolverForWhenSplitterIsFalse();
				ItDependsOn result = new ItDependsOn(maximumLowerBoundStep.getSplitterLiteral(), maximumLowerBoundStep.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
				return result;
			}
			Expression maximumLowerBound = maximumLowerBoundStep.getValue();
			
			sequelBase.initialMaximumLowerBoundStepSolver = new ConstantExpressionStepSolver(maximumLowerBound);
			
			ExpressionLiteralSplitterStepSolver minimumUpperBoundStepSolver;
			if (initialMinimumUpperBoundStepSolver == null) {
				minimumUpperBoundStepSolver
				= new MaximumExpressionStepSolver(
						getUpperBoundsIncludingImplicitOnes(context),
						GREATER_THAN_SYMBOL, // use total order > since "minimum" is maximum under it
						INFINITY, // "minimum" is maximum value because we are operating on the inverse order
						MINUS_INFINITY); // "maximum" is minimum value because we are operating on the inverse order
			}
			else {
				minimumUpperBoundStepSolver = initialMinimumUpperBoundStepSolver;
			}
			ExpressionLiteralSplitterStepSolver.Step minimumUpperBoundStep = minimumUpperBoundStepSolver.step(context);
			if (minimumUpperBoundStep.itDepends()) {
				AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver ifTrue  = makeSequelStepSolver(sequelBase);
				ifTrue.initialMinimumUpperBoundStepSolver = minimumUpperBoundStep.getStepSolverForWhenSplitterIsTrue();
				AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver ifFalse = makeSequelStepSolver(sequelBase);
				ifFalse.initialMinimumUpperBoundStepSolver = minimumUpperBoundStep.getStepSolverForWhenSplitterIsFalse();
				ItDependsOn result = new ItDependsOn(minimumUpperBoundStep.getSplitterLiteral(), minimumUpperBoundStep.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
				return result;
			}
			Expression minimumUpperBound = minimumUpperBoundStep.getValue();
			sequelBase.initialMinimumUpperBoundStepSolver = new ConstantExpressionStepSolver(minimumUpperBound);
			
			if (unboundedVariableProducesShortCircuitSolution() && 
					(maximumLowerBound.equals(MINUS_INFINITY) || minimumUpperBound.equals(INFINITY))) {
				solutionExpression = getSolutionExpressionForUnboundedVariables();
			}
			else {
				StepSolver<Boolean> boundedSpaceIsNotEmptyStepSolver;
				if (initialBoundedSpaceIsNotEmptyStepSolver == null) {
					Expression boundedSpaceIsNotEmpty = makeLiteralCheckingWhetherThereAreAnyValuesWithinBounds(maximumLowerBound, minimumUpperBound, context);
					boundedSpaceIsNotEmptyStepSolver = new LiteralStepSolver(boundedSpaceIsNotEmpty);
				}
				else {
					boundedSpaceIsNotEmptyStepSolver = initialBoundedSpaceIsNotEmptyStepSolver;
				}
				StepSolver.Step<Boolean> lowerBoundIsLessThanUpperBoundStep = boundedSpaceIsNotEmptyStepSolver.step(context);
				if (lowerBoundIsLessThanUpperBoundStep.itDepends()) {
					AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver ifTrue  = makeSequelStepSolver(sequelBase);
					ifTrue.initialBoundedSpaceIsNotEmptyStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenSplitterIsTrue();
					AbstractSingleVariableNumericConstraintFeasibilityRegionStepSolver ifFalse = makeSequelStepSolver(sequelBase);
					ifFalse.initialBoundedSpaceIsNotEmptyStepSolver = lowerBoundIsLessThanUpperBoundStep.getStepSolverForWhenSplitterIsFalse();
					ItDependsOn result = new ItDependsOn(lowerBoundIsLessThanUpperBoundStep.getSplitter(), lowerBoundIsLessThanUpperBoundStep.getContextSplittingWhenSplitterIsLiteral(), ifTrue, ifFalse);
					return result;
				}
				if ( ! lowerBoundIsLessThanUpperBoundStep.getValue()) {
					return new Solution(getSolutionExpressionGivenContradiction());
				}
				// else, bounds difference is positive and we can move on
				sequelBase.initialBoundedSpaceIsNotEmptyStepSolver = new ConstantStepSolver<Boolean>(true);

				Step result = getSolutionStepAfterBoundsAreCheckedForFeasibility(maximumLowerBound, minimumUpperBound, sequelBase, context);
				return result;
			}
		}

		return new Solution(solutionExpression);
	}

	protected Expression applyAndSimplify(String comparison, ArrayList<Expression> arguments, Context context) {
		Expression unsimplifiedAtom = apply(comparison, arguments);
		Expression result = constraint.getTheory().simplify(unsimplifiedAtom, context);
		return result;
	}

	/**
	 * A method for simplifying an expression according to the context's theory, types and global objects,
	 * but <i>without</i> considering the contextual constraint,
	 * for the purpose of computing information about the step solver that is constant across contexts
	 * (that share the same basic information).
	 * Note that the context is still an argument, for the sake of providing the basic information,
	 * but the contextual constraint is ignored.
	 * @param comparison
	 * @param arguments
	 * @param context
	 * @return
	 */
	protected Expression applyAndSimplifyWithoutConsideringContextualConstraint(String comparison, ArrayList<Expression> arguments, Context context) {
		Expression unsimplifiedAtom = apply(comparison, arguments);
		TrueContext typeContext = new TrueContext(context);
		Expression result = constraint.getTheory().simplify(unsimplifiedAtom, typeContext);
		return result;
	}
}