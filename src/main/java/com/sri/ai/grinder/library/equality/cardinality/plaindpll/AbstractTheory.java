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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.ZERO;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.AbstractEqualityTheory.Constraint.Contradiction;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

@Beta
/** 
 * Basic implementation of some methods of {@link Theory}.
 */
abstract public class AbstractTheory implements Theory {

	/**
	 * Provides a map from functors's getValue() values (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified formula
	 * according to this equalityTheory.
	 * DPLL will use these simplifiers when a new decision is made and literals are replaced by boolean constants. 
	 * @return
	 */
	abstract protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers();

	/**
	 * Provides a map from syntactic form types (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified formula
	 * according to this equalityTheory.
	 * DPLL will use these simplifiers when a new decision is made and literals are replaced by boolean constants. 
	 * @return
	 */
	abstract protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers();

	abstract protected boolean isVariableTerm(Expression term, RewritingProcess process);

	abstract protected BinaryFunction<Expression, RewritingProcess, Expression> getSplitterApplier(boolean splitterSign, Expression splitter);

	/**
	 * If expression can generate a splitter, returns the appropriate splitter's functor;
	 * for example, an equality theory may be defined so that an expression a != b generates the splitter =,
	 * so the result for that input will be =.
	 * If expression cannot generate a splitter, returns <code>null</code>.
	 * This method is only used in this class' implementation of {@link #makeSplitterIfPossible(Expression, Collection, RewritingProcess)}
	 * (so if a class overrides the latter and does not make use of it, its implementation of this method is irrelevant).
	 * @param expression
	 * @return
	 */
	abstract protected String getCorrespondingSplitterFunctorOrNull(Expression expression);
	
	/**
	 * This default implementation does the following (check the javadoc in the declaration of this method in {@link Theory#Constraint}
	 * for the more general, more relaxed requirements):
	 * If expression can originate a splitter and has at least one variable argument, returns the splitter by making it in the following way:
	 * obtain the appropriate splitter functor from {@link #getCorrespondingSplitterFunctorOrNull(Expression)}
	 * and create splitter by getting expression's first variable argument, V, and expression's first argument distinct from V.
	 * Otherwise, returns <code>null</code>.
	 * @param expression
	 * @param indices
	 * @param process
	 * @return
	 */
	@Override
	public Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		assert splittersAlwaysHaveTwoArguments() : "splittersAlwaysHaveTwoArguments indicates false and yet an implementation of makeSplitterIfPossible using that assumption is being invoked.";
		Expression result = null;
		String splitterFunctor = getCorrespondingSplitterFunctorOrNull(expression);
		if (splitterFunctor != null) {
			// remember that equality can have an arbitrary number of terms
			Expression variable  = Util.getFirstSatisfyingPredicateOrNull(expression.getArguments(), 
					e -> isVariableTerm(e, process));
			if (variable != null) {
				Expression otherTerm = Util.getFirstSatisfyingPredicateOrNull(
						expression.getArguments(),
						e -> ! e.equals(variable));
				if (otherTerm != null) {
					result = makeSplitterFromFunctorAndTwoTerms(splitterFunctor, variable, otherTerm, indices, process);
				}
			}
		}
		return result;
	}

	/**
	 * Serves as a safeguard for developers extending {@link AbstractTheory}
	 * by confirming the assumption made by this class' implementation of
	 * {@link #makeSplitterIfPossible(Expression, Collection, RewritingProcess)};
	 * if it returns false and the current implementation is used anyway,
	 * an error message is thrown.
	 * @return
	 */
	abstract boolean splittersAlwaysHaveTwoArguments();
	
	/**
	 * Makes splitter by applying given functor to two terms, indices coming first if any.
	 * Does not simplify splitter (so, if it is simplifiable, it does not get simplified).
	 * While this may not work for all theories (some may have more complex splitters),
	 * it is likely useful in most.
	 * @param splitterFunctor the splitter's functor
	 * @param term1
	 * @param term2
	 * @param indices
	 * @param process
	 * @return
	 */
	protected Expression makeSplitterFromFunctorAndTwoTerms(String splitterFunctor, Expression term1, Expression term2, Collection<Expression> indices, RewritingProcess process) {
		Expression result;
		// Places index or variable before constants.
		if (indices.contains(term1)) {
			result = Expressions.apply(splitterFunctor, term1, term2);
		}
		else if (indices.contains(term2)) {
			result = Expressions.apply(splitterFunctor, term2, term1);
		}
		else if (isVariableTerm(term1, process)) {
			result = Expressions.apply(splitterFunctor, term1, term2);
		}
		else {
			result = Expressions.apply(splitterFunctor, term2, term1);
		}
		return result;
	}

	/**
	 * Simplifies an expression by exhaustively simplifying its top expression with basic boolean operators in equality logic (including quantifier elimination),
	 * then simplifying its sub-expressions,
	 * and again exhaustively simplifying its top expression.
	 * @param expression
	 * @param topSimplifier
	 * @param process
	 * @return
	 */
	@Override
	public Expression simplify(Expression expression, RewritingProcess process) {
		return DPLLUtil.simplify(expression, getFunctionApplicationSimplifiers(), getSyntacticFormTypeSimplifiers(), process);
	}

	@Override
	public Expression pickSplitterInExpression(Expression expression, Constraint constraint, RewritingProcess process) {
		Expression result = null;
		
		Iterator<Expression> subExpressionIterator = new SubExpressionsDepthFirstIterator(expression);
		while (result == null && subExpressionIterator.hasNext()) {
			Expression subExpression = subExpressionIterator.next();
			Expression splitterCandidate = makeSplitterIfPossible(subExpression, constraint.getIndices(), process);
			result = splitterCandidate;
		}
	
		return result;
	}

	@Override
	public Expression applySplitterToExpression(boolean splitterSign, Expression splitter, Expression expression, RewritingProcess process) {
		Expression result = getSplitterApplier(splitterSign, splitter).apply(expression, process);
		return result;
	}

	@Override
	public Expression applyConstraintToSolution(Constraint constraint, Expression solution, RewritingProcess process) {
		Expression result;
		
		if (DPLLUtil.isConditionalSolution(solution, this, process)) {
			Expression solutionSplitter = IfThenElse.getCondition(solution);
			Constraint constraintUnderSolutionSplitter = constraint.applySplitter(true, solutionSplitter, process);
			if (constraintUnderSolutionSplitter != null) {
				Constraint constraintUnderSolutionSplitterNegation = constraint.applySplitter(false, solutionSplitter, process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression newSolutionSplitter = constraint.normalizeSplitterGivenConstraint(solutionSplitter, process);
					Expression thenBranch = IfThenElse.getThenBranch(solution);
					Expression elseBranch = IfThenElse.getElseBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter, thenBranch, process);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					result = IfThenElse.makeIfDistinctFrom(solution, newSolutionSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
				}
				else {
					Expression thenBranch = IfThenElse.getThenBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter, thenBranch, process);
					result = newThenBranch;
				}
			}
			else {
				Constraint constraintUnderSolutionSplitterNegation = constraint.applySplitter(false, solutionSplitter, process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression elseBranch = IfThenElse.getElseBranch(solution);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					result = newElseBranch;
				}
				else {
					throw new Error("Constraint applied to solution should be compatible with the solution splitter or its negation (otherwise either the constraint is unsatisfiable, or the sub-solution is, and in this case we should not have gotten here).");
				}
			}
		}
		else {
			result = constraint.normalize(solution, process);
		}
		
		return result;
	}
	
	/**
	 * Indicates whether variable is chosen after otherTerm in model counting choosing ordering.
	 */
	protected static boolean variableIsChosenAfterOtherTerm(Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process) {
		boolean result = process.isUniquelyNamedConstant(otherTerm) || variableIsChosenAfterOtherVariable(otherTerm, variable, indices);
		return result;
	}

	/**
	 * Indicates whether variable in chosen after otherVariable in choosing ordering.
	 */
	protected static boolean variableIsChosenAfterOtherVariable(Expression variable, Expression otherVariable, Collection<Expression> indices) {
		boolean result;
		if (indices.contains(variable)) { // index
			if ( ! indices.contains(otherVariable)) { // free variable
				result = false; // free variables always precedes indices
			}
			else { // both are indices
				result = otherVariable.toString().compareTo(variable.toString()) < 0; // indices are compared alphabetically
			}
		}
		else if (indices.contains(otherVariable)) { // variable is free variable and otherVariable is index
			result = true; // free variable always precedes indices
		}
		else { // neither is index
			result = otherVariable.toString().compareTo(variable.toString()) < 0;	// alphabetically		
		}
		return result;
	}

	private static final Times timesRewriter = new Times(); // for use in the class below

	public abstract class AbstractConstraint implements Theory.Constraint {

		protected Collection<Expression> indices;

		public AbstractConstraint(Collection<Expression> indices) {
			this.indices = indices;
		}
		
		public abstract AbstractConstraint clone();
		
		@Override
		public Collection<Expression> getIndices() {
			return indices;
		}

		/**
		 * Given an index x, return one splitter needed for us to be able to
		 * compute this index's number of values, or null if none is needed.
		 */
		abstract protected Expression provideSplitterRequiredForComputingNumberOfValuesFor(Expression x, RewritingProcess process);

		@Override
		public Expression pickSplitter(RewritingProcess process) {
			for (Expression x : getIndices()) {
				Expression splitter = provideSplitterRequiredForComputingNumberOfValuesFor(x, process);
				if (splitter != null) {
					return splitter;
				}
			}
			return null;
		}

		/**
		 * Modify this constraint's inner representation to include this splitter.
		 */
		abstract protected void applyNormalizedSplitterDestructively(boolean splitterSign, Expression splitter, RewritingProcess process);

		@Override
		public Constraint applySplitter(boolean splitterSign, Expression splitter, RewritingProcess process) {
			Constraint result;

			Expression normalizedSplitterGivenConstraint = normalizeSplitterGivenConstraint(splitter, process);
			
			if (normalizedSplitterGivenConstraint.equals(splitterSign)) {
				result = this; // splitter is redundant given constraint
			}
			else if (normalizedSplitterGivenConstraint.equals( ! splitterSign)) {
				result = null; // splitter is contradictory given constraint
			}
			else {
				try {
					result = applyNormalizedSplitter(splitterSign, normalizedSplitterGivenConstraint, process);
				}
				catch (Contradiction e) {
					result = null;
				}
			}

			return result;
		}

		private Constraint applyNormalizedSplitter(boolean splitterSign, Expression splitter, RewritingProcess process) {
			AbstractConstraint newConstraint = clone();
			newConstraint.applyNormalizedSplitterDestructively(splitterSign, splitter, process);
			return newConstraint;
		}

		abstract protected Collection<Expression> getSplittersToBeSatisfied(RewritingProcess process);

		abstract protected Collection<Expression> getSplittersToBeNotSatisfied(RewritingProcess process);

		@Override
		public Expression modelCount(RewritingProcess process) {
			Expression unconditionalCount = computeModelCountGivenConditionsOnFreeVariables(process);
			Expression result =
					makeModelCountConditionedOnFreeVariableSplittersNotAlreadyImpliedByContextualConstraint(
							unconditionalCount,
							getSplittersToBeSatisfied(process), getSplittersToBeNotSatisfied(process),
							process);
			return result;
		}
		
		/**
		 * Returns an expression (in the free variables) for the number of possible values for the given index,
		 * assuming that {@link #provideSplitterRequiredForComputingNumberOfValuesFor(Expression, RewritingProcess)}
		 * currently returns <code>null</code>,
		 * that is, we do not need anything splitters to be either imposed or negated in order to compute that.
		 * This method is only used in {@link AbstractConstraint}'s implementation of
		 * {@link #computeModelCountGivenConditionsOnFreeVariables(RewritingProcess)};
		 * if that method is overridden and the overriding version does not employ this method,
		 * its implementation in the extending class is irrelevant.
		 */
		abstract protected Expression computeNumberOfPossibleValuesFor(Expression index, RewritingProcess process);

		protected Expression computeModelCountGivenConditionsOnFreeVariables(RewritingProcess process) {
			List<Expression> numberOfPossibleValuesForIndicesSoFar = new LinkedList<Expression>();
			
			for (Expression index : indices) {
				Expression numberOfPossibleValuesForIndex = computeNumberOfPossibleValuesFor(index, process);
				numberOfPossibleValuesForIndicesSoFar.add(numberOfPossibleValuesForIndex);
			}
			
			Expression result = Times.make(numberOfPossibleValuesForIndicesSoFar);
			Expression unconditionalCount = timesRewriter.rewrite(result, process);
			return unconditionalCount;
		}

		/**
		 * Receives the model count for the case in which a certain set of splitter is satisfied, and another is unsatisfied,
		 * and returns conditional model count including the cases in which those conditions are not true
		 * (which entail model count 0),
		 * taking into account the contextual constraint.
		 */
		private Expression makeModelCountConditionedOnFreeVariableSplittersNotAlreadyImpliedByContextualConstraint(
				Expression modelCountGivenUndeterminedSplitters,
				Collection<Expression> splittersToBeSatisfied,
				Collection<Expression> splittersToBeUnsatisfied,
				RewritingProcess process) {
			
			Predicate<Expression> keepUnsatisfiedSplitters         = s -> splitterIsNotSatisfiedFromContextualConstraintAlready(true,  s, process);
			Predicate<Expression> keepUnsatisfiedSplitterNegations = s -> splitterIsNotSatisfiedFromContextualConstraintAlready(false, s, process);
		
			Collection<Expression> undeterminedSplittersThatNeedToBeTrue  = Util.filter(splittersToBeSatisfied,   keepUnsatisfiedSplitters);
			Collection<Expression> undeterminedSplittersThatNeedToBeFalse = Util.filter(splittersToBeUnsatisfied, keepUnsatisfiedSplitterNegations);
			
			Expression result = conditionExpressionOnGivenSplitters(
					modelCountGivenUndeterminedSplitters, undeterminedSplittersThatNeedToBeTrue, undeterminedSplittersThatNeedToBeFalse);
			return result;
		}

		private boolean splitterIsNotSatisfiedFromContextualConstraintAlready(boolean splitterSign, Expression splitter, RewritingProcess process) {
			boolean result;
			Expression splitterNormalizedByContextualConstraint = process.getDPLLContextualConstraint().normalizeSplitterGivenConstraint(splitter, process);
			assert ! splitterNormalizedByContextualConstraint.equals( ! splitterSign); // required splitter must be satisfiable under contextual constraint, otherwise there is a bug somewhere
			result = ! splitterNormalizedByContextualConstraint.equals(splitterSign); // if splitter is implied TRUE by contextual constraint, it is superfluous
			return result;
		}

		/**
		 * Receives an expression and conditions it on a set of splitters required to be true,
		 * and another set of splitters required to be false.
		 */
		private Expression conditionExpressionOnGivenSplitters(
				Expression expression,
				Collection<Expression> splittersThatNeedToBeTrue,
				Collection<Expression> splittersThatNeedToBeFalse) {
			
			Expression result = expression;
			for (Expression splitterToBeSatisfied : splittersThatNeedToBeTrue) {
				result = IfThenElse.make(splitterToBeSatisfied, result, ZERO, false);
			}
			for (Expression splitterToBeNotSatisfied : splittersThatNeedToBeFalse) {
				result = IfThenElse.make(splitterToBeNotSatisfied, ZERO, result, false);
			}
			return result;
		}
	}
}