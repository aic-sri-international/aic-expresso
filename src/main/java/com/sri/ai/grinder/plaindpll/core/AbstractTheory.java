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
package com.sri.ai.grinder.plaindpll.core;

import static com.sri.ai.util.Util.throwSafeguardError;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.api.ConjunctiveConstraint;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.Theory;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

@Beta
/** 
 * Basic implementation of some methods of {@link Theory}.
 */
abstract public class AbstractTheory implements Theory {

	protected abstract boolean usesDefaultImplementationOfSimplifyByOverridingGetFunctionApplicationSimplifiersAndGetSyntacticFormTypeSimplifiers();
	
	/**
	 * Provides a map from functors's getValue() values (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified formula
	 * according to this theoryWithEquality.
	 * Only required if {@link #simplify(Expression, RewritingProcess)} is not overridden by code not using it. 
	 * @return
	 */
	protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers() {
		throwSafeguardError( // OPTIMIZATION: much of this, if not all or even extra information, could be obtained by reflection inside throwAppropriateSafeguardError
				getClass().getSimpleName(),
				"getFunctionApplicationSimplifiers",
				"AbstractTheory",
				"simplify(Expression, RewritingProcess)");
		return null; // never used, as safeguardCheck throws an error no matter what.
	}

	/**
	 * Provides a map from syntactic form types (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified formula
	 * according to this theoryWithEquality.
	 * Only required if {@link #simplify(Expression, RewritingProcess)} is not overridden by code not using it. 
	 * @return
	 */
	protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers() {
		throwSafeguardError(
				getClass().getSimpleName(),
				"getSyntacticFormTypeSimplifiers",
				"AbstractTheory",
				"simplify(Expression, RewritingProcess)");
		return null; // never used, as safeguardCheck throws an error no matter what.
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
		if ( ! usesDefaultImplementationOfSimplifyByOverridingGetFunctionApplicationSimplifiersAndGetSyntacticFormTypeSimplifiers()) {
			throw new Error(getClass() + " is using default implementation of simplify, even though its usesDefaultImplementationOfSimplifyByOverridingGetFunctionApplicationSimplifiersAndGetSyntacticFormTypeSimplifiers methods returns false");
		}
		return DPLLUtil.simplify(expression, getFunctionApplicationSimplifiers(), getSyntacticFormTypeSimplifiers(), process);
	}

	/**
	 * This default implementation searches the entire splitter and its sub-expressions for an index;
	 * theories with more restricted splitters may want to implement more efficient versions.
	 */
	@Override
	public boolean splitterDependsOnIndex(Expression splitter, Collection<Expression> indices) {
		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(splitter);
		boolean result = Util.thereExists(subExpressionsIterator, e -> indices.contains(e));
		return result;
	}

	/**
	 * If expression can generate a splitter, returns the appropriate splitter's functor;
	 * for example, an equality theory may be defined so that an expression a != b generates the splitter =,
	 * so the result for that input will be =.
	 * If expression cannot generate a splitter, returns <code>null</code>.
	 * This method is only used in this class' default implementation of {@link #makeSplitterIfPossible(Expression, Collection, RewritingProcess)}
	 * so it only needs to be overridden if that default implementation is used, or some other overriding code uses it.
	 * @param expression
	 * @return
	 */
	protected String getCorrespondingSplitterFunctorOrNull(Expression expression) {
		throwSafeguardError(
				getClass().getSimpleName(),
				"getCorrespondingSplitterFunctorOrNull",
				"AbstractTheory",
				"makeSplitterIfPossible");
		return null; // never used, as safeguardCheck throws an error no matter what.
	}
	
	/**
	 * This default implementation does the following (check the javadoc in the declaration of this method in {@link Theory#AbstractEqualityConstraint}
	 * for the more general, more relaxed requirements):
	 * If expression can originate a splitter and has at least one variable argument, returns the splitter by making it in the following way:
	 * obtain the appropriate splitter functor from {@link #getCorrespondingSplitterFunctorOrNull(Expression)}
	 * and create splitter by getting expression's first variable argument, V, and expression's first argument distinct from V.
	 * Otherwise, returns <code>null</code>.
	 * The use of this default implementation will require extending classes to implement {@link #splitterDependsOnIndex(Expression, Collection)}
	 * to return true, so as to confirm the assumption required.
	 * @param expression
	 * @param indices
	 * @param process
	 * @return
	 */
	@Override
	public Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process) {
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
	public Expression makeSplitterFromFunctorAndTwoTerms(String splitterFunctor, Expression term1, Expression term2, Collection<Expression> indices, RewritingProcess process) {
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

	@Override
	public Expression pickSplitterInExpression(Expression expression, Constraint constraint, RewritingProcess process) {
		Expression result = null;
		
		Iterator<Expression> subExpressionIterator = new SubExpressionsDepthFirstIterator(expression);
		while (result == null && subExpressionIterator.hasNext()) {
			Expression subExpression = subExpressionIterator.next();
			Expression splitterCandidate = makeSplitterIfPossible(subExpression, constraint.getSupportedIndices(), process);
			result = splitterCandidate;
		}
	
		return result;
	}

	@Override
	public Expression applySplitterToExpression(boolean splitterSign, Expression splitter, Expression expression, RewritingProcess process) {
		Expression result = getSplitterApplier(splitterSign, splitter).apply(expression, process);
		return result;
	}

	protected BinaryFunction<Expression, RewritingProcess, Expression> getSplitterApplier(boolean splitterSign, Expression splitter) {
		throwSafeguardError(
				getClass().getSimpleName(),
				"getSplitterApplier", // thisClassName
				"AbstractTheory", // superClassName
				"simplify(Expression, RewritingProcess)"); // namesOfMethodsWhoseDefaultImplementationUsesThisMethod
		return null; // never used, as safeguardCheck throws an error no matter what.
	}

	@Override
	public Expression applyConstraintToSolution(ConjunctiveConstraint constraint, Expression solution, RewritingProcess process) {
		Expression result;
		
		if (DPLLUtil.isConditionalSolution(solution, this, process)) {
			Expression solutionSplitter = IfThenElse.condition(solution);
			ConjunctiveConstraint constraintUnderSolutionSplitter = constraint.incorporate(true, solutionSplitter, process);
			if (constraintUnderSolutionSplitter != null) {
				ConjunctiveConstraint constraintUnderSolutionSplitterNegation = constraint.incorporate(false, solutionSplitter, process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression newSolutionSplitter = constraint.normalizeSplitterGivenConstraint(solutionSplitter, process);
					Expression thenBranch = IfThenElse.thenBranch(solution);
					Expression elseBranch = IfThenElse.elseBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter, thenBranch, process);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					result = IfThenElse.makeIfDistinctFrom(solution, newSolutionSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
				}
				else {
					Expression thenBranch = IfThenElse.thenBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter, thenBranch, process);
					result = newThenBranch;
				}
			}
			else {
				ConjunctiveConstraint constraintUnderSolutionSplitterNegation = constraint.incorporate(false, solutionSplitter, process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression elseBranch = IfThenElse.elseBranch(solution);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					result = newElseBranch;
				}
				else {
					throw new Error("AbstractEqualityConstraint applied to solution should be compatible with the solution splitter or its negation (otherwise either the constraint is unsatisfiable, or the sub-solution is, and in this case we should not have gotten here).");
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
}