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

import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.throwSafeguardError;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

@Beta
/** 
 * Basic implementation of some methods of {@link ConstraintTheory}.
 */
abstract public class AbstractConstraintTheory implements ConstraintTheory {

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
				"AbstractConstraintTheory",
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
				"AbstractConstraintTheory",
				"simplify(Expression, RewritingProcess)");
		return null; // never used, as safeguardCheck throws an error no matter what.
	}

	/**
	 * Default implementation that simplifies an expression by exhaustively simplifying its top expression with
	 * the simplifiers provided by {@link #getFunctionApplicationSimplifiers()} and {@link #getSyntacticFormTypeSimplifiers()},
	 * then simplifying its sub-expressions,
	 * and again exhaustively simplifying its top expression.
	 * @param expression
	 * @param topSimplifier
	 * @param process
	 * @return
	 */
	@Override
	public Expression simplify(Expression expression, RewritingProcess process) {
		myAssert(
				() -> usesDefaultImplementationOfSimplifyByOverridingGetFunctionApplicationSimplifiersAndGetSyntacticFormTypeSimplifiers(),
				() -> getClass() + " is using default implementation of simplify, even though its usesDefaultImplementationOfSimplifyByOverridingGetFunctionApplicationSimplifiersAndGetSyntacticFormTypeSimplifiers method returns false");
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
	 * for example, an equality constraintTheory may be defined so that an expression a != b generates the splitter =,
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
				"AbstractConstraintTheory",
				"makeSplitterIfPossible");
		return null; // never used, as safeguard throws an error no matter what.
	}
	
	/**
	 * This default implementation does the following (check the javadoc in the declaration of this method in {@link ConstraintTheory#AbstractEqualityConstraint}
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
			Expression variable  = Util.getFirstSatisfyingPredicateOrNull(
					expression.getArguments(),
					e -> isVariableTerm(e, process));
			if (variable != null) {
				Expression termDistinctFromVariable = Util.getFirstSatisfyingPredicateOrNull(
						expression.getArguments(),
						e -> ! e.equals(variable));
				if (termDistinctFromVariable != null) {
					result = makeSplitterFromFunctorAndVariableAndTermDistinctFromVariable(splitterFunctor, variable, termDistinctFromVariable, indices, process);
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
	 * @param variable
	 * @param termDistinctFromVariable
	 * @param indices
	 * @param process
	 * @return
	 */
	public Expression makeSplitterFromFunctorAndVariableAndTermDistinctFromVariable(String splitterFunctor, Expression variable, Expression termDistinctFromVariable, Collection<Expression> indices, RewritingProcess process) {
		Expression result;
		// Places index or variable before constants.
		if (indices.contains(variable)) {
			result = Expressions.apply(splitterFunctor, variable, termDistinctFromVariable);
		}
		else if (indices.contains(termDistinctFromVariable)) {
			result = Expressions.apply(splitterFunctor, termDistinctFromVariable, variable);
		}
		else if (isVariableTerm(variable, process)) {
			result = Expressions.apply(splitterFunctor, variable, termDistinctFromVariable);
		}
		else {
			result = Expressions.apply(splitterFunctor, termDistinctFromVariable, variable);
		}
		// Experimented with the below, which always chooses the variable later in value choice for first term in splitter,
		// but this sometimes changes input expressions unnecessarily (*), so I am sticking with the above for now which is more conservative.
		// (*) the differences occur when we have two indices or two non-indices, which in choice order means they are sorted alphabetically,
		// whereas in splitters there is no preference.
//		if (variableIsChosenAfterOtherTerm(variable, termDistinctFromVariable, indices, process)) {
//			result = Expressions.apply(splitterFunctor, variable, termDistinctFromVariable);
//		}
//		else { // only other variables are chosen after a variable, so here it must be the case that termDistinctFromVariable is a variable, too.
//			result = Expressions.apply(splitterFunctor, termDistinctFromVariable, variable);
//		}
		return result;
	}

	public Expression makeSplitterFromFunctorAndTwoDistinctTermsOneOfWhichIsAVariable(String functor, Expression term1, Expression term2, Collection<Expression> indices, RewritingProcess process) {
		Expression result;
		if (isVariableTerm(term1, process)) {
			result = makeSplitterFromFunctorAndVariableAndTermDistinctFromVariable(functor, term1, term2, indices, process);
		}
		else {
			result = makeSplitterFromFunctorAndVariableAndTermDistinctFromVariable(functor, term1, term2, indices, process);
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

	abstract protected boolean usesDefaultImplementationOfSimplifyExpressionGivenSplitterByOverriddingGetSplitterApplier();

	/**
	 * Default implementation that obtains a function from {@link #getSplitterApplier(boolean, Expression)} and,
	 * if it is not <code>null</code>,
	 * invokes {@link Expression#replaceAllOccurrences(Function, RewritingProcess)} with it as a parameter and the simplifies it,
	 * otherwise just returning the original expression otherwise.
	 */
	@Override
	public Expression simplifyExpressionGivenSplitter(boolean splitterSign, Expression splitter, Expression expression, RewritingProcess process) {
		myAssert(
				() -> usesDefaultImplementationOfSimplifyExpressionGivenSplitterByOverriddingGetSplitterApplier(),
				() -> getClass() + " is using default implementation of simplifyExpressionGivenSplitter, even though its usesDefaultImplementationOfSimplifyExpressionGivenSplitterByOverriddingGetSplitterApplier returns false");

		Expression result;
		Function<Expression, Expression> splitterSubExpressionApplier = getSplitterApplier(splitterSign, splitter);
		if (splitterSubExpressionApplier != null) {
			result = expression.replaceAllOccurrences(splitterSubExpressionApplier, process);
			result = simplify(result, process);
		}
		else {
			result = expression;
		}
		return result;
	}

	protected Function<Expression, Expression> getSplitterApplier(boolean splitterSign, Expression splitter) {
		throwSafeguardError(
				getClass().getSimpleName(),
				"getSplitterApplier", // thisClassName
				"AbstractConstraintTheory", // superClassName
				"simplifyExpressionGivenSplitter(boolean splitterSign, Expression splitter, Expression expression, RewritingProcess process)"); // namesOfMethodsWhoseDefaultImplementationUsesThisMethod
		return null; // never used, as safeguard throws an error no matter what.
	}


	@Override
	public Expression applyConstraintToSolution(Constraint constraint, Expression solution, RewritingProcess process) {
		Expression result;
		
		if (DPLLUtil.isConditionalSolution(solution, this, process)) {
			Expression solutionSplitter = IfThenElse.condition(solution);
			Constraint constraintUnderSolutionSplitter = constraint.incorporate(true, solutionSplitter, process);
			if (constraintUnderSolutionSplitter != null) {
				Constraint constraintUnderSolutionSplitterNegation = constraint.incorporate(false, solutionSplitter, process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression newSolutionSplitter = constraint.normalizeSplitterGivenConstraint(solutionSplitter, process);
					Expression thenBranch = IfThenElse.thenBranch(solution);
					Expression elseBranch = IfThenElse.elseBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter, thenBranch, process);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					if (newThenBranch == null) {
						result = newElseBranch;
					}
					else if (newElseBranch == null) {
						result = newThenBranch;
					}
					else {
						result = IfThenElse.makeIfDistinctFrom(solution, newSolutionSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
					}
				}
				else {
					Expression thenBranch = IfThenElse.thenBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter, thenBranch, process);
					result = newThenBranch;
				}
			}
			else {
				Constraint constraintUnderSolutionSplitterNegation = constraint.incorporate(false, solutionSplitter, process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression elseBranch = IfThenElse.elseBranch(solution);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					result = newElseBranch;
				}
				else {
					// throw new Error("AbstractEqualityConstraint applied to solution should be compatible with the solution splitter or its negation (otherwise either the constraint is unsatisfiable, or the sub-solution is, and in this case we should not have gotten here).");
					// Not true; due to incompleteness, the constraint might have been inconsistent but that only got detected now.
					result = null;
				}
			}
		}
		else {
			result = constraint.normalizeExpressionWithoutLiterals(solution, process);
		}
		
		return result;
	}
	
	/**
	 * Indicates whether variable is chosen after otherTerm in model counting choosing ordering.
	 */
	public static boolean variableIsChosenAfterOtherTerm(Expression variable, Expression otherTerm, Collection<Expression> indices, RewritingProcess process) {
		boolean result = process.isUniquelyNamedConstant(otherTerm) || variableIsChosenAfterOtherVariable(variable, otherTerm, indices);
		return result;
	}

	/**
	 * Indicates whether variable in chosen after otherVariable in choosing ordering.
	 */
	protected static boolean variableIsChosenAfterOtherVariable(Expression variable, Expression otherVariable, Collection<Expression> indices) {
		boolean result;
		if (indices.contains(variable)) { // index
			if ( ! indices.contains(otherVariable)) { // free variable
				result = true; // free variables always precedes indices
			}
			else { // both are indices
				result = choosingOrderTieBreaker.compare(otherVariable, variable) > 0;
			}
		}
		else if (indices.contains(otherVariable)) { // variable is free variable and otherVariable is index
			result = false; // free variable always precedes indices
		}
		else { // neither is index
			result = choosingOrderTieBreaker.compare(otherVariable, variable) > 0;		
		}
		return result;
	}
	
	/**
	 * The tie-breaker used for choosing order within the same group (indices, free variables and constants).
	 */
	public static final Comparator<Expression> choosingOrderTieBreaker = (a, b) -> a.toString().compareTo(b.toString());
}