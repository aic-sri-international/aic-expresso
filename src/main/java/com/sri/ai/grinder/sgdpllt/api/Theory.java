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
package com.sri.ai.grinder.sgdpllt.api;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.sgdpllt.library.FormulaUtil.isInterpretedInPropositionalLogicIncludingConditionals;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.NOT;
import static com.sri.ai.grinder.sgdpllt.library.boole.And.getConjuncts;
import static com.sri.ai.util.Util.addAllToSet;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.thereExists;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.core.constraint.DefaultMultiVariableConstraint;
import com.sri.ai.grinder.sgdpllt.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.FormulaUtil;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.util.collect.PredicateIterator;

/**
 * An interface for theories to be plugged into quantifier problems.
 * <p>
 * One of its tasks is to select and manipulate <i>splitters</i>.
 * A splitter is a literal on which DPLL splits the possible interpretations of an expression.
 * The theoryWithEquality needs to know how to simplify expressions based on the fact that a splitter is true or false,
 * as well as how to simplify a <i>solution</i> based on a splitter's being true or false into a simpler solution.
 * A solution is an if-then-else expression in which all conditions are splitters.
 * 
 * @author braz
 *
 */
@Beta
public interface Theory extends Cloneable {

	// TODO
	// In the future, we may want to give more structure to this interface,
	// making the constraint theory and the language theory (the interpretations
	// of functions in the body of expressions) more separately defined
	// so that the same constraint theory can be re-used in multiple combinations
	// with language theories.
	// 
	// At this point, this is not the case because the top simplifiers provided
	// by a theory have to be the combination of all the simplifiers for all the functions
	// in both the constraint theory and the language theory.
	// So there is no way to easily separate the constraint and language theories.
	// 
	// In the future, we may have constraint and language theories defined separately,
	// each with their own set of simplifiers, and implementations of Theory
	// will take them and combine the simplifiers automatically.
	// BTW, this will also require a restructuring of map-based simplifiers
	// so that multiple occurrences of simplifiers will be detected and dealt with.
	//
	// What I have in mind for this future structure is the following:
    //
	// A language theory T_L is a set of simplifiers for its functions
	// 
	// A quantifier eliminator is a tuple (G, T_C, T_L)
	// for a group G
	// a constraint theory T_C
	// and a language theory T_L
    //
	// A theory is a pair (T_C, f_Q)
	// - a constraint theory
	// - a function from groups to quantifier eliminators
	//
	// So, as you see, a constraint theory will typically be combined with
	// multiple quantifier eliminators and language theories, one for each
	// operation/group/quantifier.
	
	/**
	 * A method simplifying an expression according to this theory.
	 * It must at a minimum replace expressions
	 * by their values, if their values are defined by some of their arguments
	 * (such as <code>X + 0</code> being equal to <code>X</code>.
	 * @param expression
	 * @param context
	 * @return
	 */
	Expression simplify(Expression expression, Context context);

	/**
	 * Returns a {@link TopRewriter} performing the same simplification as {@link #simplify(Expression, Context)},
	 * but only on the top expression (that is, without recursing to sub-expressions).
	 * @return
	 */
	TopRewriter getTopRewriter();
	
	/**
	 * Returns a cached version of a recursive exhaustive rewriter based on {@link getTopRewriter()}.
	 * @return
	 */
	Rewriter getRewriter();
	
	ExpressionLiteralSplitterStepSolver makeEvaluatorStepSolver(Expression expression);
	
	default Expression evaluate(Expression expression, Context context) {
		ExpressionLiteralSplitterStepSolver evaluatorStepSolver = makeEvaluatorStepSolver(expression);
		Expression result = ContextDependentExpressionProblemSolver.staticSolve(evaluatorStepSolver, context);
		return result;
	}
	
	boolean isSuitableFor(Expression variable, Type type);
	
	/**
	 * Indicates whether an expression is a literal in this theory.
	 * @param expression
	 * @param context
	 * @return 
	 * @return
	 */
	default boolean isLiteral(Expression expression, Context context) {
		if (expression.equals(TRUE) || expression.equals(FALSE)) {
			return true;
		}
		return isNonTrivialLiteral(expression, context);
	}
	
	default boolean isConjunctiveClause(Expression formula, Context context) {
		boolean result = forAll(getConjuncts(formula), c -> isLiteral(c, context));
		return result;
	}

	/**
	 * Indicates whether an expression is a non-trivial atom in this theory.
	 * @param expression
	 * @param context
	 * @return
	 */
	boolean isNonTrivialAtom(Expression expression, Context context);
	
	/**
	 * Indicates whether an expression is a non-trivial literal in this theory.
	 * This is defined as its being either a non-trivial atom, or a non-trivial negative literals,
	 * which in turn is the negation of a non-trivial atom.
	 * @param expression
	 * @param context
	 * @return
	 */
	default boolean isNonTrivialLiteral(Expression expression, Context context) {
		boolean result = isNonTrivialAtom(expression, context) || isNonTrivialNegativeLiteral(expression, context);
		return result;
	}

	default boolean isNonTrivialNegativeLiteral(Expression expression, Context context) {
		boolean result = expression.hasFunctor(NOT) && isNonTrivialAtom(expression.get(0), context);
		return result;
	}
	
	/**
	 * Make a new single-variable constraint for this theory.
	 * @param variable 
	 * @param theory the theory of the application (not necessarily <code>this</code> because <code>this</code> may be a sub-theory in a compound one
	 * @param context
	 * @return
	 */
	SingleVariableConstraint makeSingleVariableConstraint(Expression variable, Theory theory, Context context);
	
	/**
	 * Returns a new true (empty conjunction) constraint for this theory.
	 * @return
	 */
	default Constraint makeTrueConstraint() {
		return new DefaultMultiVariableConstraint(this);
	}
	
	/**
	 * Indicates whether single-variable constraint solver is complete (for its variable).
	 * @return whether single-variable constraint solver is complete (for its variable).
	 */
	boolean singleVariableConstraintIsCompleteWithRespectToItsVariable();
	
	/**
	 * Indicates whether an expression is an application of a function belonging to this theory,
	 * or a constant (including constant functions) belonging to this theory.
	 * @param expression
	 * @return
	 */
	boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression);

	/**
	 * Given a single-variable constraint in this theory, returns
	 * a {@link ExpressionLiteralSplitterStepSolver} deciding its satisfiability.
	 * @param context TODO
	 * @return a {@link ExpressionLiteralSplitterStepSolver} deciding a constraint's satisfiability.
	 */
	ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context);
	
	/**
	 * Given a single-variable constraint in this theory, returns
	 * a {@link ExpressionLiteralSplitterStepSolver} computing its model count.
	 * @param context TODO
	 * @return a {@link ExpressionLiteralSplitterStepSolver} computing a constraint's model count.
	 */
	ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context);

	/**
	 * Provides a quantifier eliminator for use with given single-variable constraint and body.
	 * @param group
	 * @param constraint
	 * @param currentBody
	 * @param context
	 * @return
	 */
	ExpressionLiteralSplitterStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraint, Expression currentBody, Context context);

	/**
	 * Returns the negation of a literal.
	 * While one could simply add or remove a <code>not</code> in the original literal,
	 * this methods provides a way of generating theory-specific representations at the generic level of the {@link Theory} interface.
	 * For example, for equality
	 * we may prefer the negation of <code>X = a</code> to be represented as <code>X != a</code> instead of <code> not X = a</code>.
	 * @param literal the literal
	 * @param context TODO
	 * @return the negation of literal
	 */
	Expression getLiteralNegation(Expression literal, Context context);
	
	/**
	 * Provides a collection of all generalized variables (according to this theory) in a given expression,
	 * where a generalized variable is an expression that is not a boolean connective or an interpreted element in this theory
	 * (see {@link #isInterpretedInThisTheoryBesidesBooleanConnectives(Expression, Context)}).
	 * @param expression
	 * @param context
	 * @return
	 */
	default Collection<Expression> getVariablesIn(Expression expression, Context context) {
//      For debugging:
//		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
//		Collection<Expression> subExpressions = Util.addAllToList(subExpressionsIterator);
//		System.out.println("Sub-expressions: " + subExpressions);	
//		Predicate<Expression> isVariablePredicate = e -> isVariable(e, context);
//		Iterator<Expression> variablesIterator = PredicateIterator.make(subExpressions, isVariablePredicate);
//		LinkedHashSet<Expression> variables = addAllToSet(variablesIterator);
//		System.out.println("Variables: " + variables);	
//		return variables;

		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
		Predicate<Expression> isVariablePredicate = e -> isVariable(e, context);
		Iterator<Expression> variablesIterator = PredicateIterator.make(subExpressionsIterator, isVariablePredicate);
		LinkedHashSet<Expression> variables = addAllToSet(variablesIterator);
		return variables;
	}

	/**
	 * Indicates whether an expression is considered a variable in this theory,
	 * meaning that it is not a constants of any of the registered types in context
	 * (per {@link Context#getTypes()} and {@link Context#isUniquelyNamedConstant(Expression)}),
	 * it is not interpreted in propositional logic
	 * (per {@link FormulaUtil#isInterpretedInPropositionalLogicIncludingConditionals(Expression)}),
	 * and is not interpreted in this theory besides boolean connectives
	 * (per {@link #isInterpretedInThisTheoryBesidesBooleanConnectives(Expression, Context)}.
	 * @param expression
	 * @param context
	 * @return
	 */
	default boolean isVariable(Expression expression, Context context) {
		Expression typeExpression;
		Type type;
		boolean result =
				!context.isUniquelyNamedConstant(expression)
				&& !(expression instanceof QuantifiedExpression)
				&& !isInterpretedInPropositionalLogicIncludingConditionals(expression)  
				&& !isInterpretedInThisTheoryBesidesBooleanConnectives(expression)
				&& (typeExpression = GrinderUtil.getType(expression, context)) != null
				&& (type = context.getType(typeExpression)) != null
				&& isSuitableFor(expression, type)
				&& !thereExists(context.getTypes(), t -> t.contains(expression));		
		return result;
	}
	
	/**
	 * Provides types that must be present in the context while using theory,
	 * even if no variables are associated with them. Default
	 * implementation is no types; override for introducing them.
	 * 
	 * @return
	 */
	Collection<Type> getNativeTypes();
	
	Theory clone();
}