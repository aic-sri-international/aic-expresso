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
package com.sri.ai.grinder.api;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.core.solver.ContextDependentExpressionProblemSolver.staticSolve;
import static com.sri.ai.grinder.library.FormulaUtil.isInterpretedInPropositionalLogicIncludingConditionals;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.grinder.library.boole.And.getConjuncts;
import static com.sri.ai.util.Util.addAllToSet;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getFirstOrNull;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.Util.thereExists;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

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
import com.sri.ai.grinder.library.FormulaUtil;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.grinder.theory.help.TheoryWrapper;
import com.sri.ai.util.collect.PredicateIterator;

/**
 * An interface for theories.
 * <p>
 * Theories have a few distinct roles:
 * <ul>
 * 
 * <li> Recognize literals in the theory and create single-variable constraints.
 * <li> Provide symbolic quantifier eliminator step solvers for different groups when available.
 * <li> Evaluates expressions using a set of simplifiers and quantifier eliminators for its language.
 * This last function should be phased out because some quantifier eliminators may be completely
 * unrelated to the quantifier eliminator step solvers currently provided.
 * The step solvers are symbolic but quantifier eliminators could be based on brute force, or sampling,
 * so putting them all together in the same theory could create creating potential confusion.
 * Besides, quantifier eliminators don't need to be inside a theory (although they may use the theory provided in the context).
 *
 * </ul>
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
	 * Provides an evaluator step solver that must be based on a recursive exhaustive
	 * application of {@link #getTopRewriter()} and also externalize literals
	 * (this ensures completeness in the sense that, if the resulting value
	 * is the same under all contexts, which will be detected and that single value will be returned,
	 * because <code>if <Context1> then Value else if <Context2> then Value else Value</code> gets
	 * simplified to <code>Value</code>).
	 * @param expression
	 * @return
	 */
	ExpressionLiteralSplitterStepSolver makeEvaluatorStepSolver(Expression expression);
	
	default Expression evaluate(Expression expression, Context context) {
		return explanationBlock("Theory.evaluate ", expression, " under ", context, code( () -> {
			
			ExpressionLiteralSplitterStepSolver evaluatorStepSolver = explanationBlock("Making evaluator step solver ", code( () -> 
			makeEvaluatorStepSolver(expression)
			), "Step solver is ", RESULT);
			
			Expression result = explanationBlock("Solving step solver ", code( () -> 
			staticSolve(evaluatorStepSolver, context)
			), "Result is ", RESULT);
			
			return result;
			
		}), "Result is ", RESULT);
	}

	boolean isSuitableFor(Type type);
	
	default boolean isConjunctiveClause(Expression formula, Context context) {
		boolean result = forAll(getConjuncts(formula), c -> isLiteralOrBooleanConstant(c, context));
		return result;
	}

	/**
	 * Indicates whether an expression is a literal in this theory or a boolean constant.
	 * @param expression
	 * @param context
	 * @return 
	 * @return
	 */
	default boolean isLiteralOrBooleanConstant(Expression expression, Context context) {
		if (expression.equals(TRUE) || expression.equals(FALSE)) {
			return true;
		}
		return isLiteral(expression, context);
	}
	
	/**
	 * Indicates whether an expression is an atom in this theory.
	 * @param expression
	 * @param context
	 * @return
	 */
	boolean isAtom(Expression expression, Context context);
	
	/**
	 * Indicates whether an expression is a literal in this theory.
	 * This is defined as its being either an atom, or a negated atom,
	 * which in turn is the negation of an atom.
	 * @param expression
	 * @param context
	 * @return
	 */
	default boolean isLiteral(Expression expression, Context context) {
		boolean result = isAtom(expression, context) || isNegatedAtom(expression, context);
		return result;
	}

	default boolean isNegatedAtom(Expression expression, Context context) {
		boolean result = expression.hasFunctor(NOT) && isAtom(expression.get(0), context);
		return result;
	}
	
	/**
	 * Returns the negation of a literal
	 * by removing 'not' of negated literals, flipping true and false, and
	 * resorting to {@link #getAtomNegation(Expression, Context)} otherwise.
	 */
	default Expression getLiteralNegation(Expression literal, Context context) {
		Expression result;
		
		if (literal.hasFunctor(NOT)) {
			result = literal.get(0);
		} else 
		if (literal.equals(TRUE)) {
			result = FALSE;
		} 
		else if (literal.equals(FALSE)) {
			result = TRUE;
		} 
		else {
			result = getAtomNegation(literal, context);
		}
		
		return result;
	}

	/**
	 * Returns the negation of an atom.
	 * Used to have a default implementation applying 'not' to it,
	 * but this may not be correct to all theories (some may choose a more compact representation, such as x = y ---> x != y).
	 * Just leaving the default implementation to be overridden sounds like a good idea but upon creating {@link TheoryWrapper}
	 * I realized that default methods that are not correct for all implementations are dangerous.
	 * In this case, the wrapper kept the default implementation and stopped behaving like base theories it wrapped
	 * if they did not follow the default implementation.
	 * @param atom
	 * @param context
	 * @return
	 */
	Expression getAtomNegation(Expression atom, Context context);

	/**
	 * Make a new single-variable constraint for this theory.
	 * Default implementation checks suitability of type and invokes {@link #makeSingleVariableConstraint(Expression, Context)}.
	 * @param variable 
	 * @param context
	 * @return the constraint, or null if there is no theory for the variable.
	 */
	default SingleVariableConstraint makeSingleVariableConstraint(Expression variable, Context context) {
		Type variableType = context.getTypeOfRegisteredSymbol(variable);
		myAssert(variableType != null, () -> this + " does not know how to make constraints for variable " + variable + " because it does not have a registered type");
		myAssert(context.getTheory().isSuitableFor(variableType), () -> this + " does not know how to make constraints for variable " + variable + " of type " + variableType);
		SingleVariableConstraint result = makeSingleVariableConstraintAfterBookkeeping(variable, context);
		return result;
	}
	
	/**
	 * Implements how to make a new single-variable constraint for this theory after bookeeping.
	 * @param variable 
	 * @param context
	 * @return the constraint, or null if there is no theory for the variable.
	 */
	SingleVariableConstraint makeSingleVariableConstraintAfterBookkeeping(Expression variable, Context context);
	
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
	 * @return a {@link ExpressionLiteralSplitterStepSolver} deciding a constraint's satisfiability, or null if there is no appropriate theory.
	 */
	ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context);
	
	/**
	 * Given a single-variable constraint in this theory, returns
	 * a {@link ExpressionLiteralSplitterStepSolver} computing its model count.
	 * @param context TODO
	 * @return a {@link ExpressionLiteralSplitterStepSolver} computing a constraint's model count, or null if there is no appropriate theory.
	 */
	ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context);

	/**
	 * Provides a quantifier eliminator step solver for use with given single-variable constraint and body, or null if there is no appropriate theory.
	 */
	ExpressionLiteralSplitterStepSolver getSingleQuantifierEliminatorStepSolver(SingleQuantifierEliminationProblem problem, Context context);
	
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
				&& expression.getSyntacticFormType().equals("Symbol")
				&& !(expression instanceof QuantifiedExpression)
				&& !isInterpretedSymbolInThisTheory(expression)
				&& (typeExpression = GrinderUtil.getTypeExpressionOfExpression(expression, context)) != null
				&& (type = context.getTypeFromTypeExpression(typeExpression)) != null
				&& isSuitableFor(type)
				&& !thereExists(context.getTypes(), t -> t.contains(expression));		
		return result;
	}

	/** Indicates whether an expression is an interpreted symbol in this theory. */
	default boolean isInterpretedSymbolInThisTheory(Expression expression) {
		boolean result = 
				expression.getSyntacticFormType().equals("Symbol")
				&&
				knownSymbolIsInterpretedInThisTheory(expression);
		return result;
	}
	
	/** Indicates whether an expression known to be a symbol is interpreted in this theory. */
	default boolean knownSymbolIsInterpretedInThisTheory(Expression symbol) {
		boolean result = 
				isInterpretedInPropositionalLogicIncludingConditionals(symbol)  
				|| 
				isInterpretedInThisTheoryBesidesBooleanConnectives(symbol);
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

	default SingleVariableConstraint makeSingleVariableConstraintOnSomeVariableOfLiteral(Expression literal, Context context) {
		Collection<Expression> variablesInLiteral = getVariablesIn(literal, context);
		myAssert( ! variablesInLiteral.isEmpty(), () -> "There has been a request for making a single variable constraint on some variable of literal " + literal + " but no variables were identified in this literal. This may be caused by a mismatch between the variables used and the criterion used for deciding if a symbol is a variable (see Context.isVariable)");
		SingleVariableConstraint result = makeNewSingleVariableConstraintOnSomeVariableOfLiteral(literal, variablesInLiteral, context);
		return result;
	}

	/**
	 * Same as {@link #makeSingleVariableConstraintOnSomeVariableOfLiteral(Expression, Context)} for when the variables in literal are already known.
	 * @param literal
	 * @param variablesInLiteral
	 * @param context
	 * @return
	 */
	default SingleVariableConstraint makeNewSingleVariableConstraintOnSomeVariableOfLiteral(Expression literal, Collection<Expression> variablesInLiteral, Context context) {
		return explanationBlock("Making single-variable constraint for some variable of ", literal, code( () -> {

			Expression firstVariable = getFirstOrNull(variablesInLiteral);
			explain("Variable selected is ", firstVariable, " out of ", variablesInLiteral);
			
			SingleVariableConstraint newSingleVariableConstraint = makeSingleVariableConstraint(firstVariable, context);
			newSingleVariableConstraint = newSingleVariableConstraint.conjoin(literal, context);
			return newSingleVariableConstraint;

		}), "Result is ", RESULT);
	}
}