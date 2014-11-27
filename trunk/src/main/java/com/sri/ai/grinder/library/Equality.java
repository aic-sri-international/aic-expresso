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
package com.sri.ai.grinder.library;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.Pair;

/**
 * Implements equality.
 * 
 * @author braz
 */
@Beta
public class Equality extends AbstractRewriter {

	public static final Expression FUNCTOR = Expressions.makeSymbol("=");
	
	public Equality() {
		this.setReifiedTests(new HasKind(FUNCTOR));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (expression.numberOfArguments() > 1) {
			return equalityResultIfItIsKnown(expression, process);
		}
		else {
			// 1 or 0 arguments is equivalent to True
			return Expressions.TRUE;
		}
	}

	public static Expression equalityResultIfItIsKnown(Expression expression, RewritingProcess process) {
		Boolean equalityResult = equalityResultIfItIsKnownOrNull(expression, process.getIsConstantPredicate(), process);
		if (equalityResult != null) {
			return Expressions.makeSymbol(equalityResult);
		}
		return expression;
	}

	private static Boolean equalityResultIfItIsKnownOrNull(Expression expression, Predicate<Expression> isConstantPredicate, RewritingProcess process) {
		int maxIndex = expression.numberOfArguments() - 1;
		for (int i = 0; i != maxIndex; i++) {
			Boolean equalityOfPair =
				equalityOfPairIfItIsKnownOrNull(
						expression.get(i), expression.get(i+1), isConstantPredicate, process);
			if (equalityOfPair == null) {
				return null;
			}
			if (!equalityOfPair) {
				return false;
			}
		}
		return true;
	}

	private static Boolean equalityOfPairIfItIsKnownOrNull(
			Expression expression1, Expression expression2, Predicate<Expression> isConstantPredicate, RewritingProcess process) {
		if (isConstantPredicate.apply(expression1)) {
			if (isConstantPredicate.apply(expression2)) {
				return expression1.equals(expression2); // two constants; true if they are equal, false if they are distinct
			}
			else {
				return null;  // non-constant and constant; we don't know
			}
		}
		else if (isConstantPredicate.apply(expression2)) {
			return null; // non-constant and constant; we don't know
		}
		else if (expression1.equals(expression2)) {
			return true; // both are the same expression
		}
		else if ( process.isVariable(expression1) && process.isVariable(expression2) &&
				Util.notNullAndDistinct(process.getContextualSymbolType(expression1), process.getContextualSymbolType(expression2))){
			return false; // distinct variables with different types, so not equal under current assumption that types do not overlap (which will probably be removed in the future).
		}
		else {
			return null; // distinct variables; we don't know
		}
	}

	/**
	 * Given a two-argument equality with at least one variable argument,
	 * returns an equivalent equality with first argument guaranteed not to be a constant,
	 * guaranteeing that original instance is returned if both arguments are variables.
	 */
	public static Expression makeSureFirstArgumentIsNotAConstant(Expression twoArgumentEquality, RewritingProcess process) {
		Expression result = twoArgumentEquality;
		if (process.isConstant(twoArgumentEquality.get(0))) {
			result = make(twoArgumentEquality.get(1), twoArgumentEquality.get(0));
		}
		return result;
	}

	public static Expression conditionForSubExpressionsEquality(Expression expression1, Expression expression2) {
		List<Expression> conditionsForSubExpressionsToBeEqual = listOfEqualitiesOfSubExpressions(
				expression1, expression2);
		Expression condition = And.make(conditionsForSubExpressionsToBeEqual);
		return condition;
	}

	public static List<Expression> listOfEqualitiesOfSubExpressions(
			Expression expression1, Expression expression2) {
		List<Expression> conditionsForSubExpressionsToBeEqual =
			Util.zipWith(
					MAKE_PAIR_EQUALITY,
					Util.listFrom(expression1.getImmediateSubExpressionsIterator()),
					Util.listFrom(expression2.getImmediateSubExpressionsIterator()));
		return conditionsForSubExpressionsToBeEqual;
	}

	/**
	 * Makes an equality of some expressions, returning Expressions. Returns TRUE if the expressions are identical.
	 */
	public static Expression make(Object... arguments) {
		if (arguments.length == 1 && arguments[0] instanceof List) {
			arguments = ((List)arguments[0]).toArray();
		}
		List<Expression> expressions = Expressions.wrap(arguments);
		Set<Expression> uniqueExpressions = new LinkedHashSet<Expression>(expressions);
		if (uniqueExpressions.size() < 2) {
			return Expressions.TRUE;
		}
		return Expressions.apply("=", expressions);
	}

	/**
	 * Makes an equality application on two terms possibly simplifying it (taking constants into account).
	 */
	public static Expression makeWithConstantSimplification(Expression term1, Expression term2, RewritingProcess process) {
		Expression result;
		if (term1.equals(term2)) {
			result = Expressions.TRUE;
		}
		else if (process.isConstant(term1) && process.isConstant(term2)) {
			result = Expressions.FALSE;
		}
		else {
			result = make(term1, term2);
		}
		return result;
	}
	
	/**
	 * Makes an equality application while removing duplicates,
	 * returning {@link Expressions.FALSE} if there is more than one uniquely named constant in arguments,
	 * and returning {@link Expressions.TRUE} if all arguments are identical.
	 */
	public static Expression makeWithConstantSimplification(Expression equality, RewritingProcess process) {
		
		Collection<Expression> arguments = equality.getArguments();
		
		Expression result = null;

		LinkedHashSet<Expression> newArguments = new LinkedHashSet<Expression>();
		Expression constant = null;
		
		for (Expression argument : arguments) {
			if (process.isConstant(argument)) {
				if (constant == null) {
					constant = argument;
				}
				else if (! argument.equals(constant)){
					result = Expressions.FALSE;
					break;
				}
			}
			else {
				newArguments.add(argument);
			}
		}
		
		if (result == null) {
			if (constant != null) {
				newArguments.add(constant);
			}

			if (newArguments.size() == 1) {
				result = Expressions.TRUE;
			}
		}

		if (result == null) {
			if (arguments.size() == newArguments.size()) {
				result = equality;
			}
			else {
				result = Expressions.apply(FunctorConstants.EQUALITY, newArguments.toArray());
			}
		}

		return result;
	}

	public static BinaryFunction<Expression, Expression, Expression> MAKE_PAIR_EQUALITY = new BinaryFunction<Expression, Expression, Expression>() {
		@Override
		public Expression apply(Expression e1, Expression e2) {
			return make(e1, e2);
		}
	};
	
	/**
	 * If given constraint is an equality binding a set of variables to a constant,
	 * substitutes them in expression by that constant.
	 * Can potentially throw {@link SemanticSubstitute#NoException} if such substitutions are not supported
	 * (this can happen in certain types of complex expressions -- see {@link SemanticSubstitute} for more details).
	 */
	public static Expression substituteVariablesByConstantIfTheyAreBoundToItByEquality(Expression expression, Expression constraint, RewritingProcess process) {
		if (isEquality(constraint)) {
			Pair<List<Expression>, Expression> variablesListAndConstant = getVariablesListAndConstantOrNullIfNoConstant(constraint, process);
			if (variablesListAndConstant != null) {
				for (Expression variable : variablesListAndConstant.first) {
					expression = SemanticSubstitute.replace(expression, variable, variablesListAndConstant.second, process);
				}
			}
		}
		return expression;
	}

	/**
	 * Assumes that first argument is an equality (with possibly more than two arguments) and returns a pair,
	 * the first member of which is a collection of the variables in the equality,
	 * and the second member of which is the first constant present.
	 * Returns null if there are no constants in the equality arguments.
	 */
	public static Pair<List<Expression>, Expression> getVariablesListAndConstantOrNullIfNoConstant(Expression equality, RewritingProcess process) {
		Pair<List<Expression>, Expression> result;
		List<Expression> variables = new LinkedList<Expression>();
		List<Expression> constants = new LinkedList<Expression>();
		Predicate<Expression> notIsConstant = Predicates.not(process.getIsConstantPredicate());
		Util.collectOrReturnFalseIfElementDoesNotFitEither(
				equality.getArguments(),
				variables, notIsConstant,
				constants, process.getIsConstantPredicate());
		if (constants.isEmpty()) {
			result = null;
		}
		else {
			result = Pair.make(variables, Util.getFirst(constants));
		}
		return result;
	}

	/**
	 * Assumes that first argument is an equality (with possibly more than two arguments) and returns a pair,
	 * the first member of which is a set of the variables in the equality,
	 * and the second member of which is a set of constants in the equality.
	 */
	public static Pair<Set<Expression>, Set<Expression>> getVariablesListAndConstantsList(Expression equality, RewritingProcess process) {
		Pair<Set<Expression>, Set<Expression>> result;
		Set<Expression> variables = new LinkedHashSet<Expression>();
		Set<Expression> constants = new LinkedHashSet<Expression>();
		Predicate<Expression> notIsConstant = Predicates.not(process.getIsConstantPredicate());
		Util.collectOrReturnFalseIfElementDoesNotFitEither(
				equality.getArguments(),
				variables, notIsConstant,
				constants, process.getIsConstantPredicate());
		result = Pair.make(variables, constants);
		return result;
	}

	public static boolean isEquality(Expression expression) {
		return expression.hasFunctor(FunctorConstants.EQUAL);
	}
	
	public static Set<Expression> getSymbolsBoundToSomethingElse(Expression equality) {
		Set<Expression> result = new LinkedHashSet<Expression>();
		
		if (isEquality(equality)) {
			for (Expression term : equality.getArguments()) {
				result.add(term);
			}
			// i.e. A = A, A is not bound to something else. 
			if (result.size() == 1) {
				result.clear();
			}
		}
		
		return result;
	}

	/**
	 * Returns X = c, if 'expression' is c = X, for X a variable and c a constant, or 'expression' otherwise.
	 * It actually works for any other functor as well. 
	 */
	public static Expression normalize(Expression expression, RewritingProcess process) {
		if (expression.numberOfArguments() == 2
				&& process.isConstant(expression.get(0))) {
			if ( ! process.isConstant(expression.get(1))) {
				Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(expression.getFunctor(), expression.get(1), expression.get(0));
				return result;
			}
		}
		return expression;
	}

	/**
	 * Returns a conjunction of equalities between the corresponding elements of two lists
	 * (two elements are corresponding if they have the indices).
	 * Returns false if the lists have different sizes.
	 */
	public static Expression makePairwiseEquality(List<Expression> list1, List<Expression> list2) {
		if (list1.size() == list2.size()) {
			Expression result = And.make(Expressions.makePairwiseApplications(FunctorConstants.EQUAL, list1, list2));
			return result;
		}
		return Expressions.FALSE;
	}

	/**
	 * Given an equality literal L (using disequality or equality, the latter with possibly many arguments) and a variable V,
	 * returns a pair of equality literals (L1, L2) such that "L1 and L2 <=> L", and such that the only variable in L1 is V.
	 * Returns <code>null</code> if it is not an equality literal.
	 */
	public static Pair<Expression, Expression> separateVariableLiteral(Expression variable, Expression literal, RewritingProcess process) {
		Pair<Expression, Expression> result;
		if (literal.equals(Expressions.TRUE) | literal.equals(Expressions.FALSE)) {
			result = Pair.make(literal, literal);
		}
		else if (literal.hasFunctor(FunctorConstants.DISEQUALITY)) {
			if ( ! literal.getArguments().contains(variable)) {
				result = Pair.make(Expressions.TRUE, literal);
			}
			else {
				result = Pair.make(literal, Expressions.TRUE); // this holds because disequalities have only two arguments
			}
		}
		else if (literal.hasFunctor(FunctorConstants.EQUALITY)) {
			if ( ! literal.getArguments().contains(variable)) {
				result = Pair.make(Expressions.TRUE, literal);
			}
			else {
				Pair<Set<Expression>, Set<Expression>> variablesAndConstants = getVariablesListAndConstantsList(literal, process);
				Set<Expression> constants = variablesAndConstants.second;
				if (constants.size() > 1) { // can't be equal to distinct constants
					result = Pair.make(Expressions.FALSE, Expressions.FALSE);
				}
				else {
					Set<Expression> variables = variablesAndConstants.first;
					variables.remove(variable);
					List<Expression> others = new LinkedList<Expression>(variables);
					others.addAll(constants);
					if (others.isEmpty()) {
						result = Pair.make(Expressions.TRUE, Expressions.TRUE); // only argument is V, so it is saying nothing
					}
					else {
						Expression representativeToBeEqualledToVariable = Util.getLast(others); // we get the last one because it is best to use a constant if there is one
						Expression variableLiteral = Equality.make(variable, representativeToBeEqualledToVariable);
						Expression othersLiteral   = Equality.make(others);
						result = Pair.make(variableLiteral, othersLiteral);
					}
				}
			}
		}
		else {
			throw new Error(literal + " is not an equality literal as required by Equality.separateVariableLiteral");
		}
		return result;
	}
	
	/** 
	 * Receives an equality T1 = T2 = ... = Tn and an expression E.
	 * If n = 1 and E is equal to Ti, then returns Tj where j != i (the other term).
	 * Otherwise, returns <code>null</code>. 
	 */
	public static Expression getWhatExpressionIsComparedToIfUniqueOrNull(Expression equality, Expression expression) {
		Expression result;
		if (equality.numberOfArguments() == 2) {
			if (equality.get(0).equals(expression)) {
				result = equality.get(1);
			}
			else if (equality.get(1).equals(expression)) {
				result = equality.get(0);
			}
			else {
				result = null;
			}
		}
		else {
			result = null;
		}
		return result;
	}

	/**
	 * Checks an expression for equality or disequality (top) simplification.
	 * @param expression
	 * @param process
	 */
	public static Expression simplifyIfEqualityOrDisequality(Expression expression, RewritingProcess process) {
		Expression result;
		if (isEquality(expression)) {
			result = simplify(expression, process);
		}
		else if (Disequality.isDisequality(expression)) {
			result = Disequality.simplify(expression, process);
		}
		else {
			result = expression;
		}
		return result;
	}

	/**
	 * Returns TRUE if given equality has all-equal arguments, FALSE if they contain distinct constants,
	 * and the equality itself otherwise.
	 * Note that this is much faster than eliminating duplicates as well, which requires constructing another equality.
	 */
	public static Expression simplify(Expression equality, RewritingProcess process) {
		Expression result;
		if (Util.allEqual(equality.getArguments())) {
			result = Expressions.TRUE;
		}
		else {
			Set<Expression> constants = new LinkedHashSet<Expression>();
			Util.collect(equality.getArguments(), constants, process.getIsConstantPredicate());
			if (constants.size() > 1) {
				result = Expressions.FALSE;
			}
			else {
				result = equality;
			}
		}
		return result;
	}

	/**
	 * Returns an expression equivalent to equality (and perhaps simpler) given equality between a variable and another term.
	 */
	public static Expression simplifyGivenEquality(Expression equality, Expression variable, Expression otherTerm) {
		Expression result;
		if (equality.getArguments().contains(variable) && equality.getArguments().contains(otherTerm)) {
			result = Expressions.TRUE;
		}
		else {
			result = equality;
		}
		return result;
	}

	/**
	 * Returns an expression equivalent to equality (and perhaps simpler) given a disequality.
	 */
	public static Expression simplifyGivenDisequality(Expression equality, Expression variable, Expression otherTerm) {
		Expression result;
		if (equality.getArguments().contains(variable) && equality.getArguments().contains(otherTerm)) {
			result = Expressions.FALSE;
		}
		else {
			result = equality;
		}
		return result;
	}
}
