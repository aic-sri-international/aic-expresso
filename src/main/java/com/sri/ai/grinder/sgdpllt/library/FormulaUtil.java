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
package com.sri.ai.grinder.sgdpllt.library;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUAL;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.NOT;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAll;
import com.sri.ai.grinder.sgdpllt.library.boole.Or;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExists;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;

/**
 * A collection of utility routines specific to formulas.
 * 
 * @author oreilly
 *
 */
@Beta
public class FormulaUtil {
	
	/**
	 * Determine if an expression is a formula.
	 * 
	 * @param expression
	 *            the expression to be tested if it is a formula.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if the expression passed in is a formula, false otherwise.
	 */
	public static boolean isFormula(Expression expression, Context context) {
		boolean result = false;
		
		// the Boolean constants False and True are formulas;
		if (expression.equals(TRUE) || expression.equals(FALSE)) {
			result = true; 
		} 
		// if alpha and beta are variable or constant symbols of finite types,
		// then alpha = beta is a formula.
		else if ((expression.hasFunctor(EQUAL) || expression.hasFunctor(DISEQUALITY)) &&
				expression.numberOfArguments() > 1) {
			// in this case assume is a formula till proven otherwise
			result = true;
			for (Expression arg : expression.getArguments()) {
				if (!(context.isVariable(arg) || isLegalFormulaConstant(arg, context))) {
					// is not a formula.
					result = false; 
					break;
				}
			}
		} 
		// if phi is a formula, then not(phi) is a formula
		else if (expression.hasFunctor(NOT) && 
				 expression.numberOfArguments() == 1) {
			result = isFormula(expression.get(0), context);
		}
		// if phi and phi' are formulas, then and(phi, phi'), and or(phi, phi') are formulas
		else if (expression.hasFunctor(FunctorConstants.AND) ||
				 expression.hasFunctor(FunctorConstants.OR)) {
			// in this case assume is a formula till proven otherwise.
			result = true;
			for (Expression arg : expression.getArguments()) {
				if (!(result = isFormula(arg, context))) {
					// is not a formula
					break;
				}
			} 
		}
		// if phi and phi' are formulas then phi => phi' and phi <=> phi' are formulas
		else if ((expression.hasFunctor(FunctorConstants.IMPLICATION) ||
				  expression.hasFunctor(FunctorConstants.EQUIVALENCE)   ) &&
				  expression.numberOfArguments() == 2) {
			result = isFormula(expression.get(0), context) && isFormula(expression.get(1), context);
		}
		// if phi is a formula, then 'exists x phi' is a formula
		else if (expression.getSyntacticFormType().equals(ThereExists.SYNTACTIC_FORM_TYPE)
				||
				expression.hasFunctor(FunctorConstants.THERE_EXISTS)) {
			result = isFormula(ThereExists.getBody(expression), context);
		}
		// if phi is a formula, then 'for all x phi' is a formula
		else if (expression.getSyntacticFormType().equals(ForAll.SYNTACTIC_FORM_TYPE)
				||
				expression.hasFunctor(FunctorConstants.FOR_ALL)) {
			result = isFormula(ForAll.getBody(expression), context);
		}
		// if phi1, phi2 and phi3 are formulas, then 'if ph1 then ph2 else ph3' is a formula
		else if (IfThenElse.isIfThenElse(expression)) {
			result = isFormula(IfThenElse.condition(expression), context)
					&&
					isFormula(IfThenElse.thenBranch(expression), context)
					&&
					isFormula(IfThenElse.elseBranch(expression), context);
		}
		
		return result;
	}
	
	/**
	 * Determine if an expression is a quantifier free formula.
	 * 
	 * @param expression
	 *            the expression to be tested if it is a quantifier free formula.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if the expression passed in is a quantifier free formula, false otherwise.
	 */
	public static boolean isQuantifierFreeFormula(Expression expression, Context context) {
		boolean result = false;

		if (isFormula(expression, context)) {
			result = !Util.thereExists(new SubExpressionsDepthFirstIterator(expression), new Predicate<Expression>() {
				@Override
				public boolean apply(Expression expression) {
					boolean result = ForAll.isForAll(expression) || ThereExists.isThereExists(expression);
					return result;
				}
			});
		}
		
		return result;
	}
	
	/**
	 * Determine if an expression is a legal formula constant.
	 * 
	 * @param expression
	 *            the expression to be tested if it is a legal formula constant.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if the expression passed in is a legal formula constant, false otherwise.
	 */
	public static boolean isLegalFormulaConstant(Expression expression, Context context) {
		boolean result = false;
		
		// Note: the corresponding paper describes a legal constant as being finite but in the
		// implementation we will allow all constants (including numbers).
		if (context.isUniquelyNamedConstant(expression)) {
			result = true;
		}
		
		return result;
	}
	
	/**
	 * Determine if an expression is a finite constant.
	 * 
	 * @param expression
	 *            the expression to be tested if it is a finite constant.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if the expression passed in is a finite constant, false otherwise.
	 */
	public static boolean isFiniteConstant(Expression expression, Context context) {
		boolean result = false;
		
		if (context.isUniquelyNamedConstant(expression)) {
			// if a constant we know its a symbol at least
			Object value = expression.getValue();
			// We only consider string or boolean values to be finite.
			// Expression values are not really constant
			// and numeric values are not finite.
			if (value instanceof String || value instanceof Boolean  ) {
				result = true;
			}
		}
		
		return result;
	}
	
	/**
	 * Extract the constants from the given formula. This is done by checking
	 * the terms of any diseaulities or equalities in the formula as to whether
	 * or not they are constants. These are the only locations constants should
	 * exist in a formula.
	 * 
	 * @param formula
	 *            the formula constants are to be collected from.
	 * @param context
	 *            the current context, which is used to identify if a
	 *            term is a constant.
	 * @return the set of constants present in the given formula (may be empty).
	 */
	public static Set<Expression> getConstants(Expression formula, Context context) {
		Set<Expression> consts = new LinkedHashSet<Expression>();
		
		Iterator<Expression> subExpressionsIterator =  new SubExpressionsDepthFirstIterator(formula);
		while (subExpressionsIterator.hasNext()) {
			Expression expression = subExpressionsIterator.next();
			if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
				for (Expression term : expression.getArguments()) {
					if (context.isUniquelyNamedConstant(term)) {
						consts.add(term);
					}
				}
			}
		}
		
		return consts;
	}
	
	/**
	 * Retrieve the postive literals in a formula.
	 * 
	 * @param formula
	 *            a formula.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return the positive literals in the formula.
	 */
	public static Set<Expression> getPositiveLiterals(Expression formula, Context context) {
		Set<Expression> result = Expressions.getSubExpressionsSatisfying(formula, new Predicate<Expression>() {
			@Override
			public boolean apply(Expression arg) {
				return Equality.isEquality(arg);
			}
		});
		
		return result;
	}
	
	/**
	 * Retrieve the negative literals in a formula.
	 * 
	 * @param formula
	 *            a formula.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return the negative literals in the formula.
	 */
	public static Set<Expression> getNegativeLiterals(Expression formula, Context context) {
		Set<Expression> result = Expressions.getSubExpressionsSatisfying(formula, new Predicate<Expression>() {
			@Override
			public boolean apply(Expression arg) {
				return Disequality.isDisequality(arg);
			}
		});
		
		return result;
	}
	
	/**
	 * Determine if the given expression is in NNF form.
	 * 
	 * @param expression
	 *            the expression to be tested if in NNF form.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if is in NNF form, false otherwise.
	 */
	public static boolean isNNF(Expression expression, final Context context) {
		boolean result = false;

		if (isFormula(expression, context)) {
			result = !Util.thereExists(new SubExpressionsDepthFirstIterator(expression), new Predicate<Expression>() {
				@Override
				public boolean apply(Expression expression) {
					boolean result = false;
					if (Equality.isEquality(expression) || Disequality.isDisequality(expression)) {
						result = !isLiteral(expression, context);
					}
					else {
						result = ForAll.isForAll(expression) || 
								 ThereExists.isThereExists(expression) ||
								 Expressions.hasFunctor(expression, FunctorConstants.IMPLICATION) ||
								 Expressions.hasFunctor(expression, FunctorConstants.EQUIVALENCE) ||
								 Expressions.hasFunctor(expression, FunctorConstants.NOT); // not(...) around literals should be collapsed to != or =.
					}
					return result;
				}
			});
		}

		return result;
	}
	
	/**
	 * Determine if the given expression is in CNF form.
	 * 
	 * @param expression
	 *            the expression to be tested if in CNF form.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if is in CNF form, false otherwise.
	 */
	public static boolean isCNF(Expression expression, Context context) {
		boolean result = false;

		if (And.isConjunction(expression) && expression.numberOfArguments() > 0) {
			result = true;
			// Each conjunct must be a clause.
			for (Expression conjunct : expression.getArguments()) {
				if (!isClause(conjunct, context)) {
					result = false;
					break;
				}
			}

		}

		return result;
	}
	
	/**
	 * Determine if the given expression is in DNF form.
	 * 
	 * @param expression
	 *            the expression to be tested if in DNF form.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if is in DNF form, false otherwise.
	 */
	public static boolean isDNF(Expression expression, Context context) {
		boolean result = false;

		if (Or.isDisjunction(expression) && expression.numberOfArguments() > 0) {
			result = true;
			// Each disjunct must be a conjunction of literals.
			for (Expression disjunct : expression.getArguments()) {
				if (!isConjunctionOfLiterals(disjunct, context)) {
					result = false;
					break;
				}
			}
		}

		return result;
	}

	/**
	 * Determine if the given expression is a Clause (i.e. a disjunction of
	 * literals only).
	 * 
	 * @param expression
	 *            the expression to be tested if a clause.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if a clause, false otherwise.
	 */
	public static boolean isClause(Expression expression, Context context) {
		boolean result = false;
		
		if (Or.isDisjunction(expression) && expression.numberOfArguments() > 0) {
			result = true;
			// Each disjunct must be a literal (no nesting allowed).
			for (Expression disjunct : expression.getArguments()) {
				if (!isLiteral(disjunct, context)) {
					result = false;
					break;
				}
			}
		}

		return result;
	}
	
	/**
	 * Determine if the given expression is a conjunction of literals
	 * 
	 * @param expression
	 *            the expression to be tested if a clause.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if a conjunction of literals, false otherwise.
	 */
	public static boolean isConjunctionOfLiterals(Expression expression, Context context) {
		boolean result = false;
		
		if (And.isConjunction(expression) && expression.numberOfArguments() > 0) {
			result = true;
			// Each conjunct must be a literal (no nesting allowed).
			for (Expression conjunct : expression.getArguments()) {
				if (!isLiteral(conjunct, context)) {
					result = false;
					break;
				}
			}
		}

		return result;
	}

	/**
	 * Determine if the given expression is a literal, i.e. an equality or
	 * disequality with 2 arguments, i.e. we exclude 'X = Y = Z' types of
	 * equality expressions and dont consider not(X = y) or not(X != Y) as literals
	 * in order to work with a simple form for literals.
	 * 
	 * @param expression
	 *            the expression to be tested whether or not is a 2 argument
	 *            equality or inequality.
	 * @param context
	 *            the context in which the expression is being used.
	 * @return true if is a 2 argument equality or inequality.
	 */
	public static boolean isLiteral(Expression expression, Context context) {
		boolean result = false;

		if (Equality.isEquality(expression)
				|| Disequality.isDisequality(expression)) {
			if (expression.numberOfArguments() == 2) {
				if ((isLegalFormulaConstant(expression.get(0), context) || context.isVariable(expression.get(0))) &&
					(isLegalFormulaConstant(expression.get(1), context) || context.isVariable(expression.get(1)))	) {
					result = true;
				}	
			}
		}

		return result;
	}

	/**
	 * Indicates whether an expression is a conditional formula, that is,
	 * an if-then-else expression with formulas in its then and else branches.
	 */
	public static boolean isConditionalFormula(Expression expressionF, Context context) {
		boolean result =
				IfThenElse.isIfThenElse(expressionF) &&
				isFormula(IfThenElse.thenBranch(expressionF), context) &&
				isFormula(IfThenElse.elseBranch(expressionF), context);
		return result;
	}

	/**
	 * Picks first atom in quantifier-free formula satisfying a given predicate.
	 */
	public static Expression pickAtomSatisfyingPredicateFromQuantifierFreeFormula(Expression formula, final Predicate<Expression> predicate, final Context context) {
		Function<Expression, Expression> recursiveCallFunction = new Function<Expression, Expression>() {
			@Override
			public Expression apply(Expression input) {
				return pickAtomSatisfyingPredicateFromQuantifierFreeFormula(input, predicate, context);
			}
		};
		Expression result = pickAtomSatisfyingPredicateFromQuantifierFreeFormula(formula, predicate, recursiveCallFunction, context);
		return result;
	}

	private static Expression pickAtomSatisfyingPredicateFromQuantifierFreeFormula(Expression formula, Predicate<Expression> predicate, Function<Expression, Expression> recursiveCall, Context context) {
		Expression result;
		if (formula.equals(Expressions.TRUE) || formula.equals(Expressions.FALSE)) {
			result = null;
		}
		else if (functorIsALogicalConnectiveIncludingConditionals(formula)) {
			result = Util.getFirstNonNullResultOrNull(formula.getArguments(), recursiveCall);
		}
		else {
			// it is an atom, so now see if it a literal of interest
			result = predicate.apply(formula)? formula : null;
		}
		
		return result;
	}

	final public static Set<Expression> LOGICAL_CONNECTIVES =
	Util.set(
			Expressions.makeSymbol(FunctorConstants.NOT),         
			Expressions.makeSymbol(FunctorConstants.AND),         Expressions.makeSymbol(FunctorConstants.OR),
			Expressions.makeSymbol(FunctorConstants.IMPLIED),     Expressions.makeSymbol(FunctorConstants.IMPLICATION),
			Expressions.makeSymbol(FunctorConstants.EQUIVALENCE));

	final public static Set<Expression> LOGICAL_CONNECTIVES_INCLUDING_CONDITIONALS =
	Util.set(
			Expressions.makeSymbol(FunctorConstants.NOT),         Expressions.makeSymbol(FunctorConstants.IF_THEN_ELSE),
			Expressions.makeSymbol(FunctorConstants.AND),         Expressions.makeSymbol(FunctorConstants.OR),
			Expressions.makeSymbol(FunctorConstants.IMPLIED),     Expressions.makeSymbol(FunctorConstants.IMPLICATION),
			Expressions.makeSymbol(FunctorConstants.EQUIVALENCE));

	final public static Set<Expression> EQUALITY_FORMULAS_PRIMITIVE_SYMBOLS =
	Util.set(
			Expressions.TRUE,                                     Expressions.FALSE,
			Expressions.makeSymbol(FunctorConstants.EQUALITY),    Expressions.makeSymbol(FunctorConstants.DISEQUALITY),
			Expressions.makeSymbol(FunctorConstants.NOT),         Expressions.makeSymbol(FunctorConstants.IF_THEN_ELSE),
			Expressions.makeSymbol(FunctorConstants.AND),         Expressions.makeSymbol(FunctorConstants.OR),
			Expressions.makeSymbol(FunctorConstants.IMPLIED),     Expressions.makeSymbol(FunctorConstants.IMPLICATION),
			Expressions.makeSymbol(FunctorConstants.EQUIVALENCE));

	final public static Set<Expression> PROPOSITIONAL_FORMULAS_PRIMITIVE_SYMBOLS_INCLUDING_CONDITIONALS =
	Util.set(
			Expressions.TRUE,                                     Expressions.FALSE,
			Expressions.makeSymbol(FunctorConstants.NOT),         Expressions.makeSymbol(FunctorConstants.IF_THEN_ELSE),
			Expressions.makeSymbol(FunctorConstants.AND),         Expressions.makeSymbol(FunctorConstants.OR),
			Expressions.makeSymbol(FunctorConstants.IMPLIED),     Expressions.makeSymbol(FunctorConstants.IMPLICATION),
			Expressions.makeSymbol(FunctorConstants.EQUIVALENCE));

	/**
	 * Indicates whether an expression is logical connective application, including if then else
	 * (does not check sub-expressions).
	 */
	public static boolean functorIsALogicalConnectiveIncludingConditionals(Expression formula) {
		boolean result = LOGICAL_CONNECTIVES_INCLUDING_CONDITIONALS.contains(formula.getFunctor());
		return result;
	}

	/**
	 * Indicates whether this is a symbol in interpreted propositional logic (including if-then-else),
	 * or the function application of one.
	 */
	public static boolean isInterpretedInPropositionalLogicIncludingConditionals(Expression formula) {
		boolean result = 
				PROPOSITIONAL_FORMULAS_PRIMITIVE_SYMBOLS_INCLUDING_CONDITIONALS.contains(formula) ||
				PROPOSITIONAL_FORMULAS_PRIMITIVE_SYMBOLS_INCLUDING_CONDITIONALS.contains(formula.getFunctor());
		return result;
	}

	/**
	 * Indicates whether an expression is logical connective application.
	 */
	public static boolean isApplicationOfBooleanConnective(Expression formula) {
		boolean result = LOGICAL_CONNECTIVES.contains(formula.getFunctor());
		return result;
	}

	/**
	 * Indicates whether an expression is logical connective application, including if then else
	 * (does not check sub-expressions).
	 */
	public static boolean isApplicationOfBooleanConnectiveIncludingConditionals(Expression formula) {
		boolean result = LOGICAL_CONNECTIVES_INCLUDING_CONDITIONALS.contains(formula.getFunctor());
		return result;
	}

	/**
	 * Indicates whether an expression is an equality logical connective application, including if then else
	 * (does not check sub-expressions).
	 */
	public static boolean functorIsAnEqualityLogicalConnectiveIncludingConditionals(Expression formula) {
		boolean result = EQUALITY_FORMULAS_PRIMITIVE_SYMBOLS.contains(formula.getFunctor());
		return result;
	}
}
