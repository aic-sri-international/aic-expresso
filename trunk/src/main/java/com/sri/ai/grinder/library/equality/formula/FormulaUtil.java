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
package com.sri.ai.grinder.library.equality.formula;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;

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
	 * @param process
	 *            the rewriting process in which the expression is being used.
	 * @return true if the expression passed in is a formula, false otherwise.
	 */
	public static boolean isFormula(Expression expression, RewritingProcess process) {
		boolean result = false;
		
		// the Boolean constants False and True are formulas;
		if (expression.equals(Expressions.TRUE) || expression.equals(Expressions.FALSE)) {
			result = true; 
		} 
		// if alpha and beta are variable or constant symbols of finite types,
		// then alpha = beta is a formula.
		else if ((expression.hasFunctor(FunctorConstants.EQUAL) || expression.hasFunctor(FunctorConstants.INEQUALITY)) &&
				expression.numberOfArguments() > 1) {
			// in this case assume is a formula till proven otherwise
			result = true;
			for (Expression arg : expression.getArguments()) {
				if (!(process.isVariable(arg) || isLegalFormulaConstant(arg, process))) {
					// is not a formula.
					result = false; 
					break;
				}
			}
		} 
		// if phi is a formula, then not(phi) is a formula
		else if (expression.hasFunctor(FunctorConstants.NOT) && 
				 expression.numberOfArguments() == 1) {
			result = isFormula(expression.get(0), process);
		}
		// if phi and phi' are formulas, then and(phi, phi'), and or(phi, phi') are formulas
		else if (expression.hasFunctor(FunctorConstants.AND) ||
				 expression.hasFunctor(FunctorConstants.OR)) {
			// in this case assume is a formula till proven otherwise.
			result = true;
			for (Expression arg : expression.getArguments()) {
				if (!(result = isFormula(arg, process))) {
					// is not a formula
					break;
				}
			} 
		}
		// if phi and phi' are formulas then phi => phi' and phi <=> phi' are formulas
		else if ((expression.hasFunctor(FunctorConstants.IMPLICATION) ||
				  expression.hasFunctor(FunctorConstants.EQUIVALENCE)   ) &&
				  expression.numberOfArguments() == 2) {
			result = isFormula(expression.get(0), process) && isFormula(expression.get(1), process);
		}
		// if phi is a formula, then 'exists x phi' is a formula
		else if (expression.hasFunctor(FunctorConstants.THERE_EXISTS)) {
			result = isFormula(ThereExists.getBody(expression), process);
		}
		// if phi is a formula, then 'for all x phi' is a formula
		else if (expression.hasFunctor(FunctorConstants.FOR_ALL)) {
			result = isFormula(ForAll.getBody(expression), process);
		}
		
		return result;
	}
	
	/**
	 * Determine if an expression is a legal formula constant.
	 * 
	 * @param expression
	 *            the expression to be tested if it is a legal formula constant.
	 * @param process
	 *            the rewriting process in which the expression is being used.
	 * @return true if the expression passed in is a legal formula constant, false otherwise.
	 */
	public static boolean isLegalFormulaConstant(Expression expression, RewritingProcess process) {
		boolean result = false;
		
		// Note: the corresponding paper describes a legal constant as being finite but in the
		// implementation we will allow all constants (including numbers).
		if (process.isConstant(expression)) {
			result = true;
		}
		
		return result;
	}
	
	/**
	 * Determine if an expression is a finite constant.
	 * 
	 * @param expression
	 *            the expression to be tested if it is a finite constant.
	 * @param process
	 *            the rewriting process in which the expression is being used.
	 * @return true if the expression passed in is a finite constant, false otherwise.
	 */
	public static boolean isFiniteConstant(Expression expression, RewritingProcess process) {
		boolean result = false;
		
		if (process.isConstant(expression)) {
			// if a constant we know its a symbol at least
			Object value = ((Symbol) expression).getValue();
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
	 * Determine if the given expression is in CNF form.
	 * 
	 * @param expression
	 *            the expression to be tested if in CNF form.
	 * @return true if is in CNF form, false otherwise.
	 */
	public static boolean isCNF(Expression expression) {
		boolean result = false;

		if (And.isConjunction(expression) && expression.numberOfArguments() > 0) {
			result = true;
			// Each conjunct must be a clause.
			for (Expression conjunct : expression.getArguments()) {
				if (!isClause(conjunct)) {
					result = false;
					break;
				}
			}

		} else {
			result = isClause(expression);
		}

		return result;
	}

	/**
	 * Determine if the given expression is a Clause (i.e. a disjunction of
	 * literals only).
	 * 
	 * @param expression
	 *            the expression to be tested if a clause.
	 * @return true if a clause, false otherwise.
	 */
	public static boolean isClause(Expression expression) {
		boolean result = false;
		
		if (Or.isDisjunction(expression) && expression.numberOfArguments() > 0) {
			result = true;
			// Each disjunct must be a literal (no nesting allowed).
			for (Expression disjunct : expression.getArguments()) {
				if (!isLiteral(disjunct)) {
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
	 * @return true if is a 2 argument equality or inequality.
	 */
	public static boolean isLiteral(Expression expression) {
		boolean result = false;

		if (Equality.isEquality(expression)
				|| Disequality.isDisequality(expression)) {
			if (expression.numberOfArguments() == 2) {
				result = true;
			}
		}

		return result;
	}

}
