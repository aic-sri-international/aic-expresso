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
package com.sri.ai.grinder.library.boole;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.collect.Lists;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;

/**
 * A collection of utility routines related to handling boolean expressions.
 * 
 * @author saadati
 *
 */
@Beta
public class BooleanUtil {

	/**
	 * 
	 * @param expression
	 * @return removes any duplicates from expression
	 */
	public static Expression removeUnnecessary(Expression expression) {
		if (expression.getSyntacticFormType().equals("Function application")) {
			Expression functor = expression.getFunctor();
			if ( And.isConjunction(expression) || Or.isDisjunction(expression) ) {
				boolean isConjunction = And.isConjunction(expression);
				List<Expression> arguments = expression.getArguments();
				ArrayList<Expression> newArguments = new ArrayList<Expression>();
				for (Expression arg: arguments) {
					if ( (isConjunction && arg.equals(Expressions.TRUE)) || (!isConjunction && arg.equals(Expressions.FALSE)) ) {
						continue;
					}
					boolean isDuplicate = false;
					Expression arg2 = removeUnnecessary(arg);
					for (Expression nArg: newArguments) {
						if ( areEquivalent(arg2, nArg) ) {
							isDuplicate = true;
							break;
						}
					}
					if ( !isDuplicate ) {
						newArguments.add(arg2);
					}
				}
				if ( newArguments.size() == 1 ) {
					return newArguments.get(0);
				} 
				else {
					return Expressions.make(functor, newArguments);
				}
			} 
			else if ( functor.equals(Not.FUNCTOR) ) {
				return Expressions.apply(functor, removeUnnecessary(expression.get(0)));
			}
		}
		return expression;
	}
		
	private static boolean areEquivalent(Expression literal1, Expression literal2) {
		if ( literal1.hasFunctor(Not.FUNCTOR) ) {
			if ( literal2.hasFunctor(Not.FUNCTOR) ) {
				return areEquivalent(literal1.get(0), literal2.get(0));
			} 
			else if ( literal2.hasFunctor(Disequality.FUNCTOR) ) {
				return areEquivalent(literal1.get(0), Expressions.apply(Equality.FUNCTOR, literal2.get(0), literal2.get(1)));
			}
		} 
		else if ( literal2.hasFunctor(Not.FUNCTOR) ) {
			if ( literal1.hasFunctor(Disequality.FUNCTOR) ) {
				return areEquivalent(literal2.get(0), Expressions.apply(Equality.FUNCTOR, literal1.get(0), literal1.get(1)));
			}
		} 
		else if ( literal1.hasFunctor(Disequality.FUNCTOR) && literal2.hasFunctor(Disequality.FUNCTOR) ) {
			return pairwiseEquivalent(literal1.get(0), literal1.get(1), literal2.get(0), literal2.get(1));
		}
		else if ( literal1.hasFunctor(Equality.FUNCTOR) && literal2.hasFunctor(Equality.FUNCTOR) ) {
			for (Expression term1: literal1.getArguments()) {
				boolean conjunction = true;
				for (Expression term2: literal2.getArguments()) {
					conjunction = conjunction && term1.equals(term2);
					if ( !conjunction ) break;
				}
				if ( conjunction ) return true;
			}
			//return pairwiseEquivalent(literal1.get(0), literal1.get(1), literal2.get(0), literal2.get(1));
		}
		return false;
	}

	private static boolean pairwiseEquivalent(Expression x1, Expression y1, Expression x2, Expression y2) {
		return (x1.equals(x2) && y1.equals(y2)) || (x1.equals(y2) && y1.equals(x2));
		
	}

	/**
	 * 
	 * @param expression
	 * @return true if expression is of the form "x=y" or "not (x != y)"
	 */
	public static boolean isEquality(Expression expression) {
		return (expression.hasFunctor(Equality.FUNCTOR) && expression.getArguments().size()==2) || 
				(expression.hasFunctor(Not.FUNCTOR) && isNotEquality(expression.get(0)));
	}
	
	/**
	 * 
	 * @param expression
	 * @return true if expression is of the form x=y=...=z"
	 */
	public static boolean isMultiEquality(Expression expression) {
		return (expression.hasFunctor(Equality.FUNCTOR) && expression.getArguments().size()>2);
	}

	public static Expression expandMultiEquality(Expression expression, Expression mainVar) {
		Expression result = expression;
		if ( isMultiEquality(expression) && expression.getArguments().contains(mainVar) ) {
			ArrayList<Expression> newEqualities = new ArrayList<Expression>();
			for (Expression var: expression.getArguments()) {
				if ( !var.equals(mainVar) ) {
					Expression newEq = Equality.make(mainVar, var);
					newEqualities.add(newEq);
				}
			}
			result = And.make(newEqualities);
		}
		return result;
	}
	
	/**
	 * 
	 * @param expression
	 * @return true if expression is either an equality or a disequality
	 */
	public static boolean isLiteral(Expression expression) {
		return isEquality(expression) || isNotEquality(expression);
	}

	/**
	 * 
	 * @param expression
	 * @return true if expression is either of the form "x=y" or "not (x != y)"
	 */
	public static boolean isNotEquality(Expression expression) {
		return (expression.hasFunctor(Not.FUNCTOR) && isEquality(expression.get(0))) || expression.hasFunctor(Disequality.FUNCTOR);
	}

	/**
	 * 
	 * @param expression
	 * @return true if expression is a conjunction
	 */
	public static boolean isConjunction(Expression expression) {
		return expression.hasFunctor(And.FUNCTOR);
	}

	/**
	 * 
	 * @param expression
	 * @return true if expression is a disjunction
	 */
	public static boolean isDisjunction(Expression expression) {
		return expression.hasFunctor(Or.FUNCTOR);
	}

	/**
	 * 
	 * @param expression
	 * @return true if expression is an implication
	 */
	public static boolean isImplication(Expression expression) {
		return expression.hasFunctor(Implication.FUNCTOR);
	}

	/**
	 * 
	 * @param expression
	 * @return true if expression is an Equivalence
	 */
	public static boolean isEquivalence(Expression expression) {
		return expression.hasFunctor(Equivalence.FUNCTOR);
	}

	/**
	 * 
	 * @param expression
	 * @return true if expression is a disjunction
	 */
	public static boolean isNegation(Expression expression) {
		return expression.hasFunctor(Not.FUNCTOR);
	}

	/**
	 * 
	 * @param expression
	 * @param element
	 * @return true if element occurs in expression
	 */
	public static boolean occurs(Expression expression, Expression element) {
		return Expressions.containsAnyOf(expression, Lists.newArrayList(element));
	}

	/**
	 * 
	 * @param variable
	 * @return |type(X)| if variable is X
	 */
	public static Expression sizeof(Expression var, RewritingProcess process) {
		CardinalityTypeOfLogicalVariable cardLogical = new CardinalityTypeOfLogicalVariable();
		Expression card = Expressions.apply(FunctorConstants.CARDINALITY, var);
		Expression result = cardLogical.rewrite(card, process);
		return result;	
	}
	
	public static Set<Expression> getTerms(Expression expression) {
		Set<Expression> set = new LinkedHashSet<Expression>();
		if ( expression.hasFunctor(Equality.FUNCTOR) || expression.hasFunctor(Disequality.FUNCTOR) ) {
			set.add(expression.get(0));
			set.add(expression.get(1));
		} 
		else if ( expression.hasFunctor(Not.FUNCTOR) ) {
			set.addAll(getTerms(expression.get(0)));
		}
		else if ( expression.hasFunctor(Or.FUNCTOR) || expression.hasFunctor(And.FUNCTOR) || expression.hasFunctor(Implication.FUNCTOR)) {
			for (Expression arg: expression.getArguments()) {
			set.addAll(getTerms(arg));
			}
		}
		return set;
	}

}
