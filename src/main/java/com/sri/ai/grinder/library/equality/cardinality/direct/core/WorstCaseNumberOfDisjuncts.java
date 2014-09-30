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
package com.sri.ai.grinder.library.equality.cardinality.direct.core;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * Computes the worst-case number of disjuncts in a DNF equivalent to a given boolean formula
 * (using boolean connectives and quantifiers -- all else is considered an atom) in linear time.
 * This is useful in model counting, where one can obtain the number of models of a formula
 * from the formula itself, or from its negation,
 * (and then subtracting from the total number of possible interpretations),
 * because the worst case number of disjunctions provides a
 * heuristic for the worst case time complexity of each of them.
 * 
 * worstCaseNumberOfDisjuncts(F, sign)
 * Returns a worst-case estimate of the number of disjuncts of the DNF equivalent to formula F, if sign is true, or to not F, if sign is false.
 * 
 * if F is not F'
 * 	return worstCaseNumberOfDisjuncts(F', not sign)
 * 
 * if F is F1 and ... and Fn
 * 	if sign
 * 		return prod_i worstCaseNumberOfDisjuncts(Fi, sign)
 * 	else // sign is negative, this will be a disjunction in the DNF.
 * 		return sum_i worstCaseNumberOfDisjuncts(Fi, sign)
 * 
 * if F is F1 or ... or Fn
 * 	if sign
 * 		return sum_i worstCaseNumberOfDisjuncts(Fi, sign)
 * 	else // sign is negative, this will be a conjunction in the DNF.
 * 		return prod_i worstCaseNumberOfDisjuncts(Fi, sign)
 * 
 * if F is there exists X : F'
 * 	return worstCaseNumberOfDisjuncts(F', sign)
 * 
 * if F is for all X : F'
 * 	return worstCaseNumberOfDisjuncts(F', sign)
 * 
 * // if the expression is not the application of a boolean connective as above,
 * // we assume an atom:
 * return 1
 * 
 * @author braz
 */
@Beta
public class WorstCaseNumberOfDisjuncts {
	
	private static final Recursive POSITIVE_RECURSIVE_CALL = new Recursive(true);
	private static final Recursive NEGATIVE_RECURSIVE_CALL = new Recursive(false);

	/**
	 * Returns {@link #get(Expression, boolean)} with sign equal to true.
	 */
	public static int get(Expression expression) {
		return get(expression, true);
	}
	
	/**
	 * Computes the worst-case number of disjuncts of a DNF equivalent to a given formula,
	 * given a "sign", that is, whether the formula is not negated (sign 'true')
	 * or negated (sign 'false').
	 * A false sign makes the computation consider negated sub-expressions
	 * as non-negated, conjunctions as disjunctions, and disjunctions as conjunctions.
	 * The use of a sign allows for greater efficiency, by keeping track of formulas that would
	 * be inverted by DeMorgan's law without actually doing it.
	 * For example, the result for formula "X0 = a0 or not(X1 = a1 or ... or Xn = an)" is 2
	 * because the negated disjunction would become a conjunction in the equivalent DNF.
	 * Instead of actually computing such conjunction, we simply perform a recursive call
	 * with negated sign, which will consider the disjunction a conjunction, and return result 1,
	 * which combined with X0 = a0 results in 2.
	 */
	public static int get(Expression expression, boolean sign) {
		
		if (expression.hasFunctor(FunctorConstants.NOT)) {
			return get(expression.get(0), ! sign);
		}
		
		if (expression.hasFunctor(FunctorConstants.AND)) {
			if (sign) {
				return Util.product(new FunctionIterator<Expression, Number>(POSITIVE_RECURSIVE_CALL /* sign is true */, expression.getArguments())).intValue();
			}
			else {
				return Util.sum(new FunctionIterator<Expression, Number>(NEGATIVE_RECURSIVE_CALL /* sign is false */, expression.getArguments())).intValue();
			}
		}

		if (expression.hasFunctor(FunctorConstants.OR)) {
			if (sign) {
				return Util.sum(new FunctionIterator<Expression, Number>(POSITIVE_RECURSIVE_CALL /* sign is true */, expression.getArguments())).intValue();
			}
			else {
				return Util.product(new FunctionIterator<Expression, Number>(NEGATIVE_RECURSIVE_CALL /* sign is false */, expression.getArguments())).intValue();
			}
		}

		if (expression.getSyntacticFormType().equals(ThereExists.SYNTACTIC_FORM_TYPE)
				||
				expression.hasFunctor(FunctorConstants.THERE_EXISTS)) {
			return get(ThereExists.getBody(expression), sign);
		}

		if (expression.getSyntacticFormType().equals(ForAll.SYNTACTIC_FORM_TYPE)
				||
				expression.hasFunctor(FunctorConstants.FOR_ALL)) {
			return get(ForAll.getBody(expression), sign);
		}

		if (expression.hasFunctor(FunctorConstants.EQUAL)) {
			// negation of equalities produce one disjunction for each argument but the last.
			return expression.numberOfArguments() - 1;
		}
		
		// if the expression is not the application of a boolean connective as above,
		// we assume an atom that does not produce disjuncts when negated.
		return 1;
	}
	
	/** A recursive call to {@link #get(Expression, boolean)} as a {@link Function}. */
	private static class Recursive implements Function<Expression, Number> {
		private boolean sign;
		
		public Recursive(boolean sign) {
			this.sign = sign;
		}

		@Override
		public Number apply(Expression expression) {
			return WorstCaseNumberOfDisjuncts.get(expression, sign);
		}
	}
}
