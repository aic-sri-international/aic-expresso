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
package com.sri.ai.grinder.library.number;

import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.util.Util.list;

import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.collect.LinkedHashMultiset;
import com.google.common.collect.Multiset;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.ExpressionIsSymbolOfType;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.CommutativeAssociative;
import com.sri.ai.grinder.library.CommutativeAssociativeWithOperationOnConstantsOnly;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Triple;

/**
 * Implements a rewriter for the plus operation.
 * 
 * @author braz
 *
 */
@Beta
public class Plus extends CommutativeAssociativeWithOperationOnConstantsOnly {

	private final static Expression            neutralElement              = Expressions.makeSymbol(0);
	private final static Predicate<Expression> isOperableArgumentPredicate = new ExpressionIsSymbolOfType(Number.class);

	@Override
	public Object getFunctor() {
		return PLUS;
	}
	
	@Override
	protected Expression getNeutralElement() {
		return neutralElement;
	}
	
	@Override
	protected Expression getAbsorbingElement() {
		return null; // no absorbing element
	}
	
	@Override
	protected boolean isIdempotent() {
		return false;
	}

	@Override
	protected Predicate<Expression> getIsOperableArgumentSyntaxTreePredicate() {
		return isOperableArgumentPredicate;
	}

	@Override
	@SuppressWarnings("unchecked")
	protected Object operationOnOperableValues(List listOfConstants) {
		return Util.sumArbitraryPrecision(listOfConstants);
	}

	/**
	 * Given a sum, or an expression to be interpreted as a single-term sum,
	 * returns a triple containing a multiset of positive terms, a multiset of negative terms,
	 * and the sum of all numerical constants in it.
	 * If <code>makeDuplicateError</code> is non-null and a duplicate term is found,
	 * the duplicate is passed to it as a parameter and the resulting Error is thrown.
	 * @param expression
	 * @param makeDuplicateError a function that, if non-null, gets a duplicate term and makes an Error to be thrown 
	 * @return
	 */
	public static Triple<Multiset<Expression>, Multiset<Expression>, Integer> gatherPositiveAndNegativeTermsAndConstantInteger(Expression expression, Function<Expression, Error> makeDuplicateError) {
		Triple<Multiset<Expression>, Multiset<Expression>, Integer> result =
				Triple.make(LinkedHashMultiset.create(), LinkedHashMultiset.create(), new Integer(0));
		
		List<Expression> arguments = getSummands(expression);
		for (Expression argument : arguments) {
			if (argument.hasFunctor(MINUS)) {
				Expression negationArgument = argument.get(0);
				if (negationArgument.getValue() instanceof Number) {
					result.third = result.third.intValue() - ((Number) negationArgument.getValue()).intValue(); // note the -  !
				}
				else {
					if (makeDuplicateError != null && result.second.contains(negationArgument)) {
						throw makeDuplicateError.apply(negationArgument);
					}
					else {
						result.second.add(negationArgument);
					}
				}
			}
			else {
				if (argument.getValue() instanceof Number) {
					result.third = result.third.intValue() + ((Number) argument.getValue()).intValue(); // note the +  !
				}
				else {
					if (makeDuplicateError != null && result.second.contains(argument)) {
						throw makeDuplicateError.apply(argument);
					}
					else {
						result.first.add(argument);
					}
				}
			}
		}
		
		return result;
	}

	/**
	 * Makes an addition, automatically accounting for neutral element occurrences.
	 */
	public static Expression make(List<Expression> arguments) {
		return CommutativeAssociative.make("+", arguments, Expressions.ZERO, false);
	}

	/**
	 * Returns the arguments if <code>sum</code> is an application of +, or the expression itself otherwise
	 * (taken to be a "unary sum").
	 * @param sum
	 * @return
	 */
	public static List<Expression> getSummands(Expression sum) {
		List<Expression> result;
		if (sum.hasFunctor(PLUS)) {
			result = sum.getArguments();
		}
		else {
			result = list(sum);
		}
		return result;
	}
}
