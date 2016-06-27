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
import static com.sri.ai.util.Util.arrayList;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.ExpressionIsSymbolOfType;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.CommutativeAssociative;
import com.sri.ai.grinder.library.CommutativeAssociativeOnNumbers;
import com.sri.ai.grinder.library.CommutativeAssociativeWithOperationOnConstantsOnly;
import com.sri.ai.grinder.sgdpll.simplifier.api.TopSimplifier;
import com.sri.ai.util.Util;

/**
 * @author braz
 *
 */
@Beta
public class Plus extends CommutativeAssociativeWithOperationOnConstantsOnly implements TopSimplifier {

	private final static Expression            neutralElement              = Expressions.makeSymbol(0);

	private final static Predicate<Expression> isOperableArgumentPredicate = new ExpressionIsSymbolOfType(Number.class);

	@Override
	public Expression apply(Expression expression, Context context) {
		// takes care of infinity arguments before deferring to super method
		if ( ! expression.hasFunctor(getFunctor())) {
			return expression;
		}
		Expression result = 
				CommutativeAssociativeOnNumbers.dealWithInfinity(
						expression,
						context,
						(e, p) -> super.apply(e, p));
		return result;
	}

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
	 * Makes an addition, automatically accounting for neutral element occurrences.
	 */
	public static Expression make(List<Expression> arguments) {
		return CommutativeAssociative.make("+", arguments, Expressions.ZERO, false);
	}

	/**
	 * Looks at given expression as a sum and returns the terms being summed,
	 * taking into account nested + applications as well as looking at minus applications
	 * as sums of their negative terms.
	 * Considers any other expression as a "unary sum" of itself
	 * (so, if the given expressions is X, the method returns X).
	 * @param sum
	 * @return
	 */
	public static ArrayList<Expression> getSummands(Expression sum) {
		ArrayList<Expression> result;
		if (sum.hasFunctor(PLUS)) {
			result = new ArrayList<Expression>();
			for (Expression argument : sum.getArguments()) {
				result.addAll(getSummands(argument));
			}
		}
		else if (sum.hasFunctor(MINUS) && sum.numberOfArguments() == 2) {
			result = new ArrayList<Expression>();
			result.addAll(getSummands(sum.get(0)));
			result.addAll(getSummands(Expressions.apply(MINUS, sum.get(1))));
		}
		else if (sum.hasFunctor(MINUS) && sum.numberOfArguments() == 1) {
			result = new ArrayList<Expression>();
			for (Expression argument : sum.getArguments()) {
				Util.mapIntoList(getSummands(argument), s -> Expressions.apply(MINUS, s), result);
			}
		}
		else {
			result = arrayList(sum);
		}
		return result;
	}
}
