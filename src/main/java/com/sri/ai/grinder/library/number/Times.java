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

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.ExpressionIsSymbolOfType;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.CommutativeAssociative;
import com.sri.ai.grinder.library.CommutativeAssociativeOnNumbers;
import com.sri.ai.grinder.library.CommutativeAssociativeWithOperationOnJavaConstantsOnly;
import com.sri.ai.grinder.rewriter.api.Simplifier;
import com.sri.ai.util.Util;

/**
 * @author braz
 *
 */
@Beta
public class Times extends CommutativeAssociativeWithOperationOnJavaConstantsOnly implements Simplifier {

	private final static Predicate<Expression> isOperableArgumentPredicate = new ExpressionIsSymbolOfType(Number.class);

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		// takes care of infinity arguments before deferring to super method
		if ( ! expression.hasFunctor(getFunctor())) {
			return expression;
		}
		Expression result = CommutativeAssociativeOnNumbers.dealWithInfinity(expression, context, (e, p) -> super.apply(e, p));
		return result;
	}

	@Override
	public Object getFunctor() {
		return "*";
	}
	
	@Override
	protected Expression getNeutralElement() {
		return ONE;
	}
	
	@Override
	protected Expression getAbsorbingElement() {
		return ZERO;
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
		return Util.productArbitraryPrecision(listOfConstants);
	}
	
	/**
	 * Makes a product, automatically accounting for neutral element occurrences.
	 */
	public static Expression make(Expression... arguments) {
		return CommutativeAssociative.make("*", Arrays.asList(arguments), ZERO, ONE, false);
	}

	/**
	 * Makes a product, automatically accounting for neutral element occurrences.
	 */
	public static Expression make(List<Expression> arguments) {
		return CommutativeAssociative.make("*", arguments, ZERO, ONE, false);
	}

	/**
	 * Same as {@link CommutativeAssociative#make(Iterator<Expression>, Object, Object, Object)},
	 * but not requiring the parameters already determined for times applications.
	 */
	public static Expression make(Iterator<Expression> argumentsIterator) {
		return CommutativeAssociative.make("*", argumentsIterator, ZERO, ONE, false);
	}

	/**
	 * Returns the list of multiplicands in a product expression,
	 * including a singleton list with the expression itself if it is not a product
	 * (since then it can be considered the only multiplicand in a "unary product").
	 */
	public static List<Expression> getMultiplicands(Expression expression) {
		if (expression.hasFunctor("*")) {
			return expression.getArguments();
		}
		return Util.list(expression);
	}

	/**
	 * Returns the list of multiplicands in a product expression,
	 * including a singleton list with the expression itself if it is not a product
	 * (since then it can be considered the only multiplicand in a "unary product").
	 */
	public static List<Expression> getMultiplicands(Expression expression, Context context) {
		if (expression.hasFunctor("*")) {
			return expression.getArguments();
		}
		return Util.list(expression);
	}
}
