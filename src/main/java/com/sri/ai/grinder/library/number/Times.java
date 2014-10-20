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

import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.ExpressionIsSymbolOfType;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.CommutativeAssociative;
import com.sri.ai.grinder.library.CommutativeAssociativeWithOperationOnConstantsOnly;
import com.sri.ai.util.Util;

/**
 * Implements a rewriter for the times operation.
 * 
 * @author braz
 *
 */
@Beta
public class Times extends CommutativeAssociativeWithOperationOnConstantsOnly {

	private final static Expression            neutralElement              = Expressions.makeSymbol(1);
	private final static Expression            absorbingElement            = Expressions.makeSymbol(0);
	private final static Predicate<Expression> isOperableArgumentPredicate = new ExpressionIsSymbolOfType(Number.class);

	@Override
	public Object getFunctor() {
		return "*";
	}
	
	@Override
	protected Expression getNeutralElement() {
		return neutralElement;
	}
	
	@Override
	protected Expression getAbsorbingElement() {
		return absorbingElement;
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
	public static Expression make(List<Expression> arguments) {
		return CommutativeAssociative.make("*", arguments, Expressions.ZERO, Expressions.ONE);
	}

	/**
	 * Same as {@link CommutativeAssociative#make(Iterator<Expression>, Object, Object, Object)},
	 * but not requiring the parameters already determined for times applications.
	 */
	public static Expression make(Iterator<Expression> argumentsIterator) {
		return CommutativeAssociative.make("*", argumentsIterator, Expressions.ZERO, Expressions.ONE);
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
	public static List<Expression> getMultiplicands(Expression expression, RewritingProcess process) {
		if (expression.hasFunctor("*")) {
			return expression.getArguments();
		}
		return Util.list(expression);
	}
}
