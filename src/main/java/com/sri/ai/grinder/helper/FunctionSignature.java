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
 * Neither the name of the aic-praise nor the names of its
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
package com.sri.ai.grinder.helper;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;

/**
 * A function signature containing a name and corresponding arity.
 * Originally developed to register which functions are random in PRAiSE Models.
 * 
 * @author braz
 * 
 */
@Beta
public class FunctionSignature {

	public Expression functorOrSymbol;
	public int arity;

	/**
	 * Constructor with a function signature identified by a string of the form:
	 * 
	 * <pre>
	 * name / arity
	 * </pre>
	 * 
	 * e.g.
	 * 
	 * <pre>
	 * p / 1
	 * </pre>
	 * 
	 * @param nameAndArity
	 */
	public FunctionSignature(String nameAndArity) {
		String[] parts = nameAndArity.split("/");
		if (parts.length != 2) {
			throw new IllegalArgumentException(
					"Argument is not of the form name/arity: " + nameAndArity);
		}
		functorOrSymbol = Expressions.makeSymbol(parts[0]);
		arity = Integer.decode(parts[1]);
	}

	public FunctionSignature(Expression expression) {
		this(expression.getFunctorOrSymbol(), expression.numberOfArguments());
	}

	public FunctionSignature(Expression functorOrSymbol, int arity) {
		this.functorOrSymbol = functorOrSymbol;
		this.arity = arity;
	}

	@Override
	public boolean equals(Object another) {
		if (another instanceof FunctionSignature) {
			FunctionSignature anotherFunctionSignature = (FunctionSignature) another;
			boolean result = functorOrSymbol
					.equals(anotherFunctionSignature.functorOrSymbol)
					&& arity == anotherFunctionSignature.arity;
			return result;
		}
		return false;
	}

	@Override
	public int hashCode() {
		if (functorOrSymbol == null) {
			return System.identityHashCode(null) + arity;
		}
		return functorOrSymbol.hashCode() + arity;
	}

	@Override
	public String toString() {
		return functorOrSymbol + "/" + arity;
	}

	public static final Function<Expression, FunctionSignature> MAKER_FROM_EXPRESSION = new Function<Expression, FunctionSignature>() {
		@Override
		public FunctionSignature apply(Expression expression) {
			FunctionSignature result = new FunctionSignature(expression);
			return result;
		}
	};
}
