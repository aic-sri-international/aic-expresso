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
package com.sri.ai.expresso.api;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.core.DefaultTuple;

/**
 * An {@link Expression} that represents a tuple of sub-expressions.
 * Like a {@link FunctionApplication}, a tuple has a number of arguments;
 * but unlike function applications, it does not have a functor.
 * <p>
 * 
 * @author braz
 */
@Beta
public interface Tuple extends Expression {
	String TUPLE_LABEL = "( . )";
	public final static Tuple EMPTY_TUPLE = new DefaultTuple();
	
	/**
	 * Returns the arguments of a function application expression if this is one.
	 */
	@Override
	List<Expression> getArguments();
	
	/**
	 * Same as {@link #getArguments()}<code>.size()</code>, but potentially more efficient.
	 */
	@Override
	int numberOfArguments();
	
	/**
	 * Same as {@link #getArguments()}<code>.get(i)</code>, but potentially more efficient.
	 */
	@Override
	Expression get(int i);
	
	/**
	 * If this is a tuple,
	 * returns an expression equal to this one, but for replacing the i-th argument by the given one.
	 * Generates an error otherwise.
	 */
	@Override
	Expression set(int i, Expression newIthArgument);
	
	/**
	 * Is given expression a Tuple.
	 * 
	 * @param expression
	 *            to be tested if a Tuple.
	 * @return true if the given expressions is a Tuple, false otherwise.
	 */
	static boolean isTuple(Expression expression) {
		boolean result =
				expression.getSyntaxTree().getLabel().equals(TUPLE_LABEL)
				||
				expression.getSyntaxTree().getLabel().equals("tuple");
		return result;
	}
}
