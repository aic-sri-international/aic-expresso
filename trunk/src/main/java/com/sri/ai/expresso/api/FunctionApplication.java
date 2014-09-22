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

/**
 * An {@link Expression} that represents a function application.
 * 
 * @author braz
 */
@Beta
public interface FunctionApplication extends Expression {
	
	/** Returns the functor if the expression is a function application, or <code>null</code> otherwise. */
	public Expression getFunctor();
	
	/** Indicates whether expression is a function application with given functor. */
	public boolean hasFunctor(Object functor);
	
	/**
	 * Returns the arguments of a function application expression if this is one.
	 */
	public List<Expression> getArguments();
	
	/**
	 * Same as {@link #getArguments()}<code>.size()</code>, but potentially more efficient.
	 */
	public int numberOfArguments();
	
	/**
	 * Same as {@link #getArguments()}<code>.get(i)</code>, but potentially more efficient.
	 */
	public Expression get(int i);
	
	/**
	 * If this is a function application,
	 * returns an expression equal to this one, but for replacing the i-th argument by the given one.
	 * Generates an error otherwise.
	 */
	Expression set(int i, Expression newIthArgument);
	
}
