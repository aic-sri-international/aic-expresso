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
package com.sri.ai.grinder.sgdpllt.library.number;

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Switch;

/**
 * A {@link Rewriter} with common numeric functions:
 * 
 * <ul>
 * <li> arithmetic (<code>+, -, *, /</code>)
 * <li> inequalities (<code><, <=, >=, ></code>)
 * </ul>
 * 
 * @author braz
 *
 */
@Beta
public class NumericSimplifier extends Switch<String> {
	
	public NumericSimplifier() {
		super(Switch.FUNCTOR, makeFunctionApplicationSimplifiers());
	}
	
	private static Simplifier plus = new Plus();
	private static Simplifier times = new Times();
	private static Simplifier max = new Max();

	public static Map<String, Rewriter> makeFunctionApplicationSimplifiers() {
		return map(
				FunctorConstants.TIMES,           (Simplifier) (f, context) ->
				times.apply(f, context),

				FunctorConstants.DIVISION,        (Simplifier) (f, context) ->
				Division.simplify(f),

				FunctorConstants.PLUS,            (Simplifier) (f, context) ->
				plus.apply(f, context),

				FunctorConstants.MAX,            (Simplifier) (f, context) ->
				max.apply(f, context),

				FunctorConstants.MINUS,           (Simplifier) (f, context) ->
				(f.numberOfArguments() == 2? Minus.simplify(f) : f.numberOfArguments() == 1? UnaryMinus.simplify(f) : f),

				FunctorConstants.EXPONENTIATION,  (Simplifier) (f, context) ->
				Exponentiation.simplify(f, context),

				FunctorConstants.LESS_THAN,       (Simplifier) (f, context) ->
				LessThan.simplify(f, context),

				FunctorConstants.LESS_THAN_OR_EQUAL_TO,     (Simplifier) (f, context) ->
				LessThanOrEqualTo.simplify(f, context),

				FunctorConstants.GREATER_THAN,              (Simplifier) (f, context) ->
				GreaterThan.simplify(f, context),

				FunctorConstants.GREATER_THAN_OR_EQUAL_TO,  (Simplifier) (f, context) ->
				GreaterThanOrEqualTo.simplify(f, context)
				);
	}
}