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

import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.simplifier.RecursiveExhaustiveMapBasedSimplifier;
import com.sri.ai.grinder.library.FunctorConstants;

/**
 * A {@link Simplifier} with common numeric functions:
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
public class NumericSimplifier extends RecursiveExhaustiveMapBasedSimplifier {
	
	public NumericSimplifier() {
		super(makeFunctionApplicationSimplifiers(), makeSyntacticFormTypeSimplifiers());
	}
	
	private static Rewriter plus = new Plus();
	private static Rewriter times = new Times();
	private static Rewriter exponentiation = new Exponentiation();

	public static Map<String, Simplifier> makeFunctionApplicationSimplifiers() {
		return map(
				FunctorConstants.TIMES,           (Simplifier) (f, process) ->
				times.rewrite(f, process),

				FunctorConstants.DIVISION,        (Simplifier) (f, process) ->
				Division.simplify(f),

				FunctorConstants.PLUS,            (Simplifier) (f, process) ->
				plus.rewrite(f, process),

				FunctorConstants.MINUS,           (Simplifier) (f, process) ->
				(f.numberOfArguments() == 2? Minus.simplify(f) : f),

				FunctorConstants.EXPONENTIATION,  (Simplifier) (f, process) ->
				exponentiation.rewrite(f, process),

				FunctorConstants.LESS_THAN,       (Simplifier) (f, process) ->
				LessThan.simplify(f, process),

				FunctorConstants.LESS_THAN_OR_EQUAL_TO,     (Simplifier) (f, process) ->
				LessThanOrEqualTo.simplify(f, process),

				FunctorConstants.GREATER_THAN,              (Simplifier) (f, process) ->
				GreaterThan.simplify(f, process),

				FunctorConstants.GREATER_THAN_OR_EQUAL_TO,  (Simplifier) (f, process) ->
				GreaterThanOrEqualTo.simplify(f, process)
				);
	}

	public static Map<String, Simplifier> makeSyntacticFormTypeSimplifiers() {
		return map();
	}
}