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
package com.sri.ai.grinder.rewriter.core;

import static com.sri.ai.util.Util.map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.rewriter.api.Rewriter;
import com.sri.ai.grinder.rewriter.api.Simplifier;

/**
 * A {@link Switch} rewriter with a syntactic form type key maker for symbols,
 * that takes a rewriter and adds a check for a given symbol.
 * This is a convenient way of creating such a switch that later can be combined with other
 * top rewriters in a larger, indexed top rewriter.
 * @author braz
 * @see FunctorSwitch
 *
 */
public class SymbolSwitch<T> extends Switch<Object> {

	public SymbolSwitch(String symbolName, Rewriter rewriter) {
		super(
				Switch.SYNTACTIC_FORM_TYPE,
				map("Symbol", makeRewriterWithArityCheck(rewriter, symbolName)));
	}

	private static Simplifier makeRewriterWithArityCheck(Rewriter rewriter, String symbolName) {
		return (Simplifier) (e, c) -> {
			Expression result;
			if (e.toString().equals(symbolName)) {
				result = rewriter.apply(e, c);
			}
			else {
				result = e;
			}
			return result;
		};
	}
}