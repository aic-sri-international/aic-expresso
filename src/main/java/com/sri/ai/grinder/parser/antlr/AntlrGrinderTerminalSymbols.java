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
package com.sri.ai.grinder.parser.antlr;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.google.common.annotations.Beta;

@Beta
public class AntlrGrinderTerminalSymbols {
	// NOTE: ENSURE THIS LIST IS UPDATED WHENEVER THE SYMBOLS IN
	// AntlrGrinder.g are updated.
	private static final Set<String> _terminalSymbols;
	static {
		Set<String> terminalSymbols = new HashSet<String>();
		// Keywords
		terminalSymbols.add("not");
		terminalSymbols.add("and");
		terminalSymbols.add("or");
		terminalSymbols.add("for");
		terminalSymbols.add("all");
		terminalSymbols.add("there");
		terminalSymbols.add("exists");
		terminalSymbols.add("lambda");
		terminalSymbols.add("if");
		terminalSymbols.add("then");
		terminalSymbols.add("else");
		terminalSymbols.add("intersection");
		terminalSymbols.add("union");
		terminalSymbols.add("on");
		terminalSymbols.add("in");
		// Special Functions
		terminalSymbols.add("x");
		terminalSymbols.add("->");
		// Logic Operators
		terminalSymbols.add("=>");
		terminalSymbols.add("<=>");
		// Arithmetic
		terminalSymbols.add("^");
		terminalSymbols.add("/");
		terminalSymbols.add("*");
		terminalSymbols.add("+");
		terminalSymbols.add("-");
		// Comparison
		terminalSymbols.add("<");
		terminalSymbols.add("<=");
		terminalSymbols.add("=");
		terminalSymbols.add("!=");
		terminalSymbols.add(">=");
		terminalSymbols.add(">");
		// Brackets
		terminalSymbols.add("(");
		terminalSymbols.add(")");
		terminalSymbols.add("[");
		terminalSymbols.add("]");
		terminalSymbols.add("{");
		terminalSymbols.add("}");
		terminalSymbols.add("{{");
		terminalSymbols.add("}}");
		// Misc
		terminalSymbols.add(";");
		terminalSymbols.add(":");
		terminalSymbols.add("|");
		terminalSymbols.add(",");
		terminalSymbols.add("_");
		terminalSymbols.add(".");
		
		_terminalSymbols = Collections.unmodifiableSet(terminalSymbols);
	}

	public static boolean isTerminalSymbol(String symbol) {
		boolean result = _terminalSymbols.contains(symbol);
		return result;
	}
}
