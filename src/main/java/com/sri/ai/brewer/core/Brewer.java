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
package com.sri.ai.brewer.core;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;

/**
 * Utility interface for parsing.
 * 
 * @author braz
 *
 */
@Beta
public class Brewer {
	public static Expression parse(String string, Grammar grammar) {
		Expression result = parse(string, grammar, grammar.getInitialNonTerminal());
		return result;
	}
	
	public static Expression parse(String string, Grammar grammar, String initialNonTerminal) {
		ParsingResult parsingResult = parsingResult(string, grammar, initialNonTerminal);
		Expression result = DefaultParsingResult.isSuccessful(parsingResult)? parsingResult.getParse() : null;
		return result;
	}
	
	public static ParsingResult parsingResult(String string, Grammar grammar, String initialNonTerminal) {
		ParsingProcess process = new DefaultParsingProcess(string, grammar);
		return process.parsingResultOfNonTerminal(initialNonTerminal);
	}

	public static Expression parseWholeStream(String string, Grammar grammar, String initialNonTerminal) {
		ParsingResult parsingResult = parsingResultWholeStream(string, grammar, initialNonTerminal);
		Expression result = DefaultParsingResult.isSuccessful(parsingResult)? parsingResult.getParse() : null;
		return result;
	}
	
	public static ParsingResult parsingResultWholeStream(String string, Grammar grammar, String initialNonTerminal) {
		ParsingProcess process = new DefaultParsingProcess(string, grammar);
		return process.parsingResultOfNonTerminal(initialNonTerminal);
	}
	
	/**
	 * Call this to generate the Java code to produce the given expression
	 * object.
	 * 
	 * @param expr
	 *            The expression object.
	 * @return A string of Java code for generating the given object.
	 */
	public static String generateBuildString (Expression expr) {
		StringBuffer sb = new StringBuffer();

		if (expr == null) {
			return "";
		}

		generateFunctionApplicationString(sb, expr, 3, true);

		return sb.toString();
	}

	/**
	 * Appends the Java code to generate the given object.
	 * 
	 * @param sb
	 *            The string buffer to append to.
	 * @param expr
	 *            The expression object to generate.
	 * @param tabLevel
	 *            The number of tabs to indent.
	 */
	public static void generateFunctionApplicationString(StringBuffer sb, Expression expr, int tabLevel) {
		generateFunctionApplicationString(sb, expr, tabLevel, false);
	}
	
	/**
	 * Appends the Java code to generate the given object.
	 * 
	 * @param sb
	 *            The string buffer to append to.
	 * @param expression
	 *            The expression object to generate.
	 * @param tabLevel
	 *            The number of tabs to indent.
	 * @param forceSymbolPrint
	 *            Whether to print "DefaultSymbol.createSymbol()" for symbols or
	 *            just the string.
	 */
	public static void generateFunctionApplicationString(StringBuffer sb, Expression expression, int tabLevel, boolean forceSymbolPrint) {
		if (expression instanceof CompoundSyntaxTree) {
			sb.append("\n");
			for (int i = 0; i < tabLevel; i++)
				sb.append("\t");
			sb.append("new DefaultCompoundSyntaxTree(");

			boolean firstChild = true;
			List<Expression> children = expression.getSubExpressions();
			for (Expression child : children) {
				if (firstChild)
					firstChild = false;
				else
					sb.append(", ");
				generateFunctionApplicationString(sb, child, tabLevel + 2);

			}
			sb.append(")");
		}
		else if (expression instanceof Symbol) {
			Symbol symbol = (Symbol) expression;
			Object label = symbol.getValue();
			if (label instanceof Expression) {
				sb.append("\n");
				for (int i = 0; i < tabLevel; i++)
					sb.append("\t");
				sb.append("DefaultSymbol.createSymbol(");
				generateFunctionApplicationString(sb, (Expression)label, tabLevel + 2, true);
				sb.append(")");
			}
			else {
				if (forceSymbolPrint) {
					sb.append("DefaultSymbol.createSymbol(");
				}
				sb.append("\"");
				String string = expression.toString();
				if (string.startsWith("'"))
					string = string.substring(1, string.length()-1);
				sb.append(string);
				sb.append("\"");
				if (forceSymbolPrint) {
					sb.append(")");
				}
			}
		}
		else if (expression == null) {
			sb.append("null");
		}

	}
}
