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


import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.api.BasicParsingExpression;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.Writer;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A default implementation for {@link Writer}.
 * 
 * @author braz
 *
 */
@Beta
public class DefaultWriter implements Writer {
	private Grammar grammar;
	
	public DefaultWriter(Grammar grammar) {
		setGrammar(grammar);
	}
	
	/**
	 * 
	 * @return a new instance of DefaultWriter with its Grammar initialized to
	 *         the class described by
	 *         {@link BrewerConfiguration#KEY_DEFAULT_GRAMMAR_CLASS}. Note: the
	 *         class corresponding to the name must have a default constructor.
	 */
	public static Writer newDefaultConfiguredWriter() {
		Grammar grammar = BrewerConfiguration.newConfiguredInstance(BrewerConfiguration.getDefaultGrammarClass());
		Writer  result = new DefaultWriter(grammar);
		return result;
	}
	
	@Override
	public Grammar getGrammar() {
		return grammar;
	}

	@Override
	public void setGrammar(Grammar grammar) {
		this.grammar = grammar;
	}

	/**
	 * A temporary solution for obtaining the syntax tree of an expression even if the expression is null.
	 * This should eventually become unnecessary because writers and parsers should only deal with syntax trees anyway.
	 */
	public static SyntaxTree getSyntaxTreeOrNullIfNull(Expression expression) {
		if (expression != null) {
			return expression.getSyntaxTree();
		}
		return null;
	}
	
	@Override
	public String toString(Expression expression) {
		return toString(expression.getSyntaxTree());
	}
	
	@Override
	public String toString(SyntaxTree syntaxTree) {
		BasicParsingExpression parsingExpression =
			getGrammar().getBasicParsingExpressionFor(syntaxTree);

		if (parsingExpression == null) {
			return writeSyntaxTreeThatIsNotFromBasicParsingExpression(syntaxTree);
		}
		
		String string = parsingExpression.toString(syntaxTree, this);
		return string;
	}

	private String writeSyntaxTreeThatIsNotFromBasicParsingExpression(SyntaxTree syntaxTree) {
		if (syntaxTree == null) {
			return "null";
		}
		if (syntaxTree.numberOfImmediateSubTrees() > 0) {
			return writeFunctionApplicationThatIsNotFromBasicParsingExpression(syntaxTree);
		}
		return syntaxTree.defaultToString();
	}

	private String writeFunctionApplicationThatIsNotFromBasicParsingExpression(SyntaxTree syntaxTree) {
		if (syntaxTree == null) {
			return "";
		}
		StringBuffer result = new StringBuffer();
		SyntaxTree rootTree = syntaxTree.getRootTree();
		if (rootTree.numberOfImmediateSubTrees() > 0) {
			result.append("(" + toString(rootTree) + ")");
		}
		else {
			result.append(toString(rootTree));
		}
		String subTreesString =
			Util.join(", ",
					new FunctionIterator<SyntaxTree, String>(
							syntaxTree.getImmediateSubTrees(),
							new Function<SyntaxTree, String>() {
								public String apply(SyntaxTree syntaxTree) {
									return DefaultWriter.this.toString(syntaxTree);
								}
							}));
		result.append("(" + subTreesString + ")");
		return result.toString();
	}

	@Override
	public boolean precedenceLessThan(SyntaxTree syntaxTree1, SyntaxTree syntaxTree2) {
		return getGrammar().precedenceIsLessThan(syntaxTree1, syntaxTree2);
	}

	@Override
	public boolean precedenceLessThanOrEqualTo(SyntaxTree syntaxTree1, SyntaxTree syntaxTree2) {
		return getGrammar().precedenceIsLessThanOrEqualTo(syntaxTree1, syntaxTree2);
	}

	/**
	 * Returns the string (representing an argument of expression, according to a writer) 
	 * that needs to be added to this expression's representation,
	 * taking care of enveloping it in parentheses if precedence requires it.
	 * Parameter noNeedForParentheses indicates that the context makes surrounding parentheses
	 * unnecessary regardless of precedence.
	 */
	public static String subTreeRepresentation(
			SyntaxTree syntaxTree, SyntaxTree subSyntaxTree,
			Writer writer, boolean noNeedForParentheses) {
		String subTreeString = writer.toString(subSyntaxTree);
		if ( ! noNeedForParentheses && writer.precedenceLessThanOrEqualTo(subSyntaxTree, syntaxTree)
		) {
			return "(" + subTreeString + ")";
		}
		else {
			return subTreeString;
		}
	}

	public static class SubTreeRepresentation implements Function<SyntaxTree, String> {
		SyntaxTree syntaxTree;
		Writer writer;
		boolean leftOfExpression;
		private boolean noNeedForParentheses;
		
		public SubTreeRepresentation(SyntaxTree syntaxTree, Writer writer,
				boolean leftOfExpression,
				boolean noNeedForParentheses) {
			super();
			this.syntaxTree = syntaxTree;
			this.writer = writer;
			this.leftOfExpression = leftOfExpression;
			this.noNeedForParentheses = noNeedForParentheses;
		}

		@Override
		public String apply(SyntaxTree subTree) {
			return subTreeRepresentation(syntaxTree, subTree, writer, noNeedForParentheses);
		}
	}
}
