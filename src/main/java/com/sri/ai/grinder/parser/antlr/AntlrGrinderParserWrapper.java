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

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTree;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.Parser;
import com.sri.ai.expresso.api.Expression;

/**
 * An implementation of the {@link Parser} interface that uses Antlr as the
 * underlying implementation.
 * 
 * @author tsai
 * 
 */
@Beta
public class AntlrGrinderParserWrapper implements Parser {

	public AntlrGrinderParserWrapper() {

	}

	@Override
	public Expression parse(String string) {
		Expression result = null;
		try {			
			ErrorListener lexerErrorListener = new ErrorListener("Lexer Error");
			ErrorListener parseErrorListener = new ErrorListener("Parse Error");
			
			ANTLRInputStream input = new ANTLRInputStream(string);
			AntlrGrinderLexer lexer = new AntlrGrinderLexer(input);
			
			CommonTokenStream tokens = new CommonTokenStream(lexer);
			AntlrGrinderParser parser = new AntlrGrinderParser(tokens);

			lexer.removeErrorListeners();
			parser.removeErrorListeners();
			lexer.addErrorListener(lexerErrorListener);
			parser.addErrorListener(parseErrorListener);
			
			ParseTree tree = parser.expression();
			
			boolean eof = parser.getInputStream().LA(1) == Recognizer.EOF;
			
			if (!lexerErrorListener.errorsDetected && !parseErrorListener.errorsDetected) {
				if (!eof) {
					System.err.println("Unable to parse the complete input expression: "+input);
				}
				else {
					lexer.removeErrorListeners();
					parser.removeErrorListeners();
					ExpressionVisitor expressionVisitor = new ExpressionVisitor();
					result = expressionVisitor.visit(tree);
				}
			}
		} catch (RecognitionException re) {
			re.printStackTrace();
		} catch (RuntimeException re) {
			re.printStackTrace();
		}

		return result;
	}

	@Override
	public void close() {

	}
	
	//
	// PRIVATE
	//
	private class ErrorListener extends BaseErrorListener {
		public boolean errorsDetected = false;
		private String name;
		
		public ErrorListener(String name) {
			this.name = name;
		}

		@Override
		public void syntaxError(Recognizer<?, ?> recognizer,
				Object offendingSymbol, int line, int charPositionInLine,
				String msg, RecognitionException e) {
			System.err.println(name+": line " + line + ":" + charPositionInLine + " " + msg);
			errorsDetected = true;
		}
	}
}
