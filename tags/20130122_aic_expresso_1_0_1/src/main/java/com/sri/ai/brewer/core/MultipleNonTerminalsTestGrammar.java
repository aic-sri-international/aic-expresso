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
import com.sri.ai.brewer.parsingexpression.core.Disjunction;
import com.sri.ai.brewer.parsingexpression.core.NonTerminal;
import com.sri.ai.brewer.parsingexpression.core.Symbol;
import com.sri.ai.brewer.parsingexpression.helper.ParsingExpressionForFunctionApplications;

/**
 * A {@link DefaultGrammar} containing parsing expressions for simple test of
 * parsing with more than one terminal. See {@link WriterForTestGrammarTest} for
 * more details.
 * 
 * @author braz
 */
@Beta
public class MultipleNonTerminalsTestGrammar extends DefaultGrammar {
	private static final long serialVersionUID = 1L;

	public MultipleNonTerminalsTestGrammar() {
//		setInitialNonTerminal("Expression");
		setInitialNonTerminal("Term");
//		setInitialNonTerminal("Factor");

//		put("Expression", new Disjunction(
//				new AssociativeSequence(new NonTerminal("Term"), new Terminal("+"), new NonTerminal("Term")),
//				new            Sequence(new NonTerminal("Term"), new Terminal("-"), new NonTerminal("Term")),
//				new NonTerminal("Term")
//		));
		
		put("Term", new Disjunction(
//				new AssociativeSequence(new NonTerminal("Factor"), new Terminal("*"), new NonTerminal("Factor")),
//				new            Sequence(new NonTerminal("Factor"), new Terminal("/"), new NonTerminal("Factor")),
				new NonTerminal("Factor")
		));
		
		put("Factor", new Disjunction(
				new ParsingExpressionForFunctionApplications(new NonTerminal("Term")),
//				new ParenthesizedNonTerminal("Expression"),
//				new ParenthesizedNonTerminal("Term"),
				new Symbol()
		));

//		put("Term", new Disjunction(
//				new NonTerminal("Arithmetic term"),
//				new NonTerminal("Formula")
//		));
//
//		put("Formula", new Disjunction(
//				new            Sequence(new NonTerminal("Formula"), new Terminal("=>"),  new NonTerminal("Formula")),
//				new            Sequence(new NonTerminal("Formula"), new Terminal("<=>"), new NonTerminal("Formula")),
//				new AssociativeSequence(new NonTerminal("Formula"), new Terminal("or"), new NonTerminal("Formula")),
//				new AssociativeSequence(new NonTerminal("Formula"), new Terminal("and"), new NonTerminal("Formula")),
//				new AssociativeSequence(new NonTerminal("Term"), new Terminal("="), new NonTerminal("Term")),
//				new            Sequence(new NonTerminal("Term"), new Terminal("!="), new NonTerminal("Term")),
//				new            Sequence(new Terminal("not"), new NonTerminal("Formula")),
//				new ParsingExpressionForFunctionApplications(new NonTerminal("Term")),
//				new ParenthesizedNonTerminal("Formula"),
//				new Symbol()
//		));
//
//		put("Arithmetic term", new Disjunction(
//				new AssociativeSequence(new NonTerminal("Arithmetic term"), new Terminal("+"), new NonTerminal("Arithmetic term")),
//				new            Sequence(new NonTerminal("Arithmetic term"), new Terminal("-"), new NonTerminal("Arithmetic term")),
//				new AssociativeSequence(new NonTerminal("Arithmetic term"), new Terminal("*"), new NonTerminal("Arithmetic term")),
//				new            Sequence(new NonTerminal("Arithmetic term"), new Terminal("/"), new NonTerminal("Arithmetic term")),
//				new            Sequence(new Terminal("-"), new NonTerminal("Arithmetic term")),
//				new ParsingExpressionForFunctionApplications(new NonTerminal("Term")),
//				new ParenthesizedNonTerminal("Arithmetic term"),
//				new Symbol()
//		));
	}
}
