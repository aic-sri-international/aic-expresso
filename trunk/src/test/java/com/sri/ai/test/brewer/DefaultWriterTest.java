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
package com.sri.ai.test.brewer;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.Writer;
import com.sri.ai.brewer.core.Brewer;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.brewer.core.DefaultGrammar;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.brewer.parsingexpression.core.Disjunction;
import com.sri.ai.brewer.parsingexpression.core.NonTerminal;
import com.sri.ai.brewer.parsingexpression.core.Null;
import com.sri.ai.brewer.parsingexpression.core.Optional;
import com.sri.ai.brewer.parsingexpression.core.Sequence;
import com.sri.ai.brewer.parsingexpression.core.ParsingSymbol;
import com.sri.ai.brewer.parsingexpression.core.Terminal;
import com.sri.ai.brewer.parsingexpression.helper.AssociativeSequence;
import com.sri.ai.brewer.parsingexpression.helper.ParenthesizedNonTerminal;
import com.sri.ai.brewer.parsingexpression.helper.ParsingExpressionForFunctionApplications;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.Configuration;

public class DefaultWriterTest {

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testToStringExpression() {
		String testCase;

// Problematic because parse of tree notation "-(-(1, 2), 3)" is interpreted as the negation of a tuple!
//		testCase = "(1 - 2) - 3";
//		test(testCase);
//		testCase = "1 - (2 - 3)";
//		test(testCase);
		
		testCase = "2 + 2";
		test(testCase);
		
		testCase = "1 + 2 + 3"; // tests associative sequences
		test(testCase);
		
		testCase = "1 + 2 * 3 + 4"; // tests associative sequences with precedence
		test(testCase);
		
		testCase = "1 * (2 + 3) * 4"; // tests associative sequences with precedence
		test(testCase);
		
		testCase = "2";
		test(testCase);
		
		testCase = "f(2)";
		test(testCase);
		
		testCase = "f(2, g(3))";
		test(testCase);
		
		testCase = "(1 != 2) != false"; // this tests the use of ()'s to enforce left association (since the parser, by default, right-associates).
		test(testCase);
		
		testCase = "(composition(f, g))(2, g(3))";
		test(testCase);
		
		testCase = "2 * 2 + 4";
		test(testCase);
		
		testCase = "(2 + 2) * 4";
		test(testCase);
		
		testCase = "2 + - 2";
		test(testCase);

		testCase = "'unknown function with string name'(2, 2)";
		test(testCase);
	}

	@Test
	public void testKleeneParsingExpression() {
		String testCase;
		
		testCase = "{ }";
		test(testCase);

		testCase = "{ 1, 2, 3 }";
		test(testCase);
		
		testCase = "{ 1, ({ a, b, c }), 2 }";
		test(testCase);
		
		testCase = "{ 1, ({ }), 2 }";
		test(testCase);
	}
	
	
	@Test
	public void testLowerPrecedenceArgumentsWithoutParenthesis() {
		Grammar myGrammar = new DefaultGrammar();
		myGrammar.put("Expression",
				new Disjunction(
						new AssociativeSequence(new NonTerminal("Expression"), new Terminal("+"), new NonTerminal("Expression")),
						new AssociativeSequence(new NonTerminal("Expression"), new Terminal("*"), new NonTerminal("Expression")),
						new Sequence(new Terminal("blah"), new NonTerminal("Expression"), new Terminal("blih")),
						new ParsingExpressionForFunctionApplications(new NonTerminal("Expression")),
						new ParenthesizedNonTerminal("Expression"),
						new ParsingSymbol("Expression")));
	
		String testCase;
		
		testCase = "2 * (1 + 2)";
		test(testCase, myGrammar);
		
		testCase = "blah 1 + 2 blih";
		test(testCase, myGrammar);
	}
	
	@Test
	public void testOptionalParsingExpression() {
		Grammar myGrammar = new DefaultGrammar();
		myGrammar.put("Expression",
				new Disjunction(
						new Sequence(new Terminal("blah"), new Optional(new Terminal("bleh")), new Terminal("blih")),
						new ParsingExpressionForFunctionApplications(new NonTerminal("Expression")),
						new Null(),
						new ParsingSymbol("Expression")));
		// we need the last three parsing expressions so the grammar can parse the prefix form,
		// which includes function application with parentheses, with a (quoted) symbol, and null.
	
		String testCase;
		
		// REPEAT FOR DEBUGGING
		testCase = "blah blih";
		test(testCase, myGrammar);

		testCase = "blah bleh blih";
		test(testCase, myGrammar);
		
		testCase = "blah blih";
		test(testCase, myGrammar);
	}
	
	private void test(String originalString) {
		test(originalString, new CommonGrammar());
	}
	
	private void test(String originalString, Grammar grammar) {		
		// Ensure the grammar class passed in is used where necessary.
		Configuration.setProperty(BrewerConfiguration.KEY_DEFAULT_GRAMMAR_CLASS, grammar.getClass().getName());
		Writer writer = new DefaultWriter(grammar);
		
		Expression expression = Brewer.parse(originalString, grammar, "Expression");
		
		Expression parseOfTheTreeNotation = Brewer.parse(expression.getSyntaxTree().toString(), grammar, "Expression");
		String unparse               = writer.toString(expression);
		Expression parseOfTheUnparse = Brewer.parse(unparse, grammar, "Expression");
		System.out.println("Test case                 : " + originalString);
		System.out.println("Parse                     : " + expression.getSyntaxTree().toString());
		System.out.println("Parse of the tree notation: " + parseOfTheTreeNotation.getSyntaxTree().toString());
		System.out.println("Unparse                   : " + unparse);
		System.out.println("Parsed of the unparse     : " + parseOfTheUnparse.getSyntaxTree().toString() + "\n");
		assertEquals(originalString, unparse);
		assertEquals(expression, parseOfTheUnparse);
		assertEquals(expression, parseOfTheTreeNotation);
	}
}
