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
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.brewer.BrewerConfiguration;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.Writer;
import com.sri.ai.brewer.core.Brewer;
import com.sri.ai.brewer.core.DefaultWriter;
import com.sri.ai.brewer.core.MultipleNonTerminalsTestGrammar;
import com.sri.ai.expresso.api.Expression;

/*
 * A class written for a simple test parsing with a grammar with more than one terminal at {@link TestGrammar}.
 * It does not work right now because the parser, upon going over Factor for the first time,
 * places a constraint for sub-expressions not being Factor (to avoid infinite recursion)
 * and then when it's time to read the functor + for +(1) it rejects + since it is also a Factor.
 * Choosing not to fix it for now (Sept 2012).
 */
public class MultipleNonTerminalsTestGrammarTest {

	@Before
	public void setUp() throws Exception {
	}

	@Ignore
	@Test
	public void testToStringExpression() {
		String testCase;
		
		testCase = "+(1)";
		test(testCase);
//		
//		testCase = "1";
//		test(testCase);
//		
//		testCase = "1 + 2";
//		test(testCase);
//		
//		testCase = "1 + 2*3";
//		test(testCase);
//		
//		testCase = "1*2 + 3";
//		test(testCase);
//		
//		testCase = "1*(2 + 3) + 4";
//		test(testCase);
	}

	private void test(String originalString) {
		test(originalString, new MultipleNonTerminalsTestGrammar());
	}
	
	private void test(String originalString, Grammar grammar) {
// TODO: restore this function properly after debugging
		
		// Ensure the grammar class passed in is used where necessary.
		BrewerConfiguration.setProperty(BrewerConfiguration.KEY_DEFAULT_GRAMMAR_CLASS, grammar.getClass().getName());
		Writer writer = new DefaultWriter(grammar);
		
		Expression expression = Brewer.parse(originalString, grammar);
		
		Expression parseOfTheTreeNotation = Brewer.parse(expression.getSyntaxTree().defaultToString(), grammar);
		String unparse              = writer.toString(expression);
		Expression parseOfTheUnparse = Brewer.parse(unparse, grammar);
		System.out.println("Test case                 : " + originalString);
		System.out.println("Parse                     : " + expression.getSyntaxTree().defaultToString());
		System.out.println("Parse of the tree notation: " + parseOfTheTreeNotation.getSyntaxTree().defaultToString());
		System.out.println("Unparse                   : " + unparse);
		System.out.println("Parsed of the unparse     : " + parseOfTheUnparse.getSyntaxTree().defaultToString() + "\n");
		assertEquals(originalString, unparse);
		assertEquals(expression, parseOfTheUnparse);
		assertEquals(expression, parseOfTheTreeNotation);
	}
}

