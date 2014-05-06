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
package com.sri.ai.test.grinder.parser.antlr;

import java.io.IOException;
import java.util.ArrayList;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.test.grinder.AbstractParserTest;

public class AntlrGrinderParserTest extends AbstractParserTest {

	public AntlrGrinderParserTest () {
		parser = new AntlrGrinderParserWrapper();
	}
	
	@Test
	public void testSymbol () {
		String string;
		string = "";
		test(string, null);

		string = "e";
		test(string, Expressions.createSymbol("e"));

		string = "3";
		test(string, Expressions.createSymbol(3));

		string = "3e7";
		test(string, Expressions.createSymbol(30000000));

		string = "3e-1";
		test(string, Expressions.createSymbol("0.30"));

		string = ".3e7";
		test(string, Expressions.createSymbol(3000000));

		string = ".3e-1";
		test(string, Expressions.createSymbol("0.030"));

		string = "1.3e7";
		test(string, Expressions.createSymbol(13000000));

		string = "1.3e-1";
		test(string, Expressions.createSymbol("0.130"));

		string = "false";
		test(string, Expressions.createSymbol(false));

		string = "keyword";
		test(string, Expressions.createSymbol("keyword"));

		string = "Keyword";
		test(string, Expressions.createSymbol("Keyword"));

		string = "foo";
		test(string, Expressions.createSymbol("foo"));

		string = "foo1";
		test(string, Expressions.createSymbol("foo1"));

		string = "foo10bar";
		test(string, Expressions.createSymbol("foo10bar"));

		string = "1foo";
		test(string, Expressions.createSymbol("1foo"));

		string = "'foo1'";
		test(string, Expressions.createSymbol("foo1"));

		string = "foo1'";
		test(string, Expressions.createSymbol("foo1'"));

		string = "foo1'''";
		test(string, Expressions.createSymbol("foo1'''"));

		string = "'This is a test.'";
		test(string, Expressions.createSymbol("This is a test."));

		string = "\"This is a test.\"";
		test(string, Expressions.createSymbol("This is a test."));

		// Testing illegal symbol names.
		string = "foo1'bar";
		testFail(string);

		string = "foo1 bar";
		testFail(string);

		string = "foo1@";
		testFail(string);
	}
	
	@Test
	public void testEscapeSequences() {
		String string;
		
		string = "'Test \\u0061'";
		test(string, Expressions.createSymbol("Test a"));
		
		string = "\"Test \\u0061\"";
		test(string, Expressions.createSymbol("Test a"));

		string = "'Testing the  preservation \\t of	whitespace\\ncharacters.'";
		test(string, Expressions.createSymbol("Testing the  preservation 	 of	whitespace\ncharacters."));

		string = "\"Testing the  preservation \\t of	whitespace\\ncharacters.\"";
		test(string, Expressions.createSymbol("Testing the  preservation 	 of	whitespace\ncharacters."));

		string = "'This is a test *()#@$%!-_=+<>,./?:;\\'\"\\\"\\\\'";
		test(string, Expressions.createSymbol("This is a test *()#@$%!-_=+<>,./?:;'\"\"\\"));

		string = "\"This is a test *()#@$%!-_=+<>,./?:;\\''\\\"\\\"\\\\\"";
		test(string, Expressions.createSymbol("This is a test *()#@$%!-_=+<>,./?:;''\"\"\\"));
		
		string = "foo(bar1', 'bar2\\'', bar3''')";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "bar1'", "bar2'", "bar3'''"));
	}
	
	@Test 
	public void testComment () {
		String string;
		string = "3";
		test(string, Expressions.createSymbol(3));

		string = "3 // This is a test.\n";
		test(string, Expressions.createSymbol(3));

		string = "// This is a test.\n 3";
		test(string, Expressions.createSymbol(3));

		string = "3 // This is a test.";
		test(string, Expressions.createSymbol(3));

		string = "3 // This is a test.\n + 4";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 3, 4));

		string = "// Test\n 3 // This is a test.\n + 4 // Test";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 3, 4));

		string = "3 /* This is a test. */";
		test(string, Expressions.createSymbol(3));

		string = "/* This is a test. */ 3";
		test(string, Expressions.createSymbol(3));

		string = "3 /* This is a test. */ + 4";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 3, 4));
	}

	@Test
	public void testParen () {
		String string;
		string = "(foo)";
		test(string, Expressions.createSymbol("foo"));

		string = "(foo1)";
		test(string, Expressions.createSymbol("foo1"));

		string = "(foo10bar)";
		test(string, Expressions.createSymbol("foo10bar"));

		string = "(1foo)";
		test(string, Expressions.createSymbol("1foo"));

		string = "('foo1')";
		test(string, Expressions.createSymbol("foo1"));

		string = "(foo1')";
		test(string, Expressions.createSymbol("foo1'"));

		string = "(foo1''')";
		test(string, Expressions.createSymbol("foo1'''"));

		// Testing illegal strings.
		string = "(foo1'bar)";
		testFail(string);

		string = "(foo1 bar)";
		testFail(string);

		string = "(foo1@)";
		testFail(string);

		string = "(fo)aao";
		testFail(string);

		string = "(foo";
		testFail(string);

		string = "(foo)a1";
		testFail(string);
	}
	
	@Test
	public void testFunction () {
		String string;
		string = "foo()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo"));

		string = "foo(1)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", 1));

		string = "foo(1, 2, 3)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", 1, 2, 3));

		string = "foo(bar)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "bar"));

		string = "foo(bar1, bar2, bar3)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "bar1", "bar2", "bar3"));

		string = "foo(1+2, bar in hello)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 1, 2),
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "bar", "hello")));

		string = "foo(1+2, (bar in hello))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 1, 2),
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "bar", "hello")));

		string = "'foo bar'()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo bar"));

		string = "'foo bar'(a)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo bar", "a"));

		string = "'foo bar'(a, b, c)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo bar", "a", "b", "c"));
		
		string = "foo(bar(X))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", "X")));
		
		string = "foo(bar(X), baz(Y))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("baz", "Y")));
		
		string = "foo(W, bar(X), Y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "W", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), "Y"));

		string = "(lambda x : y)(a, b, c)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", "x", "y"), "a", "b", "c"));

		// Testing illegal strings.
		string = "foo(1,)";
		testFail(string);

		string = "foo(1)a";
		testFail(string);

		string = "foo(,)";
		testFail(string);

		string = "foo(";
		testFail(string);
	}
	
	@Test
	public void testTuple () {
		String string;
		string = "(foo, bar)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar")));

		string = "(x, y, z)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z")));

		string = "(a in b, x + y + z, i, j, k)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "a", "b"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y", "z"), 
						"i", "j", "k")));

		string = "'( . )'()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )"));

		string = "'( . )'(a)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", "a"));

		string = "'( . )'(a, b, c)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", "a", "b", "c"));

		// Testing illegal strings.
		string = "( 1, 2, )";
		testFail(string);

		string = "( 1, 2 ) 3";
		testFail(string);
	}

	@Test
	public void testAssocPlus() throws IOException {
		String string;
		string = "1 + 2";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2"));

		string = "1 + 2 + 3";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3"));

		string = "1 + 2 + 3 + 4";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4"));

		string = "1 + 2 + 3 + 4 + 5";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4", "5"));

		string = "1 + 2 + 3 + 4 + 5 + 6";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4", "5", "6"));

		string = "1 + 2 + 3 + 4 + 5 + 6 + 7";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4", "5", "6", "7"));
	}

	@Test
	public void testAssocTimes() throws IOException {
		String string;
		string = "1 * 2";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2"));

		string = "1 * 2 * 3";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3"));

		string = "1 * 2 * 3 * 4";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3", "4"));

		string = "1 * 2 * 3 * 4 * 5";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3", "4", "5"));

		string = "1 * 2 * 3 * 4 * 5 * 6";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3", "4", "5", "6"));

		string = "1 * 2 * 3 * 4 * 5 * 6 * 7";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocUnion() throws IOException {
		String string;
		string = "1 union 2";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2"));

		string = "1 union 2 union 3";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3"));

		string = "1 union 2 union 3 union 4";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3", "4"));

		string = "1 union 2 union 3 union 4 union 5";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3", "4", "5"));

		string = "1 union 2 union 3 union 4 union 5 union 6";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3", "4", "5", "6"));

		string = "1 union 2 union 3 union 4 union 5 union 6 union 7";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocEqual() throws IOException {
		String string;
		string = "1 = 2";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2"));

		string = "1 = 2 = 3";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3"));

		string = "1 = 2 = 3 = 4";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3", "4"));

		string = "1 = 2 = 3 = 4 = 5";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3", "4", "5"));

		string = "1 = 2 = 3 = 4 = 5 = 6";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3", "4", "5", "6"));

		string = "1 = 2 = 3 = 4 = 5 = 6 = 7";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocAnd() throws IOException {
		String string;
		string = "1 and 2";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2"));

		string = "1 and 2 and 3";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3"));

		string = "1 and 2 and 3 and 4";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3", "4"));

		string = "1 and 2 and 3 and 4 and 5";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3", "4", "5"));

		string = "1 and 2 and 3 and 4 and 5 and 6";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3", "4", "5", "6"));

		string = "1 and 2 and 3 and 4 and 5 and 6 and 7";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocOr() throws IOException {
		String string;
		string = "1 or 2";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2"));

		string = "1 or 2 or 3";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3"));

		string = "1 or 2 or 3 or 4";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4"));

		string = "1 or 2 or 3 or 4 or 5";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4", "5"));

		string = "1 or 2 or 3 or 4 or 5 or 6";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4", "5", "6"));

		string = "1 or 2 or 3 or 4 or 5 or 6 or 7";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4", "5", "6", "7"));
	}
	
	@Test
	public void testAssoc () {
		String string;
		string = "(1 + 2 + 3 + 4) * (5 + 6 + 7 + 8) * (9 + 10 + 11 + 12)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "5", "6", "7", "8"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "9", "10", "11", "12")));

		string = "(1 + 2 + 3 + 4) * (5 + 6 + 7 + 8) * (9 + 10 + 11 + (12 * 13 * 14 * 15))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "5", "6", "7", "8"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "9", "10", "11", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "12", "13", "14", "15"))));

		string = "(1 or 2 or 3 or 4) and (5 or 6 or 7 or 8) and (9 or 10 or 11 or (12 and 13 and 14 and 15))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "5", "6", "7", "8"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "9", "10", "11", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "12", "13", "14", "15"))));

		// Testing to make sure the associative node walker doesn't get confused by the function form of
		// the operators.
		string = "+(+(b, c))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "b", "c")));

		string = "+(+(b))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "b")));

		string = "+(a, +(+(b)))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "a", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "b"))));

		string = "+(a, +(+(b, c)))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "a", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "b", "c"))));

		string = "+({a}, +(+({b}, {c})))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")))));

		string = "union({a}, union(union({b}, {c})))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")))));

		string = "union({a}, union(union({b}, {c}), {d}))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"))));

		string = "union({a}, union(union({b}, {c}), {d}, {E}))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "E"))));

		string = "union({a}, union(union({b}, {c}), {d}, {E}, {f}))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "E"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "f"))));
	}

	@Test
	public void testExpressionSymbol () {
		String string;
		
		string = "<x>";
		test(string, Expressions.createSymbol(Expressions.createSymbol("x")));

		string = "<foo''>";
		test(string, Expressions.createSymbol(Expressions.createSymbol("foo''")));

		string = "<x in y>";
		test(string, Expressions.createSymbol(
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y")));

		string = "<{(on x, y) f(x,y) | y}>";
		test(string, Expressions.createSymbol(
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "x", "y"), 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "y"))));

		string = "< x < y >";
		test(string, Expressions.createSymbol(
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "x", "y")));

		string = "<x>y>";
		test(string, Expressions.createSymbol(
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y")));

		string = "< x > y >";
		test(string, Expressions.createSymbol(
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y")));

		string = "< if x > y then x else y >";
		test(string, Expressions.createSymbol(
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y"), "x", "y")));
		
		string = "if <x> > <y> then x else y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">",
						Expressions.createSymbol(Expressions.createSymbol("x")),
						Expressions.createSymbol(Expressions.createSymbol("y"))
						),
				"x", "y"));

		// Test for illegal strings.
		string = "< x";
		testFail(string);

		string = " x >";
		testFail(string);
	}

	@Test
	public void testCardinality () {
		String string;
		
		string = "|foo|";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", "foo"));

		string = "| foo in bar |";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "foo", "bar")));

		string = "| {} |";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

		string = "| { foo, bar } |";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar"))));

		string = "| ({ foo, bar }) |";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar"))));

		string = "| 1 + 2 |";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 1, 2)));

		string = "'| . |'()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |"));

		string = "'| . |'(a)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", "a"));

		string = "'| . |'(a, b, c)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", "a", "b", "c"));

		// Testing illegal strings.
		string = "| 1, 2, |";
		testFail(string);

		string = "| 1, 2 | 3";
		testFail(string);
	}

	@Test
	public void testMultiset () {
		String string;
		
		string = "{{ ( on ) ([ if X then 1 else 0 ]) }}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", IntensionalSet.makeScopingSyntaxTree(new ArrayList<Expression>()), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "X", "1", "0")), null));
		
		string = "{{ foo }}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "foo"));

		string = "{{ (on foo) bar }}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "foo"), "bar", null));

		string = "{{ (on x in y) z }}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y")), "z", null));

		string = "{{ foo | bar }}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", null, "foo", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "bar")));

		string = "{{f(X) | false}}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", null, 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "false")));
		
		string = "{{ [if p(a) then 1 else 0] | true }}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", null, 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a"), "1", "0")), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "true")));

		string = "{{ (on foo, fooz) bar }}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "fooz")), 
						"bar", null));

		string = "{{ (on foo, fooz) bar | barz }}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "fooz")), 
						"bar", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "barz")));

		string = "{{ foo, bar, foo + bar }}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "foo", "bar"))));

		string = "{{}}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

		string = "'{{ . }}'()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}"));

		string = "'{{ . }}'(a)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "a"));

		string = "'{{ . }}'(a, b, c)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "a", "b", "c"));

		// Test illegal strings.
		string = "{{ foo";
		testFail(string);

		string = "{{ (on foo1, foo2, foo3, foo4) bar1 | bar2 | bar3";
		testFail(string);

		string = "{{ (on foo1, foo2, foo3, foo4) bar1 | bar2 | bar3 }";
		testFail(string);

		string = "{{ (on foo1, foo2, foo3, foo4 bar1 | bar2 | bar3 }}";
		testFail(string);

		string = "{{";
		testFail(string);

		string = "{{ , }}";
		testFail(string);

		string = "{{ foo, }}";
		testFail(string);

		string = "{{ 1 }} 2";
		testFail(string);

	}

	@Test
	public void testSet () {
		String string;	
		
		string = "{ ( on ) p(X, X) | true }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", IntensionalSet.makeScopingSyntaxTree(new ArrayList<Expression>()), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "X"), Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, "true")));

		string = "{ ( on ) X | true }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", IntensionalSet.makeScopingSyntaxTree(new ArrayList<Expression>()),  
				"X", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, "true")));
				
		string = "{ a | true}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", null, "a", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, "true")));
		
		string = "{ foo }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "foo"));

		string = "{ (on foo) bar }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "foo"), "bar", null));

		string = "{ (on x in y) z }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y")), "z", null));

		string = "{ foo | bar }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", null, "foo", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "bar")));

		string = "{f(X) | false}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", null, 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "false")));
		
		string = "{ [if p(a) then 1 else 0] | true }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", null, 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a"), "1", "0")), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "true")));

		string = "{ (on foo, fooz) bar }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "fooz")), 
						"bar", null));

		string = "{ (on foo, fooz) bar | barz }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "fooz")), 
						"bar", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "barz")));

		string = "{ foo, bar, foo + bar }";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "foo", "bar"))));

		string = "{}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

		string = "'{ . }'()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }"));

		string = "'{ . }'(a)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"));

		string = "'{ . }'(a, b, c)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a", "b", "c"));

		// Test illegal strings.
		string = "{ foo";
		testFail(string);

		string = "{ (on foo1, foo2, foo3, foo4) bar1 | bar2 | bar3";
		testFail(string);

		string = "{ (on foo1, foo2, foo3, foo4) bar1 | bar2 | bar3 }";
		testFail(string);

		string = "{ (on foo1, foo2, foo3, foo4 bar1 | bar2 | bar3 }";
		testFail(string);

		string = "{";
		testFail(string);

		string = "{ , }";
		testFail(string);

		string = "{ foo, }";
		testFail(string);
	}
	
	@Test
	public void testSquareBracket () {
		String string;
		string = "[ x in y ]";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y")));

		string = "[ x+1 ]";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", 1)));

		string = "[ (x, y) ]";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));

		string = "'[ . ]'()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]"));

		string = "'[ . ]'(a)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", "a"));

		string = "'[ . ]'(a, b, c)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", "a", "b", "c"));

		// Testing illegal strings.
		string = "[]";
		testFail(string);

		string = "[";
		testFail(string);

		string = "[ x";
		testFail(string);

		string = "[ x, y ]";
		testFail(string);
	}

	@Test
	public void testNeighborFactor () {
		String string;
		string = "neighbors of factor x";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", "x"));

		string = "neighbors of factor [x]";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", "x")));

		string = "neighbors of factor {x}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x")));

		string = "neighbors of factor {{x}}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "x")));

		string = "'neighbors of factor'(a)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", "a"));

		string = "neighbors of factor neighbors of factor x";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", "x")));
	}
	
	@Test
	public void testNeighborVariable () {
		String string;
		string = "neighbors of variable x";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", "x"));

		string = "neighbors of variable [x]";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", "x")));

		string = "neighbors of variable {x}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x")));

		string = "neighbors of variable {{x}}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "x")));

		string = "'neighbors of variable'(a)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", "a"));

		string = "neighbors of variable neighbors of variable x";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", "x")));
	}
	
	@Test
	public void testNeighborOf () {
		String string;
		string = "neighbors of x from y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", "x", "y"));

		string = "neighbors of [x] from y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("[ . ]", "x"), "y"));

		string = "neighbors of {x} from y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "neighbors of {{x}} from y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "x"), "y"));

		string = "'neighbors of . from .'(a, b)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", "a", "b"));

		string = "neighbors of neighbors of x from y from neighbors of a from b";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", "x", "y"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", "a", "b")));

		// Testing illegal strings.
		string = "neighbors of {{x}}";
		testFail(string);

		string = "neighbors of from y";
		testFail(string);

		string = "neighbors of x";
		testFail(string);

		string = "neighbors of x from";
		testFail(string);
	}

	@Test
	public void testNot () {
		String string;
		string = "not x";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"));

		string = "not (x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"));

		string = "not";
		test(string, Expressions.createSymbol("not"));

		string = "not()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not"));

		string = "not(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));

		string = "not {x, y, z}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z"))));

		string = "not (x + y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")));

		string = "not x + y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"), "y"));
	}

	@Test
	public void testNegative () {
		String string;
		
		string = "-3";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "3"));
		
		string = "-3e7";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 30000000));

		string = "-3e-1";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "0.30"));

		string = "-.3e7";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 3000000));

		string = "-.3e-1";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "0.030"));

		string = "-1.3e7";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 13000000));

		string = "-1.3e-1";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "0.130"));
		
		string = "-x";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

		string = "- x";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

		string = "--x";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x")));

		string = "-(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

		string = "-";
		test(string, Expressions.createSymbol("-"));

		string = "-()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-"));

		string = "-(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));

		string = "-x - y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x"), "y"));

		string = "x - -y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "y")));

		string = "--x";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x")));

		string = "-(-x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x")));

		string = "- {x, y, z}";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z"))));

		string = "- x + y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x"), "y"));

		string = "- (x + y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")));

		string = "(- x) + y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x"), "y"));
	}

	@Test
	public void testExponentiation() {
		String string;
		string = "x ^ y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "x", "y"));

		string = "x^y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "x", "y"));

		string = "w ^ x ^ y ^ z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "w",
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^",  "x",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "y", "z"))));

		string = "x ^ y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "x", "y"), "z"));

		string = "{x} ^ y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} ^ y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")),	"y"));

		string = "^(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "x"));

		string = "^";
		test(string, Expressions.createSymbol("^"));

		string = "^()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^"));

		string = "^(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "x", "y"));

		// Testing illegal strings.
		string = "x ^";
		testFail(string);

		string = "^ x";
		testFail(string);
	}

	@Test
	public void testDivide () {
		String string;
		string = "x / y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "x", "y"));

		string = "x/y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "x", "y"));
		
		string = "w / x / y / z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/",  
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "w", "x"), "y"), "z"));

		string = "x / y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "x", "y"), "z"));

		string = "{x} / y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} / y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")),
				"y"));

		string = "/(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "x"));

		string = "/";
		test(string, Expressions.createSymbol("/"));

		string = "/()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/"));

		string = "/(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "x", "y"));

		// Testing illegal strings.
		string = "x /";
		testFail(string);

		string = "/ x";
		testFail(string);
	}

	@Test
	public void testMultiply () {
		String string;
		string = "x * y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"));

		string = "x*y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"));

		string = "w * x * y * z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
				"w", "x", "y", "z"));

		string = "x * y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"), "z"));

		string = "{x} * y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} * y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "*(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x"));

		string = "*";
		test(string, Expressions.createSymbol("*"));

		string = "*()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*"));

		string = "*(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"));

		// Testing illegal strings.
		string = "x *";
		testFail(string);

		string = "* x";
		testFail(string);
	}

	@Test
	public void testMinus () {
		String string;
		string = "x - y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x", "y"));
		
		string = "x-y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x", "y"));
		
		string = "2-0";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "2", "0"));
		
		string = "w - x - y - z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-",  
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "w", "x"), "y"), "z"));

		string = "x - y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x", "y"), "z"));

		string = "{x} - y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} - y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "-(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

		string = "-";
		test(string, Expressions.createSymbol("-"));

		string = "-()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-"));

		string = "-(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));
	}

	@Test
	public void testPlus () {
		String string;
		string = "x + y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y"));

		string = "x+y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y"));

		string = "1+2";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2"));

		string = "1+-2";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "2")));

		string = "w + x + y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				"w", "x", "y", "z"));

		string = "{x} + y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} + y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "+(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x"));

		string = "+";
		test(string, Expressions.createSymbol("+"));

		string = "+()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+"));

		string = "+(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y"));

		// Testing illegal strings.
		string = "x +";
		testFail(string);

		string = "+ x";
		testFail(string);
	}

	@Test
	public void testIntersection () {
		String string;
		string = "x intersection y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("intersection", "x", "y"));

		string = "w intersection x intersection y intersection z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("intersection", "w", "x", "y", "z"));

		string = "{x} intersection y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("intersection", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x intersection y} intersection y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("intersection", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("intersection", "x", "y")), "y"));

		string = "intersection(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("intersection", "x"));

		string = "intersection";
		test(string, Expressions.createSymbol("intersection"));
		
		string = "intersection * 8";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "intersection", "8"));

		string = "intersection()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("intersection"));

		string = "intersection(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("intersection", "x", "y"));

		// Testing illegal strings.
		string = "x intersection";
		testFail(string);

		string = "intersection x";
		testFail(string);
	}

	@Test
	public void testUnion () {
		String string;
		string = "x union y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "x", "y"));

		string = "w union x union y union z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "w", "x", "y", "z"));

		string = "x union y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} union y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} union y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "union(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "x"));

		string = "union";
		test(string, Expressions.createSymbol("union"));

		string = "union()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union"));

		string = "union(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "x", "y"));

		// Testing illegal strings.
		string = "x union";
		testFail(string);

		string = "union x";
		testFail(string);
	}

	@Test
	public void testIn () {
		String string;
		string = "x in y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y"));
		
		string = "w in x in y in z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in",  
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "w", "x"), "y"), "z"));

		string = "x in y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} in y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} in y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "in(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x"));

		string = "in";
		test(string, Expressions.createSymbol("in"));

		string = "in()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in"));

		string = "in(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y"));

		// Testing illegal strings.
		string = "x in";
		testFail(string);

		string = "in x";
		testFail(string);
	}

	@Test
	public void testLessThanEqual () {
		String string;
		string = "x <= y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "x", "y"));
		
		string = "x<=y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "x", "y"));
		
		string = "w <= x <= y <= z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=",  
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "w", "x"), "y"), "z"));

		string = "x <= y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} <= y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} <= y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "<=(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "x"));

		string = "<=";
		test(string, Expressions.createSymbol("<="));

		string = "<=()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<="));

		string = "<=(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "x", "y"));

		// Testing illegal strings.
		string = "x <=";
		testFail(string);

		string = "<= x";
		testFail(string);
	}

	@Test
	public void testLessThan () {
		String string;
		string = "x < y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "x", "y"));
		
		string = "x<y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "x", "y"));
		
		string = "w < x < y < z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<",  
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "w", "x"), "y"), "z"));

		string = "x < y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} < y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} < y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "'<'(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "x"));

		string = "'<'";
		test(string, Expressions.createSymbol("<"));

		string = "'<'()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<"));

		string = "'<'(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "x", "y"));

		// Testing illegal strings.
		string = "x <";
		testFail(string);

		string = "< x";
		testFail(string);
	}

	@Test
	public void testGreaterThanEqual () {
		String string;
		string = "x >= y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "x", "y"));
		
		string = "x>=y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "x", "y"));
		
		string = "w >= x >= y >= z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=",  
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "w", "x"), "y"), "z"));

		string = "x >= y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} >= y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} >= y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")),
				"y"));

		string = ">=(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "x"));

		string = ">=";
		test(string, Expressions.createSymbol(">="));

		string = ">=()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">="));

		string = ">=(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "x", "y"));

		// Testing illegal strings.
		string = "x >=";
		testFail(string);

		string = ">= x";
		testFail(string);
	}

	@Test
	public void testGreaterThan () {
		String string;
		string = "x > y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y"));
		
		string = "x>y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y"));
		
		string = "w > x > y > z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">",  
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "w", "x"), "y"), "z"));

		string = "x > y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} > y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} > y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "'>'(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "x"));

		string = "'>'";
		test(string, Expressions.createSymbol(">"));

		string = "'>'()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">"));

		string = "'>'(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y"));

		// Testing illegal strings.
		string = "x >";
		testFail(string);

		string = "> x";
		testFail(string);
	}

	@Test
	public void testNotEqual () {
		String string;
		string = "x != y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "x", "y"));
		
		string = "x!=y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "x", "y"));
		
		string = "w != x != y != z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=",  
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "w", "x"), "y"), "z"));

		string = "x != y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} != y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} != y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "!=(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "x"));

		string = "!=";
		test(string, Expressions.createSymbol("!="));

		string = "!=()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!="));

		string = "!=(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "x", "y"));

		// Testing illegal strings.
		string = "x !=";
		testFail(string);

		string = "!= x";
		testFail(string);
	}

	@Test
	public void testEqual () {
		String string;
		string = "x = y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "x", "y"));

		string = "x=y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "x", "y"));

		string = "w = x = y = z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
				"w", "x", "y", "z"));

		string = "x = y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} = y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} = y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "=(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "x"));

		string = "=";
		test(string, Expressions.createSymbol("="));

		string = "=()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("="));

		string = "=(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "x", "y"));

		// Testing illegal strings.
		string = "x =";
		testFail(string);

		string = "= x";
		testFail(string);
	}

	@Test
	public void testAnd () {
		String string;
		
		string = "x and (y and z)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "y", "z")));
		
		string = "x and y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x", "y"));

		string = "w and x and y and z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
				"w", "x", "y", "z"));

		string = "x and y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "x in y and y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y"), "y"));

		string = "{x} and y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} and y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "and(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x"));

		string = "and";
		test(string, Expressions.createSymbol("and"));

		string = "and()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and"));

		string = "and(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x", "y"));

		// Testing illegal strings.
		string = "x and";
		testFail(string);

		string = "and x";
		testFail(string);
	}

	@Test
	public void testOr () {
		String string;
		
		string = "x or (y or z)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "y", "z")));
		
		string = "x or y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y"));

		string = "w or x or y or z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
				"w", "x", "y", "z"));

		string = "x or y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "x in y or y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y"), "y"));

		string = "{x} or y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} or y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "or(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x"));

		string = "or";
		test(string, Expressions.createSymbol("or"));

		string = "or()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or"));

		string = "or(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y"));

		// Testing illegal strings.
		string = "x or";
		testFail(string);

		string = "or x";
		testFail(string);
	}

	@Test
	public void testBiconditional () {
		String string;
		string = "x <=> y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>", "x", "y"));
		
		string = "x<=>y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>", "x", "y"));
		
		string = "w <=> x <=> y <=> z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>", "w", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>",  "x", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>", "y", "z"))));

		string = "x <=> y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} <=> y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} <=> y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "<=>(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>", "x"));

		string = "<=>";
		test(string, Expressions.createSymbol("<=>"));

		string = "<=>()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>"));

		string = "<=>(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=>", "x", "y"));

		// Testing illegal strings.
		string = "x <=>";
		testFail(string);

		string = "<=> x";
		testFail(string);
	}

	@Test
	public void testImplication() {
		String string;
		string = "x => y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", "x", "y"));
		
		string = "x=>y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", "x", "y"));
		
		string = "w => x => y => z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", "w", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", "x", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", "y", "z"))));

		string = "x => y + z";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} => y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} => y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")),
				"y"));

		string = "=>(x)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", "x"));

		string = "=>";
		test(string, Expressions.createSymbol("=>"));

		string = "=>()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>"));

		string = "=>(x, y)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", "x", "y"));

		// Testing illegal strings.
		string = "x =>";
		testFail(string);

		string = "=> x";
		testFail(string);
	}

	@Test
	public void testExists () {
		String string;
		string = "there exists a : b";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "a", "b"));
		
		string = "there exists a : there exists b : c";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "a", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "b", "c")));
		
		string = "'there exists . : .'(a, b)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "a", "b"));

		// Testing illegal strings
		string = "there exi a : b";
		testFail(string);

		string = "there exists a";
		testFail(string);

		string = "there exists a b";
		testFail(string);

		string = "there exists : b";
		testFail(string);
	}
	
	@Test
	public void testForAll () {
		String string;
		string = "for all x : y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "x", "y"));

		string = "for all x = 5 : a";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "x", "5"), "a"));

		string = "for all x : for all y : for all z : true";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "x", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "y", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "z", "true"))));

		string = "for all Y : (for all X : ((X != Y) => (there exists W : (there exists Z : ((Z != a) => (Alpha = Beta))))))";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "Y", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "X", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "W", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "Z", 
												Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"), 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => there exists W : there exists Z : (Z != a => Alpha = Beta)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "Y", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "X", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "W", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "Z", 
												Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"), 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => (there exists W : there exists Z : Z != a => Alpha = Beta)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "Y", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "X", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "W", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "Z", 
												Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"), 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => there exists W : there exists Z : Z != a => Alpha = Beta";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "Y", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "X", 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y"), 
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "W", 
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "Z", 
												Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=>", 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"), 
														Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Alpha", "Beta"))))))));
	}

	@Test
	public void testIfThenElse () {
		String string;
		string = "if a then b else c";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				"a", "b", "c"));

		string = "'if . then . else .'(a, b, c)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "a", "b", "c"));

		// Testing illegal strings
		string = "if a then b";
		testFail(string);

		string = "if a else c";
		testFail(string);

		string = "if a then b else";
		testFail(string);

		string = "if a then else c";
		testFail(string);

		string = "if then b else c";
		testFail(string);

		string = "if a than b else c";
		testFail(string);

		string = "if a then b ele c";
		testFail(string);
	}
	
	@Test
	public void testLambda () {
		String string;
		string = "lambda x : a";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", "x", "a"));

		string = "lambda x, y, z : a";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z"), "a"));

		string = "lambda x, y, z : lambda a : b";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", "a", "b")));

		// Testing illegal strings
		string = "lambda a :";
		testFail(string);
	}

	@Test
	public void testMessage () {
		String string;
		string = "message to a from b";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("message to . from .", "a", "b"));

		string = "'message to . from .'(a, b)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("message to . from .", "a", "b"));

		// Testing illegal strings
		string = "message from a to";
		testFail(string);

		string = "message from to b";
		testFail(string);

		string = "message fom a to b";
		testFail(string);

		string = "message from a too b";
		testFail(string);

		string = "message from to ";
		testFail(string);
	}

	@Test
	public void testPreviousMessage () {
		String string;
		string = "previous message to a from b";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("previous message to . from .", "a", "b"));

		string = "'previous message to . from .'(a, b)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("previous message to . from .", "a", "b"));

		// Testing illegal strings
		string = "previous message from a to";
		testFail(string);

		string = "previous message from to b";
		testFail(string);

		string = "previous message fom a to b";
		testFail(string);

		string = "previous message from a too b";
		testFail(string);

		string = "previous message from to ";
		testFail(string);
	}

	@Test
	public void testPrecedence() {
		String string;
		string = "a^a-a";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-",
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "a", "a"), "a"));

		string = "a*a+a";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+",
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "a", "a"), "a"));

		string = "a+a*a";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "a",
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "a", "a")));

		string = "-a*a+a";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+",
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "a"), "a"), "a"));

		string = "-a*-a-a";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-",
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "a"),
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "a")), "a"));

		string = "-a*-a^a-a";
		test(string,
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*",
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "a"),
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^",
										Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "a"), "a")), "a"));

		string = "-a*-a^-a";
		test(string, 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "a"),
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^",
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "a"),
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "a"))));
	}

	@Test
	public void testFunctionsAndSequences() {
		String string;
		string = "x+y";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+","x","y"));

		string = "f()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f"));

		string = "f(10)";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 10));

		string = "f(10 + a)";
		test(string, 								
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(
								"+", 10, "a")));

		string = "f(10+a) - f(a, b)";
		test(string,
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f",
								Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 10, "a")),
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")));
		
		string = "f(10)-f()";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-",
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 10),
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f")));

		string = "10 + 10";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 10, 10));

		string = "10 + x*20";
		test(string, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 10, 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x",20)));

		string = "f(10, 20, 30, f (x), f, (x))";
		test(string,
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 10, 20, 30, 
						Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "x"), "f", "x"));
	}
	
	
	@Test
	public void testGrinder () {
		String expression;
		expression = "if X = 1 then 0.0003 + 0.000000000001 else 0.150004 + 0.1 + 0.776699029126213691398561";
		test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "1"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "0.0003", "0.000000000001"), 
				Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "0.150004", "0.1", "0.776699029126213691398561")));


			expression = "3";
			test(expression, Expressions.createSymbol(3));

			expression = "x + y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y"));

			expression = "1 + 2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2"));

			expression = "x + 2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "2"));

			expression = "+(x, 2, y, 6)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "2", "y", "6"));

			expression = "+(x, +(1, y), 11)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 1, "y"), "11"));

			expression = "+(x, 2, 1 + 2, 1 + y, 6)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", 2, 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 1, 2), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 1, "y"), "6"));

			expression = "+(x, 2, 3)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", 2, 3));

			expression = "+(x)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x"));

			expression = "+";
			test(expression, Expressions.createSymbol("+"));

			expression = "1";
			test(expression, Expressions.createSymbol(1));

			expression = "x";
			test(expression, Expressions.createSymbol("x"));

			expression = "+()";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+"));

			expression = "3 - 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 3, 1));

			expression = "1 - 3";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 1, 3));

			expression = "X - Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "X", "Y"));

			expression = "X - 0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "X", 0));

			expression = "0 - X";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 0, "X"));

			expression = "-1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "1"));

			expression = "-x";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

			expression = "3";
			test(expression, Expressions.createSymbol(3));

			expression = "x * y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"));

			expression = "2 * 2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 2, 2));

			expression = "x * 2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", 2));

			expression = "*(x, 2, y, 6)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", 2, "y", 6));

			expression = "*(x, 0, y, 6)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", 0, "y", 6));

			expression = "*(x, 2, 1 * 2, 1 * y, 6)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", 2, 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 1, 2),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 1, "y"), 6));

			expression = "*(x, 2, 3)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", 2, 3));

			expression = "*(x)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x"));

			expression = "*";
			test(expression, Expressions.createSymbol("*"));

			expression = "*()";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*"));

			expression = "3^2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", 3, 2));

			expression = "x^1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "x", 1));

			expression = "x^0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "x", 0));

			expression = "x^0.0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "x", "0.0"));

			expression = "1^n";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", 1, "n"));

			expression = "2^n";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", 2, "n"));

			expression = "4/2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", 4, 2));

			expression = "4.0/2.0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "4.0", "2.0"));

			expression = "4.0/3.0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "4.0", "3.0"));

			expression = "a/b";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", "a", "b"));

			expression = "4/0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", 4, 0));

			expression = "4/2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", 4, 2));

			expression = "(4*2)/(2*3)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 4, 2),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 2, 3)));

			expression = "(4*2)/2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 4, 2), 2));

			expression = "2/2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("/", 2, 2));

			expression = "3 > 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 3, 1));

			expression = "3 > 3";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 3, 3));

			expression = "1 > 3";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 1, 3));

			expression = "1 > y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 1, "y"));

			expression = "1 > false";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 1, "false"));

			expression = "3 >= 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", 3, 1));

			expression = "3 >= 3";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", 3, 3));

			expression = "1 >= 3";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", 1, 3));

			expression = "1 >= y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", 1, "y"));

			expression = "1 >= false";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", 1, "false"));

			expression = "not (3 > 1)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 3, 1)));

			expression = "not(3 > 3)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 3, 3)));

			expression = "true and x";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "true", "x"));

			expression = "true";
			test(expression, Expressions.createSymbol("true"));

			expression = "x and y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x", "y"));

			expression = "true and false";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "true", "false"));

			expression = "x and false";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x", "false"));

			expression = "x and false and y and true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
					"x", "false", "y", "true"));

			expression = "and(x, false, false and true, false and y, false)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x", "false", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "false", "true"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "false", "y"),
					"false"));

			expression = "and(x, true, false)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x", "true", "false")); 


			expression = "and(x)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x"));

			expression = "and";
			test(expression, Expressions.createSymbol("and"));

			expression = "and()";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and")); 

			expression = "false or x";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "false", "x"));

			expression = "x or false";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", "false"));

			expression = "true";
			test(expression, Expressions.createSymbol(true));

			expression = "x or y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y"));

			expression = "true or false";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "true", "false"));

			expression = "x or true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", "true"));

			expression = "x or false or y or true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
					"x", "false", "y", "true"));

			expression = "or()";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or")); 

			expression = "x or y or x or y or z";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
					"x", "y", "x", "y", "z"));

			expression = "x or x";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", "x"));

			expression = "not true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "true"));

			expression = "not false";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "false"));

			expression = "not x";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"));

			expression = "not x and y and x";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"), "y", "x"));

			expression = "not not x";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x")));

			expression = "'index of . in .'(b, f(a,b))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("index of . in .", "b", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")));

			expression = "'index of . in .'(c, f(a,b))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("index of . in .", "c", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")));

			expression = "'index of . in .'(c, f(a,b)) > 0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("index of . in .", "c", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")), "0"));

			expression = "x + 2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", "2"));

			expression = "f(g(h(x)))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("h", "x"))));

			expression = "{(on x) x}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "x"), "x", null));

			expression = "x + {(on x) f(x)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "x"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "2 + {(on x) f(x)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 2,
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "x"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "x + {(on x in group(x)) f(x)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x",
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("group", "x"))),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "2 + {(on x in group(2)) f(x)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 2,
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "x",
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("group", 2))),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "x + {(on y) f(x)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "y"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "2 + {(on y) f(2)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 2,
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "y"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 2), null)));


			expression = "f(x)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "x"));

			expression = "f(f(x) + g(x))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "x"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "x"))));


			expression = "(1 + x + y)*(x + 3)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "1", "x", "y"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", 3)));

			expression = "1*x + 1*3 + x*x + x*3 + y*x + y*3";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 1, "x"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 1, 3),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", "x"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*",	"x", 3),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "y", "x"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "y", 3)));

			// Testing case where one one of the operator applications is empty.
			// We use an evaluator with the distributive law alone so +() does not get evaluated to 0.
			expression = "+()*(x + 3)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "x", 3)));

			expression = "(1 * x * y)+(x * 3)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "1", "x", "y"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "x", 3)));

			expression = "not x";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"));

			expression = "not(x and y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "x", "y")));

			expression = "not x or not y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "y")));

			expression = "not(x or y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y")));

			expression = "not x and not y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "y")));

			expression = "not(x or y or z)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y", "z")));

			expression = "not x and not y and not z";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "y"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "z")));

			expression = "not(x or y or (z and w))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "z", "w"))));

			expression = "not x and not y and (not z or not w)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "x"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "y"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "z"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", "w"))));

			expression = "'scoped variables'({(on X) p(X) | X != a})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("scoped variables",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|",
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "list(X)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "X"));

			expression = "'scoped variables'({(on X,Y) p(X,Y) | X != a})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("scoped variables",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|",
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "list(X, Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "X", "Y"));

			expression = "'scoped variables'(f(X,Y))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("scoped variables",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y")));

			expression = "list()";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list"));


			expression = "aConstantSymbol";
			test(expression, Expressions.createSymbol("aConstantSymbol"));

			expression = "if A = B then aAndBEqual else aAndBNotEqual";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "A", "B"),
					"aAndBEqual", "aAndBNotEqual"));

			expression = "{(on X) X | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));



			expression = "if true then 1 else 2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					"true", 1, 2));

			expression = "if false then 1 else 2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					"false", 1, 2));

			expression = "if X then 1 else 2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					"X", 1, 2));


			expression = "f(a, b, c, if Y = 2 then X else X + 1, d, e, f)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b", "c",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", 2), "X",
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)), 
							"d", "e", "f"));
			expression = "if Y = 2 then f(a, b, c, X, d, e, f) else f(a, b, c, X + 1, d, e, f)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "2"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b", "c", "X", "d", "e", "f"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b", "c", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1), "d", "e", "f")));

			expression = "{(on X) if Y = 2 then X else X + 1 | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", 2), "X", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "if Y = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", 2),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "{(on X) if X = 2 then X else X + 1 | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", 2), "X", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) if p(Y) = 2 then X else X + 1 | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "Y"), 2), "X", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "if p(Y) = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "Y"), 2),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));


			expression = "{(on X) if p(X) = 2 then X else X + 1 | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 2), "X", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X in (if Y = a then Set1 else Set2)) p(X) | X != a}"; // lack of ()'s around if then else makes parse fail, not sure why. Entered in bug database.
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a"),
											"Set1", "Set2"))),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			// lack of ()'s around if then else makes parse fail, due to precedence for "in".
//			expression = "{(on X in if Y = a then Set1 else Set2) p(X) | X != a}";
//			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
//					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
//							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
//									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
//											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a"),
//											"Set1", "Set2"))),
//					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
//					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
//							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "if Y = a then {(on X in Set1) p(X) | X != a} else {(on X in Set2) p(X) | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )",
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", "Set1")),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )",
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", "Set2")),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "{(on Y, X in (if Y = a then Set1 else Set2)) p(X) | X != a}"; // lack of ()'s around if then else makes parse fail, not sure why. Entered in bug database.
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "Y",
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a"),
													"Set1", "Set2")))),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "if X = a then X != a else X = Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"),
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")));

			expression = "if X = a then false else X = Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"),
					"false", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")));

			expression = "if X = a then f(X != a, 1, 2) else g(X = Y, a, b)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), "1", "2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), "a", "b")));

			expression = "if X = a then f(false, 1, 2) else g(X = Y, a, b)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "false", "1", "2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), "a", "b")));

			expression = "if X != a or Y != b then X != a else false";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), "false"));

			expression = "if X != a or Y != b then X != a or Y != b or Z != c else false";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c")), "false"));

			expression = "X != a or Y != b";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")));

			expression = "if X != a then X != a and Y = d else true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "d")), "true"));

			expression = "if X != a then Y = d else true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "d"), "true"));

			expression = "if X != a or Y != b then f(X != a or Y != b or Z != c) else true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c"))), "true"));

			expression = "if X != a or Y != b then f(true) else true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "true"), "true"));

			expression = "if X != a or Y != b then not(X != a or Y != b or Z != c) else true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c"))), "true"));

			expression = "if X != a or Y != b then false else true";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), "false", "true"));

			expression = "if A and B then if A then 1 else 0 else 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "A", "B"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "A", "1", "0"), "1"));

			expression = "1";
			test(expression, Expressions.createSymbol("1"));

			expression = "if X = a then if p(X) then E1 else E2 else if p(X) then E1 else E2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), "E1", "E2")));

			expression = "if X = a then if p(a) then E1 else E2 else if p(X) then E1 else E2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a"), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X, Y) then E1 else E2 else if p(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a, b) then E1 else E2 else if p(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a", "b"), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X) and q(Y) then E1 else E2 else if p(X) and q(Y) then E1 else E2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "Y")), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a) and q(b) then E1 else E2 else if p(X) and q(Y) then E1 else E2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "b")), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X) and q(X, Y) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2")));

			expression = ("if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2");
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "a", "b")), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2")));

			expression = "if even(X) then f(X) else X + 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1")));

			expression = "if X = a then 1 else 2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), "1", "2"));

			expression = "if X = a then f(X) else X + 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1")));

			expression = "if X = a then f(a) else X + 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1")));

			expression = "if X = a and Y = b then f(X,Y) else X + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = a and Y = b then f(a, b) else X + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a and Y = W then f(X,Y,Z) else X + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "W")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a and W = Y then f(a, W, a) else X + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "W", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a and W != Y then f(X,W,Z) else X + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a and W != Y then f(a, W, a) else X + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "W", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z and W != Y then f(X,W,Z) else X + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z and W != Y then f(X,W,Z) + {(on Z) foo(Z)} else Z + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), null)), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "Z", "Y")));

			expression = "if X = Z and W != Y then f(X, W, X) + {(on Z) foo(X)} else Z + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), null)), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "Z", "Y")));

			expression = "if X = Z = a or W != Y then f(X,W,Z) else X + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a or W != Y then f(X,W,Z) else X + W";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "W")));

			expression = "if X != a then {(on X) X != a} else {(on Y) X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null)));

			expression = "if X != a then {(on X) X != a} else {(on Y) false}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), "false", null)));

			expression = "if X != a then X + 1 else 42";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1"), "42"));

			expression = "if X != a then X + 1 else f(X)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X")));

			expression = "if X != a then X + 1 else f(a)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a")));

			expression = "if X != a or Y != b then X + Y else f(X,Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y")));

			expression = "if X != a or Y != b then  X + Y else f(a, b)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")));

			expression = "if X != Z or W != Y then f(X,W,Z) else foo(X, Z, W, Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Z"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Z", "W", "Y")));

			expression = "if X != Z or W != Y then f(X, W, Z) else foo(X, X, W, W)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Z"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "X", "W", "W")));

			expression = "if X != a and W != Y then f(X,W,Z) else X + Y";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X != a then {(on X) X != a} else 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null), "1"));

			expression = "if X = a then {(on Y) X != a} else {(on X) X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null)));

			expression = "if X = a then {(on Y) false} else {(on X) X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), "false", null), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null)));

			expression = "if X < 3 then f(X < 2, X < 3, X < 4) else g(X < 2, X < 3, X < 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4"))));

			expression = "if X < 3 then f(X < 2, true, true)     else g(false, false, X < 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), "true", "true"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false", "false", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4"))));

			expression = "if X < 3 then f(X <= 2, X <= 3, X <= 4) else g(X <= 2, X <= 3, X <= 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4"))));

			expression = "if X < 3 then f(X <= 2, true, true) else g(false, X <= 3, X <= 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), "true", "true"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4"))));

			expression = "if X < 3 then f(X > 2, X > 3, X > 4) else g(X > 2, X > 3, X > 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4"))));

			expression = "if X < 3 then f(X > 2, false, false) else g(true, X > 3, X > 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), "false", "false"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "true", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4"))));

			expression = "if X < 3 then f(X >= 2, X >= 3, X >= 4) else g(X >= 2, X >= 3, X >= 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4"))));

			expression = "if X < 3 then f(X >= 2, false, false) else g(true, true, X >= 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), "false", "false"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "true", "true", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4"))));

			expression = "if X <= 3 then f(X < 2, X < 3, X < 4) else g(X < 2, X < 3, X < 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4"))));

			expression = "if X <= 3 then f(X < 2, X < 3, true)     else g(false, false, X < 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), "true"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false", "false", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4"))));

			expression = "if X <= 3 then f(X <= 2, X <= 3, X <= 4) else g(X <= 2, X <= 3, X <= 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4"))));

			expression = "if X <= 3 then f(X <= 2, true, true) else g(false, false, X <= 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), "true", "true"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false", "false", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4"))));

			expression = "if X <= 3 then f(X > 2, X > 3, X > 4) else g(X > 2, X > 3, X > 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4"))));

			expression = "if X <= 3 then f(X > 2, false, false) else g(true, true, X > 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), "false", "false"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "true", "true", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4"))));

			expression = "if X <= 3 then f(X >= 2, X >= 3, X >= 4) else g(X >= 2, X >= 3, X >= 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4"))));

			expression = "if X <= 3 then f(X >= 2, X >= 3, false) else g(true, true, X >= 4)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), "false"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "true", "true", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4"))));

			expression = "if even(X) then f(even(X)) else g(even(X))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"))));

			expression = "if even(X) then f(true) else g(false)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "true"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if even(X) then f(even(Y)) else g(even(X))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"))));

			expression = "if even(X) then f(if Y = X then true else even(Y)) else g(false)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "X"), "true", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "Y"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if even(X) then {(on even(a)) f(even(Y))} else g(even(X))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "a")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "Y")), null), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"))));

			expression = "if even(X) then {(on even(a)) f(if Y != a and Y = X then true else even(Y))} else g(false)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "a")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "a"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "X")), "true", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("even", "Y"))), null), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if X = Y then f(X = Y) else g(X = Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))));

			expression = "if X = Y then f(true) else g(false)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "true"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if X = Y then f(X = Z) else g(X = Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))));

			expression = "if X = Y then f(X = Z) else g(false)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if X = Y then {(on X = a) f(X = Y)} else g(X = Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")), null), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))));

			expression = "if X = Y then {(on X = a) f(X = Y)} else g(false)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")), null), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "a", "b")), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "a", "b")), "E1", "E2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2")));

			expression = "sum({(on Person in World) if Person = rich then 2000 else 50 | Person = american})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"), "2000", "50"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american")))));

			expression = 
					"sum({(on Person in World) 2000 | Person = american and Person = rich})" +
					"+" + 
					"sum({(on Person in World) 50 | Person = american and not (Person = rich)})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), "2000", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), "50", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"))))))));
			
			expression = "f(X)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"));

			expression = "g(X',X',X) and h(Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "X'", "X'", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("h", "Y")));

			expression = "f(X',X'',Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "X''", "Y"));

			expression = "{(on X) X | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) X | X != b }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) X | X != b }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X', Y) f(X',Y) | X' != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X'", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))));

			expression = "{(on X) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) f(X)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), null));

			expression = "{(on X) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "f(X, Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"));

			expression = "{(on X') f(X',Y) | X' != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X'"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "f(X, Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"));

			expression = "{(on X', Y') f(X',Y') | X' != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X'", "Y'")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y'"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on Y) f(X, Y) }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), null));

			expression = "{(on X', Y) f(X',Y) | X' != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X'", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))));

			expression = "{(on X, X') f(X,X') | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "X'")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "X'"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "f(X)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"));

			expression = "{(on X'', X') f(X'',X') | X'' != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X''", "X'")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X''", "X'"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X''", "a"))));

			expression = "{(on X, X') f(X,X') | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "X'")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "X'"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "f(X, X')";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "X'"));

			expression = "{(on X'', X''') f(X'',X''') | X'' != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X''", "X'''")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X''", "X'''"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X''", "a"))));

			expression = "{(on Z) {(on X, Y) f(X,Y,Z) | X != a } | Z != c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c"))));

			expression = "{(on Y) f(X, Y, Z) }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z"), null));

			expression = "{(on Z') {(on X', Y) f(X',Y,Z') | X' != a } | Z' != c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z'"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X'", "Y")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y", "Z'"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z'", "c"))));

			expression = "{(on Z) {(on Z', Y) f(X,Y,Z') | X != a } | Z != c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "Z'", "Y")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z'"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c"))));

			expression = "{(on Y) f(X, Y, Z, Z') }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z", "Z'"), null));

			expression = "{(on Z''') {(on Z'', Y) f(X,Y,Z'') | X != a } | Z''' != c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z'''"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "Z''", "Y")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z''"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z'''", "c"))));

			expression = "{(on X, p(Z)) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "Z"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) f(X)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), null));

			expression = "{(on X) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X in {1,2,3}) p(X) | false}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "false")));

			expression = "{ }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "{(on X in {1,2,3} - {1}, Y in {1,2} union {3}) p(X,Y) + 1 + 1 | true and Z}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "1"))), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2")), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "3"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "1", "1"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", "true", "Z"))));

			expression = "{(on X in {2,3}, Y in {1,2,3}) p(X,Y) + 2 | Z}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "2", "3"))), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "Z")));

			expression = "{(on X in {1,2,3} - {1}, Y) p(X,Y) + 1 + 1}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "1"))), "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "1", "1"), null));

			expression = "{(on X in {2,3}, Y) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "2", "3"))), "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X,Y) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "if V = (<X>) then {(on X) p(X,Y) + 2} else 0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "V", 
							Expressions.createSymbol(Expressions.createSymbol("X"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null), "0"));

			expression = "{(on X, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X, Z, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Z", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X in set1, Z in set2, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", "set1"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Z", "set2"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "if V = (<X>) then {(on X, Y in {1,2,3}) p(X,Y) + 2} else 0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "V", 
							Expressions.createSymbol(Expressions.createSymbol("X"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null), "0"));

			expression = "{(on X,Y) f(X) | Z = X}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Z", "X"))));

			expression = "sum({(on X in {1,2,3}) 0 | X != 2})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), "0", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "2")))));

			expression = "product({(on X in {1,2,3}) 1 | X != 2})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), "1", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "2")))));

			expression = "sum(partition(A, B))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", "A", "B")));

			expression = "sum(partition())";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition")));

			expression = "{(on ) foo(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "if X != a and X != b then {foo(X)} else {}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{(on X in {1,2,3}) p(X) | false}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", "false")));

			expression = "{ }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "{(on X in {a,b,c}) foo(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "partition(" +
					"if first({a,b,c}) != a and first({a,b,c}) != b then { foo(first({a,b,c})) } else {}," +
					"{(on X in rest({a,b,c})) foo(X) | X != a and X != b})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("first", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("first", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), "b")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("first", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))))), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("rest", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))))), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b"))))));


			expression = "{(on X in {a,b,c}, Y in {1,2,3}) foo(X,Y) | X != a and Y != 1}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1")))));

			expression = "partition(" +
					"{(on                     Y in {1,2,3}) foo(first({a,b,c}),Y) | first({a,b,c}) != a and Y != 1}," +
					"{(on X in rest({a,b,c}), Y in {1,2,3}) foo(X,Y)              | X              != a and Y != 1})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("first", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), "Y"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("first", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("rest", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Y"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1"))))));


			expression = "{(on X in rest({a,b,c}), Y in {1,2,3}, Z) foo(X,Y) | X != a and Y != 1}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("rest", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), "Z")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1")))));

			expression = "partition(" +
					"{(on X in rest({a,b,c}),                     Z) foo(X, first({1,2,3})) | X != a and first({1,2,3}) != 1}," +
					"{(on X in rest({a,b,c}), Y in rest({1,2,3}), Z) foo(X,Y)               | X != a and Y              != 1})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("rest", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), "Z")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("first", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("first", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), "1")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("rest", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("rest", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), "Z")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Y"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1"))))));

			expression = "{(on X in {}) foo(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{(on X in {1,2,3}, Y in {a,b,c}) p(X,Y)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), null));

			expression = "{p(1,a), p(1,b), p(1,c), p(2,a), p(2,b), p(2,c), p(3,a), p(3,b), p(3,c)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "1", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "1", "b"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "1", "c"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "2", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "2", "b"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "2", "c"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "3", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "3", "b"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "3", "c"))));

			expression = "{(on X) if X = 2 then p(X) else q(X) | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "partition({(on X) p(X) | X != a and X = 2}, {(on X) q(X) | X != a and not(X = 2)})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2")))))));


			expression = "{(on X) if Y = 2 then p(X) else q(X) | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "2"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));


			expression = "sum({(on Person in World) if Person = rich then 2000 else 50 | Person = american})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"), "2000", "50"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american")))));

			expression = 
					"sum(" +
					"partition(" + 
					"{(on Person in World) 2000 | Person = american and Person = rich}," + 
					"{(on Person in World) 50 | Person = american and not (Person = rich)}))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), "2000", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich")))), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), "50", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"))))))));
		
			expression = "{(on X) if X = 2 and something then p(X) else q(X) | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2"), "something"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "partition({(on X) if something then p(X) else q(X) | X != a and X = 2}, {(on X) q(X) | X != a and not(X = 2)})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "something", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("q", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("not", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2")))))));

			expression = "{(on X in {(on Y) foo(Y)}) bar(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), null))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{(on Y) bar(foo(Y)) | foo(Y) != a and foo(Y) != b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), "b")))));

			expression = "{(on X in {(on Y) foo(Y) | Y != c}) bar(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "c"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{(on Y) bar(foo(Y)) | foo(Y) != a and foo(Y) != b and Y != c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), "b"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "c")))));

			expression = "{(on X in {(on Y in {(on Z) Z}) foo(Y) | Y != c}) bar(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), "Z", null))), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "c"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{(on Z) bar(foo(Z)) | foo(Z) != a and foo(Z) != b and Z != c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Z")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), "b"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c")))));

			expression = "{(on X in {(on Y in {(on Z) Z}) foo(Y) | Y != c}) bar(X) | X != a and X != b and 'for all'({(on Z) blah(Z)})}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), "Z", null))), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "c"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("blah", "Z"), null))))));

			expression = "{(on Z) bar(foo(Z)) | foo(Z) != a and foo(Z) != b and 'for all'({(on Z) blah(Z)}) and Z != c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("bar", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Z")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), "b"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("for all", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("blah", "Z"), null)), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c")))));

			expression = "{(on X in 'some set') p(X) | X != a} - {somethingElse}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", "some set")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "somethingElse")));

			expression = "{(on X in 'some set') p(X) | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", "some set")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) p(X) | X != a} - {p(Y)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "Y"))));

			expression = "{(on X) p(X) | X != a and X != Y}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y")))));

			expression = "{(on X) p(X) | X != a} - {p(a)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a"))));

			expression = "{(on X) p(X) | X != a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) p(X) | X != a} - {p(b)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "b"))));

			expression = "{(on X) p(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{ }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "{a, b, c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")));

			expression = "{a, a, b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "a", "b")));

			expression = "{a, b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b")));

			expression = "{a, b, b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "b")));

			expression = "{a, b, b, a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "b", "a")));

			expression = "{a, b, b, a, b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "b", "a", "b")));

			expression = "{X, Y}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")));

			expression = "if X = Y then {X} else {X,Y}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y"))));

			expression = "{X, Y, a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "a")));

			expression = 
					"if X = Y " +
					"then if X = a then {X} else {X,a} " +
					"else if X = a then {X,Y} else if Y = a then {X,Y} else {X,Y,a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "a"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "a"))))));

			expression = "partition({}, X, {}, Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")), "X", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")), "Y"));

			expression = "partition(X, Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", "X", "Y"));

			expression = "partition({}, X, {1}, Y, {2,3})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")), "X", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "1"), "Y", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "2", "3"))));

			expression = "partition({1,2,3}, X, Y)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")), "X", "Y"));

			expression = "partition({}, {})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "partition(X)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("partition", "X"));

			expression = "X";
			test(expression, Expressions.createSymbol("X"));

			expression = "(1, 2)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2")));

			expression = "(X, Y) = (X, Y, Z)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "Z"))));

			expression = "tuple(X, Y) = (X, Y, Z)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("tuple", "X", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "Z"))));

			expression = "(X, Y) = tuple(X, Y, Z)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("tuple", "X", "Y", "Z")));

			expression = "(X, Y, Z) = (a, b, c)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "Z")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))));

			expression = "X = a and Y = b and Z = c";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Z", "c")));

			expression = "tuple(X, Y, Z) = tuple(a, b, c)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("tuple", "X", "Y", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("tuple", "a", "b", "c")));

			expression = "tuple(X, Y, Z) = (a, b, c)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("tuple", "X", "Y", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))));

			expression = "(X, Y, Z) = tuple(a, b, c)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "Z")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("tuple", "a", "b", "c")));

			expression = "first(list(a,b,c))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("first", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "a", "b", "c")));

			expression = "first(list(a))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("first", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "a")));

			expression = "rest(list(a,b,c))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("rest", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "a", "b", "c")));

			expression = "list(b,c)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "b", "c"));

			expression = "rest(list(a))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("rest", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "a")));

			expression = "list()";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list"));

			expression = "size(list(a,b,c))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("size", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "a", "b", "c")));

			expression = "size(list(a))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("size", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "a")));

			expression = "size(list())";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("size", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list")));


			expression = "{x} union {y}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "y")));

			expression = "{x, y}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")));

			expression = "{x, y} union {z}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "z")));

			expression = "{x, y, z}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z")));

			expression = "{x, y} union {}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{} union {x, y} union {}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "A union {x, y} union {}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "A", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "A union {x, y}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "A", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));

			expression = "A union {x, y} union {z} union B";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "A", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "z"), "B"));

			expression = "A union {x, y, z} union B";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("union", "A", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z")), "B"));


			expression = "sum({{(on X) c}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null)));

			expression = "sum({{(on X) c}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null)));

			expression = "c * |{{(on X) c}}|";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", "c", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null))));

			expression = "product({{(on X) c}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null)));

			expression = "c ^ |{{(on X) c}}|";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("^", "c", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null))));

			expression = "sum({{(on X) f(Y)}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "Y"), null)));

			expression = "f(Y) * |{{(on X) f(Y)}}|";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "Y"), null))));

			expression = "sum({{(on X) f(X)}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), null)));

			expression = "sum({{1,2,3}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))));


			expression = "{}-{d}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d")));

			expression = "{}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "{d}-{}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{d}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"));

			expression = "{a,b,c}-{d}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d")));

			expression = "{a,b,c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")));

			expression = "{a,b,c}-{b}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b")));

			expression = "{a,c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "c")));

			expression = "{a,b,c}-{b,a}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "b", "a"))));

			expression = "{c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c"));

			expression = "{a,b,c}-{a,b,c}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))));

			expression = "{{a,b,c}}-{{a,X,c}}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "X", "c"))));

			expression = "if X = b then {} else { b }";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "b"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b")));

			expression = "{{X,a,b,Y,Y}}-{{X,X,Y,a}}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "a", "b", "Y", "Y")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "X", "Y", "a"))));

			expression = "if X = b then { Y } else if X = Y then { b } else {{ b, Y }}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "b"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "b", "Y")))));

			expression = "{(on X in {1,2,3}, Y in D) f(X) | X = Z}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "D"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"))));

			expression = "{(on Y in D) f(Z)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "D")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "Z"), null));

			expression = "{(on X in {1,2,3}, Y in D) f(X) | X = Z and h(X)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "D"))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("h", "X")))));

			expression = "{(on Y in D) f(Z) | h(Z)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "D")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("h", "Z"))));

			expression = "{(on X in {1,2,3}) f(X) | X = Z and h(X)}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("h", "X")))));

			expression = "if h(Z) then {f(Z)} else {}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("h", "Z"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "Z")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{{ ( on Y in People ) sum({{ ( on p(a1, Y) ) p(a1, Y) }}) | Y = a2 }}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "People")), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a1", "Y")), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a1", "Y"), null)), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a2"))));

			expression = "{{ sum({{ ( on p(a1, a2) ) p(a1, a2) }}) }}";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a1", "a2")), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("p", "a1", "a2"), null))));

			expression = "if Z = a then f(lambda Z : g(Z)) else 0";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "Z", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", "Z", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Z"))), "0"));

			expression = "'scoped variables'(lambda Z : Z)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("scoped variables", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", "Z", "Z")));

			expression = "list(Z)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", "Z"));

			expression = "'scoped variables'(lambda f(X) : f(X))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("scoped variables", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"))));

			expression = "list(f(X))";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("list", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X")));

			expression = "if X = a then lambda f(X) : f(X) else 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X")), "1"));

			expression = "if X = a then lambda f(a) : f(a) else 1";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a")), "1"));

			expression = "(lambda f(X) : 2 + f(X))(1)";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "2", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X"))), "1"));

			expression = "product({{(on X) f(X, Y) | X != a}}) * product({{(on Z) g(Z) | Z != b}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "b"))))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y}}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"))))));

			expression = "product({{ ( on Y ) (f(Y, Y) * g(Y)) | Y != a }})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "Y", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Y")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "a")))));

			expression = "product({{(on X, Y) f(X, Y) | X != a}}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"))))));

			expression = "product({{ (on X) (product({{(on Y) f(X, Y)}}) * g(X))  | X != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), null)), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "X")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "product({{(on X) f(X, Y)}}) * product({{(on Z) g(Z)}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), null)), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), null))));

			expression = "product({{ (on X) (f(X, Y) * g(X)) }})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "X")), null)));

			expression = "product({{(on X, Y, V) f(X, Y, V) | X != a}}) * product({{(on W, Z) g(Z, W) | Z != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "V")), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "V"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "W", "Z")), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Z", "W"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"))))));

			expression = "product({{ ( on X ) (product({{ ( on Y ) (product({{ ( on V ) f(X, Y, V) }}) * g(X, Y)) }})) | X != a }})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
															Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "V"), 
																	Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "V"), null)), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "X", "Y")), null)), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "sum({{(on X) f(X, Y) * g(Y) | X != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Y")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "g(Y) * sum({{(on X) f(X, Y) | X != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))))));

			expression = "sum({{(on X) f(X, Y) * g(Y) * h() | X != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("h")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "g(Y) * h() * sum({{(on X) f(X, Y) | X != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("h"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))))));

			expression = "sum({{(on X) g(Y) | X != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "g(Y) * | type(X) - { a } |";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("-", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("type", "X"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a")))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y }}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))))), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"))))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y and W = a}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "W", "a"))))));

			expression = "X + product({{(on X, Y) |{(on W) W }|  |  X != a and X = Y}})";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("+", "X", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("| . |", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("( on . )", "W"), "W", null)), 
									Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
													Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")))))));

			expression = "if X = a then type(X) else nothing";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("type", "X"), "nothing"));

			expression = "if X = a then f(X, type(X)) else nothing";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("type", "X")), "nothing"));

			expression = "if X = a then f(a, type(X)) else nothing";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "a", 
							Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("type", "X")), "nothing"));

			expression = "f(X,g())";
			test(expression, Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("f", "X", 
					Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees("g")));

			System.out.println("test count = " + testCount);
	}


	@Test
	public void testSpeed() throws IOException {
		String string;
		string = "1 + 2";
		test(string);

		string = "if A = Z then 9 + (| type(X) | - 1) * 8 else 16 + (| type(X) | - 2) * 7";
		test(string);

		string = "sum({{(on X) if X=Z then if X=A then 9 else 8 else if X=A then 8 else if Z=A then 8 else 7}})";
		test(string);
	}

	@Test
	public void testSpeedOfSingletonSet() throws IOException {
		String string;
		string = "{ f(X) }";
		test(string);
	}
	
	@Test
	public void testSpeedOfSetDefinition() throws IOException {
		String string;
		string = "{{ ( on X ) X | X != bob }}";
		test(string);
	}

	@Test
	public void testSpeedOfSetDefinition2() throws IOException {
		String string;
		string = "{{ ( on X ) X }}";
		test(string);
	}

	@Test
	public void testSpeedOfSetDefinition3() throws IOException {
		String string;
		string = "{{ X, Y, f(X, Y, Z), g() }}";
		test(string);
	}
}
