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
import com.sri.ai.expresso.helper.SyntaxTrees;
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
		test(string, SyntaxTrees.makeSymbol("e"));

		string = "3";
		test(string, SyntaxTrees.makeSymbol(3));

		string = "3e7";
		test(string, SyntaxTrees.makeSymbol(30000000));

		string = "3e-1";
		test(string, SyntaxTrees.makeSymbol("0.30"));

		string = ".3e7";
		test(string, SyntaxTrees.makeSymbol(3000000));

		string = ".3e-1";
		test(string, SyntaxTrees.makeSymbol("0.030"));

		string = "1.3e7";
		test(string, SyntaxTrees.makeSymbol(13000000));

		string = "1.3e-1";
		test(string, SyntaxTrees.makeSymbol("0.130"));

		string = "false";
		test(string, SyntaxTrees.makeSymbol(false));

		string = "keyword";
		test(string, SyntaxTrees.makeSymbol("keyword"));

		string = "Keyword";
		test(string, SyntaxTrees.makeSymbol("Keyword"));

		string = "foo";
		test(string, SyntaxTrees.makeSymbol("foo"));

		string = "foo1";
		test(string, SyntaxTrees.makeSymbol("foo1"));

		string = "foo10bar";
		test(string, SyntaxTrees.makeSymbol("foo10bar"));

		string = "1foo";
		test(string, SyntaxTrees.makeSymbol("1foo"));

		string = "'foo1'";
		test(string, SyntaxTrees.makeSymbol("foo1"));

		string = "foo1'";
		test(string, SyntaxTrees.makeSymbol("foo1'"));

		string = "foo1'''";
		test(string, SyntaxTrees.makeSymbol("foo1'''"));

		string = "'This is a test.'";
		test(string, SyntaxTrees.makeSymbol("This is a test."));

		string = "\"This is a test.\"";
		test(string, SyntaxTrees.makeSymbol("This is a test."));

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
		test(string, SyntaxTrees.makeSymbol("Test a"));
		
		string = "\"Test \\u0061\"";
		test(string, SyntaxTrees.makeSymbol("Test a"));

		string = "'Testing the  preservation \\t of	whitespace\\ncharacters.'";
		test(string, SyntaxTrees.makeSymbol("Testing the  preservation 	 of	whitespace\ncharacters."));

		string = "\"Testing the  preservation \\t of	whitespace\\ncharacters.\"";
		test(string, SyntaxTrees.makeSymbol("Testing the  preservation 	 of	whitespace\ncharacters."));

		string = "'This is a test *()#@$%!-_=+<>,./?:;\\'\"\\\"\\\\'";
		test(string, SyntaxTrees.makeSymbol("This is a test *()#@$%!-_=+<>,./?:;'\"\"\\"));

		string = "\"This is a test *()#@$%!-_=+<>,./?:;\\''\\\"\\\"\\\\\"";
		test(string, SyntaxTrees.makeSymbol("This is a test *()#@$%!-_=+<>,./?:;''\"\"\\"));
		
		string = "foo(bar1', 'bar2\\'', bar3''')";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", "bar1'", "bar2'", "bar3'''"));
	}
	
	@Test 
	public void testComment () {
		String string;
		string = "3";
		test(string, SyntaxTrees.makeSymbol(3));

		string = "3 // This is a test.\n";
		test(string, SyntaxTrees.makeSymbol(3));

		string = "// This is a test.\n 3";
		test(string, SyntaxTrees.makeSymbol(3));

		string = "3 // This is a test.";
		test(string, SyntaxTrees.makeSymbol(3));

		string = "3 // This is a test.\n + 4";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 3, 4));

		string = "// Test\n 3 // This is a test.\n + 4 // Test";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 3, 4));

		string = "3 /* This is a test. */";
		test(string, SyntaxTrees.makeSymbol(3));

		string = "/* This is a test. */ 3";
		test(string, SyntaxTrees.makeSymbol(3));

		string = "3 /* This is a test. */ + 4";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 3, 4));
	}

	@Test
	public void testParen () {
		String string;
		string = "(foo)";
		test(string, SyntaxTrees.makeSymbol("foo"));

		string = "(foo1)";
		test(string, SyntaxTrees.makeSymbol("foo1"));

		string = "(foo10bar)";
		test(string, SyntaxTrees.makeSymbol("foo10bar"));

		string = "(1foo)";
		test(string, SyntaxTrees.makeSymbol("1foo"));

		string = "('foo1')";
		test(string, SyntaxTrees.makeSymbol("foo1"));

		string = "(foo1')";
		test(string, SyntaxTrees.makeSymbol("foo1'"));

		string = "(foo1''')";
		test(string, SyntaxTrees.makeSymbol("foo1'''"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo"));

		string = "foo(1)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", 1));

		string = "foo(1, 2, 3)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", 1, 2, 3));

		string = "foo(bar)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", "bar"));

		string = "foo(bar1, bar2, bar3)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", "bar1", "bar2", "bar3"));

		string = "foo(1+2, bar in hello)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", 
				SyntaxTrees.makeCompoundSyntaxTree("+", 1, 2),
				SyntaxTrees.makeCompoundSyntaxTree("in", "bar", "hello")));

		string = "foo(1+2, (bar in hello))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", 
				SyntaxTrees.makeCompoundSyntaxTree("+", 1, 2),
				SyntaxTrees.makeCompoundSyntaxTree("in", "bar", "hello")));

		string = "'foo bar'()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo bar"));

		string = "'foo bar'(a)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo bar", "a"));

		string = "'foo bar'(a, b, c)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo bar", "a", "b", "c"));
		
		string = "foo(bar(X))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", 
				SyntaxTrees.makeCompoundSyntaxTree("bar", "X")));
		
		string = "foo(bar(X), baz(Y))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", 
				SyntaxTrees.makeCompoundSyntaxTree("bar", "X"), 
						SyntaxTrees.makeCompoundSyntaxTree("baz", "Y")));
		
		string = "foo(W, bar(X), Y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("foo", "W", 
				SyntaxTrees.makeCompoundSyntaxTree("bar", "X"), "Y"));

		string = "(lambda x : y)(a, b, c)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(
				SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", "x", "y"), "a", "b", "c"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("( . )", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list", "foo", "bar")));

		string = "(x, y, z)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("( . )", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y", "z")));

		string = "(a in b, x + y + z, i, j, k)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("( . )", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
						SyntaxTrees.makeCompoundSyntaxTree("in", "a", "b"), 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y", "z"), 
						"i", "j", "k")));

		string = "'( . )'()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("( . )"));

		string = "'( . )'(a)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("( . )", "a"));

		string = "'( . )'(a, b, c)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("( . )", "a", "b", "c"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2"));

		string = "1 + 2 + 3";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2", "3"));

		string = "1 + 2 + 3 + 4";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2", "3", "4"));

		string = "1 + 2 + 3 + 4 + 5";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2", "3", "4", "5"));

		string = "1 + 2 + 3 + 4 + 5 + 6";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2", "3", "4", "5", "6"));

		string = "1 + 2 + 3 + 4 + 5 + 6 + 7";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2", "3", "4", "5", "6", "7"));
	}

	@Test
	public void testAssocTimes() throws IOException {
		String string;
		string = "1 * 2";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "1", "2"));

		string = "1 * 2 * 3";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "1", "2", "3"));

		string = "1 * 2 * 3 * 4";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "1", "2", "3", "4"));

		string = "1 * 2 * 3 * 4 * 5";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "1", "2", "3", "4", "5"));

		string = "1 * 2 * 3 * 4 * 5 * 6";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "1", "2", "3", "4", "5", "6"));

		string = "1 * 2 * 3 * 4 * 5 * 6 * 7";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocUnion() throws IOException {
		String string;
		string = "1 union 2";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "1", "2"));

		string = "1 union 2 union 3";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "1", "2", "3"));

		string = "1 union 2 union 3 union 4";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "1", "2", "3", "4"));

		string = "1 union 2 union 3 union 4 union 5";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "1", "2", "3", "4", "5"));

		string = "1 union 2 union 3 union 4 union 5 union 6";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "1", "2", "3", "4", "5", "6"));

		string = "1 union 2 union 3 union 4 union 5 union 6 union 7";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocEqual() throws IOException {
		String string;
		string = "1 = 2";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "1", "2"));

		string = "1 = 2 = 3";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "1", "2", "3"));

		string = "1 = 2 = 3 = 4";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "1", "2", "3", "4"));

		string = "1 = 2 = 3 = 4 = 5";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "1", "2", "3", "4", "5"));

		string = "1 = 2 = 3 = 4 = 5 = 6";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "1", "2", "3", "4", "5", "6"));

		string = "1 = 2 = 3 = 4 = 5 = 6 = 7";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocAnd() throws IOException {
		String string;
		string = "1 and 2";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "1", "2"));

		string = "1 and 2 and 3";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "1", "2", "3"));

		string = "1 and 2 and 3 and 4";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "1", "2", "3", "4"));

		string = "1 and 2 and 3 and 4 and 5";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "1", "2", "3", "4", "5"));

		string = "1 and 2 and 3 and 4 and 5 and 6";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "1", "2", "3", "4", "5", "6"));

		string = "1 and 2 and 3 and 4 and 5 and 6 and 7";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocOr() throws IOException {
		String string;
		string = "1 or 2";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "1", "2"));

		string = "1 or 2 or 3";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "1", "2", "3"));

		string = "1 or 2 or 3 or 4";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "1", "2", "3", "4"));

		string = "1 or 2 or 3 or 4 or 5";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "1", "2", "3", "4", "5"));

		string = "1 or 2 or 3 or 4 or 5 or 6";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "1", "2", "3", "4", "5", "6"));

		string = "1 or 2 or 3 or 4 or 5 or 6 or 7";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "1", "2", "3", "4", "5", "6", "7"));
	}
	
	@Test
	public void testAssoc () {
		String string;
		string = "(1 + 2 + 3 + 4) * (5 + 6 + 7 + 8) * (9 + 10 + 11 + 12)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2", "3", "4"), 
				SyntaxTrees.makeCompoundSyntaxTree("+", "5", "6", "7", "8"), 
				SyntaxTrees.makeCompoundSyntaxTree("+", "9", "10", "11", "12")));

		string = "(1 + 2 + 3 + 4) * (5 + 6 + 7 + 8) * (9 + 10 + 11 + (12 * 13 * 14 * 15))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2", "3", "4"), 
				SyntaxTrees.makeCompoundSyntaxTree("+", "5", "6", "7", "8"), 
				SyntaxTrees.makeCompoundSyntaxTree("+", "9", "10", "11", 
						SyntaxTrees.makeCompoundSyntaxTree("*", "12", "13", "14", "15"))));

		string = "(1 or 2 or 3 or 4) and (5 or 6 or 7 or 8) and (9 or 10 or 11 or (12 and 13 and 14 and 15))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", 
				SyntaxTrees.makeCompoundSyntaxTree("or", "1", "2", "3", "4"), 
				SyntaxTrees.makeCompoundSyntaxTree("or", "5", "6", "7", "8"), 
				SyntaxTrees.makeCompoundSyntaxTree("or", "9", "10", "11", 
						SyntaxTrees.makeCompoundSyntaxTree("and", "12", "13", "14", "15"))));

		// Testing to make sure the associative node walker doesn't get confused by the function form of
		// the operators.
		string = "+(+(b, c))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "b", "c")));

		string = "+(+(b))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "b")));

		string = "+(a, +(+(b)))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "a", 
				SyntaxTrees.makeCompoundSyntaxTree("+", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "b"))));

		string = "+(a, +(+(b, c)))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "a", 
				SyntaxTrees.makeCompoundSyntaxTree("+", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "b", "c"))));

		string = "+({a}, +(+({b}, {c})))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "a"), 
				SyntaxTrees.makeCompoundSyntaxTree("+", 
						SyntaxTrees.makeCompoundSyntaxTree("+", 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "b"), 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "c")))));

		string = "union({a}, union(union({b}, {c})))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "a"), 
				SyntaxTrees.makeCompoundSyntaxTree("union", 
						SyntaxTrees.makeCompoundSyntaxTree("union", 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "b"), 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "c")))));

		string = "union({a}, union(union({b}, {c}), {d}))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "a"), 
				SyntaxTrees.makeCompoundSyntaxTree("union", 
						SyntaxTrees.makeCompoundSyntaxTree("union", 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "b"), 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "c")), 
						SyntaxTrees.makeCompoundSyntaxTree("{ . }", "d"))));

		string = "union({a}, union(union({b}, {c}), {d}, {E}))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "a"), 
				SyntaxTrees.makeCompoundSyntaxTree("union", 
						SyntaxTrees.makeCompoundSyntaxTree("union", 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "b"), 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "c")), 
						SyntaxTrees.makeCompoundSyntaxTree("{ . }", "d"), 
						SyntaxTrees.makeCompoundSyntaxTree("{ . }", "E"))));

		string = "union({a}, union(union({b}, {c}), {d}, {E}, {f}))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "a"), 
				SyntaxTrees.makeCompoundSyntaxTree("union", 
						SyntaxTrees.makeCompoundSyntaxTree("union", 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "b"), 
								SyntaxTrees.makeCompoundSyntaxTree("{ . }", "c")), 
						SyntaxTrees.makeCompoundSyntaxTree("{ . }", "d"), 
						SyntaxTrees.makeCompoundSyntaxTree("{ . }", "E"), 
						SyntaxTrees.makeCompoundSyntaxTree("{ . }", "f"))));
	}

	@Test
	public void testExpressionSymbol () {
		String string;
		
		string = "<x>";
		test(string, SyntaxTrees.makeSymbol(SyntaxTrees.makeSymbol("x")));

		string = "<foo''>";
		test(string, SyntaxTrees.makeSymbol(SyntaxTrees.makeSymbol("foo''")));

		string = "<x in y>";
		test(string, SyntaxTrees.makeSymbol(
				SyntaxTrees.makeCompoundSyntaxTree("in", "x", "y")));

		string = "<{(on x, y) f(x,y) | y}>";
		test(string, SyntaxTrees.makeSymbol(
				SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
						SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
								SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y")), 
						SyntaxTrees.makeCompoundSyntaxTree("f", "x", "y"), 
						SyntaxTrees.makeCompoundSyntaxTree("|", "y"))));

		string = "< x < y >";
		test(string, SyntaxTrees.makeSymbol(
				SyntaxTrees.makeCompoundSyntaxTree("<", "x", "y")));

		string = "<x>y>";
		test(string, SyntaxTrees.makeSymbol(
				SyntaxTrees.makeCompoundSyntaxTree(">", "x", "y")));

		string = "< x > y >";
		test(string, SyntaxTrees.makeSymbol(
				SyntaxTrees.makeCompoundSyntaxTree(">", "x", "y")));

		string = "< if x > y then x else y >";
		test(string, SyntaxTrees.makeSymbol(
				SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
						SyntaxTrees.makeCompoundSyntaxTree(">", "x", "y"), "x", "y")));
		
		string = "if <x> > <y> then x else y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .",
				SyntaxTrees.makeCompoundSyntaxTree(">",
						SyntaxTrees.makeSymbol(SyntaxTrees.makeSymbol("x")),
						SyntaxTrees.makeSymbol(SyntaxTrees.makeSymbol("y"))
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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("| . |", "foo"));

		string = "| foo in bar |";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("| . |", 
				SyntaxTrees.makeCompoundSyntaxTree("in", "foo", "bar")));

		string = "| {} |";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("| . |", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }",
						SyntaxTrees.makeCompoundSyntaxTree("kleene list"))));

		string = "| { foo, bar } |";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("| . |", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }",
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "foo", "bar"))));

		string = "| ({ foo, bar }) |";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("| . |", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }",
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "foo", "bar"))));

		string = "| 1 + 2 |";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("| . |", 
				SyntaxTrees.makeCompoundSyntaxTree("+", 1, 2)));

		string = "'| . |'()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("| . |"));

		string = "'| . |'(a)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("| . |", "a"));

		string = "'| . |'(a, b, c)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("| . |", "a", "b", "c"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", IntensionalSet.makeScopingSyntaxTree(new ArrayList<Expression>()), 
				SyntaxTrees.makeCompoundSyntaxTree("[ . ]",
						SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", "X", "1", "0")), null));
		
		string = "{{ foo }}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", "foo"));

		string = "{{ (on foo) bar }}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
				SyntaxTrees.makeCompoundSyntaxTree("( on . )", "foo"), "bar", null));

		string = "{{ (on x in y) z }}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
				SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
						SyntaxTrees.makeCompoundSyntaxTree("in", "x", "y")), "z", null));

		string = "{{ foo | bar }}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", null, "foo", 
				SyntaxTrees.makeCompoundSyntaxTree("|", "bar")));

		string = "{{f(X) | false}}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", null, 
				SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
				SyntaxTrees.makeCompoundSyntaxTree("|", "false")));
		
		string = "{{ [if p(a) then 1 else 0] | true }}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", null, 
				SyntaxTrees.makeCompoundSyntaxTree("[ . ]", 
						SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
								SyntaxTrees.makeCompoundSyntaxTree("p", "a"), "1", "0")), 
				SyntaxTrees.makeCompoundSyntaxTree("|", "true")));

		string = "{{ (on foo, fooz) bar }}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
				SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "foo", "fooz")), 
						"bar", null));

		string = "{{ (on foo, fooz) bar | barz }}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
				SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "foo", "fooz")), 
						"bar", SyntaxTrees.makeCompoundSyntaxTree("|", "barz")));

		string = "{{ foo, bar, foo + bar }}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list", "foo", "bar", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "foo", "bar"))));

		string = "{{}}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list")));

		string = "'{{ . }}'()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . }}"));

		string = "'{{ . }}'(a)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", "a"));

		string = "'{{ . }}'(a, b, c)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", "a", "b", "c"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", IntensionalSet.makeScopingSyntaxTree(new ArrayList<Expression>()), 
				SyntaxTrees.makeCompoundSyntaxTree("p", "X", "X"), Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, "true")));

		string = "{ ( on ) X | true }";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", IntensionalSet.makeScopingSyntaxTree(new ArrayList<Expression>()),  
				"X", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, "true")));
				
		string = "{ a | true}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", null, "a", Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, "true")));
		
		string = "{ foo }";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . }", "foo"));

		string = "{ (on foo) bar }";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
				SyntaxTrees.makeCompoundSyntaxTree("( on . )", "foo"), "bar", null));

		string = "{ (on x in y) z }";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
				SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
						SyntaxTrees.makeCompoundSyntaxTree("in", "x", "y")), "z", null));

		string = "{ foo | bar }";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", null, "foo", 
				SyntaxTrees.makeCompoundSyntaxTree("|", "bar")));

		string = "{f(X) | false}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", null, 
				SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
				SyntaxTrees.makeCompoundSyntaxTree("|", "false")));
		
		string = "{ [if p(a) then 1 else 0] | true }";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", null, 
				SyntaxTrees.makeCompoundSyntaxTree("[ . ]", 
						SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
								SyntaxTrees.makeCompoundSyntaxTree("p", "a"), "1", "0")), 
				SyntaxTrees.makeCompoundSyntaxTree("|", "true")));

		string = "{ (on foo, fooz) bar }";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
				SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "foo", "fooz")), 
						"bar", null));

		string = "{ (on foo, fooz) bar | barz }";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
				SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "foo", "fooz")), 
						"bar", SyntaxTrees.makeCompoundSyntaxTree("|", "barz")));

		string = "{ foo, bar, foo + bar }";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list", "foo", "bar", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "foo", "bar"))));

		string = "{}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list")));

		string = "'{ . }'()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . }"));

		string = "'{ . }'(a)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . }", "a"));

		string = "'{ . }'(a, b, c)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("{ . }", "a", "b", "c"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("[ . ]", 
				SyntaxTrees.makeCompoundSyntaxTree("in", "x", "y")));

		string = "[ x+1 ]";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("[ . ]", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "x", 1)));

		string = "[ (x, y) ]";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("[ . ]", SyntaxTrees.makeCompoundSyntaxTree("( . )", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y"))));

		string = "'[ . ]'()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("[ . ]"));

		string = "'[ . ]'(a)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("[ . ]", "a"));

		string = "'[ . ]'(a, b, c)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("[ . ]", "a", "b", "c"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of factor", "x"));

		string = "neighbors of factor [x]";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of factor", 
				SyntaxTrees.makeCompoundSyntaxTree("[ . ]", "x")));

		string = "neighbors of factor {x}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of factor", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x")));

		string = "neighbors of factor {{x}}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of factor", 
				SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", "x")));

		string = "'neighbors of factor'(a)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of factor", "a"));

		string = "neighbors of factor neighbors of factor x";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of factor", 
				SyntaxTrees.makeCompoundSyntaxTree("neighbors of factor", "x")));
	}
	
	@Test
	public void testNeighborVariable () {
		String string;
		string = "neighbors of variable x";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of variable", "x"));

		string = "neighbors of variable [x]";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of variable", 
				SyntaxTrees.makeCompoundSyntaxTree("[ . ]", "x")));

		string = "neighbors of variable {x}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of variable", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x")));

		string = "neighbors of variable {{x}}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of variable", 
				SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", "x")));

		string = "'neighbors of variable'(a)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of variable", "a"));

		string = "neighbors of variable neighbors of variable x";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of variable", 
				SyntaxTrees.makeCompoundSyntaxTree("neighbors of variable", "x")));
	}
	
	@Test
	public void testNeighborOf () {
		String string;
		string = "neighbors of x from y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of . from .", "x", "y"));

		string = "neighbors of [x] from y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of . from .", 
				SyntaxTrees.makeCompoundSyntaxTree("[ . ]", "x"), "y"));

		string = "neighbors of {x} from y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of . from .", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "neighbors of {{x}} from y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of . from .", 
				SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", "x"), "y"));

		string = "'neighbors of . from .'(a, b)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of . from .", "a", "b"));

		string = "neighbors of neighbors of x from y from neighbors of a from b";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("neighbors of . from .", 
				SyntaxTrees.makeCompoundSyntaxTree("neighbors of . from .", "x", "y"), 
				SyntaxTrees.makeCompoundSyntaxTree("neighbors of . from .", "a", "b")));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("not", "x"));

		string = "not (x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("not", "x"));

		string = "not";
		test(string, SyntaxTrees.makeSymbol("not"));

		string = "not()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("not"));

		string = "not(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("not", 
				SyntaxTrees.makeCompoundSyntaxTree("( . )", 
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y"))));

		string = "not {x, y, z}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("not", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y", "z"))));

		string = "not (x + y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("not", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")));

		string = "not x + y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("not", "x"), "y"));
	}

	@Test
	public void testNegative () {
		String string;
		
		string = "-3";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "3"));
		
		string = "-3e7";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 30000000));

		string = "-3e-1";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "0.30"));

		string = "-.3e7";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 3000000));

		string = "-.3e-1";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "0.030"));

		string = "-1.3e7";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 13000000));

		string = "-1.3e-1";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "0.130"));
		
		string = "-x";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "x"));

		string = "- x";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "x"));

		string = "--x";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", SyntaxTrees.makeCompoundSyntaxTree("-", "x")));

		string = "-(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "x"));

		string = "-";
		test(string, SyntaxTrees.makeSymbol("-"));

		string = "-()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-"));

		string = "-(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
				SyntaxTrees.makeCompoundSyntaxTree("( . )", 
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y"))));

		string = "-x - y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
				SyntaxTrees.makeCompoundSyntaxTree("-", "x"), "y"));

		string = "x - -y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("-", "y")));

		string = "--x";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
						SyntaxTrees.makeCompoundSyntaxTree("-", "x")));

		string = "-(-x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
						SyntaxTrees.makeCompoundSyntaxTree("-", "x")));

		string = "- {x, y, z}";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y", "z"))));

		string = "- x + y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("-", "x"), "y"));

		string = "- (x + y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")));

		string = "(- x) + y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("-", "x"), "y"));
	}

	@Test
	public void testExponentiation() {
		String string;
		string = "x ^ y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("^", "x", "y"));

		string = "x^y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("^", "x", "y"));

		string = "w ^ x ^ y ^ z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("^", "w",
				SyntaxTrees.makeCompoundSyntaxTree("^",  "x",
						SyntaxTrees.makeCompoundSyntaxTree("^", "y", "z"))));

		string = "x ^ y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("^", "x", "y"), "z"));

		string = "{x} ^ y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("^", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} ^ y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("^", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")),	"y"));

		string = "^(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("^", "x"));

		string = "^";
		test(string, SyntaxTrees.makeSymbol("^"));

		string = "^()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("^"));

		string = "^(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("^", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("/", "x", "y"));

		string = "x/y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("/", "x", "y"));
		
		string = "w / x / y / z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("/", 
				SyntaxTrees.makeCompoundSyntaxTree("/",  
						SyntaxTrees.makeCompoundSyntaxTree("/", "w", "x"), "y"), "z"));

		string = "x / y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("/", "x", "y"), "z"));

		string = "{x} / y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("/", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} / y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("/", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")),
				"y"));

		string = "/(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("/", "x"));

		string = "/";
		test(string, SyntaxTrees.makeSymbol("/"));

		string = "/()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("/"));

		string = "/(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("/", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "x", "y"));

		string = "x*y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "x", "y"));

		string = "w * x * y * z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", 
				"w", "x", "y", "z"));

		string = "x * y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("*", "x", "y"), "z"));

		string = "{x} * y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} * y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "*(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "x"));

		string = "*";
		test(string, SyntaxTrees.makeSymbol("*"));

		string = "*()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*"));

		string = "*(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "x", "y"));
		
		string = "x-y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "x", "y"));
		
		string = "2-0";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "2", "0"));
		
		string = "w - x - y - z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
				SyntaxTrees.makeCompoundSyntaxTree("-",  
						SyntaxTrees.makeCompoundSyntaxTree("-", "w", "x"), "y"), "z"));

		string = "x - y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("-", "x", "y"), "z"));

		string = "{x} - y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} - y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "-(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", "x"));

		string = "-";
		test(string, SyntaxTrees.makeSymbol("-"));

		string = "-()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-"));

		string = "-(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-", 
				SyntaxTrees.makeCompoundSyntaxTree("( . )", 
						SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y"))));
	}

	@Test
	public void testPlus () {
		String string;
		string = "x + y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y"));

		string = "x+y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y"));

		string = "1+2";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2"));

		string = "1+-2";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "1", 
				SyntaxTrees.makeCompoundSyntaxTree("-", "2")));

		string = "w + x + y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				"w", "x", "y", "z"));

		string = "{x} + y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} + y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "+(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "x"));

		string = "+";
		test(string, SyntaxTrees.makeSymbol("+"));

		string = "+()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+"));

		string = "+(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("intersection", "x", "y"));

		string = "w intersection x intersection y intersection z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("intersection", "w", "x", "y", "z"));

		string = "{x} intersection y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("intersection", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x intersection y} intersection y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("intersection", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("intersection", "x", "y")), "y"));

		string = "intersection(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("intersection", "x"));

		string = "intersection";
		test(string, SyntaxTrees.makeSymbol("intersection"));
		
		string = "intersection * 8";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("*", "intersection", "8"));

		string = "intersection()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("intersection"));

		string = "intersection(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("intersection", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "x", "y"));

		string = "w union x union y union z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "w", "x", "y", "z"));

		string = "x union y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} union y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} union y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "union(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "x"));

		string = "union";
		test(string, SyntaxTrees.makeSymbol("union"));

		string = "union()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union"));

		string = "union(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("union", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("in", "x", "y"));
		
		string = "w in x in y in z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("in", 
				SyntaxTrees.makeCompoundSyntaxTree("in",  
						SyntaxTrees.makeCompoundSyntaxTree("in", "w", "x"), "y"), "z"));

		string = "x in y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("in", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} in y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("in", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} in y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("in", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "in(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("in", "x"));

		string = "in";
		test(string, SyntaxTrees.makeSymbol("in"));

		string = "in()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("in"));

		string = "in(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("in", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=", "x", "y"));
		
		string = "x<=y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=", "x", "y"));
		
		string = "w <= x <= y <= z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=", 
				SyntaxTrees.makeCompoundSyntaxTree("<=",  
						SyntaxTrees.makeCompoundSyntaxTree("<=", "w", "x"), "y"), "z"));

		string = "x <= y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} <= y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} <= y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "<=(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=", "x"));

		string = "<=";
		test(string, SyntaxTrees.makeSymbol("<="));

		string = "<=()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<="));

		string = "<=(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<", "x", "y"));
		
		string = "x<y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<", "x", "y"));
		
		string = "w < x < y < z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<", 
				SyntaxTrees.makeCompoundSyntaxTree("<",  
						SyntaxTrees.makeCompoundSyntaxTree("<", "w", "x"), "y"), "z"));

		string = "x < y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} < y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} < y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "'<'(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<", "x"));

		string = "'<'";
		test(string, SyntaxTrees.makeSymbol("<"));

		string = "'<'()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<"));

		string = "'<'(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">=", "x", "y"));
		
		string = "x>=y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">=", "x", "y"));
		
		string = "w >= x >= y >= z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">=", 
				SyntaxTrees.makeCompoundSyntaxTree(">=",  
						SyntaxTrees.makeCompoundSyntaxTree(">=", "w", "x"), "y"), "z"));

		string = "x >= y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">=", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} >= y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">=", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} >= y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">=", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")),
				"y"));

		string = ">=(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">=", "x"));

		string = ">=";
		test(string, SyntaxTrees.makeSymbol(">="));

		string = ">=()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">="));

		string = ">=(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">=", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">", "x", "y"));
		
		string = "x>y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">", "x", "y"));
		
		string = "w > x > y > z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">", 
				SyntaxTrees.makeCompoundSyntaxTree(">",  
						SyntaxTrees.makeCompoundSyntaxTree(">", "w", "x"), "y"), "z"));

		string = "x > y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} > y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} > y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "'>'(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">", "x"));

		string = "'>'";
		test(string, SyntaxTrees.makeSymbol(">"));

		string = "'>'()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">"));

		string = "'>'(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree(">", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("!=", "x", "y"));
		
		string = "x!=y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("!=", "x", "y"));
		
		string = "w != x != y != z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("!=", 
				SyntaxTrees.makeCompoundSyntaxTree("!=",  
						SyntaxTrees.makeCompoundSyntaxTree("!=", "w", "x"), "y"), "z"));

		string = "x != y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("!=", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} != y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("!=", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} != y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("!=", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "!=(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("!=", "x"));

		string = "!=";
		test(string, SyntaxTrees.makeSymbol("!="));

		string = "!=()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("!="));

		string = "!=(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("!=", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "x", "y"));

		string = "x=y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "x", "y"));

		string = "w = x = y = z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", 
				"w", "x", "y", "z"));

		string = "x = y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} = y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} = y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "=(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "x"));

		string = "=";
		test(string, SyntaxTrees.makeSymbol("="));

		string = "=()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("="));

		string = "=(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "x", SyntaxTrees.makeCompoundSyntaxTree("and", "y", "z")));
		
		string = "x and y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "x", "y"));

		string = "w and x and y and z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", 
				"w", "x", "y", "z"));

		string = "x and y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "x in y and y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", 
				SyntaxTrees.makeCompoundSyntaxTree("in", "x", "y"), "y"));

		string = "{x} and y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} and y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "and(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "x"));

		string = "and";
		test(string, SyntaxTrees.makeSymbol("and"));

		string = "and()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and"));

		string = "and(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("and", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "x", SyntaxTrees.makeCompoundSyntaxTree("or", "y", "z")));
		
		string = "x or y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "x", "y"));

		string = "w or x or y or z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", 
				"w", "x", "y", "z"));

		string = "x or y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "x in y or y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", 
				SyntaxTrees.makeCompoundSyntaxTree("in", "x", "y"), "y"));

		string = "{x} or y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} or y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "or(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "x"));

		string = "or";
		test(string, SyntaxTrees.makeSymbol("or"));

		string = "or()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or"));

		string = "or(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("or", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=>", "x", "y"));
		
		string = "x<=>y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=>", "x", "y"));
		
		string = "w <=> x <=> y <=> z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=>", "w", 
				SyntaxTrees.makeCompoundSyntaxTree("<=>",  "x", 
						SyntaxTrees.makeCompoundSyntaxTree("<=>", "y", "z"))));

		string = "x <=> y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=>", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} <=> y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=>", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} <=> y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=>", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "<=>(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=>", "x"));

		string = "<=>";
		test(string, SyntaxTrees.makeSymbol("<=>"));

		string = "<=>()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=>"));

		string = "<=>(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("<=>", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=>", "x", "y"));
		
		string = "x=>y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=>", "x", "y"));
		
		string = "w => x => y => z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=>", "w", 
				SyntaxTrees.makeCompoundSyntaxTree("=>", "x", 
						SyntaxTrees.makeCompoundSyntaxTree("=>", "y", "z"))));

		string = "x => y + z";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=>", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("+", "y", "z")));

		string = "{x} => y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=>", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} => y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=>", 
				SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
						SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y")),
				"y"));

		string = "=>(x)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=>", "x"));

		string = "=>";
		test(string, SyntaxTrees.makeSymbol("=>"));

		string = "=>()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=>"));

		string = "=>(x, y)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("=>", "x", "y"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "a", "b"));
		
		string = "there exists a : there exists b : c";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "a", 
				SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "b", "c")));
		
		string = "'there exists . : .'(a, b)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "a", "b"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "x", "y"));

		string = "for all x = 5 : a";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("for all . : .", 
				SyntaxTrees.makeCompoundSyntaxTree("=", "x", "5"), "a"));

		string = "for all x : for all y : for all z : true";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "x", 
				SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "y", 
						SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "z", "true"))));

		string = "for all Y : (for all X : ((X != Y) => (there exists W : (there exists Z : ((Z != a) => (Alpha = Beta))))))";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "Y", 
				SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "X", 
						SyntaxTrees.makeCompoundSyntaxTree("=>", 
								SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "Y"), 
								SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "W", 
										SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "Z", 
												SyntaxTrees.makeCompoundSyntaxTree("=>", 
														SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "a"), 
														SyntaxTrees.makeCompoundSyntaxTree("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => there exists W : there exists Z : (Z != a => Alpha = Beta)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "Y", 
				SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "X", 
						SyntaxTrees.makeCompoundSyntaxTree("=>", 
								SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "Y"), 
								SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "W", 
										SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "Z", 
												SyntaxTrees.makeCompoundSyntaxTree("=>", 
														SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "a"), 
														SyntaxTrees.makeCompoundSyntaxTree("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => (there exists W : there exists Z : Z != a => Alpha = Beta)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "Y", 
				SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "X", 
						SyntaxTrees.makeCompoundSyntaxTree("=>", 
								SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "Y"), 
								SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "W", 
										SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "Z", 
												SyntaxTrees.makeCompoundSyntaxTree("=>", 
														SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "a"), 
														SyntaxTrees.makeCompoundSyntaxTree("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => there exists W : there exists Z : Z != a => Alpha = Beta";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "Y", 
				SyntaxTrees.makeCompoundSyntaxTree("for all . : .", "X", 
						SyntaxTrees.makeCompoundSyntaxTree("=>", 
								SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "Y"), 
								SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "W", 
										SyntaxTrees.makeCompoundSyntaxTree("there exists . : .", "Z", 
												SyntaxTrees.makeCompoundSyntaxTree("=>", 
														SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "a"), 
														SyntaxTrees.makeCompoundSyntaxTree("=", "Alpha", "Beta"))))))));
	}

	@Test
	public void testIfThenElse () {
		String string;
		string = "if a then b else c";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
				"a", "b", "c"));

		string = "'if . then . else .'(a, b, c)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", "a", "b", "c"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", "x", "a"));

		string = "lambda x, y, z : a";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y", "z"), "a"));

		string = "lambda x, y, z : lambda a : b";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", 
				SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y", "z"), 
				SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", "a", "b")));

		// Testing illegal strings
		string = "lambda a :";
		testFail(string);
	}

	@Test
	public void testMessage () {
		String string;
		string = "message to a from b";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("message to . from .", "a", "b"));

		string = "'message to . from .'(a, b)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("message to . from .", "a", "b"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("previous message to . from .", "a", "b"));

		string = "'previous message to . from .'(a, b)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("previous message to . from .", "a", "b"));

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
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-",
				SyntaxTrees.makeCompoundSyntaxTree("^", "a", "a"), "a"));

		string = "a*a+a";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+",
				SyntaxTrees.makeCompoundSyntaxTree("*", "a", "a"), "a"));

		string = "a+a*a";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", "a",
				SyntaxTrees.makeCompoundSyntaxTree("*", "a", "a")));

		string = "-a*a+a";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+",
				SyntaxTrees.makeCompoundSyntaxTree("*",
						SyntaxTrees.makeCompoundSyntaxTree("-", "a"), "a"), "a"));

		string = "-a*-a-a";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-",
				SyntaxTrees.makeCompoundSyntaxTree("*",
						SyntaxTrees.makeCompoundSyntaxTree("-", "a"),
						SyntaxTrees.makeCompoundSyntaxTree("-", "a")), "a"));

		string = "-a*-a^a-a";
		test(string,
				SyntaxTrees.makeCompoundSyntaxTree("-",
						SyntaxTrees.makeCompoundSyntaxTree("*",
								SyntaxTrees.makeCompoundSyntaxTree("-", "a"),
								SyntaxTrees.makeCompoundSyntaxTree("^",
										SyntaxTrees.makeCompoundSyntaxTree("-", "a"), "a")), "a"));

		string = "-a*-a^-a";
		test(string, 
				SyntaxTrees.makeCompoundSyntaxTree("*",
						SyntaxTrees.makeCompoundSyntaxTree("-", "a"),
						SyntaxTrees.makeCompoundSyntaxTree("^",
								SyntaxTrees.makeCompoundSyntaxTree("-", "a"),
								SyntaxTrees.makeCompoundSyntaxTree("-", "a"))));
	}

	@Test
	public void testFunctionsAndSequences() {
		String string;
		string = "x+y";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+","x","y"));

		string = "f()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("f"));

		string = "f(10)";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("f", 10));

		string = "f(10 + a)";
		test(string, 								
				SyntaxTrees.makeCompoundSyntaxTree("f",
						SyntaxTrees.makeCompoundSyntaxTree(
								"+", 10, "a")));

		string = "f(10+a) - f(a, b)";
		test(string,
				SyntaxTrees.makeCompoundSyntaxTree("-",
						SyntaxTrees.makeCompoundSyntaxTree("f",
								SyntaxTrees.makeCompoundSyntaxTree("+", 10, "a")),
						SyntaxTrees.makeCompoundSyntaxTree("f", "a", "b")));
		
		string = "f(10)-f()";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("-",
						SyntaxTrees.makeCompoundSyntaxTree("f", 10),
						SyntaxTrees.makeCompoundSyntaxTree("f")));

		string = "10 + 10";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 10, 10));

		string = "10 + x*20";
		test(string, SyntaxTrees.makeCompoundSyntaxTree("+", 10, 
				SyntaxTrees.makeCompoundSyntaxTree("*", "x",20)));

		string = "f(10, 20, 30, f (x), f, (x))";
		test(string,
				SyntaxTrees.makeCompoundSyntaxTree("f", 10, 20, 30, 
						SyntaxTrees.makeCompoundSyntaxTree("f", "x"), "f", "x"));
	}
	
	
	@Test
	public void testGrinder () {
		String expression;
		expression = "if X = 1 then 0.0003 + 0.000000000001 else 0.150004 + 0.1 + 0.776699029126213691398561";
		test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
				SyntaxTrees.makeCompoundSyntaxTree("=", "X", "1"), 
				SyntaxTrees.makeCompoundSyntaxTree("+", "0.0003", "0.000000000001"), 
				SyntaxTrees.makeCompoundSyntaxTree("+", "0.150004", "0.1", "0.776699029126213691398561")));


			expression = "3";
			test(expression, SyntaxTrees.makeSymbol(3));

			expression = "x + y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x", "y"));

			expression = "1 + 2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "1", "2"));

			expression = "x + 2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x", "2"));

			expression = "+(x, 2, y, 6)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x", "2", "y", "6"));

			expression = "+(x, +(1, y), 11)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x", 
					SyntaxTrees.makeCompoundSyntaxTree("+", 1, "y"), "11"));

			expression = "+(x, 2, 1 + 2, 1 + y, 6)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x", 2, 
					SyntaxTrees.makeCompoundSyntaxTree("+", 1, 2), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 1, "y"), "6"));

			expression = "+(x, 2, 3)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x", 2, 3));

			expression = "+(x)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x"));

			expression = "+";
			test(expression, SyntaxTrees.makeSymbol("+"));

			expression = "1";
			test(expression, SyntaxTrees.makeSymbol(1));

			expression = "x";
			test(expression, SyntaxTrees.makeSymbol("x"));

			expression = "+()";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+"));

			expression = "3 - 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 3, 1));

			expression = "1 - 3";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 1, 3));

			expression = "X - Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", "X", "Y"));

			expression = "X - 0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", "X", 0));

			expression = "0 - X";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 0, "X"));

			expression = "-1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", "1"));

			expression = "-x";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", "x"));

			expression = "3";
			test(expression, SyntaxTrees.makeSymbol(3));

			expression = "x * y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", "x", "y"));

			expression = "2 * 2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 2, 2));

			expression = "x * 2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", "x", 2));

			expression = "*(x, 2, y, 6)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", "x", 2, "y", 6));

			expression = "*(x, 0, y, 6)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", "x", 0, "y", 6));

			expression = "*(x, 2, 1 * 2, 1 * y, 6)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", "x", 2, 
							SyntaxTrees.makeCompoundSyntaxTree("*", 1, 2),
							SyntaxTrees.makeCompoundSyntaxTree("*", 1, "y"), 6));

			expression = "*(x, 2, 3)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", "x", 2, 3));

			expression = "*(x)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", "x"));

			expression = "*";
			test(expression, SyntaxTrees.makeSymbol("*"));

			expression = "*()";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*"));

			expression = "3^2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("^", 3, 2));

			expression = "x^1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("^", "x", 1));

			expression = "x^0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("^", "x", 0));

			expression = "x^0.0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("^", "x", "0.0"));

			expression = "1^n";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("^", 1, "n"));

			expression = "2^n";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("^", 2, "n"));

			expression = "4/2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("/", 4, 2));

			expression = "4.0/2.0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("/", "4.0", "2.0"));

			expression = "4.0/3.0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("/", "4.0", "3.0"));

			expression = "a/b";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("/", "a", "b"));

			expression = "4/0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("/", 4, 0));

			expression = "4/2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("/", 4, 2));

			expression = "(4*2)/(2*3)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("/", 
					SyntaxTrees.makeCompoundSyntaxTree("*", 4, 2),
					SyntaxTrees.makeCompoundSyntaxTree("*", 2, 3)));

			expression = "(4*2)/2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("/", 
					SyntaxTrees.makeCompoundSyntaxTree("*", 4, 2), 2));

			expression = "2/2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("/", 2, 2));

			expression = "3 > 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">", 3, 1));

			expression = "3 > 3";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">", 3, 3));

			expression = "1 > 3";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">", 1, 3));

			expression = "1 > y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">", 1, "y"));

			expression = "1 > false";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">", 1, "false"));

			expression = "3 >= 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">=", 3, 1));

			expression = "3 >= 3";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">=", 3, 3));

			expression = "1 >= 3";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">=", 1, 3));

			expression = "1 >= y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">=", 1, "y"));

			expression = "1 >= false";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">=", 1, "false"));

			expression = "not (3 > 1)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not",
					SyntaxTrees.makeCompoundSyntaxTree(">", 3, 1)));

			expression = "not(3 > 3)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not",
					SyntaxTrees.makeCompoundSyntaxTree(">", 3, 3)));

			expression = "true and x";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", "true", "x"));

			expression = "true";
			test(expression, SyntaxTrees.makeSymbol("true"));

			expression = "x and y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", "x", "y"));

			expression = "true and false";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", "true", "false"));

			expression = "x and false";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", "x", "false"));

			expression = "x and false and y and true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", 
					"x", "false", "y", "true"));

			expression = "and(x, false, false and true, false and y, false)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", "x", "false", 
					SyntaxTrees.makeCompoundSyntaxTree("and", "false", "true"),
					SyntaxTrees.makeCompoundSyntaxTree("and", "false", "y"),
					"false"));

			expression = "and(x, true, false)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", "x", "true", "false")); 


			expression = "and(x)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", "x"));

			expression = "and";
			test(expression, SyntaxTrees.makeSymbol("and"));

			expression = "and()";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and")); 

			expression = "false or x";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", "false", "x"));

			expression = "x or false";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", "x", "false"));

			expression = "true";
			test(expression, SyntaxTrees.makeSymbol(true));

			expression = "x or y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", "x", "y"));

			expression = "true or false";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", "true", "false"));

			expression = "x or true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", "x", "true"));

			expression = "x or false or y or true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", 
					"x", "false", "y", "true"));

			expression = "or()";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or")); 

			expression = "x or y or x or y or z";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", 
					"x", "y", "x", "y", "z"));

			expression = "x or x";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", "x", "x"));

			expression = "not true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not", "true"));

			expression = "not false";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not", "false"));

			expression = "not x";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not", "x"));

			expression = "not x and y and x";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", 
					SyntaxTrees.makeCompoundSyntaxTree("not", "x"), "y", "x"));

			expression = "not not x";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not", 
					SyntaxTrees.makeCompoundSyntaxTree("not", "x")));

			expression = "'index of . in .'(b, f(a,b))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("index of . in .", "b", 
					SyntaxTrees.makeCompoundSyntaxTree("f", "a", "b")));

			expression = "'index of . in .'(c, f(a,b))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("index of . in .", "c", 
					SyntaxTrees.makeCompoundSyntaxTree("f", "a", "b")));

			expression = "'index of . in .'(c, f(a,b)) > 0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(">", 
					SyntaxTrees.makeCompoundSyntaxTree("index of . in .", "c", 
							SyntaxTrees.makeCompoundSyntaxTree("f", "a", "b")), "0"));

			expression = "x + 2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x", "2"));

			expression = "f(g(h(x)))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f",
					SyntaxTrees.makeCompoundSyntaxTree("g",
							SyntaxTrees.makeCompoundSyntaxTree("h", "x"))));

			expression = "{(on x) x}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "x"), "x", null));

			expression = "x + {(on x) f(x)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x",
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "x"),
							SyntaxTrees.makeCompoundSyntaxTree("f", "x"), null)));

			expression = "2 + {(on x) f(x)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", 2,
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "x"),
							SyntaxTrees.makeCompoundSyntaxTree("f", "x"), null)));

			expression = "x + {(on x in group(x)) f(x)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x",
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "x",
											SyntaxTrees.makeCompoundSyntaxTree("group", "x"))),
							SyntaxTrees.makeCompoundSyntaxTree("f", "x"), null)));

			expression = "2 + {(on x in group(2)) f(x)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", 2,
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "x",
											SyntaxTrees.makeCompoundSyntaxTree("group", 2))),
							SyntaxTrees.makeCompoundSyntaxTree("f", "x"), null)));

			expression = "x + {(on y) f(x)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "x",
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "y"),
							SyntaxTrees.makeCompoundSyntaxTree("f", "x"), null)));

			expression = "2 + {(on y) f(2)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", 2,
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "y"),
							SyntaxTrees.makeCompoundSyntaxTree("f", 2), null)));


			expression = "f(x)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", "x"));

			expression = "f(f(x) + g(x))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("f", "x"),
							SyntaxTrees.makeCompoundSyntaxTree("g", "x"))));


			expression = "(1 + x + y)*(x + 3)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("+", "1", "x", "y"),
							SyntaxTrees.makeCompoundSyntaxTree("+", "x", 3)));

			expression = "1*x + 1*3 + x*x + x*3 + y*x + y*3";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", 
					SyntaxTrees.makeCompoundSyntaxTree("*", 1, "x"),
					SyntaxTrees.makeCompoundSyntaxTree("*", 1, 3),
					SyntaxTrees.makeCompoundSyntaxTree("*", "x", "x"),
					SyntaxTrees.makeCompoundSyntaxTree("*",	"x", 3),
					SyntaxTrees.makeCompoundSyntaxTree("*", "y", "x"),
					SyntaxTrees.makeCompoundSyntaxTree("*", "y", 3)));

			// Testing case where one one of the operator applications is empty.
			// We use an evaluator with the distributive law alone so +() does not get evaluated to 0.
			expression = "+()*(x + 3)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*",
					SyntaxTrees.makeCompoundSyntaxTree("+"),
							SyntaxTrees.makeCompoundSyntaxTree("+", "x", 3)));

			expression = "(1 * x * y)+(x * 3)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", 
					SyntaxTrees.makeCompoundSyntaxTree("*", "1", "x", "y"),
							SyntaxTrees.makeCompoundSyntaxTree("*", "x", 3)));

			expression = "not x";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not", "x"));

			expression = "not(x and y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not", 
					SyntaxTrees.makeCompoundSyntaxTree("and", "x", "y")));

			expression = "not x or not y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", 
					SyntaxTrees.makeCompoundSyntaxTree("not", "x"), 
					SyntaxTrees.makeCompoundSyntaxTree("not", "y")));

			expression = "not(x or y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not", 
					SyntaxTrees.makeCompoundSyntaxTree("or", "x", "y")));

			expression = "not x and not y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", 
					SyntaxTrees.makeCompoundSyntaxTree("not", "x"), 
					SyntaxTrees.makeCompoundSyntaxTree("not", "y")));

			expression = "not(x or y or z)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not", 
					SyntaxTrees.makeCompoundSyntaxTree("or", "x", "y", "z")));

			expression = "not x and not y and not z";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", 
					SyntaxTrees.makeCompoundSyntaxTree("not", "x"), 
					SyntaxTrees.makeCompoundSyntaxTree("not", "y"),
					SyntaxTrees.makeCompoundSyntaxTree("not", "z")));

			expression = "not(x or y or (z and w))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("not", 
					SyntaxTrees.makeCompoundSyntaxTree("or", "x", "y", 
							SyntaxTrees.makeCompoundSyntaxTree("and", "z", "w"))));

			expression = "not x and not y and (not z or not w)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", 
					SyntaxTrees.makeCompoundSyntaxTree("not", "x"), 
					SyntaxTrees.makeCompoundSyntaxTree("not", "y"),
					SyntaxTrees.makeCompoundSyntaxTree("or",
							SyntaxTrees.makeCompoundSyntaxTree("not", "z"),
							SyntaxTrees.makeCompoundSyntaxTree("not", "w"))));

			expression = "'scoped variables'({(on X) p(X) | X != a})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("scoped variables",
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"),
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"),
							SyntaxTrees.makeCompoundSyntaxTree("|",
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));

			expression = "list(X)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("list", "X"));

			expression = "'scoped variables'({(on X,Y) p(X,Y) | X != a})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("scoped variables",
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")),
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"),
							SyntaxTrees.makeCompoundSyntaxTree("|",
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));

			expression = "list(X, Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("list", "X", "Y"));

			expression = "'scoped variables'(f(X,Y))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("scoped variables",
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y")));

			expression = "list()";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("list"));


			expression = "aConstantSymbol";
			test(expression, SyntaxTrees.makeSymbol("aConstantSymbol"));

			expression = "if A = B then aAndBEqual else aAndBNotEqual";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .",
					SyntaxTrees.makeCompoundSyntaxTree("=", "A", "B"),
					"aAndBEqual", "aAndBNotEqual"));

			expression = "{(on X) X | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "X", 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));



			expression = "if true then 1 else 2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					"true", 1, 2));

			expression = "if false then 1 else 2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					"false", 1, 2));

			expression = "if X then 1 else 2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					"X", 1, 2));


			expression = "f(a, b, c, if Y = 2 then X else X + 1, d, e, f)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", "a", "b", "c",
							SyntaxTrees.makeCompoundSyntaxTree("if . then . else .",
									SyntaxTrees.makeCompoundSyntaxTree("=", "Y", 2), "X",
									SyntaxTrees.makeCompoundSyntaxTree("+", "X", 1)), 
							"d", "e", "f"));
			expression = "if Y = 2 then f(a, b, c, X, d, e, f) else f(a, b, c, X + 1, d, e, f)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .",
					SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "2"),
					SyntaxTrees.makeCompoundSyntaxTree("f", "a", "b", "c", "X", "d", "e", "f"),
					SyntaxTrees.makeCompoundSyntaxTree("f", "a", "b", "c", 
							SyntaxTrees.makeCompoundSyntaxTree("+", "X", 1), "d", "e", "f")));

			expression = "{(on X) if Y = 2 then X else X + 1 | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"),
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", 2), "X", 
							SyntaxTrees.makeCompoundSyntaxTree("+", "X", 1)),
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "if Y = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "Y", 2),
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "X", 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))),
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("+", "X", 1),
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));

			expression = "{(on X) if X = 2 then X else X + 1 | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"),
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", 2), "X", 
							SyntaxTrees.makeCompoundSyntaxTree("+", "X", 1)),
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) if p(Y) = 2 then X else X + 1 | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"),
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("=", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "Y"), 2), "X", 
							SyntaxTrees.makeCompoundSyntaxTree("+", "X", 1)),
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "if p(Y) = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "Y"), 2),
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "X", 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))),
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("+", "X", 1),
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));


			expression = "{(on X) if p(X) = 2 then X else X + 1 | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"),
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("=", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 2), "X", 
							SyntaxTrees.makeCompoundSyntaxTree("+", "X", 1)),
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X in (if Y = a then Set1 else Set2)) p(X) | X != a}"; // lack of ()'s around if then else makes parse fail, not sure why. Entered in bug database.
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
											SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "a"),
											"Set1", "Set2"))),
					SyntaxTrees.makeCompoundSyntaxTree("p", "X"),
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			// lack of ()'s around if then else makes parse fail, due to precedence for "in".
//			expression = "{(on X in if Y = a then Set1 else Set2) p(X) | X != a}";
//			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
//					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
//							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
//									SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
//											SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "a"),
//											"Set1", "Set2"))),
//					SyntaxTrees.makeCompoundSyntaxTree("p", "X"),
//					SyntaxTrees.makeCompoundSyntaxTree("|", 
//							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "if Y = a then {(on X in Set1) p(X) | X != a} else {(on X in Set2) p(X) | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .",
					SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "a"),
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
							SyntaxTrees.makeCompoundSyntaxTree("( on . )",
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", "Set1")),
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"),
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))),
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }",
							SyntaxTrees.makeCompoundSyntaxTree("( on . )",
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", "Set2")),
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"),
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));

			expression = "{(on Y, X in (if Y = a then Set1 else Set2)) p(X) | X != a}"; // lack of ()'s around if then else makes parse fail, not sure why. Entered in bug database.
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "Y",
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
													SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "a"),
													"Set1", "Set2")))),
					SyntaxTrees.makeCompoundSyntaxTree("p", "X"),
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "if X = a then X != a else X = Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .",
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"),
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"),
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y")));

			expression = "if X = a then false else X = Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .",
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"),
					"false", SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y")));

			expression = "if X = a then f(X != a, 1, 2) else g(X = Y, a, b)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), "1", "2"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), "a", "b")));

			expression = "if X = a then f(false, 1, 2) else g(X = Y, a, b)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "false", "1", "2"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), "a", "b")));

			expression = "if X != a or Y != b then X != a else false";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), "false"));

			expression = "if X != a or Y != b then X != a or Y != b or Z != c else false";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "c")), "false"));

			expression = "X != a or Y != b";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("or", 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b")));

			expression = "if X != a then X != a and Y = d else true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "d")), "true"));

			expression = "if X != a then Y = d else true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "d"), "true"));

			expression = "if X != a or Y != b then f(X != a or Y != b or Z != c) else true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("or", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "c"))), "true"));

			expression = "if X != a or Y != b then f(true) else true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "true"), "true"));

			expression = "if X != a or Y != b then not(X != a or Y != b or Z != c) else true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("not", 
							SyntaxTrees.makeCompoundSyntaxTree("or", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "c"))), "true"));

			expression = "if X != a or Y != b then false else true";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b")), "false", "true"));

			expression = "if A and B then if A then 1 else 0 else 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", "A", "B"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", "A", "1", "0"), "1"));

			expression = "1";
			test(expression, SyntaxTrees.makeSymbol("1"));

			expression = "if X = a then if p(X) then E1 else E2 else if p(X) then E1 else E2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), "E1", "E2")));

			expression = "if X = a then if p(a) then E1 else E2 else if p(X) then E1 else E2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "a"), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X, Y) then E1 else E2 else if p(X, Y) then E1 else E2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a, b) then E1 else E2 else if p(X, Y) then E1 else E2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "a", "b"), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X) and q(Y) then E1 else E2 else if p(X) and q(Y) then E1 else E2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "Y")), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a) and q(b) then E1 else E2 else if p(X) and q(Y) then E1 else E2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "b")), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X) and q(X, Y) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "X", "Y")), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "X", "Y")), "E1", "E2")));

			expression = ("if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2");
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "a", "b")), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "X", "Y")), "E1", "E2")));

			expression = "if even(X) then f(X) else X + 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("even", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "1")));

			expression = "if X = a then 1 else 2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), "1", "2"));

			expression = "if X = a then f(X) else X + 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "1")));

			expression = "if X = a then f(a) else X + 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "1")));

			expression = "if X = a and Y = b then f(X,Y) else X + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = a and Y = b then f(a, b) else X + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "a", "b"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a and Y = W then f(X,Y,Z) else X + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "W")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a and W = Y then f(a, W, a) else X + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "a", "W", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a and W != Y then f(X,W,Z) else X + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "W", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a and W != Y then f(a, W, a) else X + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "a", "W", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z and W != Y then f(X,W,Z) else X + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "W", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z and W != Y then f(X,W,Z) + {(on Z) foo(Z)} else Z + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X", "W", "Z"), 
							SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("foo", "Z"), null)), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "Z", "Y")));

			expression = "if X = Z and W != Y then f(X, W, X) + {(on Z) foo(X)} else Z + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X", "W", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("foo", "X"), null)), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "Z", "Y")));

			expression = "if X = Z = a or W != Y then f(X,W,Z) else X + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "W", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a or W != Y then f(X,W,Z) else X + W";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "W", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "W")));

			expression = "if X != a then {(on X) X != a} else {(on Y) X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), null), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), null)));

			expression = "if X != a then {(on X) X != a} else {(on Y) false}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), null), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), "false", null)));

			expression = "if X != a then X + 1 else 42";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "1"), "42"));

			expression = "if X != a then X + 1 else f(X)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "1"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X")));

			expression = "if X != a then X + 1 else f(a)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "1"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "a")));

			expression = "if X != a or Y != b then X + Y else f(X,Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y")));

			expression = "if X != a or Y != b then  X + Y else f(a, b)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "a", "b")));

			expression = "if X != Z or W != Y then f(X,W,Z) else foo(X, Z, W, Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "Z"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "W", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("foo", "X", "Z", "W", "Y")));

			expression = "if X != Z or W != Y then f(X, W, Z) else foo(X, X, W, W)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("or", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "Z"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "W", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("foo", "X", "X", "W", "W")));

			expression = "if X != a and W != Y then f(X,W,Z) else X + Y";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "W", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "W", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X != a then {(on X) X != a} else 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), null), "1"));

			expression = "if X = a then {(on Y) X != a} else {(on X) X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), null), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), null)));

			expression = "if X = a then {(on Y) false} else {(on X) X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), "false", null), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), null)));

			expression = "if X < 3 then f(X < 2, X < 3, X < 4) else g(X < 2, X < 3, X < 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "4")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "4"))));

			expression = "if X < 3 then f(X < 2, true, true)     else g(false, false, X < 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "2"), "true", "true"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false", "false", 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "4"))));

			expression = "if X < 3 then f(X <= 2, X <= 3, X <= 4) else g(X <= 2, X <= 3, X <= 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "4")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "4"))));

			expression = "if X < 3 then f(X <= 2, true, true) else g(false, X <= 3, X <= 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "2"), "true", "true"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false", 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "4"))));

			expression = "if X < 3 then f(X > 2, X > 3, X > 4) else g(X > 2, X > 3, X > 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "4")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "4"))));

			expression = "if X < 3 then f(X > 2, false, false) else g(true, X > 3, X > 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "2"), "false", "false"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "true", 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "4"))));

			expression = "if X < 3 then f(X >= 2, X >= 3, X >= 4) else g(X >= 2, X >= 3, X >= 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "4")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "4"))));

			expression = "if X < 3 then f(X >= 2, false, false) else g(true, true, X >= 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "2"), "false", "false"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "true", "true", 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "4"))));

			expression = "if X <= 3 then f(X < 2, X < 3, X < 4) else g(X < 2, X < 3, X < 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "4")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "4"))));

			expression = "if X <= 3 then f(X < 2, X < 3, true)     else g(false, false, X < 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "3"), "true"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false", "false", 
							SyntaxTrees.makeCompoundSyntaxTree("<", "X", "4"))));

			expression = "if X <= 3 then f(X <= 2, X <= 3, X <= 4) else g(X <= 2, X <= 3, X <= 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "4")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "4"))));

			expression = "if X <= 3 then f(X <= 2, true, true) else g(false, false, X <= 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "2"), "true", "true"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false", "false", 
							SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "4"))));

			expression = "if X <= 3 then f(X > 2, X > 3, X > 4) else g(X > 2, X > 3, X > 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "4")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "4"))));

			expression = "if X <= 3 then f(X > 2, false, false) else g(true, true, X > 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "2"), "false", "false"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "true", "true", 
							SyntaxTrees.makeCompoundSyntaxTree(">", "X", "4"))));

			expression = "if X <= 3 then f(X >= 2, X >= 3, X >= 4) else g(X >= 2, X >= 3, X >= 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "4")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "3"), 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "4"))));

			expression = "if X <= 3 then f(X >= 2, X >= 3, false) else g(true, true, X >= 4)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("<=", "X", "3"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "3"), "false"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "true", "true", 
							SyntaxTrees.makeCompoundSyntaxTree(">=", "X", "4"))));

			expression = "if even(X) then f(even(X)) else g(even(X))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("even", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("even", "X")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("even", "X"))));

			expression = "if even(X) then f(true) else g(false)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("even", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "true"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false")));

			expression = "if even(X) then f(even(Y)) else g(even(X))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("even", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("even", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("even", "X"))));

			expression = "if even(X) then f(if Y = X then true else even(Y)) else g(false)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("even", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "X"), "true", 
									SyntaxTrees.makeCompoundSyntaxTree("even", "Y"))), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false")));

			expression = "if even(X) then {(on even(a)) f(even(Y))} else g(even(X))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("even", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("even", "a")), 
							SyntaxTrees.makeCompoundSyntaxTree("f", 
									SyntaxTrees.makeCompoundSyntaxTree("even", "Y")), null), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("even", "X"))));

			expression = "if even(X) then {(on even(a)) f(if Y != a and Y = X then true else even(Y))} else g(false)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("even", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("even", "a")), 
							SyntaxTrees.makeCompoundSyntaxTree("f", 
									SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
											SyntaxTrees.makeCompoundSyntaxTree("and", 
													SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "a"), 
													SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "X")), "true", 
											SyntaxTrees.makeCompoundSyntaxTree("even", "Y"))), null), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false")));

			expression = "if X = Y then f(X = Y) else g(X = Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"))));

			expression = "if X = Y then f(true) else g(false)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "true"), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false")));

			expression = "if X = Y then f(X = Z) else g(X = Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"))));

			expression = "if X = Y then f(X = Z) else g(false)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z")), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false")));

			expression = "if X = Y then {(on X = a) f(X = Y)} else g(X = Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a")), 
							SyntaxTrees.makeCompoundSyntaxTree("f", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y")), null), 
					SyntaxTrees.makeCompoundSyntaxTree("g", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"))));

			expression = "if X = Y then {(on X = a) f(X = Y)} else g(false)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a")), 
							SyntaxTrees.makeCompoundSyntaxTree("f", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y")), null), 
					SyntaxTrees.makeCompoundSyntaxTree("g", "false")));

			expression = "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "a", "b")), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "X", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "a", "b")), "E1", "E2"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "X", "Y")), "E1", "E2")));

			expression = "sum({(on Person in World) if Person = rich then 2000 else 50 | Person = american})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Person", "World")), 
							SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "rich"), "2000", "50"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "american")))));

			expression = 
					"sum({(on Person in World) 2000 | Person = american and Person = rich})" +
					"+" + 
					"sum({(on Person in World) 50 | Person = american and not (Person = rich)})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", 
					SyntaxTrees.makeCompoundSyntaxTree("sum", 
							SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("in", "Person", "World")), "2000", 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("and", 
													SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "american"), 
													SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "rich"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("sum", 
							SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("in", "Person", "World")), "50", 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("and", 
													SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "american"), 
													SyntaxTrees.makeCompoundSyntaxTree("not", 
															SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "rich"))))))));
			
			expression = "f(X)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", "X"));

			expression = "g(X',X',X) and h(Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", 
					SyntaxTrees.makeCompoundSyntaxTree("g", "X'", "X'", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("h", "Y")));

			expression = "f(X',X'',Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", "X'", "X''", "Y"));

			expression = "{(on X) X | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "X", 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) X | X != b }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "X", 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) X | X != b }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "X", 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X', Y) f(X',Y) | X' != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X'", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X'", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X'", "a"))));

			expression = "{(on X) f(X,Y) | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) f(X)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X"), null));

			expression = "{(on X) f(X,Y) | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "f(X, Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"));

			expression = "{(on X') f(X',Y) | X' != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X'"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X'", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X'", "a"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "f(X, Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"));

			expression = "{(on X', Y') f(X',Y') | X' != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X'", "Y'")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X'", "Y'"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X'", "a"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on Y) f(X, Y) }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), null));

			expression = "{(on X', Y) f(X',Y) | X' != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X'", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X'", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X'", "a"))));

			expression = "{(on X, X') f(X,X') | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "X'")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "X'"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "f(X)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", "X"));

			expression = "{(on X'', X') f(X'',X') | X'' != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X''", "X'")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X''", "X'"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X''", "a"))));

			expression = "{(on X, X') f(X,X') | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "X'")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "X'"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "f(X, X')";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", "X", "X'"));

			expression = "{(on X'', X''') f(X'',X''') | X'' != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X''", "X'''")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X''", "X'''"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X''", "a"))));

			expression = "{(on Z) {(on X, Y) f(X,Y,Z) | X != a } | Z != c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y", "Z"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "c"))));

			expression = "{(on Y) f(X, Y, Z) }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y", "Z"), null));

			expression = "{(on Z') {(on X', Y) f(X',Y,Z') | X' != a } | Z' != c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z'"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X'", "Y")), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X'", "Y", "Z'"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X'", "a"))), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Z'", "c"))));

			expression = "{(on Z) {(on Z', Y) f(X,Y,Z') | X != a } | Z != c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "Z'", "Y")), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y", "Z'"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "c"))));

			expression = "{(on Y) f(X, Y, Z, Z') }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y", "Z", "Z'"), null));

			expression = "{(on Z''') {(on Z'', Y) f(X,Y,Z'') | X != a } | Z''' != c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z'''"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "Z''", "Y")), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y", "Z''"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "Z'''", "c"))));

			expression = "{(on X, p(Z)) f(X,Y) | X != a }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "Z"))), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) f(X)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X"), null));

			expression = "{(on X) p(X,Y) + 2}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X in {1,2,3}) p(X) | false}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
					SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", "false")));

			expression = "{ }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list")));

			expression = "{(on X in {1,2,3} - {1}, Y in {1,2} union {3}) p(X,Y) + 1 + 1 | true and Z}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("-", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")), 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", "1"))), 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("union", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2")), 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", "3"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "1", "1"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", "true", "Z"))));

			expression = "{(on X in {2,3}, Y in {1,2,3}) p(X,Y) + 2 | Z}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "2", "3"))), 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", "Z")));

			expression = "{(on X in {1,2,3} - {1}, Y) p(X,Y) + 1 + 1}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("-", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")), 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", "1"))), "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "1", "1"), null));

			expression = "{(on X in {2,3}, Y) p(X,Y) + 2}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "2", "3"))), "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X,Y) p(X,Y) + 2}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "if V = (<X>) then {(on X) p(X,Y) + 2} else 0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "V", 
							SyntaxTrees.makeSymbol(SyntaxTrees.makeSymbol("X"))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("+", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), null), "0"));

			expression = "{(on X, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X, Z, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Z", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X in set1, Z in set2, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", "set1"), 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Z", "set2"), 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("+", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "if V = (<X>) then {(on X, Y in {1,2,3}) p(X,Y) + 2} else 0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "V", 
							SyntaxTrees.makeSymbol(SyntaxTrees.makeSymbol("X"))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
							SyntaxTrees.makeCompoundSyntaxTree("+", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), "2"), null), "0"));

			expression = "{(on X,Y) f(X) | Z = X}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Z", "X"))));

			expression = "sum({(on X in {1,2,3}) 0 | X != 2})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")))), "0", 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "2")))));

			expression = "product({(on X in {1,2,3}) 1 | X != 2})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("product", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")))), "1", 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "2")))));

			expression = "sum(partition(A, B))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("partition", "A", "B")));

			expression = "sum(partition())";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("partition")));

			expression = "{(on ) foo(X) | X != a and X != b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list")), 
					SyntaxTrees.makeCompoundSyntaxTree("foo", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b")))));

			expression = "if X != a and X != b then {foo(X)} else {}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("and", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("foo", "X")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list"))));

			expression = "{(on X in {1,2,3}) p(X) | false}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
					SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", "false")));

			expression = "{ }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list")));

			expression = "{(on X in {a,b,c}) foo(X) | X != a and X != b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")))), 
					SyntaxTrees.makeCompoundSyntaxTree("foo", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b")))));

			expression = "partition(" +
					"if first({a,b,c}) != a and first({a,b,c}) != b then { foo(first({a,b,c})) } else {}," +
					"{(on X in rest({a,b,c})) foo(X) | X != a and X != b})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("first", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))), "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("first", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))), "b")), 
							SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
									SyntaxTrees.makeCompoundSyntaxTree("foo", 
											SyntaxTrees.makeCompoundSyntaxTree("first", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))))), 
							SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list"))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("rest", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))))), 
							SyntaxTrees.makeCompoundSyntaxTree("foo", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b"))))));


			expression = "{(on X in {a,b,c}, Y in {1,2,3}) foo(X,Y) | X != a and Y != 1}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))), 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("foo", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "1")))));

			expression = "partition(" +
					"{(on                     Y in {1,2,3}) foo(first({a,b,c}),Y) | first({a,b,c}) != a and Y != 1}," +
					"{(on X in rest({a,b,c}), Y in {1,2,3}) foo(X,Y)              | X              != a and Y != 1})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
							SyntaxTrees.makeCompoundSyntaxTree("foo", 
									SyntaxTrees.makeCompoundSyntaxTree("first", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))), "Y"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", 
													SyntaxTrees.makeCompoundSyntaxTree("first", 
															SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
																	SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))), "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "1")))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
											SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
													SyntaxTrees.makeCompoundSyntaxTree("rest", 
															SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
																	SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")))), 
											SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
							SyntaxTrees.makeCompoundSyntaxTree("foo", "X", "Y"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "1"))))));


			expression = "{(on X in rest({a,b,c}), Y in {1,2,3}, Z) foo(X,Y) | X != a and Y != 1}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("rest", 
													SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
															SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")))), 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))), "Z")), 
					SyntaxTrees.makeCompoundSyntaxTree("foo", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "1")))));

			expression = "partition(" +
					"{(on X in rest({a,b,c}),                     Z) foo(X, first({1,2,3})) | X != a and first({1,2,3}) != 1}," +
					"{(on X in rest({a,b,c}), Y in rest({1,2,3}), Z) foo(X,Y)               | X != a and Y              != 1})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
											SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
													SyntaxTrees.makeCompoundSyntaxTree("rest", 
															SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
																	SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")))), "Z")), 
							SyntaxTrees.makeCompoundSyntaxTree("foo", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("first", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("!=", 
													SyntaxTrees.makeCompoundSyntaxTree("first", 
															SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
																	SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))), "1")))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
											SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
													SyntaxTrees.makeCompoundSyntaxTree("rest", 
															SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
																	SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")))), 
											SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
													SyntaxTrees.makeCompoundSyntaxTree("rest", 
															SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
																	SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")))), "Z")), 
							SyntaxTrees.makeCompoundSyntaxTree("foo", "X", "Y"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "1"))))));

			expression = "{(on X in {}) foo(X) | X != a and X != b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list")))), 
					SyntaxTrees.makeCompoundSyntaxTree("foo", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{(on X in {1,2,3}, Y in {a,b,c}) p(X,Y)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))), 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("p", "X", "Y"), null));

			expression = "{p(1,a), p(1,b), p(1,c), p(2,a), p(2,b), p(2,c), p(3,a), p(3,b), p(3,c)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "1", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "1", "b"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "1", "c"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "2", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "2", "b"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "2", "c"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "3", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "3", "b"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "3", "c"))));

			expression = "{(on X) if X = 2 then p(X) else q(X) | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("q", "X")), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "partition({(on X) p(X) | X != a and X = 2}, {(on X) q(X) | X != a and not(X = 2)})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("=", "X", "2")))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("q", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("not", 
													SyntaxTrees.makeCompoundSyntaxTree("=", "X", "2")))))));


			expression = "{(on X) if Y = 2 then p(X) else q(X) | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "2"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("q", "X")), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));


			expression = "sum({(on Person in World) if Person = rich then 2000 else 50 | Person = american})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Person", "World")), 
							SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "rich"), "2000", "50"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "american")))));

			expression = 
					"sum(" +
					"partition(" + 
					"{(on Person in World) 2000 | Person = american and Person = rich}," + 
					"{(on Person in World) 50 | Person = american and not (Person = rich)}))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("partition", 
							SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("in", "Person", "World")), "2000", 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("and", 
													SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "american"), 
													SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "rich")))), 
							SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("in", "Person", "World")), "50", 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("and", 
													SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "american"), 
													SyntaxTrees.makeCompoundSyntaxTree("not", 
															SyntaxTrees.makeCompoundSyntaxTree("=", "Person", "rich"))))))));
		
			expression = "{(on X) if X = 2 and something then p(X) else q(X) | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "X", "2"), "something"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("q", "X")), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "partition({(on X) if something then p(X) else q(X) | X != a and X = 2}, {(on X) q(X) | X != a and not(X = 2)})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", "something", 
									SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("q", "X")), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("=", "X", "2")))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("q", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("not", 
													SyntaxTrees.makeCompoundSyntaxTree("=", "X", "2")))))));

			expression = "{(on X in {(on Y) foo(Y)}) bar(X) | X != a and X != b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
											SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Y"), null))), 
					SyntaxTrees.makeCompoundSyntaxTree("bar", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{(on Y) bar(foo(Y)) | foo(Y) != a and foo(Y) != b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("bar", 
							SyntaxTrees.makeCompoundSyntaxTree("foo", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Y"), "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Y"), "b")))));

			expression = "{(on X in {(on Y) foo(Y) | Y != c}) bar(X) | X != a and X != b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
											SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Y"), 
											SyntaxTrees.makeCompoundSyntaxTree("|", 
													SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "c"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("bar", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{(on Y) bar(foo(Y)) | foo(Y) != a and foo(Y) != b and Y != c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("bar", 
							SyntaxTrees.makeCompoundSyntaxTree("foo", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Y"), "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Y"), "b"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "c")))));

			expression = "{(on X in {(on Y in {(on Z) Z}) foo(Y) | Y != c}) bar(X) | X != a and X != b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
											SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
													SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
															SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
																	SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), "Z", null))), 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Y"), 
											SyntaxTrees.makeCompoundSyntaxTree("|", 
													SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "c"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("bar", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{(on Z) bar(foo(Z)) | foo(Z) != a and foo(Z) != b and Z != c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("bar", 
							SyntaxTrees.makeCompoundSyntaxTree("foo", "Z")), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Z"), "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Z"), "b"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "c")))));

			expression = "{(on X in {(on Y in {(on Z) Z}) foo(Y) | Y != c}) bar(X) | X != a and X != b and 'for all'({(on Z) blah(Z)})}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
											SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
													SyntaxTrees.makeCompoundSyntaxTree("in", "Y", 
															SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
																	SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), "Z", null))), 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Y"), 
											SyntaxTrees.makeCompoundSyntaxTree("|", 
													SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "c"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("bar", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b"), 
									SyntaxTrees.makeCompoundSyntaxTree("for all", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
													SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
													SyntaxTrees.makeCompoundSyntaxTree("blah", "Z"), null))))));

			expression = "{(on Z) bar(foo(Z)) | foo(Z) != a and foo(Z) != b and 'for all'({(on Z) blah(Z)}) and Z != c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("bar", 
							SyntaxTrees.makeCompoundSyntaxTree("foo", "Z")), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Z"), "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", 
											SyntaxTrees.makeCompoundSyntaxTree("foo", "Z"), "b"), 
									SyntaxTrees.makeCompoundSyntaxTree("for all", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
													SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
													SyntaxTrees.makeCompoundSyntaxTree("blah", "Z"), null)), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "c")))));

			expression = "{(on X in 'some set') p(X) | X != a} - {somethingElse}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", "some set")), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "somethingElse")));

			expression = "{(on X in 'some set') p(X) | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", "some set")), 
					SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) p(X) | X != a} - {p(Y)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "Y"))));

			expression = "{(on X) p(X) | X != a and X != Y}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "Y")))));

			expression = "{(on X) p(X) | X != a} - {p(a)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "a"))));

			expression = "{(on X) p(X) | X != a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) p(X) | X != a} - {p(b)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("p", "b"))));

			expression = "{(on X) p(X) | X != a and X != b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("p", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{ }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list")));

			expression = "{a, b, c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")));

			expression = "{a, a, b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "a", "b")));

			expression = "{a, b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b")));

			expression = "{a, b, b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "b")));

			expression = "{a, b, b, a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "b", "a")));

			expression = "{a, b, b, a, b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "b", "a", "b")));

			expression = "{X, Y}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")));

			expression = "if X = Y then {X} else {X,Y}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y"))));

			expression = "{X, Y, a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y", "a")));

			expression = 
					"if X = Y " +
					"then if X = a then {X} else {X,a} " +
					"else if X = a then {X,Y} else if Y = a then {X,Y} else {X,Y,a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("{ . }", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "a"))), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
							SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "a"), 
									SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
									SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y", "a"))))));

			expression = "partition({}, X, {}, Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list")), "X", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list")), "Y"));

			expression = "partition(X, Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", "X", "Y"));

			expression = "partition({}, X, {1}, Y, {2,3})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list")), "X", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "1"), "Y", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "2", "3"))));

			expression = "partition({1,2,3}, X, Y)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")), "X", "Y"));

			expression = "partition({}, {})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list"))));

			expression = "{}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list")));

			expression = "partition(X)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("partition", "X"));

			expression = "X";
			test(expression, SyntaxTrees.makeSymbol("X"));

			expression = "(1, 2)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("( . )", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2")));

			expression = "(X, Y) = (X, Y, Z)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("=", 
					SyntaxTrees.makeCompoundSyntaxTree("( . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("( . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y", "Z"))));

			expression = "tuple(X, Y) = (X, Y, Z)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("=", 
					SyntaxTrees.makeCompoundSyntaxTree("tuple", "X", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("( . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y", "Z"))));

			expression = "(X, Y) = tuple(X, Y, Z)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("=", 
					SyntaxTrees.makeCompoundSyntaxTree("( . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("tuple", "X", "Y", "Z")));

			expression = "(X, Y, Z) = (a, b, c)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("=", 
					SyntaxTrees.makeCompoundSyntaxTree("( . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y", "Z")), 
					SyntaxTrees.makeCompoundSyntaxTree("( . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))));

			expression = "X = a and Y = b and Z = c";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("and", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "b"), 
					SyntaxTrees.makeCompoundSyntaxTree("=", "Z", "c")));

			expression = "tuple(X, Y, Z) = tuple(a, b, c)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("=", 
					SyntaxTrees.makeCompoundSyntaxTree("tuple", "X", "Y", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("tuple", "a", "b", "c")));

			expression = "tuple(X, Y, Z) = (a, b, c)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("=", 
					SyntaxTrees.makeCompoundSyntaxTree("tuple", "X", "Y", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("( . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))));

			expression = "(X, Y, Z) = tuple(a, b, c)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("=", 
					SyntaxTrees.makeCompoundSyntaxTree("( . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y", "Z")), 
					SyntaxTrees.makeCompoundSyntaxTree("tuple", "a", "b", "c")));

			expression = "first(list(a,b,c))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("first", 
					SyntaxTrees.makeCompoundSyntaxTree("list", "a", "b", "c")));

			expression = "first(list(a))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("first", 
					SyntaxTrees.makeCompoundSyntaxTree("list", "a")));

			expression = "rest(list(a,b,c))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("rest", 
					SyntaxTrees.makeCompoundSyntaxTree("list", "a", "b", "c")));

			expression = "list(b,c)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("list", "b", "c"));

			expression = "rest(list(a))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("rest", 
					SyntaxTrees.makeCompoundSyntaxTree("list", "a")));

			expression = "list()";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("list"));

			expression = "size(list(a,b,c))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("size", 
					SyntaxTrees.makeCompoundSyntaxTree("list", "a", "b", "c")));

			expression = "size(list(a))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("size", 
					SyntaxTrees.makeCompoundSyntaxTree("list", "a")));

			expression = "size(list())";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("size", 
					SyntaxTrees.makeCompoundSyntaxTree("list")));


			expression = "{x} union {y}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("union", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "x"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "y")));

			expression = "{x, y}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y")));

			expression = "{x, y} union {z}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("union", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "z")));

			expression = "{x, y, z}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y", "z")));

			expression = "{x, y} union {}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("union", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list"))));

			expression = "{} union {x, y} union {}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("union", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list"))));

			expression = "A union {x, y} union {}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("union", "A", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list"))));

			expression = "A union {x, y}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("union", "A", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y"))));

			expression = "A union {x, y} union {z} union B";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("union", "A", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "z"), "B"));

			expression = "A union {x, y, z} union B";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("union", "A", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "x", "y", "z")), "B"));


			expression = "sum({{(on X) c}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "c", null)));

			expression = "sum({{(on X) c}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "c", null)));

			expression = "c * |{{(on X) c}}|";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", "c", 
					SyntaxTrees.makeCompoundSyntaxTree("| . |", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "c", null))));

			expression = "product({{(on X) c}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("product", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "c", null)));

			expression = "c ^ |{{(on X) c}}|";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("^", "c", 
					SyntaxTrees.makeCompoundSyntaxTree("| . |", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), "c", null))));

			expression = "sum({{(on X) f(Y)}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "Y"), null)));

			expression = "f(Y) * |{{(on X) f(Y)}}|";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("f", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("| . |", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("f", "Y"), null))));

			expression = "sum({{(on X) f(X)}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X"), null)));

			expression = "sum({{1,2,3}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))));


			expression = "{}-{d}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "d")));

			expression = "{}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list")));

			expression = "{d}-{}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "d"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list"))));

			expression = "{d}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", "d"));

			expression = "{a,b,c}-{d}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "d")));

			expression = "{a,b,c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")));

			expression = "{a,b,c}-{b}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "b")));

			expression = "{a,c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
					SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "c")));

			expression = "{a,b,c}-{b,a}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "b", "a"))));

			expression = "{c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . }", "c"));

			expression = "{a,b,c}-{a,b,c}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c"))));

			expression = "{{a,b,c}}-{{a,X,c}}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "a", "X", "c"))));

			expression = "if X = b then {} else { b }";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "b"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "b")));

			expression = "{{X,a,b,Y,Y}}-{{X,X,Y,a}}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("-", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "a", "b", "Y", "Y")), 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "X", "Y", "a"))));

			expression = "if X = b then { Y } else if X = Y then { b } else {{ b, Y }}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "b"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
							SyntaxTrees.makeCompoundSyntaxTree("{ . }", "b"), 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "b", "Y")))));

			expression = "{(on X in {1,2,3}, Y in D) f(X) | X = Z}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))), 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", "D"))), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z"))));

			expression = "{(on Y in D) f(Z)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "Y", "D")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "Z"), null));

			expression = "{(on X in {1,2,3}, Y in D) f(X) | X = Z and h(X)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list", 
									SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
													SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3"))), 
									SyntaxTrees.makeCompoundSyntaxTree("in", "Y", "D"))), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("h", "X")))));

			expression = "{(on Y in D) f(Z) | h(Z)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "Y", "D")), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("h", "Z"))));

			expression = "{(on X in {1,2,3}) f(X) | X = Z and h(X)}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "X", 
									SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("and", 
									SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("h", "X")))));

			expression = "if h(Z) then {f(Z)} else {}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("h", "Z"), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("f", "Z")), 
					SyntaxTrees.makeCompoundSyntaxTree("{ . }", 
							SyntaxTrees.makeCompoundSyntaxTree("kleene list"))));

			expression = "{{ ( on Y in People ) sum({{ ( on p(a1, Y) ) p(a1, Y) }}) | Y = a2 }}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
					SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
							SyntaxTrees.makeCompoundSyntaxTree("in", "Y", "People")), 
					SyntaxTrees.makeCompoundSyntaxTree("sum", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("p", "a1", "Y")), 
									SyntaxTrees.makeCompoundSyntaxTree("p", "a1", "Y"), null)), 
					SyntaxTrees.makeCompoundSyntaxTree("|", 
							SyntaxTrees.makeCompoundSyntaxTree("=", "Y", "a2"))));

			expression = "{{ sum({{ ( on p(a1, a2) ) p(a1, a2) }}) }}";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("{{ . }}", 
					SyntaxTrees.makeCompoundSyntaxTree("sum", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("p", "a1", "a2")), 
									SyntaxTrees.makeCompoundSyntaxTree("p", "a1", "a2"), null))));

			expression = "if Z = a then f(lambda Z : g(Z)) else 0";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "Z", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", 
							SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", "Z", 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Z"))), "0"));

			expression = "'scoped variables'(lambda Z : Z)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("scoped variables", 
					SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", "Z", "Z")));

			expression = "list(Z)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("list", "Z"));

			expression = "'scoped variables'(lambda f(X) : f(X))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("scoped variables", 
					SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X"))));

			expression = "list(f(X))";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("list", 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X")));

			expression = "if X = a then lambda f(X) : f(X) else 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X")), "1"));

			expression = "if X = a then lambda f(a) : f(a) else 1";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", 
							SyntaxTrees.makeCompoundSyntaxTree("f", "a"), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "a")), "1"));

			expression = "(lambda f(X) : 2 + f(X))(1)";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree(
					SyntaxTrees.makeCompoundSyntaxTree("lambda . : .", 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("+", "2", 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X"))), "1"));

			expression = "product({{(on X) f(X, Y) | X != a}}) * product({{(on Z) g(Z) | Z != b}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))), 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "b"))))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y}}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("and", 
													SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
													SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "a"))))));

			expression = "product({{ ( on Y ) (f(Y, Y) * g(Y)) | Y != a }})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("product", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
							SyntaxTrees.makeCompoundSyntaxTree("*", 
									SyntaxTrees.makeCompoundSyntaxTree("f", "Y", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Y")), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "Y", "a")))));

			expression = "product({{(on X, Y) f(X, Y) | X != a}}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))), 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "a"))))));

			expression = "product({{ (on X) (product({{(on Y) f(X, Y)}}) * g(X))  | X != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("product", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("*", 
									SyntaxTrees.makeCompoundSyntaxTree("product", 
											SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
													SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
													SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), null)), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "X")), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));

			expression = "product({{(on X) f(X, Y)}}) * product({{(on Z) g(Z)}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), null)), 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Z"), null))));

			expression = "product({{ (on X) (f(X, Y) * g(X)) }})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("product", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("*", 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "X")), null)));

			expression = "product({{(on X, Y, V) f(X, Y, V) | X != a}}) * product({{(on W, Z) g(Z, W) | Z != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y", "V")), 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y", "V"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))), 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "W", "Z")), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Z", "W"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "a"))))));

			expression = "product({{ ( on X ) (product({{ ( on Y ) (product({{ ( on V ) f(X, Y, V) }}) * g(X, Y)) }})) | X != a }})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("product", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("product", 
									SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
											SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Y"), 
											SyntaxTrees.makeCompoundSyntaxTree("*", 
													SyntaxTrees.makeCompoundSyntaxTree("product", 
															SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
																	SyntaxTrees.makeCompoundSyntaxTree("( on . )", "V"), 
																	SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y", "V"), null)), 
													SyntaxTrees.makeCompoundSyntaxTree("g", "X", "Y")), null)), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));

			expression = "sum({{(on X) f(X, Y) * g(Y) | X != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("*", 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Y")), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));

			expression = "g(Y) * sum({{(on X) f(X, Y) | X != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("g", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("sum", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))))));

			expression = "sum({{(on X) f(X, Y) * g(Y) * h() | X != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("*", 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("h")), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));

			expression = "g(Y) * h() * sum({{(on X) f(X, Y) | X != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("g", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("h"), 
					SyntaxTrees.makeCompoundSyntaxTree("sum", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"))))));

			expression = "sum({{(on X) g(Y) | X != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("sum", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", "X"), 
							SyntaxTrees.makeCompoundSyntaxTree("g", "Y"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a")))));

			expression = "g(Y) * | type(X) - { a } |";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("g", "Y"), 
					SyntaxTrees.makeCompoundSyntaxTree("| . |", 
							SyntaxTrees.makeCompoundSyntaxTree("-", 
									SyntaxTrees.makeCompoundSyntaxTree("type", "X"), 
									SyntaxTrees.makeCompoundSyntaxTree("{ . }", "a")))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y }}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("*", 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
									SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("and", 
													SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
													SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"))))), 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("g", "Z"), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "Z", "a"))))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y and W = a}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("product", 
					SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
							SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
									SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
							SyntaxTrees.makeCompoundSyntaxTree("f", "X", "Y"), 
							SyntaxTrees.makeCompoundSyntaxTree("|", 
									SyntaxTrees.makeCompoundSyntaxTree("and", 
											SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
											SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y"), 
											SyntaxTrees.makeCompoundSyntaxTree("=", "W", "a"))))));

			expression = "X + product({{(on X, Y) |{(on W) W }|  |  X != a and X = Y}})";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("+", "X", 
					SyntaxTrees.makeCompoundSyntaxTree("product", 
							SyntaxTrees.makeCompoundSyntaxTree("{{ . . . }}", 
									SyntaxTrees.makeCompoundSyntaxTree("( on . )", 
											SyntaxTrees.makeCompoundSyntaxTree("kleene list", "X", "Y")), 
									SyntaxTrees.makeCompoundSyntaxTree("| . |", 
											SyntaxTrees.makeCompoundSyntaxTree("{ . . . }", 
													SyntaxTrees.makeCompoundSyntaxTree("( on . )", "W"), "W", null)), 
									SyntaxTrees.makeCompoundSyntaxTree("|", 
											SyntaxTrees.makeCompoundSyntaxTree("and", 
													SyntaxTrees.makeCompoundSyntaxTree("!=", "X", "a"), 
													SyntaxTrees.makeCompoundSyntaxTree("=", "X", "Y")))))));

			expression = "if X = a then type(X) else nothing";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("type", "X"), "nothing"));

			expression = "if X = a then f(X, type(X)) else nothing";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "X", 
							SyntaxTrees.makeCompoundSyntaxTree("type", "X")), "nothing"));

			expression = "if X = a then f(a, type(X)) else nothing";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("if . then . else .", 
					SyntaxTrees.makeCompoundSyntaxTree("=", "X", "a"), 
					SyntaxTrees.makeCompoundSyntaxTree("f", "a", 
							SyntaxTrees.makeCompoundSyntaxTree("type", "X")), "nothing"));

			expression = "f(X,g())";
			test(expression, SyntaxTrees.makeCompoundSyntaxTree("f", "X", 
					SyntaxTrees.makeCompoundSyntaxTree("g")));

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
