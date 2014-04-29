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
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSymbol;
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
		test(string, DefaultSymbol.createSymbol("e"));

		string = "3";
		test(string, DefaultSymbol.createSymbol(3));

		string = "3e7";
		test(string, DefaultSymbol.createSymbol(30000000));

		string = "3e-1";
		test(string, DefaultSymbol.createSymbol("0.30"));

		string = ".3e7";
		test(string, DefaultSymbol.createSymbol(3000000));

		string = ".3e-1";
		test(string, DefaultSymbol.createSymbol("0.030"));

		string = "1.3e7";
		test(string, DefaultSymbol.createSymbol(13000000));

		string = "1.3e-1";
		test(string, DefaultSymbol.createSymbol("0.130"));

		string = "false";
		test(string, DefaultSymbol.createSymbol(false));

		string = "keyword";
		test(string, DefaultSymbol.createSymbol("keyword"));

		string = "Keyword";
		test(string, DefaultSymbol.createSymbol("Keyword"));

		string = "foo";
		test(string, DefaultSymbol.createSymbol("foo"));

		string = "foo1";
		test(string, DefaultSymbol.createSymbol("foo1"));

		string = "foo10bar";
		test(string, DefaultSymbol.createSymbol("foo10bar"));

		string = "1foo";
		test(string, DefaultSymbol.createSymbol("1foo"));

		string = "'foo1'";
		test(string, DefaultSymbol.createSymbol("foo1"));

		string = "foo1'";
		test(string, DefaultSymbol.createSymbol("foo1'"));

		string = "foo1'''";
		test(string, DefaultSymbol.createSymbol("foo1'''"));

		string = "'This is a test.'";
		test(string, DefaultSymbol.createSymbol("This is a test."));

		string = "\"This is a test.\"";
		test(string, DefaultSymbol.createSymbol("This is a test."));

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
		test(string, DefaultSymbol.createSymbol("Test a"));
		
		string = "\"Test \\u0061\"";
		test(string, DefaultSymbol.createSymbol("Test a"));

		string = "'Testing the  preservation \\t of	whitespace\\ncharacters.'";
		test(string, DefaultSymbol.createSymbol("Testing the  preservation 	 of	whitespace\ncharacters."));

		string = "\"Testing the  preservation \\t of	whitespace\\ncharacters.\"";
		test(string, DefaultSymbol.createSymbol("Testing the  preservation 	 of	whitespace\ncharacters."));

		string = "'This is a test *()#@$%!-_=+<>,./?:;\\'\"\\\"\\\\'";
		test(string, DefaultSymbol.createSymbol("This is a test *()#@$%!-_=+<>,./?:;'\"\"\\"));

		string = "\"This is a test *()#@$%!-_=+<>,./?:;\\''\\\"\\\"\\\\\"";
		test(string, DefaultSymbol.createSymbol("This is a test *()#@$%!-_=+<>,./?:;''\"\"\\"));
		
		string = "foo(bar1', 'bar2\\'', bar3''')";
		test(string, new DefaultCompoundSyntaxTree("foo", "bar1'", "bar2'", "bar3'''"));
	}
	
	@Test 
	public void testComment () {
		String string;
		string = "3";
		test(string, DefaultSymbol.createSymbol(3));

		string = "3 // This is a test.\n";
		test(string, DefaultSymbol.createSymbol(3));

		string = "// This is a test.\n 3";
		test(string, DefaultSymbol.createSymbol(3));

		string = "3 // This is a test.";
		test(string, DefaultSymbol.createSymbol(3));

		string = "3 // This is a test.\n + 4";
		test(string, new DefaultCompoundSyntaxTree("+", 3, 4));

		string = "// Test\n 3 // This is a test.\n + 4 // Test";
		test(string, new DefaultCompoundSyntaxTree("+", 3, 4));

		string = "3 /* This is a test. */";
		test(string, DefaultSymbol.createSymbol(3));

		string = "/* This is a test. */ 3";
		test(string, DefaultSymbol.createSymbol(3));

		string = "3 /* This is a test. */ + 4";
		test(string, new DefaultCompoundSyntaxTree("+", 3, 4));
	}

	@Test
	public void testParen () {
		String string;
		string = "(foo)";
		test(string, DefaultSymbol.createSymbol("foo"));

		string = "(foo1)";
		test(string, DefaultSymbol.createSymbol("foo1"));

		string = "(foo10bar)";
		test(string, DefaultSymbol.createSymbol("foo10bar"));

		string = "(1foo)";
		test(string, DefaultSymbol.createSymbol("1foo"));

		string = "('foo1')";
		test(string, DefaultSymbol.createSymbol("foo1"));

		string = "(foo1')";
		test(string, DefaultSymbol.createSymbol("foo1'"));

		string = "(foo1''')";
		test(string, DefaultSymbol.createSymbol("foo1'''"));

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
		test(string, new DefaultCompoundSyntaxTree("foo"));

		string = "foo(1)";
		test(string, new DefaultCompoundSyntaxTree("foo", 1));

		string = "foo(1, 2, 3)";
		test(string, new DefaultCompoundSyntaxTree("foo", 1, 2, 3));

		string = "foo(bar)";
		test(string, new DefaultCompoundSyntaxTree("foo", "bar"));

		string = "foo(bar1, bar2, bar3)";
		test(string, new DefaultCompoundSyntaxTree("foo", "bar1", "bar2", "bar3"));

		string = "foo(1+2, bar in hello)";
		test(string, new DefaultCompoundSyntaxTree("foo", 
				new DefaultCompoundSyntaxTree("+", 1, 2),
				new DefaultCompoundSyntaxTree("in", "bar", "hello")));

		string = "foo(1+2, (bar in hello))";
		test(string, new DefaultCompoundSyntaxTree("foo", 
				new DefaultCompoundSyntaxTree("+", 1, 2),
				new DefaultCompoundSyntaxTree("in", "bar", "hello")));

		string = "'foo bar'()";
		test(string, new DefaultCompoundSyntaxTree("foo bar"));

		string = "'foo bar'(a)";
		test(string, new DefaultCompoundSyntaxTree("foo bar", "a"));

		string = "'foo bar'(a, b, c)";
		test(string, new DefaultCompoundSyntaxTree("foo bar", "a", "b", "c"));
		
		string = "foo(bar(X))";
		test(string, new DefaultCompoundSyntaxTree("foo", 
				new DefaultCompoundSyntaxTree("bar", "X")));
		
		string = "foo(bar(X), baz(Y))";
		test(string, new DefaultCompoundSyntaxTree("foo", 
				new DefaultCompoundSyntaxTree("bar", "X"), 
						new DefaultCompoundSyntaxTree("baz", "Y")));
		
		string = "foo(W, bar(X), Y)";
		test(string, new DefaultCompoundSyntaxTree("foo", "W", 
				new DefaultCompoundSyntaxTree("bar", "X"), "Y"));

		string = "(lambda x : y)(a, b, c)";
		test(string, new DefaultCompoundSyntaxTree(
				new DefaultCompoundSyntaxTree("lambda . : .", "x", "y"), "a", "b", "c"));

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
		test(string, new DefaultCompoundSyntaxTree("( . )", 
				new DefaultCompoundSyntaxTree("kleene list", "foo", "bar")));

		string = "(x, y, z)";
		test(string, new DefaultCompoundSyntaxTree("( . )", 
				new DefaultCompoundSyntaxTree("kleene list", "x", "y", "z")));

		string = "(a in b, x + y + z, i, j, k)";
		test(string, new DefaultCompoundSyntaxTree("( . )", 
				new DefaultCompoundSyntaxTree("kleene list", 
						new DefaultCompoundSyntaxTree("in", "a", "b"), 
						new DefaultCompoundSyntaxTree("+", "x", "y", "z"), 
						"i", "j", "k")));

		string = "'( . )'()";
		test(string, new DefaultCompoundSyntaxTree("( . )"));

		string = "'( . )'(a)";
		test(string, new DefaultCompoundSyntaxTree("( . )", "a"));

		string = "'( . )'(a, b, c)";
		test(string, new DefaultCompoundSyntaxTree("( . )", "a", "b", "c"));

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
		test(string, new DefaultCompoundSyntaxTree("+", "1", "2"));

		string = "1 + 2 + 3";
		test(string, new DefaultCompoundSyntaxTree("+", "1", "2", "3"));

		string = "1 + 2 + 3 + 4";
		test(string, new DefaultCompoundSyntaxTree("+", "1", "2", "3", "4"));

		string = "1 + 2 + 3 + 4 + 5";
		test(string, new DefaultCompoundSyntaxTree("+", "1", "2", "3", "4", "5"));

		string = "1 + 2 + 3 + 4 + 5 + 6";
		test(string, new DefaultCompoundSyntaxTree("+", "1", "2", "3", "4", "5", "6"));

		string = "1 + 2 + 3 + 4 + 5 + 6 + 7";
		test(string, new DefaultCompoundSyntaxTree("+", "1", "2", "3", "4", "5", "6", "7"));
	}

	@Test
	public void testAssocTimes() throws IOException {
		String string;
		string = "1 * 2";
		test(string, new DefaultCompoundSyntaxTree("*", "1", "2"));

		string = "1 * 2 * 3";
		test(string, new DefaultCompoundSyntaxTree("*", "1", "2", "3"));

		string = "1 * 2 * 3 * 4";
		test(string, new DefaultCompoundSyntaxTree("*", "1", "2", "3", "4"));

		string = "1 * 2 * 3 * 4 * 5";
		test(string, new DefaultCompoundSyntaxTree("*", "1", "2", "3", "4", "5"));

		string = "1 * 2 * 3 * 4 * 5 * 6";
		test(string, new DefaultCompoundSyntaxTree("*", "1", "2", "3", "4", "5", "6"));

		string = "1 * 2 * 3 * 4 * 5 * 6 * 7";
		test(string, new DefaultCompoundSyntaxTree("*", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocUnion() throws IOException {
		String string;
		string = "1 union 2";
		test(string, new DefaultCompoundSyntaxTree("union", "1", "2"));

		string = "1 union 2 union 3";
		test(string, new DefaultCompoundSyntaxTree("union", "1", "2", "3"));

		string = "1 union 2 union 3 union 4";
		test(string, new DefaultCompoundSyntaxTree("union", "1", "2", "3", "4"));

		string = "1 union 2 union 3 union 4 union 5";
		test(string, new DefaultCompoundSyntaxTree("union", "1", "2", "3", "4", "5"));

		string = "1 union 2 union 3 union 4 union 5 union 6";
		test(string, new DefaultCompoundSyntaxTree("union", "1", "2", "3", "4", "5", "6"));

		string = "1 union 2 union 3 union 4 union 5 union 6 union 7";
		test(string, new DefaultCompoundSyntaxTree("union", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocEqual() throws IOException {
		String string;
		string = "1 = 2";
		test(string, new DefaultCompoundSyntaxTree("=", "1", "2"));

		string = "1 = 2 = 3";
		test(string, new DefaultCompoundSyntaxTree("=", "1", "2", "3"));

		string = "1 = 2 = 3 = 4";
		test(string, new DefaultCompoundSyntaxTree("=", "1", "2", "3", "4"));

		string = "1 = 2 = 3 = 4 = 5";
		test(string, new DefaultCompoundSyntaxTree("=", "1", "2", "3", "4", "5"));

		string = "1 = 2 = 3 = 4 = 5 = 6";
		test(string, new DefaultCompoundSyntaxTree("=", "1", "2", "3", "4", "5", "6"));

		string = "1 = 2 = 3 = 4 = 5 = 6 = 7";
		test(string, new DefaultCompoundSyntaxTree("=", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocAnd() throws IOException {
		String string;
		string = "1 and 2";
		test(string, new DefaultCompoundSyntaxTree("and", "1", "2"));

		string = "1 and 2 and 3";
		test(string, new DefaultCompoundSyntaxTree("and", "1", "2", "3"));

		string = "1 and 2 and 3 and 4";
		test(string, new DefaultCompoundSyntaxTree("and", "1", "2", "3", "4"));

		string = "1 and 2 and 3 and 4 and 5";
		test(string, new DefaultCompoundSyntaxTree("and", "1", "2", "3", "4", "5"));

		string = "1 and 2 and 3 and 4 and 5 and 6";
		test(string, new DefaultCompoundSyntaxTree("and", "1", "2", "3", "4", "5", "6"));

		string = "1 and 2 and 3 and 4 and 5 and 6 and 7";
		test(string, new DefaultCompoundSyntaxTree("and", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocOr() throws IOException {
		String string;
		string = "1 or 2";
		test(string, new DefaultCompoundSyntaxTree("or", "1", "2"));

		string = "1 or 2 or 3";
		test(string, new DefaultCompoundSyntaxTree("or", "1", "2", "3"));

		string = "1 or 2 or 3 or 4";
		test(string, new DefaultCompoundSyntaxTree("or", "1", "2", "3", "4"));

		string = "1 or 2 or 3 or 4 or 5";
		test(string, new DefaultCompoundSyntaxTree("or", "1", "2", "3", "4", "5"));

		string = "1 or 2 or 3 or 4 or 5 or 6";
		test(string, new DefaultCompoundSyntaxTree("or", "1", "2", "3", "4", "5", "6"));

		string = "1 or 2 or 3 or 4 or 5 or 6 or 7";
		test(string, new DefaultCompoundSyntaxTree("or", "1", "2", "3", "4", "5", "6", "7"));
	}
	
	@Test
	public void testAssoc () {
		String string;
		string = "(1 + 2 + 3 + 4) * (5 + 6 + 7 + 8) * (9 + 10 + 11 + 12)";
		test(string, new DefaultCompoundSyntaxTree("*", 
				new DefaultCompoundSyntaxTree("+", "1", "2", "3", "4"), 
				new DefaultCompoundSyntaxTree("+", "5", "6", "7", "8"), 
				new DefaultCompoundSyntaxTree("+", "9", "10", "11", "12")));

		string = "(1 + 2 + 3 + 4) * (5 + 6 + 7 + 8) * (9 + 10 + 11 + (12 * 13 * 14 * 15))";
		test(string, new DefaultCompoundSyntaxTree("*", 
				new DefaultCompoundSyntaxTree("+", "1", "2", "3", "4"), 
				new DefaultCompoundSyntaxTree("+", "5", "6", "7", "8"), 
				new DefaultCompoundSyntaxTree("+", "9", "10", "11", 
						new DefaultCompoundSyntaxTree("*", "12", "13", "14", "15"))));

		string = "(1 or 2 or 3 or 4) and (5 or 6 or 7 or 8) and (9 or 10 or 11 or (12 and 13 and 14 and 15))";
		test(string, new DefaultCompoundSyntaxTree("and", 
				new DefaultCompoundSyntaxTree("or", "1", "2", "3", "4"), 
				new DefaultCompoundSyntaxTree("or", "5", "6", "7", "8"), 
				new DefaultCompoundSyntaxTree("or", "9", "10", "11", 
						new DefaultCompoundSyntaxTree("and", "12", "13", "14", "15"))));

		// Testing to make sure the associative node walker doesn't get confused by the function form of
		// the operators.
		string = "+(+(b, c))";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("+", "b", "c")));

		string = "+(+(b))";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("+", "b")));

		string = "+(a, +(+(b)))";
		test(string, new DefaultCompoundSyntaxTree("+", "a", 
				new DefaultCompoundSyntaxTree("+", 
						new DefaultCompoundSyntaxTree("+", "b"))));

		string = "+(a, +(+(b, c)))";
		test(string, new DefaultCompoundSyntaxTree("+", "a", 
				new DefaultCompoundSyntaxTree("+", 
						new DefaultCompoundSyntaxTree("+", "b", "c"))));

		string = "+({a}, +(+({b}, {c})))";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("{ . }", "a"), 
				new DefaultCompoundSyntaxTree("+", 
						new DefaultCompoundSyntaxTree("+", 
								new DefaultCompoundSyntaxTree("{ . }", "b"), 
								new DefaultCompoundSyntaxTree("{ . }", "c")))));

		string = "union({a}, union(union({b}, {c})))";
		test(string, new DefaultCompoundSyntaxTree("union", 
				new DefaultCompoundSyntaxTree("{ . }", "a"), 
				new DefaultCompoundSyntaxTree("union", 
						new DefaultCompoundSyntaxTree("union", 
								new DefaultCompoundSyntaxTree("{ . }", "b"), 
								new DefaultCompoundSyntaxTree("{ . }", "c")))));

		string = "union({a}, union(union({b}, {c}), {d}))";
		test(string, new DefaultCompoundSyntaxTree("union", 
				new DefaultCompoundSyntaxTree("{ . }", "a"), 
				new DefaultCompoundSyntaxTree("union", 
						new DefaultCompoundSyntaxTree("union", 
								new DefaultCompoundSyntaxTree("{ . }", "b"), 
								new DefaultCompoundSyntaxTree("{ . }", "c")), 
						new DefaultCompoundSyntaxTree("{ . }", "d"))));

		string = "union({a}, union(union({b}, {c}), {d}, {E}))";
		test(string, new DefaultCompoundSyntaxTree("union", 
				new DefaultCompoundSyntaxTree("{ . }", "a"), 
				new DefaultCompoundSyntaxTree("union", 
						new DefaultCompoundSyntaxTree("union", 
								new DefaultCompoundSyntaxTree("{ . }", "b"), 
								new DefaultCompoundSyntaxTree("{ . }", "c")), 
						new DefaultCompoundSyntaxTree("{ . }", "d"), 
						new DefaultCompoundSyntaxTree("{ . }", "E"))));

		string = "union({a}, union(union({b}, {c}), {d}, {E}, {f}))";
		test(string, new DefaultCompoundSyntaxTree("union", 
				new DefaultCompoundSyntaxTree("{ . }", "a"), 
				new DefaultCompoundSyntaxTree("union", 
						new DefaultCompoundSyntaxTree("union", 
								new DefaultCompoundSyntaxTree("{ . }", "b"), 
								new DefaultCompoundSyntaxTree("{ . }", "c")), 
						new DefaultCompoundSyntaxTree("{ . }", "d"), 
						new DefaultCompoundSyntaxTree("{ . }", "E"), 
						new DefaultCompoundSyntaxTree("{ . }", "f"))));
	}

	@Test
	public void testExpressionSymbol () {
		String string;
		
		string = "<x>";
		test(string, DefaultSymbol.createSymbol(DefaultSymbol.createSymbol("x")));

		string = "<foo''>";
		test(string, DefaultSymbol.createSymbol(DefaultSymbol.createSymbol("foo''")));

		string = "<x in y>";
		test(string, DefaultSymbol.createSymbol(
				new DefaultCompoundSyntaxTree("in", "x", "y")));

		string = "<{(on x, y) f(x,y) | y}>";
		test(string, DefaultSymbol.createSymbol(
				new DefaultCompoundSyntaxTree("{ . . . }", 
						new DefaultCompoundSyntaxTree("( on . )", 
								new DefaultCompoundSyntaxTree("kleene list", "x", "y")), 
						new DefaultCompoundSyntaxTree("f", "x", "y"), 
						new DefaultCompoundSyntaxTree("|", "y"))));

		string = "< x < y >";
		test(string, DefaultSymbol.createSymbol(
				new DefaultCompoundSyntaxTree("<", "x", "y")));

		string = "<x>y>";
		test(string, DefaultSymbol.createSymbol(
				new DefaultCompoundSyntaxTree(">", "x", "y")));

		string = "< x > y >";
		test(string, DefaultSymbol.createSymbol(
				new DefaultCompoundSyntaxTree(">", "x", "y")));

		string = "< if x > y then x else y >";
		test(string, DefaultSymbol.createSymbol(
				new DefaultCompoundSyntaxTree("if . then . else .", 
						new DefaultCompoundSyntaxTree(">", "x", "y"), "x", "y")));
		
		string = "if <x> > <y> then x else y";
		test(string, new DefaultCompoundSyntaxTree("if . then . else .",
				new DefaultCompoundSyntaxTree(">",
						DefaultSymbol.createSymbol(DefaultSymbol.createSymbol("x")),
						DefaultSymbol.createSymbol(DefaultSymbol.createSymbol("y"))
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
		test(string, new DefaultCompoundSyntaxTree("| . |", "foo"));

		string = "| foo in bar |";
		test(string, new DefaultCompoundSyntaxTree("| . |", 
				new DefaultCompoundSyntaxTree("in", "foo", "bar")));

		string = "| {} |";
		test(string, new DefaultCompoundSyntaxTree("| . |", 
				new DefaultCompoundSyntaxTree("{ . }",
						new DefaultCompoundSyntaxTree("kleene list"))));

		string = "| { foo, bar } |";
		test(string, new DefaultCompoundSyntaxTree("| . |", 
				new DefaultCompoundSyntaxTree("{ . }",
						new DefaultCompoundSyntaxTree("kleene list", "foo", "bar"))));

		string = "| ({ foo, bar }) |";
		test(string, new DefaultCompoundSyntaxTree("| . |", 
				new DefaultCompoundSyntaxTree("{ . }",
						new DefaultCompoundSyntaxTree("kleene list", "foo", "bar"))));

		string = "| 1 + 2 |";
		test(string, new DefaultCompoundSyntaxTree("| . |", 
				new DefaultCompoundSyntaxTree("+", 1, 2)));

		string = "'| . |'()";
		test(string, new DefaultCompoundSyntaxTree("| . |"));

		string = "'| . |'(a)";
		test(string, new DefaultCompoundSyntaxTree("| . |", "a"));

		string = "'| . |'(a, b, c)";
		test(string, new DefaultCompoundSyntaxTree("| . |", "a", "b", "c"));

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
		test(string, new DefaultCompoundSyntaxTree("{{ . . . }}", IntensionalSet.makeScopingExpression(new ArrayList<Expression>()), 
				new DefaultCompoundSyntaxTree("[ . ]",
						new DefaultCompoundSyntaxTree("if . then . else .", "X", "1", "0")), null));
		
		string = "{{ foo }}";
		test(string, new DefaultCompoundSyntaxTree("{{ . }}", "foo"));

		string = "{{ (on foo) bar }}";
		test(string, new DefaultCompoundSyntaxTree("{{ . . . }}", 
				new DefaultCompoundSyntaxTree("( on . )", "foo"), "bar", null));

		string = "{{ (on x in y) z }}";
		test(string, new DefaultCompoundSyntaxTree("{{ . . . }}", 
				new DefaultCompoundSyntaxTree("( on . )", 
						new DefaultCompoundSyntaxTree("in", "x", "y")), "z", null));

		string = "{{ foo | bar }}";
		test(string, new DefaultCompoundSyntaxTree("{{ . . . }}", null, "foo", 
				new DefaultCompoundSyntaxTree("|", "bar")));

		string = "{{f(X) | false}}";
		test(string, new DefaultCompoundSyntaxTree("{{ . . . }}", null, 
				new DefaultCompoundSyntaxTree("f", "X"), 
				new DefaultCompoundSyntaxTree("|", "false")));
		
		string = "{{ [if p(a) then 1 else 0] | true }}";
		test(string, new DefaultCompoundSyntaxTree("{{ . . . }}", null, 
				new DefaultCompoundSyntaxTree("[ . ]", 
						new DefaultCompoundSyntaxTree("if . then . else .", 
								new DefaultCompoundSyntaxTree("p", "a"), "1", "0")), 
				new DefaultCompoundSyntaxTree("|", "true")));

		string = "{{ (on foo, fooz) bar }}";
		test(string, new DefaultCompoundSyntaxTree("{{ . . . }}", 
				new DefaultCompoundSyntaxTree("( on . )", 
						new DefaultCompoundSyntaxTree("kleene list", "foo", "fooz")), 
						"bar", null));

		string = "{{ (on foo, fooz) bar | barz }}";
		test(string, new DefaultCompoundSyntaxTree("{{ . . . }}", 
				new DefaultCompoundSyntaxTree("( on . )", 
						new DefaultCompoundSyntaxTree("kleene list", "foo", "fooz")), 
						"bar", new DefaultCompoundSyntaxTree("|", "barz")));

		string = "{{ foo, bar, foo + bar }}";
		test(string, new DefaultCompoundSyntaxTree("{{ . }}", 
				new DefaultCompoundSyntaxTree("kleene list", "foo", "bar", 
						new DefaultCompoundSyntaxTree("+", "foo", "bar"))));

		string = "{{}}";
		test(string, new DefaultCompoundSyntaxTree("{{ . }}", 
				new DefaultCompoundSyntaxTree("kleene list")));

		string = "'{{ . }}'()";
		test(string, new DefaultCompoundSyntaxTree("{{ . }}"));

		string = "'{{ . }}'(a)";
		test(string, new DefaultCompoundSyntaxTree("{{ . }}", "a"));

		string = "'{{ . }}'(a, b, c)";
		test(string, new DefaultCompoundSyntaxTree("{{ . }}", "a", "b", "c"));

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
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", IntensionalSet.makeScopingExpression(new ArrayList<Expression>()), 
				new DefaultCompoundSyntaxTree("p", "X", "X"), Expressions.makeFunctionApplication(IntensionalSet.CONDITION_LABEL, "true")));

		string = "{ ( on ) X | true }";
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", IntensionalSet.makeScopingExpression(new ArrayList<Expression>()),  
				"X", Expressions.makeFunctionApplication(IntensionalSet.CONDITION_LABEL, "true")));
				
		string = "{ a | true}";
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", null, "a", Expressions.makeFunctionApplication(IntensionalSet.CONDITION_LABEL, "true")));
		
		string = "{ foo }";
		test(string, new DefaultCompoundSyntaxTree("{ . }", "foo"));

		string = "{ (on foo) bar }";
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", 
				new DefaultCompoundSyntaxTree("( on . )", "foo"), "bar", null));

		string = "{ (on x in y) z }";
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", 
				new DefaultCompoundSyntaxTree("( on . )", 
						new DefaultCompoundSyntaxTree("in", "x", "y")), "z", null));

		string = "{ foo | bar }";
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", null, "foo", 
				new DefaultCompoundSyntaxTree("|", "bar")));

		string = "{f(X) | false}";
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", null, 
				new DefaultCompoundSyntaxTree("f", "X"), 
				new DefaultCompoundSyntaxTree("|", "false")));
		
		string = "{ [if p(a) then 1 else 0] | true }";
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", null, 
				new DefaultCompoundSyntaxTree("[ . ]", 
						new DefaultCompoundSyntaxTree("if . then . else .", 
								new DefaultCompoundSyntaxTree("p", "a"), "1", "0")), 
				new DefaultCompoundSyntaxTree("|", "true")));

		string = "{ (on foo, fooz) bar }";
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", 
				new DefaultCompoundSyntaxTree("( on . )", 
						new DefaultCompoundSyntaxTree("kleene list", "foo", "fooz")), 
						"bar", null));

		string = "{ (on foo, fooz) bar | barz }";
		test(string, new DefaultCompoundSyntaxTree("{ . . . }", 
				new DefaultCompoundSyntaxTree("( on . )", 
						new DefaultCompoundSyntaxTree("kleene list", "foo", "fooz")), 
						"bar", new DefaultCompoundSyntaxTree("|", "barz")));

		string = "{ foo, bar, foo + bar }";
		test(string, new DefaultCompoundSyntaxTree("{ . }", 
				new DefaultCompoundSyntaxTree("kleene list", "foo", "bar", 
						new DefaultCompoundSyntaxTree("+", "foo", "bar"))));

		string = "{}";
		test(string, new DefaultCompoundSyntaxTree("{ . }", 
				new DefaultCompoundSyntaxTree("kleene list")));

		string = "'{ . }'()";
		test(string, new DefaultCompoundSyntaxTree("{ . }"));

		string = "'{ . }'(a)";
		test(string, new DefaultCompoundSyntaxTree("{ . }", "a"));

		string = "'{ . }'(a, b, c)";
		test(string, new DefaultCompoundSyntaxTree("{ . }", "a", "b", "c"));

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
		test(string, new DefaultCompoundSyntaxTree("[ . ]", 
				new DefaultCompoundSyntaxTree("in", "x", "y")));

		string = "[ x+1 ]";
		test(string, new DefaultCompoundSyntaxTree("[ . ]", 
				new DefaultCompoundSyntaxTree("+", "x", 1)));

		string = "[ (x, y) ]";
		test(string, new DefaultCompoundSyntaxTree("[ . ]", new DefaultCompoundSyntaxTree("( . )", 
				new DefaultCompoundSyntaxTree("kleene list", "x", "y"))));

		string = "'[ . ]'()";
		test(string, new DefaultCompoundSyntaxTree("[ . ]"));

		string = "'[ . ]'(a)";
		test(string, new DefaultCompoundSyntaxTree("[ . ]", "a"));

		string = "'[ . ]'(a, b, c)";
		test(string, new DefaultCompoundSyntaxTree("[ . ]", "a", "b", "c"));

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
		test(string, new DefaultCompoundSyntaxTree("neighbors of factor", "x"));

		string = "neighbors of factor [x]";
		test(string, new DefaultCompoundSyntaxTree("neighbors of factor", 
				new DefaultCompoundSyntaxTree("[ . ]", "x")));

		string = "neighbors of factor {x}";
		test(string, new DefaultCompoundSyntaxTree("neighbors of factor", 
				new DefaultCompoundSyntaxTree("{ . }", "x")));

		string = "neighbors of factor {{x}}";
		test(string, new DefaultCompoundSyntaxTree("neighbors of factor", 
				new DefaultCompoundSyntaxTree("{{ . }}", "x")));

		string = "'neighbors of factor'(a)";
		test(string, new DefaultCompoundSyntaxTree("neighbors of factor", "a"));

		string = "neighbors of factor neighbors of factor x";
		test(string, new DefaultCompoundSyntaxTree("neighbors of factor", 
				new DefaultCompoundSyntaxTree("neighbors of factor", "x")));
	}
	
	@Test
	public void testNeighborVariable () {
		String string;
		string = "neighbors of variable x";
		test(string, new DefaultCompoundSyntaxTree("neighbors of variable", "x"));

		string = "neighbors of variable [x]";
		test(string, new DefaultCompoundSyntaxTree("neighbors of variable", 
				new DefaultCompoundSyntaxTree("[ . ]", "x")));

		string = "neighbors of variable {x}";
		test(string, new DefaultCompoundSyntaxTree("neighbors of variable", 
				new DefaultCompoundSyntaxTree("{ . }", "x")));

		string = "neighbors of variable {{x}}";
		test(string, new DefaultCompoundSyntaxTree("neighbors of variable", 
				new DefaultCompoundSyntaxTree("{{ . }}", "x")));

		string = "'neighbors of variable'(a)";
		test(string, new DefaultCompoundSyntaxTree("neighbors of variable", "a"));

		string = "neighbors of variable neighbors of variable x";
		test(string, new DefaultCompoundSyntaxTree("neighbors of variable", 
				new DefaultCompoundSyntaxTree("neighbors of variable", "x")));
	}
	
	@Test
	public void testNeighborOf () {
		String string;
		string = "neighbors of x from y";
		test(string, new DefaultCompoundSyntaxTree("neighbors of . from .", "x", "y"));

		string = "neighbors of [x] from y";
		test(string, new DefaultCompoundSyntaxTree("neighbors of . from .", 
				new DefaultCompoundSyntaxTree("[ . ]", "x"), "y"));

		string = "neighbors of {x} from y";
		test(string, new DefaultCompoundSyntaxTree("neighbors of . from .", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "neighbors of {{x}} from y";
		test(string, new DefaultCompoundSyntaxTree("neighbors of . from .", 
				new DefaultCompoundSyntaxTree("{{ . }}", "x"), "y"));

		string = "'neighbors of . from .'(a, b)";
		test(string, new DefaultCompoundSyntaxTree("neighbors of . from .", "a", "b"));

		string = "neighbors of neighbors of x from y from neighbors of a from b";
		test(string, new DefaultCompoundSyntaxTree("neighbors of . from .", 
				new DefaultCompoundSyntaxTree("neighbors of . from .", "x", "y"), 
				new DefaultCompoundSyntaxTree("neighbors of . from .", "a", "b")));

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
		test(string, new DefaultCompoundSyntaxTree("not", "x"));

		string = "not (x)";
		test(string, new DefaultCompoundSyntaxTree("not", "x"));

		string = "not";
		test(string, DefaultSymbol.createSymbol("not"));

		string = "not()";
		test(string, new DefaultCompoundSyntaxTree("not"));

		string = "not(x, y)";
		test(string, new DefaultCompoundSyntaxTree("not", 
				new DefaultCompoundSyntaxTree("( . )", 
						new DefaultCompoundSyntaxTree("kleene list", "x", "y"))));

		string = "not {x, y, z}";
		test(string, new DefaultCompoundSyntaxTree("not", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("kleene list", "x", "y", "z"))));

		string = "not (x + y)";
		test(string, new DefaultCompoundSyntaxTree("not", 
				new DefaultCompoundSyntaxTree("+", "x", "y")));

		string = "not x + y";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("not", "x"), "y"));
	}

	@Test
	public void testNegative () {
		String string;
		
		string = "-3";
		test(string, new DefaultCompoundSyntaxTree("-", "3"));
		
		string = "-3e7";
		test(string, new DefaultCompoundSyntaxTree("-", 30000000));

		string = "-3e-1";
		test(string, new DefaultCompoundSyntaxTree("-", "0.30"));

		string = "-.3e7";
		test(string, new DefaultCompoundSyntaxTree("-", 3000000));

		string = "-.3e-1";
		test(string, new DefaultCompoundSyntaxTree("-", "0.030"));

		string = "-1.3e7";
		test(string, new DefaultCompoundSyntaxTree("-", 13000000));

		string = "-1.3e-1";
		test(string, new DefaultCompoundSyntaxTree("-", "0.130"));
		
		string = "-x";
		test(string, new DefaultCompoundSyntaxTree("-", "x"));

		string = "- x";
		test(string, new DefaultCompoundSyntaxTree("-", "x"));

		string = "--x";
		test(string, new DefaultCompoundSyntaxTree("-", new DefaultCompoundSyntaxTree("-", "x")));

		string = "-(x)";
		test(string, new DefaultCompoundSyntaxTree("-", "x"));

		string = "-";
		test(string, DefaultSymbol.createSymbol("-"));

		string = "-()";
		test(string, new DefaultCompoundSyntaxTree("-"));

		string = "-(x, y)";
		test(string, new DefaultCompoundSyntaxTree("-", 
				new DefaultCompoundSyntaxTree("( . )", 
						new DefaultCompoundSyntaxTree("kleene list", "x", "y"))));

		string = "-x - y";
		test(string, new DefaultCompoundSyntaxTree("-", 
				new DefaultCompoundSyntaxTree("-", "x"), "y"));

		string = "x - -y";
		test(string, new DefaultCompoundSyntaxTree("-", "x", 
				new DefaultCompoundSyntaxTree("-", "y")));

		string = "--x";
		test(string, new DefaultCompoundSyntaxTree("-", 
						new DefaultCompoundSyntaxTree("-", "x")));

		string = "-(-x)";
		test(string, new DefaultCompoundSyntaxTree("-", 
						new DefaultCompoundSyntaxTree("-", "x")));

		string = "- {x, y, z}";
		test(string, new DefaultCompoundSyntaxTree("-", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("kleene list", "x", "y", "z"))));

		string = "- x + y";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("-", "x"), "y"));

		string = "- (x + y)";
		test(string, new DefaultCompoundSyntaxTree("-", 
				new DefaultCompoundSyntaxTree("+", "x", "y")));

		string = "(- x) + y";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("-", "x"), "y"));
	}

	@Test
	public void testExponentiation() {
		String string;
		string = "x ^ y";
		test(string, new DefaultCompoundSyntaxTree("^", "x", "y"));

		string = "x^y";
		test(string, new DefaultCompoundSyntaxTree("^", "x", "y"));

		string = "w ^ x ^ y ^ z";
		test(string, new DefaultCompoundSyntaxTree("^", "w",
				new DefaultCompoundSyntaxTree("^",  "x",
						new DefaultCompoundSyntaxTree("^", "y", "z"))));

		string = "x ^ y + z";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("^", "x", "y"), "z"));

		string = "{x} ^ y";
		test(string, new DefaultCompoundSyntaxTree("^", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} ^ y";
		test(string, new DefaultCompoundSyntaxTree("^", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")),	"y"));

		string = "^(x)";
		test(string, new DefaultCompoundSyntaxTree("^", "x"));

		string = "^";
		test(string, DefaultSymbol.createSymbol("^"));

		string = "^()";
		test(string, new DefaultCompoundSyntaxTree("^"));

		string = "^(x, y)";
		test(string, new DefaultCompoundSyntaxTree("^", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("/", "x", "y"));

		string = "x/y";
		test(string, new DefaultCompoundSyntaxTree("/", "x", "y"));
		
		string = "w / x / y / z";
		test(string, new DefaultCompoundSyntaxTree("/", 
				new DefaultCompoundSyntaxTree("/",  
						new DefaultCompoundSyntaxTree("/", "w", "x"), "y"), "z"));

		string = "x / y + z";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("/", "x", "y"), "z"));

		string = "{x} / y";
		test(string, new DefaultCompoundSyntaxTree("/", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} / y";
		test(string, new DefaultCompoundSyntaxTree("/", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")),
				"y"));

		string = "/(x)";
		test(string, new DefaultCompoundSyntaxTree("/", "x"));

		string = "/";
		test(string, DefaultSymbol.createSymbol("/"));

		string = "/()";
		test(string, new DefaultCompoundSyntaxTree("/"));

		string = "/(x, y)";
		test(string, new DefaultCompoundSyntaxTree("/", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("*", "x", "y"));

		string = "x*y";
		test(string, new DefaultCompoundSyntaxTree("*", "x", "y"));

		string = "w * x * y * z";
		test(string, new DefaultCompoundSyntaxTree("*", 
				"w", "x", "y", "z"));

		string = "x * y + z";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("*", "x", "y"), "z"));

		string = "{x} * y";
		test(string, new DefaultCompoundSyntaxTree("*", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} * y";
		test(string, new DefaultCompoundSyntaxTree("*", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "*(x)";
		test(string, new DefaultCompoundSyntaxTree("*", "x"));

		string = "*";
		test(string, DefaultSymbol.createSymbol("*"));

		string = "*()";
		test(string, new DefaultCompoundSyntaxTree("*"));

		string = "*(x, y)";
		test(string, new DefaultCompoundSyntaxTree("*", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("-", "x", "y"));
		
		string = "x-y";
		test(string, new DefaultCompoundSyntaxTree("-", "x", "y"));
		
		string = "2-0";
		test(string, new DefaultCompoundSyntaxTree("-", "2", "0"));
		
		string = "w - x - y - z";
		test(string, new DefaultCompoundSyntaxTree("-", 
				new DefaultCompoundSyntaxTree("-",  
						new DefaultCompoundSyntaxTree("-", "w", "x"), "y"), "z"));

		string = "x - y + z";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("-", "x", "y"), "z"));

		string = "{x} - y";
		test(string, new DefaultCompoundSyntaxTree("-", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} - y";
		test(string, new DefaultCompoundSyntaxTree("-", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "-(x)";
		test(string, new DefaultCompoundSyntaxTree("-", "x"));

		string = "-";
		test(string, DefaultSymbol.createSymbol("-"));

		string = "-()";
		test(string, new DefaultCompoundSyntaxTree("-"));

		string = "-(x, y)";
		test(string, new DefaultCompoundSyntaxTree("-", 
				new DefaultCompoundSyntaxTree("( . )", 
						new DefaultCompoundSyntaxTree("kleene list", "x", "y"))));
	}

	@Test
	public void testPlus () {
		String string;
		string = "x + y";
		test(string, new DefaultCompoundSyntaxTree("+", "x", "y"));

		string = "x+y";
		test(string, new DefaultCompoundSyntaxTree("+", "x", "y"));

		string = "1+2";
		test(string, new DefaultCompoundSyntaxTree("+", "1", "2"));

		string = "1+-2";
		test(string, new DefaultCompoundSyntaxTree("+", "1", 
				new DefaultCompoundSyntaxTree("-", "2")));

		string = "w + x + y + z";
		test(string, new DefaultCompoundSyntaxTree("+", 
				"w", "x", "y", "z"));

		string = "{x} + y";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} + y";
		test(string, new DefaultCompoundSyntaxTree("+", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "+(x)";
		test(string, new DefaultCompoundSyntaxTree("+", "x"));

		string = "+";
		test(string, DefaultSymbol.createSymbol("+"));

		string = "+()";
		test(string, new DefaultCompoundSyntaxTree("+"));

		string = "+(x, y)";
		test(string, new DefaultCompoundSyntaxTree("+", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("intersection", "x", "y"));

		string = "w intersection x intersection y intersection z";
		test(string, new DefaultCompoundSyntaxTree("intersection", "w", "x", "y", "z"));

		string = "{x} intersection y";
		test(string, new DefaultCompoundSyntaxTree("intersection", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x intersection y} intersection y";
		test(string, new DefaultCompoundSyntaxTree("intersection", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("intersection", "x", "y")), "y"));

		string = "intersection(x)";
		test(string, new DefaultCompoundSyntaxTree("intersection", "x"));

		string = "intersection";
		test(string, DefaultSymbol.createSymbol("intersection"));
		
		string = "intersection * 8";
		test(string, new DefaultCompoundSyntaxTree("*", "intersection", "8"));

		string = "intersection()";
		test(string, new DefaultCompoundSyntaxTree("intersection"));

		string = "intersection(x, y)";
		test(string, new DefaultCompoundSyntaxTree("intersection", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("union", "x", "y"));

		string = "w union x union y union z";
		test(string, new DefaultCompoundSyntaxTree("union", "w", "x", "y", "z"));

		string = "x union y + z";
		test(string, new DefaultCompoundSyntaxTree("union", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} union y";
		test(string, new DefaultCompoundSyntaxTree("union", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} union y";
		test(string, new DefaultCompoundSyntaxTree("union", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "union(x)";
		test(string, new DefaultCompoundSyntaxTree("union", "x"));

		string = "union";
		test(string, DefaultSymbol.createSymbol("union"));

		string = "union()";
		test(string, new DefaultCompoundSyntaxTree("union"));

		string = "union(x, y)";
		test(string, new DefaultCompoundSyntaxTree("union", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("in", "x", "y"));
		
		string = "w in x in y in z";
		test(string, new DefaultCompoundSyntaxTree("in", 
				new DefaultCompoundSyntaxTree("in",  
						new DefaultCompoundSyntaxTree("in", "w", "x"), "y"), "z"));

		string = "x in y + z";
		test(string, new DefaultCompoundSyntaxTree("in", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} in y";
		test(string, new DefaultCompoundSyntaxTree("in", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} in y";
		test(string, new DefaultCompoundSyntaxTree("in", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "in(x)";
		test(string, new DefaultCompoundSyntaxTree("in", "x"));

		string = "in";
		test(string, DefaultSymbol.createSymbol("in"));

		string = "in()";
		test(string, new DefaultCompoundSyntaxTree("in"));

		string = "in(x, y)";
		test(string, new DefaultCompoundSyntaxTree("in", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("<=", "x", "y"));
		
		string = "x<=y";
		test(string, new DefaultCompoundSyntaxTree("<=", "x", "y"));
		
		string = "w <= x <= y <= z";
		test(string, new DefaultCompoundSyntaxTree("<=", 
				new DefaultCompoundSyntaxTree("<=",  
						new DefaultCompoundSyntaxTree("<=", "w", "x"), "y"), "z"));

		string = "x <= y + z";
		test(string, new DefaultCompoundSyntaxTree("<=", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} <= y";
		test(string, new DefaultCompoundSyntaxTree("<=", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} <= y";
		test(string, new DefaultCompoundSyntaxTree("<=", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "<=(x)";
		test(string, new DefaultCompoundSyntaxTree("<=", "x"));

		string = "<=";
		test(string, DefaultSymbol.createSymbol("<="));

		string = "<=()";
		test(string, new DefaultCompoundSyntaxTree("<="));

		string = "<=(x, y)";
		test(string, new DefaultCompoundSyntaxTree("<=", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("<", "x", "y"));
		
		string = "x<y";
		test(string, new DefaultCompoundSyntaxTree("<", "x", "y"));
		
		string = "w < x < y < z";
		test(string, new DefaultCompoundSyntaxTree("<", 
				new DefaultCompoundSyntaxTree("<",  
						new DefaultCompoundSyntaxTree("<", "w", "x"), "y"), "z"));

		string = "x < y + z";
		test(string, new DefaultCompoundSyntaxTree("<", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} < y";
		test(string, new DefaultCompoundSyntaxTree("<", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} < y";
		test(string, new DefaultCompoundSyntaxTree("<", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "'<'(x)";
		test(string, new DefaultCompoundSyntaxTree("<", "x"));

		string = "'<'";
		test(string, DefaultSymbol.createSymbol("<"));

		string = "'<'()";
		test(string, new DefaultCompoundSyntaxTree("<"));

		string = "'<'(x, y)";
		test(string, new DefaultCompoundSyntaxTree("<", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree(">=", "x", "y"));
		
		string = "x>=y";
		test(string, new DefaultCompoundSyntaxTree(">=", "x", "y"));
		
		string = "w >= x >= y >= z";
		test(string, new DefaultCompoundSyntaxTree(">=", 
				new DefaultCompoundSyntaxTree(">=",  
						new DefaultCompoundSyntaxTree(">=", "w", "x"), "y"), "z"));

		string = "x >= y + z";
		test(string, new DefaultCompoundSyntaxTree(">=", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} >= y";
		test(string, new DefaultCompoundSyntaxTree(">=", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} >= y";
		test(string, new DefaultCompoundSyntaxTree(">=", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")),
				"y"));

		string = ">=(x)";
		test(string, new DefaultCompoundSyntaxTree(">=", "x"));

		string = ">=";
		test(string, DefaultSymbol.createSymbol(">="));

		string = ">=()";
		test(string, new DefaultCompoundSyntaxTree(">="));

		string = ">=(x, y)";
		test(string, new DefaultCompoundSyntaxTree(">=", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree(">", "x", "y"));
		
		string = "x>y";
		test(string, new DefaultCompoundSyntaxTree(">", "x", "y"));
		
		string = "w > x > y > z";
		test(string, new DefaultCompoundSyntaxTree(">", 
				new DefaultCompoundSyntaxTree(">",  
						new DefaultCompoundSyntaxTree(">", "w", "x"), "y"), "z"));

		string = "x > y + z";
		test(string, new DefaultCompoundSyntaxTree(">", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} > y";
		test(string, new DefaultCompoundSyntaxTree(">", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} > y";
		test(string, new DefaultCompoundSyntaxTree(">", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "'>'(x)";
		test(string, new DefaultCompoundSyntaxTree(">", "x"));

		string = "'>'";
		test(string, DefaultSymbol.createSymbol(">"));

		string = "'>'()";
		test(string, new DefaultCompoundSyntaxTree(">"));

		string = "'>'(x, y)";
		test(string, new DefaultCompoundSyntaxTree(">", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("!=", "x", "y"));
		
		string = "x!=y";
		test(string, new DefaultCompoundSyntaxTree("!=", "x", "y"));
		
		string = "w != x != y != z";
		test(string, new DefaultCompoundSyntaxTree("!=", 
				new DefaultCompoundSyntaxTree("!=",  
						new DefaultCompoundSyntaxTree("!=", "w", "x"), "y"), "z"));

		string = "x != y + z";
		test(string, new DefaultCompoundSyntaxTree("!=", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} != y";
		test(string, new DefaultCompoundSyntaxTree("!=", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} != y";
		test(string, new DefaultCompoundSyntaxTree("!=", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "!=(x)";
		test(string, new DefaultCompoundSyntaxTree("!=", "x"));

		string = "!=";
		test(string, DefaultSymbol.createSymbol("!="));

		string = "!=()";
		test(string, new DefaultCompoundSyntaxTree("!="));

		string = "!=(x, y)";
		test(string, new DefaultCompoundSyntaxTree("!=", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("=", "x", "y"));

		string = "x=y";
		test(string, new DefaultCompoundSyntaxTree("=", "x", "y"));

		string = "w = x = y = z";
		test(string, new DefaultCompoundSyntaxTree("=", 
				"w", "x", "y", "z"));

		string = "x = y + z";
		test(string, new DefaultCompoundSyntaxTree("=", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} = y";
		test(string, new DefaultCompoundSyntaxTree("=", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} = y";
		test(string, new DefaultCompoundSyntaxTree("=", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "=(x)";
		test(string, new DefaultCompoundSyntaxTree("=", "x"));

		string = "=";
		test(string, DefaultSymbol.createSymbol("="));

		string = "=()";
		test(string, new DefaultCompoundSyntaxTree("="));

		string = "=(x, y)";
		test(string, new DefaultCompoundSyntaxTree("=", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("and", "x", new DefaultCompoundSyntaxTree("and", "y", "z")));
		
		string = "x and y";
		test(string, new DefaultCompoundSyntaxTree("and", "x", "y"));

		string = "w and x and y and z";
		test(string, new DefaultCompoundSyntaxTree("and", 
				"w", "x", "y", "z"));

		string = "x and y + z";
		test(string, new DefaultCompoundSyntaxTree("and", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "x in y and y";
		test(string, new DefaultCompoundSyntaxTree("and", 
				new DefaultCompoundSyntaxTree("in", "x", "y"), "y"));

		string = "{x} and y";
		test(string, new DefaultCompoundSyntaxTree("and", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} and y";
		test(string, new DefaultCompoundSyntaxTree("and", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "and(x)";
		test(string, new DefaultCompoundSyntaxTree("and", "x"));

		string = "and";
		test(string, DefaultSymbol.createSymbol("and"));

		string = "and()";
		test(string, new DefaultCompoundSyntaxTree("and"));

		string = "and(x, y)";
		test(string, new DefaultCompoundSyntaxTree("and", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("or", "x", new DefaultCompoundSyntaxTree("or", "y", "z")));
		
		string = "x or y";
		test(string, new DefaultCompoundSyntaxTree("or", "x", "y"));

		string = "w or x or y or z";
		test(string, new DefaultCompoundSyntaxTree("or", 
				"w", "x", "y", "z"));

		string = "x or y + z";
		test(string, new DefaultCompoundSyntaxTree("or", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "x in y or y";
		test(string, new DefaultCompoundSyntaxTree("or", 
				new DefaultCompoundSyntaxTree("in", "x", "y"), "y"));

		string = "{x} or y";
		test(string, new DefaultCompoundSyntaxTree("or", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} or y";
		test(string, new DefaultCompoundSyntaxTree("or", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "or(x)";
		test(string, new DefaultCompoundSyntaxTree("or", "x"));

		string = "or";
		test(string, DefaultSymbol.createSymbol("or"));

		string = "or()";
		test(string, new DefaultCompoundSyntaxTree("or"));

		string = "or(x, y)";
		test(string, new DefaultCompoundSyntaxTree("or", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("<=>", "x", "y"));
		
		string = "x<=>y";
		test(string, new DefaultCompoundSyntaxTree("<=>", "x", "y"));
		
		string = "w <=> x <=> y <=> z";
		test(string, new DefaultCompoundSyntaxTree("<=>", "w", 
				new DefaultCompoundSyntaxTree("<=>",  "x", 
						new DefaultCompoundSyntaxTree("<=>", "y", "z"))));

		string = "x <=> y + z";
		test(string, new DefaultCompoundSyntaxTree("<=>", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} <=> y";
		test(string, new DefaultCompoundSyntaxTree("<=>", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} <=> y";
		test(string, new DefaultCompoundSyntaxTree("<=>", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")), "y"));

		string = "<=>(x)";
		test(string, new DefaultCompoundSyntaxTree("<=>", "x"));

		string = "<=>";
		test(string, DefaultSymbol.createSymbol("<=>"));

		string = "<=>()";
		test(string, new DefaultCompoundSyntaxTree("<=>"));

		string = "<=>(x, y)";
		test(string, new DefaultCompoundSyntaxTree("<=>", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("=>", "x", "y"));
		
		string = "x=>y";
		test(string, new DefaultCompoundSyntaxTree("=>", "x", "y"));
		
		string = "w => x => y => z";
		test(string, new DefaultCompoundSyntaxTree("=>", "w", 
				new DefaultCompoundSyntaxTree("=>", "x", 
						new DefaultCompoundSyntaxTree("=>", "y", "z"))));

		string = "x => y + z";
		test(string, new DefaultCompoundSyntaxTree("=>", "x", 
				new DefaultCompoundSyntaxTree("+", "y", "z")));

		string = "{x} => y";
		test(string, new DefaultCompoundSyntaxTree("=>", 
				new DefaultCompoundSyntaxTree("{ . }", "x"), "y"));

		string = "{x + y} => y";
		test(string, new DefaultCompoundSyntaxTree("=>", 
				new DefaultCompoundSyntaxTree("{ . }", 
						new DefaultCompoundSyntaxTree("+", "x", "y")),
				"y"));

		string = "=>(x)";
		test(string, new DefaultCompoundSyntaxTree("=>", "x"));

		string = "=>";
		test(string, DefaultSymbol.createSymbol("=>"));

		string = "=>()";
		test(string, new DefaultCompoundSyntaxTree("=>"));

		string = "=>(x, y)";
		test(string, new DefaultCompoundSyntaxTree("=>", "x", "y"));

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
		test(string, new DefaultCompoundSyntaxTree("there exists . : .", "a", "b"));
		
		string = "there exists a : there exists b : c";
		test(string, new DefaultCompoundSyntaxTree("there exists . : .", "a", 
				new DefaultCompoundSyntaxTree("there exists . : .", "b", "c")));
		
		string = "'there exists . : .'(a, b)";
		test(string, new DefaultCompoundSyntaxTree("there exists . : .", "a", "b"));

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
		test(string, new DefaultCompoundSyntaxTree("for all . : .", "x", "y"));

		string = "for all x = 5 : a";
		test(string, new DefaultCompoundSyntaxTree("for all . : .", 
				new DefaultCompoundSyntaxTree("=", "x", "5"), "a"));

		string = "for all x : for all y : for all z : true";
		test(string, new DefaultCompoundSyntaxTree("for all . : .", "x", 
				new DefaultCompoundSyntaxTree("for all . : .", "y", 
						new DefaultCompoundSyntaxTree("for all . : .", "z", "true"))));

		string = "for all Y : (for all X : ((X != Y) => (there exists W : (there exists Z : ((Z != a) => (Alpha = Beta))))))";
		test(string, new DefaultCompoundSyntaxTree("for all . : .", "Y", 
				new DefaultCompoundSyntaxTree("for all . : .", "X", 
						new DefaultCompoundSyntaxTree("=>", 
								new DefaultCompoundSyntaxTree("!=", "X", "Y"), 
								new DefaultCompoundSyntaxTree("there exists . : .", "W", 
										new DefaultCompoundSyntaxTree("there exists . : .", "Z", 
												new DefaultCompoundSyntaxTree("=>", 
														new DefaultCompoundSyntaxTree("!=", "Z", "a"), 
														new DefaultCompoundSyntaxTree("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => there exists W : there exists Z : (Z != a => Alpha = Beta)";
		test(string, new DefaultCompoundSyntaxTree("for all . : .", "Y", 
				new DefaultCompoundSyntaxTree("for all . : .", "X", 
						new DefaultCompoundSyntaxTree("=>", 
								new DefaultCompoundSyntaxTree("!=", "X", "Y"), 
								new DefaultCompoundSyntaxTree("there exists . : .", "W", 
										new DefaultCompoundSyntaxTree("there exists . : .", "Z", 
												new DefaultCompoundSyntaxTree("=>", 
														new DefaultCompoundSyntaxTree("!=", "Z", "a"), 
														new DefaultCompoundSyntaxTree("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => (there exists W : there exists Z : Z != a => Alpha = Beta)";
		test(string, new DefaultCompoundSyntaxTree("for all . : .", "Y", 
				new DefaultCompoundSyntaxTree("for all . : .", "X", 
						new DefaultCompoundSyntaxTree("=>", 
								new DefaultCompoundSyntaxTree("!=", "X", "Y"), 
								new DefaultCompoundSyntaxTree("there exists . : .", "W", 
										new DefaultCompoundSyntaxTree("there exists . : .", "Z", 
												new DefaultCompoundSyntaxTree("=>", 
														new DefaultCompoundSyntaxTree("!=", "Z", "a"), 
														new DefaultCompoundSyntaxTree("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => there exists W : there exists Z : Z != a => Alpha = Beta";
		test(string, new DefaultCompoundSyntaxTree("for all . : .", "Y", 
				new DefaultCompoundSyntaxTree("for all . : .", "X", 
						new DefaultCompoundSyntaxTree("=>", 
								new DefaultCompoundSyntaxTree("!=", "X", "Y"), 
								new DefaultCompoundSyntaxTree("there exists . : .", "W", 
										new DefaultCompoundSyntaxTree("there exists . : .", "Z", 
												new DefaultCompoundSyntaxTree("=>", 
														new DefaultCompoundSyntaxTree("!=", "Z", "a"), 
														new DefaultCompoundSyntaxTree("=", "Alpha", "Beta"))))))));
	}

	@Test
	public void testIfThenElse () {
		String string;
		string = "if a then b else c";
		test(string, new DefaultCompoundSyntaxTree("if . then . else .", 
				"a", "b", "c"));

		string = "'if . then . else .'(a, b, c)";
		test(string, new DefaultCompoundSyntaxTree("if . then . else .", "a", "b", "c"));

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
		test(string, new DefaultCompoundSyntaxTree("lambda . : .", "x", "a"));

		string = "lambda x, y, z : a";
		test(string, new DefaultCompoundSyntaxTree("lambda . : .", 
				new DefaultCompoundSyntaxTree("kleene list", "x", "y", "z"), "a"));

		string = "lambda x, y, z : lambda a : b";
		test(string, new DefaultCompoundSyntaxTree("lambda . : .", 
				new DefaultCompoundSyntaxTree("kleene list", "x", "y", "z"), 
				new DefaultCompoundSyntaxTree("lambda . : .", "a", "b")));

		// Testing illegal strings
		string = "lambda a :";
		testFail(string);
	}

	@Test
	public void testMessage () {
		String string;
		string = "message to a from b";
		test(string, new DefaultCompoundSyntaxTree("message to . from .", "a", "b"));

		string = "'message to . from .'(a, b)";
		test(string, new DefaultCompoundSyntaxTree("message to . from .", "a", "b"));

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
		test(string, new DefaultCompoundSyntaxTree("previous message to . from .", "a", "b"));

		string = "'previous message to . from .'(a, b)";
		test(string, new DefaultCompoundSyntaxTree("previous message to . from .", "a", "b"));

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
		test(string, new DefaultCompoundSyntaxTree("-",
				new DefaultCompoundSyntaxTree("^", "a", "a"), "a"));

		string = "a*a+a";
		test(string, new DefaultCompoundSyntaxTree("+",
				new DefaultCompoundSyntaxTree("*", "a", "a"), "a"));

		string = "a+a*a";
		test(string, new DefaultCompoundSyntaxTree("+", "a",
				new DefaultCompoundSyntaxTree("*", "a", "a")));

		string = "-a*a+a";
		test(string, new DefaultCompoundSyntaxTree("+",
				new DefaultCompoundSyntaxTree("*",
						new DefaultCompoundSyntaxTree("-", "a"), "a"), "a"));

		string = "-a*-a-a";
		test(string, new DefaultCompoundSyntaxTree("-",
				new DefaultCompoundSyntaxTree("*",
						new DefaultCompoundSyntaxTree("-", "a"),
						new DefaultCompoundSyntaxTree("-", "a")), "a"));

		string = "-a*-a^a-a";
		test(string,
				new DefaultCompoundSyntaxTree("-",
						new DefaultCompoundSyntaxTree("*",
								new DefaultCompoundSyntaxTree("-", "a"),
								new DefaultCompoundSyntaxTree("^",
										new DefaultCompoundSyntaxTree("-", "a"), "a")), "a"));

		string = "-a*-a^-a";
		test(string, 
				new DefaultCompoundSyntaxTree("*",
						new DefaultCompoundSyntaxTree("-", "a"),
						new DefaultCompoundSyntaxTree("^",
								new DefaultCompoundSyntaxTree("-", "a"),
								new DefaultCompoundSyntaxTree("-", "a"))));
	}

	@Test
	public void testFunctionsAndSequences() {
		String string;
		string = "x+y";
		test(string, new DefaultCompoundSyntaxTree("+","x","y"));

		string = "f()";
		test(string, new DefaultCompoundSyntaxTree("f"));

		string = "f(10)";
		test(string, new DefaultCompoundSyntaxTree("f", 10));

		string = "f(10 + a)";
		test(string, 								
				new DefaultCompoundSyntaxTree("f",
						new DefaultCompoundSyntaxTree(
								"+", 10, "a")));

		string = "f(10+a) - f(a, b)";
		test(string,
				new DefaultCompoundSyntaxTree("-",
						new DefaultCompoundSyntaxTree("f",
								new DefaultCompoundSyntaxTree("+", 10, "a")),
						new DefaultCompoundSyntaxTree("f", "a", "b")));
		
		string = "f(10)-f()";
		test(string, new DefaultCompoundSyntaxTree("-",
						new DefaultCompoundSyntaxTree("f", 10),
						new DefaultCompoundSyntaxTree("f")));

		string = "10 + 10";
		test(string, new DefaultCompoundSyntaxTree("+", 10, 10));

		string = "10 + x*20";
		test(string, new DefaultCompoundSyntaxTree("+", 10, 
				new DefaultCompoundSyntaxTree("*", "x",20)));

		string = "f(10, 20, 30, f (x), f, (x))";
		test(string,
				new DefaultCompoundSyntaxTree("f", 10, 20, 30, 
						new DefaultCompoundSyntaxTree("f", "x"), "f", "x"));
	}
	
	
	@Test
	public void testGrinder () {
		String expression;
		expression = "if X = 1 then 0.0003 + 0.000000000001 else 0.150004 + 0.1 + 0.776699029126213691398561";
		test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
				new DefaultCompoundSyntaxTree("=", "X", "1"), 
				new DefaultCompoundSyntaxTree("+", "0.0003", "0.000000000001"), 
				new DefaultCompoundSyntaxTree("+", "0.150004", "0.1", "0.776699029126213691398561")));


			expression = "3";
			test(expression, DefaultSymbol.createSymbol(3));

			expression = "x + y";
			test(expression, new DefaultCompoundSyntaxTree("+", "x", "y"));

			expression = "1 + 2";
			test(expression, new DefaultCompoundSyntaxTree("+", "1", "2"));

			expression = "x + 2";
			test(expression, new DefaultCompoundSyntaxTree("+", "x", "2"));

			expression = "+(x, 2, y, 6)";
			test(expression, new DefaultCompoundSyntaxTree("+", "x", "2", "y", "6"));

			expression = "+(x, +(1, y), 11)";
			test(expression, new DefaultCompoundSyntaxTree("+", "x", 
					new DefaultCompoundSyntaxTree("+", 1, "y"), "11"));

			expression = "+(x, 2, 1 + 2, 1 + y, 6)";
			test(expression, new DefaultCompoundSyntaxTree("+", "x", 2, 
					new DefaultCompoundSyntaxTree("+", 1, 2), 
					new DefaultCompoundSyntaxTree("+", 1, "y"), "6"));

			expression = "+(x, 2, 3)";
			test(expression, new DefaultCompoundSyntaxTree("+", "x", 2, 3));

			expression = "+(x)";
			test(expression, new DefaultCompoundSyntaxTree("+", "x"));

			expression = "+";
			test(expression, DefaultSymbol.createSymbol("+"));

			expression = "1";
			test(expression, DefaultSymbol.createSymbol(1));

			expression = "x";
			test(expression, DefaultSymbol.createSymbol("x"));

			expression = "+()";
			test(expression, new DefaultCompoundSyntaxTree("+"));

			expression = "3 - 1";
			test(expression, new DefaultCompoundSyntaxTree("-", 3, 1));

			expression = "1 - 3";
			test(expression, new DefaultCompoundSyntaxTree("-", 1, 3));

			expression = "X - Y";
			test(expression, new DefaultCompoundSyntaxTree("-", "X", "Y"));

			expression = "X - 0";
			test(expression, new DefaultCompoundSyntaxTree("-", "X", 0));

			expression = "0 - X";
			test(expression, new DefaultCompoundSyntaxTree("-", 0, "X"));

			expression = "-1";
			test(expression, new DefaultCompoundSyntaxTree("-", "1"));

			expression = "-x";
			test(expression, new DefaultCompoundSyntaxTree("-", "x"));

			expression = "3";
			test(expression, DefaultSymbol.createSymbol(3));

			expression = "x * y";
			test(expression, new DefaultCompoundSyntaxTree("*", "x", "y"));

			expression = "2 * 2";
			test(expression, new DefaultCompoundSyntaxTree("*", 2, 2));

			expression = "x * 2";
			test(expression, new DefaultCompoundSyntaxTree("*", "x", 2));

			expression = "*(x, 2, y, 6)";
			test(expression, new DefaultCompoundSyntaxTree("*", "x", 2, "y", 6));

			expression = "*(x, 0, y, 6)";
			test(expression, new DefaultCompoundSyntaxTree("*", "x", 0, "y", 6));

			expression = "*(x, 2, 1 * 2, 1 * y, 6)";
			test(expression, new DefaultCompoundSyntaxTree("*", "x", 2, 
							new DefaultCompoundSyntaxTree("*", 1, 2),
							new DefaultCompoundSyntaxTree("*", 1, "y"), 6));

			expression = "*(x, 2, 3)";
			test(expression, new DefaultCompoundSyntaxTree("*", "x", 2, 3));

			expression = "*(x)";
			test(expression, new DefaultCompoundSyntaxTree("*", "x"));

			expression = "*";
			test(expression, DefaultSymbol.createSymbol("*"));

			expression = "*()";
			test(expression, new DefaultCompoundSyntaxTree("*"));

			expression = "3^2";
			test(expression, new DefaultCompoundSyntaxTree("^", 3, 2));

			expression = "x^1";
			test(expression, new DefaultCompoundSyntaxTree("^", "x", 1));

			expression = "x^0";
			test(expression, new DefaultCompoundSyntaxTree("^", "x", 0));

			expression = "x^0.0";
			test(expression, new DefaultCompoundSyntaxTree("^", "x", "0.0"));

			expression = "1^n";
			test(expression, new DefaultCompoundSyntaxTree("^", 1, "n"));

			expression = "2^n";
			test(expression, new DefaultCompoundSyntaxTree("^", 2, "n"));

			expression = "4/2";
			test(expression, new DefaultCompoundSyntaxTree("/", 4, 2));

			expression = "4.0/2.0";
			test(expression, new DefaultCompoundSyntaxTree("/", "4.0", "2.0"));

			expression = "4.0/3.0";
			test(expression, new DefaultCompoundSyntaxTree("/", "4.0", "3.0"));

			expression = "a/b";
			test(expression, new DefaultCompoundSyntaxTree("/", "a", "b"));

			expression = "4/0";
			test(expression, new DefaultCompoundSyntaxTree("/", 4, 0));

			expression = "4/2";
			test(expression, new DefaultCompoundSyntaxTree("/", 4, 2));

			expression = "(4*2)/(2*3)";
			test(expression, new DefaultCompoundSyntaxTree("/", 
					new DefaultCompoundSyntaxTree("*", 4, 2),
					new DefaultCompoundSyntaxTree("*", 2, 3)));

			expression = "(4*2)/2";
			test(expression, new DefaultCompoundSyntaxTree("/", 
					new DefaultCompoundSyntaxTree("*", 4, 2), 2));

			expression = "2/2";
			test(expression, new DefaultCompoundSyntaxTree("/", 2, 2));

			expression = "3 > 1";
			test(expression, new DefaultCompoundSyntaxTree(">", 3, 1));

			expression = "3 > 3";
			test(expression, new DefaultCompoundSyntaxTree(">", 3, 3));

			expression = "1 > 3";
			test(expression, new DefaultCompoundSyntaxTree(">", 1, 3));

			expression = "1 > y";
			test(expression, new DefaultCompoundSyntaxTree(">", 1, "y"));

			expression = "1 > false";
			test(expression, new DefaultCompoundSyntaxTree(">", 1, "false"));

			expression = "3 >= 1";
			test(expression, new DefaultCompoundSyntaxTree(">=", 3, 1));

			expression = "3 >= 3";
			test(expression, new DefaultCompoundSyntaxTree(">=", 3, 3));

			expression = "1 >= 3";
			test(expression, new DefaultCompoundSyntaxTree(">=", 1, 3));

			expression = "1 >= y";
			test(expression, new DefaultCompoundSyntaxTree(">=", 1, "y"));

			expression = "1 >= false";
			test(expression, new DefaultCompoundSyntaxTree(">=", 1, "false"));

			expression = "not (3 > 1)";
			test(expression, new DefaultCompoundSyntaxTree("not",
					new DefaultCompoundSyntaxTree(">", 3, 1)));

			expression = "not(3 > 3)";
			test(expression, new DefaultCompoundSyntaxTree("not",
					new DefaultCompoundSyntaxTree(">", 3, 3)));

			expression = "true and x";
			test(expression, new DefaultCompoundSyntaxTree("and", "true", "x"));

			expression = "true";
			test(expression, DefaultSymbol.createSymbol("true"));

			expression = "x and y";
			test(expression, new DefaultCompoundSyntaxTree("and", "x", "y"));

			expression = "true and false";
			test(expression, new DefaultCompoundSyntaxTree("and", "true", "false"));

			expression = "x and false";
			test(expression, new DefaultCompoundSyntaxTree("and", "x", "false"));

			expression = "x and false and y and true";
			test(expression, new DefaultCompoundSyntaxTree("and", 
					"x", "false", "y", "true"));

			expression = "and(x, false, false and true, false and y, false)";
			test(expression, new DefaultCompoundSyntaxTree("and", "x", "false", 
					new DefaultCompoundSyntaxTree("and", "false", "true"),
					new DefaultCompoundSyntaxTree("and", "false", "y"),
					"false"));

			expression = "and(x, true, false)";
			test(expression, new DefaultCompoundSyntaxTree("and", "x", "true", "false")); 


			expression = "and(x)";
			test(expression, new DefaultCompoundSyntaxTree("and", "x"));

			expression = "and";
			test(expression, DefaultSymbol.createSymbol("and"));

			expression = "and()";
			test(expression, new DefaultCompoundSyntaxTree("and")); 

			expression = "false or x";
			test(expression, new DefaultCompoundSyntaxTree("or", "false", "x"));

			expression = "x or false";
			test(expression, new DefaultCompoundSyntaxTree("or", "x", "false"));

			expression = "true";
			test(expression, DefaultSymbol.createSymbol(true));

			expression = "x or y";
			test(expression, new DefaultCompoundSyntaxTree("or", "x", "y"));

			expression = "true or false";
			test(expression, new DefaultCompoundSyntaxTree("or", "true", "false"));

			expression = "x or true";
			test(expression, new DefaultCompoundSyntaxTree("or", "x", "true"));

			expression = "x or false or y or true";
			test(expression, new DefaultCompoundSyntaxTree("or", 
					"x", "false", "y", "true"));

			expression = "or()";
			test(expression, new DefaultCompoundSyntaxTree("or")); 

			expression = "x or y or x or y or z";
			test(expression, new DefaultCompoundSyntaxTree("or", 
					"x", "y", "x", "y", "z"));

			expression = "x or x";
			test(expression, new DefaultCompoundSyntaxTree("or", "x", "x"));

			expression = "not true";
			test(expression, new DefaultCompoundSyntaxTree("not", "true"));

			expression = "not false";
			test(expression, new DefaultCompoundSyntaxTree("not", "false"));

			expression = "not x";
			test(expression, new DefaultCompoundSyntaxTree("not", "x"));

			expression = "not x and y and x";
			test(expression, new DefaultCompoundSyntaxTree("and", 
					new DefaultCompoundSyntaxTree("not", "x"), "y", "x"));

			expression = "not not x";
			test(expression, new DefaultCompoundSyntaxTree("not", 
					new DefaultCompoundSyntaxTree("not", "x")));

			expression = "'index of . in .'(b, f(a,b))";
			test(expression, new DefaultCompoundSyntaxTree("index of . in .", "b", 
					new DefaultCompoundSyntaxTree("f", "a", "b")));

			expression = "'index of . in .'(c, f(a,b))";
			test(expression, new DefaultCompoundSyntaxTree("index of . in .", "c", 
					new DefaultCompoundSyntaxTree("f", "a", "b")));

			expression = "'index of . in .'(c, f(a,b)) > 0";
			test(expression, new DefaultCompoundSyntaxTree(">", 
					new DefaultCompoundSyntaxTree("index of . in .", "c", 
							new DefaultCompoundSyntaxTree("f", "a", "b")), "0"));

			expression = "x + 2";
			test(expression, new DefaultCompoundSyntaxTree("+", "x", "2"));

			expression = "f(g(h(x)))";
			test(expression, new DefaultCompoundSyntaxTree("f",
					new DefaultCompoundSyntaxTree("g",
							new DefaultCompoundSyntaxTree("h", "x"))));

			expression = "{(on x) x}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }",
					new DefaultCompoundSyntaxTree("( on . )", "x"), "x", null));

			expression = "x + {(on x) f(x)}";
			test(expression, new DefaultCompoundSyntaxTree("+", "x",
					new DefaultCompoundSyntaxTree("{ . . . }",
							new DefaultCompoundSyntaxTree("( on . )", "x"),
							new DefaultCompoundSyntaxTree("f", "x"), null)));

			expression = "2 + {(on x) f(x)}";
			test(expression, new DefaultCompoundSyntaxTree("+", 2,
					new DefaultCompoundSyntaxTree("{ . . . }",
							new DefaultCompoundSyntaxTree("( on . )", "x"),
							new DefaultCompoundSyntaxTree("f", "x"), null)));

			expression = "x + {(on x in group(x)) f(x)}";
			test(expression, new DefaultCompoundSyntaxTree("+", "x",
					new DefaultCompoundSyntaxTree("{ . . . }",
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("in", "x",
											new DefaultCompoundSyntaxTree("group", "x"))),
							new DefaultCompoundSyntaxTree("f", "x"), null)));

			expression = "2 + {(on x in group(2)) f(x)}";
			test(expression, new DefaultCompoundSyntaxTree("+", 2,
					new DefaultCompoundSyntaxTree("{ . . . }",
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("in", "x",
											new DefaultCompoundSyntaxTree("group", 2))),
							new DefaultCompoundSyntaxTree("f", "x"), null)));

			expression = "x + {(on y) f(x)}";
			test(expression, new DefaultCompoundSyntaxTree("+", "x",
					new DefaultCompoundSyntaxTree("{ . . . }",
							new DefaultCompoundSyntaxTree("( on . )", "y"),
							new DefaultCompoundSyntaxTree("f", "x"), null)));

			expression = "2 + {(on y) f(2)}";
			test(expression, new DefaultCompoundSyntaxTree("+", 2,
					new DefaultCompoundSyntaxTree("{ . . . }",
							new DefaultCompoundSyntaxTree("( on . )", "y"),
							new DefaultCompoundSyntaxTree("f", 2), null)));


			expression = "f(x)";
			test(expression, new DefaultCompoundSyntaxTree("f", "x"));

			expression = "f(f(x) + g(x))";
			test(expression, new DefaultCompoundSyntaxTree("f", 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("f", "x"),
							new DefaultCompoundSyntaxTree("g", "x"))));


			expression = "(1 + x + y)*(x + 3)";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("+", "1", "x", "y"),
							new DefaultCompoundSyntaxTree("+", "x", 3)));

			expression = "1*x + 1*3 + x*x + x*3 + y*x + y*3";
			test(expression, new DefaultCompoundSyntaxTree("+", 
					new DefaultCompoundSyntaxTree("*", 1, "x"),
					new DefaultCompoundSyntaxTree("*", 1, 3),
					new DefaultCompoundSyntaxTree("*", "x", "x"),
					new DefaultCompoundSyntaxTree("*",	"x", 3),
					new DefaultCompoundSyntaxTree("*", "y", "x"),
					new DefaultCompoundSyntaxTree("*", "y", 3)));

			// Testing case where one one of the operator applications is empty.
			// We use an evaluator with the distributive law alone so +() does not get evaluated to 0.
			expression = "+()*(x + 3)";
			test(expression, new DefaultCompoundSyntaxTree("*",
					new DefaultCompoundSyntaxTree("+"),
							new DefaultCompoundSyntaxTree("+", "x", 3)));

			expression = "(1 * x * y)+(x * 3)";
			test(expression, new DefaultCompoundSyntaxTree("+", 
					new DefaultCompoundSyntaxTree("*", "1", "x", "y"),
							new DefaultCompoundSyntaxTree("*", "x", 3)));

			expression = "not x";
			test(expression, new DefaultCompoundSyntaxTree("not", "x"));

			expression = "not(x and y)";
			test(expression, new DefaultCompoundSyntaxTree("not", 
					new DefaultCompoundSyntaxTree("and", "x", "y")));

			expression = "not x or not y";
			test(expression, new DefaultCompoundSyntaxTree("or", 
					new DefaultCompoundSyntaxTree("not", "x"), 
					new DefaultCompoundSyntaxTree("not", "y")));

			expression = "not(x or y)";
			test(expression, new DefaultCompoundSyntaxTree("not", 
					new DefaultCompoundSyntaxTree("or", "x", "y")));

			expression = "not x and not y";
			test(expression, new DefaultCompoundSyntaxTree("and", 
					new DefaultCompoundSyntaxTree("not", "x"), 
					new DefaultCompoundSyntaxTree("not", "y")));

			expression = "not(x or y or z)";
			test(expression, new DefaultCompoundSyntaxTree("not", 
					new DefaultCompoundSyntaxTree("or", "x", "y", "z")));

			expression = "not x and not y and not z";
			test(expression, new DefaultCompoundSyntaxTree("and", 
					new DefaultCompoundSyntaxTree("not", "x"), 
					new DefaultCompoundSyntaxTree("not", "y"),
					new DefaultCompoundSyntaxTree("not", "z")));

			expression = "not(x or y or (z and w))";
			test(expression, new DefaultCompoundSyntaxTree("not", 
					new DefaultCompoundSyntaxTree("or", "x", "y", 
							new DefaultCompoundSyntaxTree("and", "z", "w"))));

			expression = "not x and not y and (not z or not w)";
			test(expression, new DefaultCompoundSyntaxTree("and", 
					new DefaultCompoundSyntaxTree("not", "x"), 
					new DefaultCompoundSyntaxTree("not", "y"),
					new DefaultCompoundSyntaxTree("or",
							new DefaultCompoundSyntaxTree("not", "z"),
							new DefaultCompoundSyntaxTree("not", "w"))));

			expression = "'scoped variables'({(on X) p(X) | X != a})";
			test(expression, new DefaultCompoundSyntaxTree("scoped variables",
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"),
							new DefaultCompoundSyntaxTree("p", "X"),
							new DefaultCompoundSyntaxTree("|",
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));

			expression = "list(X)";
			test(expression, new DefaultCompoundSyntaxTree("list", "X"));

			expression = "'scoped variables'({(on X,Y) p(X,Y) | X != a})";
			test(expression, new DefaultCompoundSyntaxTree("scoped variables",
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", "X", "Y")),
							new DefaultCompoundSyntaxTree("p", "X", "Y"),
							new DefaultCompoundSyntaxTree("|",
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));

			expression = "list(X, Y)";
			test(expression, new DefaultCompoundSyntaxTree("list", "X", "Y"));

			expression = "'scoped variables'(f(X,Y))";
			test(expression, new DefaultCompoundSyntaxTree("scoped variables",
					new DefaultCompoundSyntaxTree("f", "X", "Y")));

			expression = "list()";
			test(expression, new DefaultCompoundSyntaxTree("list"));


			expression = "aConstantSymbol";
			test(expression, DefaultSymbol.createSymbol("aConstantSymbol"));

			expression = "if A = B then aAndBEqual else aAndBNotEqual";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .",
					new DefaultCompoundSyntaxTree("=", "A", "B"),
					"aAndBEqual", "aAndBNotEqual"));

			expression = "{(on X) X | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }",
					new DefaultCompoundSyntaxTree("( on . )", "X"), "X", 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));



			expression = "if true then 1 else 2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					"true", 1, 2));

			expression = "if false then 1 else 2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					"false", 1, 2));

			expression = "if X then 1 else 2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					"X", 1, 2));


			expression = "f(a, b, c, if Y = 2 then X else X + 1, d, e, f)";
			test(expression, new DefaultCompoundSyntaxTree("f", "a", "b", "c",
							new DefaultCompoundSyntaxTree("if . then . else .",
									new DefaultCompoundSyntaxTree("=", "Y", 2), "X",
									new DefaultCompoundSyntaxTree("+", "X", 1)), 
							"d", "e", "f"));
			expression = "if Y = 2 then f(a, b, c, X, d, e, f) else f(a, b, c, X + 1, d, e, f)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .",
					new DefaultCompoundSyntaxTree("=", "Y", "2"),
					new DefaultCompoundSyntaxTree("f", "a", "b", "c", "X", "d", "e", "f"),
					new DefaultCompoundSyntaxTree("f", "a", "b", "c", 
							new DefaultCompoundSyntaxTree("+", "X", 1), "d", "e", "f")));

			expression = "{(on X) if Y = 2 then X else X + 1 | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"),
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("=", "Y", 2), "X", 
							new DefaultCompoundSyntaxTree("+", "X", 1)),
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "if Y = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "Y", 2),
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), "X", 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))),
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("+", "X", 1),
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));

			expression = "{(on X) if X = 2 then X else X + 1 | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"),
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("=", "X", 2), "X", 
							new DefaultCompoundSyntaxTree("+", "X", 1)),
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) if p(Y) = 2 then X else X + 1 | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"),
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("=", 
									new DefaultCompoundSyntaxTree("p", "Y"), 2), "X", 
							new DefaultCompoundSyntaxTree("+", "X", 1)),
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "if p(Y) = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", 
							new DefaultCompoundSyntaxTree("p", "Y"), 2),
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), "X", 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))),
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("+", "X", 1),
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));


			expression = "{(on X) if p(X) = 2 then X else X + 1 | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"),
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("=", 
									new DefaultCompoundSyntaxTree("p", "X"), 2), "X", 
							new DefaultCompoundSyntaxTree("+", "X", 1)),
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X in (if Y = a then Set1 else Set2)) p(X) | X != a}"; // lack of ()'s around if then else makes parse fail, not sure why. Entered in bug database.
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("if . then . else .", 
											new DefaultCompoundSyntaxTree("=", "Y", "a"),
											"Set1", "Set2"))),
					new DefaultCompoundSyntaxTree("p", "X"),
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			// lack of ()'s around if then else makes parse fail, due to precedence for "in".
//			expression = "{(on X in if Y = a then Set1 else Set2) p(X) | X != a}";
//			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
//					new DefaultCompoundSyntaxTree("( on . )", 
//							new DefaultCompoundSyntaxTree("in", "X", 
//									new DefaultCompoundSyntaxTree("if . then . else .", 
//											new DefaultCompoundSyntaxTree("=", "Y", "a"),
//											"Set1", "Set2"))),
//					new DefaultCompoundSyntaxTree("p", "X"),
//					new DefaultCompoundSyntaxTree("|", 
//							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "if Y = a then {(on X in Set1) p(X) | X != a} else {(on X in Set2) p(X) | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .",
					new DefaultCompoundSyntaxTree("=", "Y", "a"),
					new DefaultCompoundSyntaxTree("{ . . . }",
							new DefaultCompoundSyntaxTree("( on . )",
									new DefaultCompoundSyntaxTree("in", "X", "Set1")),
							new DefaultCompoundSyntaxTree("p", "X"),
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))),
					new DefaultCompoundSyntaxTree("{ . . . }",
							new DefaultCompoundSyntaxTree("( on . )",
									new DefaultCompoundSyntaxTree("in", "X", "Set2")),
							new DefaultCompoundSyntaxTree("p", "X"),
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));

			expression = "{(on Y, X in (if Y = a then Set1 else Set2)) p(X) | X != a}"; // lack of ()'s around if then else makes parse fail, not sure why. Entered in bug database.
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "Y",
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("if . then . else .", 
													new DefaultCompoundSyntaxTree("=", "Y", "a"),
													"Set1", "Set2")))),
					new DefaultCompoundSyntaxTree("p", "X"),
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "if X = a then X != a else X = Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .",
					new DefaultCompoundSyntaxTree("=", "X", "a"),
					new DefaultCompoundSyntaxTree("!=", "X", "a"),
					new DefaultCompoundSyntaxTree("=", "X", "Y")));

			expression = "if X = a then false else X = Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .",
					new DefaultCompoundSyntaxTree("=", "X", "a"),
					"false", new DefaultCompoundSyntaxTree("=", "X", "Y")));

			expression = "if X = a then f(X != a, 1, 2) else g(X = Y, a, b)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), "1", "2"), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("=", "X", "Y"), "a", "b")));

			expression = "if X = a then f(false, 1, 2) else g(X = Y, a, b)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("f", "false", "1", "2"), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("=", "X", "Y"), "a", "b")));

			expression = "if X != a or Y != b then X != a else false";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), "false"));

			expression = "if X != a or Y != b then X != a or Y != b or Z != c else false";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "Y", "b"), 
							new DefaultCompoundSyntaxTree("!=", "Z", "c")), "false"));

			expression = "X != a or Y != b";
			test(expression, new DefaultCompoundSyntaxTree("or", 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), 
					new DefaultCompoundSyntaxTree("!=", "Y", "b")));

			expression = "if X != a then X != a and Y = d else true";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "d")), "true"));

			expression = "if X != a then Y = d else true";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), 
					new DefaultCompoundSyntaxTree("=", "Y", "d"), "true"));

			expression = "if X != a or Y != b then f(X != a or Y != b or Z != c) else true";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("or", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "Y", "b"), 
									new DefaultCompoundSyntaxTree("!=", "Z", "c"))), "true"));

			expression = "if X != a or Y != b then f(true) else true";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("f", "true"), "true"));

			expression = "if X != a or Y != b then not(X != a or Y != b or Z != c) else true";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("not", 
							new DefaultCompoundSyntaxTree("or", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "Y", "b"), 
									new DefaultCompoundSyntaxTree("!=", "Z", "c"))), "true"));

			expression = "if X != a or Y != b then false else true";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "Y", "b")), "false", "true"));

			expression = "if A and B then if A then 1 else 0 else 1";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", "A", "B"), 
					new DefaultCompoundSyntaxTree("if . then . else .", "A", "1", "0"), "1"));

			expression = "1";
			test(expression, DefaultSymbol.createSymbol("1"));

			expression = "if X = a then if p(X) then E1 else E2 else if p(X) then E1 else E2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("p", "X"), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("p", "X"), "E1", "E2")));

			expression = "if X = a then if p(a) then E1 else E2 else if p(X) then E1 else E2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("p", "a"), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("p", "X"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X, Y) then E1 else E2 else if p(X, Y) then E1 else E2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a, b) then E1 else E2 else if p(X, Y) then E1 else E2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("p", "a", "b"), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X) and q(Y) then E1 else E2 else if p(X) and q(Y) then E1 else E2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "X"), 
									new DefaultCompoundSyntaxTree("q", "Y")), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "X"), 
									new DefaultCompoundSyntaxTree("q", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a) and q(b) then E1 else E2 else if p(X) and q(Y) then E1 else E2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "a"), 
									new DefaultCompoundSyntaxTree("q", "b")), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "X"), 
									new DefaultCompoundSyntaxTree("q", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X) and q(X, Y) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "X"), 
									new DefaultCompoundSyntaxTree("q", "X", "Y")), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "X"), 
									new DefaultCompoundSyntaxTree("q", "X", "Y")), "E1", "E2")));

			expression = ("if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2");
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "a"), 
									new DefaultCompoundSyntaxTree("q", "a", "b")), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "X"), 
									new DefaultCompoundSyntaxTree("q", "X", "Y")), "E1", "E2")));

			expression = "if even(X) then f(X) else X + 1";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("even", "X"), 
					new DefaultCompoundSyntaxTree("f", "X"), 
					new DefaultCompoundSyntaxTree("+", "X", "1")));

			expression = "if X = a then 1 else 2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), "1", "2"));

			expression = "if X = a then f(X) else X + 1";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("f", "X"), 
					new DefaultCompoundSyntaxTree("+", "X", "1")));

			expression = "if X = a then f(a) else X + 1";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("f", "a"), 
					new DefaultCompoundSyntaxTree("+", "X", "1")));

			expression = "if X = a and Y = b then f(X,Y) else X + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("f", "X", "Y"), 
					new DefaultCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = a and Y = b then f(a, b) else X + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("f", "a", "b"), 
					new DefaultCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a and Y = W then f(X,Y,Z) else X + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "Z", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "W")), 
					new DefaultCompoundSyntaxTree("f", "X", "Y", "Z"), 
					new DefaultCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a and W = Y then f(a, W, a) else X + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "Z", "a"), 
							new DefaultCompoundSyntaxTree("=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("f", "a", "W", "a"), 
					new DefaultCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a and W != Y then f(X,W,Z) else X + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "Z", "a"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "W", "Z"), 
					new DefaultCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a and W != Y then f(a, W, a) else X + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "Z", "a"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("f", "a", "W", "a"), 
					new DefaultCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z and W != Y then f(X,W,Z) else X + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "Z"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "W", "Z"), 
					new DefaultCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z and W != Y then f(X,W,Z) + {(on Z) foo(Z)} else Z + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "Z"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("f", "X", "W", "Z"), 
							new DefaultCompoundSyntaxTree("{ . . . }", 
									new DefaultCompoundSyntaxTree("( on . )", "Z"), 
									new DefaultCompoundSyntaxTree("foo", "Z"), null)), 
					new DefaultCompoundSyntaxTree("+", "Z", "Y")));

			expression = "if X = Z and W != Y then f(X, W, X) + {(on Z) foo(X)} else Z + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "Z"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("f", "X", "W", "X"), 
							new DefaultCompoundSyntaxTree("{ . . . }", 
									new DefaultCompoundSyntaxTree("( on . )", "Z"), 
									new DefaultCompoundSyntaxTree("foo", "X"), null)), 
					new DefaultCompoundSyntaxTree("+", "Z", "Y")));

			expression = "if X = Z = a or W != Y then f(X,W,Z) else X + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("=", "X", "Z", "a"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "W", "Z"), 
					new DefaultCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X = Z = a or W != Y then f(X,W,Z) else X + W";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("=", "X", "Z", "a"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "W", "Z"), 
					new DefaultCompoundSyntaxTree("+", "X", "W")));

			expression = "if X != a then {(on X) X != a} else {(on Y) X != a}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), null), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "Y"), 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), null)));

			expression = "if X != a then {(on X) X != a} else {(on Y) false}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), null), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "Y"), "false", null)));

			expression = "if X != a then X + 1 else 42";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), 
					new DefaultCompoundSyntaxTree("+", "X", "1"), "42"));

			expression = "if X != a then X + 1 else f(X)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), 
					new DefaultCompoundSyntaxTree("+", "X", "1"), 
					new DefaultCompoundSyntaxTree("f", "X")));

			expression = "if X != a then X + 1 else f(a)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), 
					new DefaultCompoundSyntaxTree("+", "X", "1"), 
					new DefaultCompoundSyntaxTree("f", "a")));

			expression = "if X != a or Y != b then X + Y else f(X,Y)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("+", "X", "Y"), 
					new DefaultCompoundSyntaxTree("f", "X", "Y")));

			expression = "if X != a or Y != b then  X + Y else f(a, b)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("+", "X", "Y"), 
					new DefaultCompoundSyntaxTree("f", "a", "b")));

			expression = "if X != Z or W != Y then f(X,W,Z) else foo(X, Z, W, Y)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "Z"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "W", "Z"), 
					new DefaultCompoundSyntaxTree("foo", "X", "Z", "W", "Y")));

			expression = "if X != Z or W != Y then f(X, W, Z) else foo(X, X, W, W)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("or", 
							new DefaultCompoundSyntaxTree("!=", "X", "Z"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "W", "Z"), 
					new DefaultCompoundSyntaxTree("foo", "X", "X", "W", "W")));

			expression = "if X != a and W != Y then f(X,W,Z) else X + Y";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "W", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "W", "Z"), 
					new DefaultCompoundSyntaxTree("+", "X", "Y")));

			expression = "if X != a then {(on X) X != a} else 1";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("!=", "X", "a"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), null), "1"));

			expression = "if X = a then {(on Y) X != a} else {(on X) X != a}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "Y"), 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), null), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), null)));

			expression = "if X = a then {(on Y) false} else {(on X) X != a}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "Y"), "false", null), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), null)));

			expression = "if X < 3 then f(X < 2, X < 3, X < 4) else g(X < 2, X < 3, X < 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("<", "X", "2"), 
							new DefaultCompoundSyntaxTree("<", "X", "3"), 
							new DefaultCompoundSyntaxTree("<", "X", "4")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("<", "X", "2"), 
							new DefaultCompoundSyntaxTree("<", "X", "3"), 
							new DefaultCompoundSyntaxTree("<", "X", "4"))));

			expression = "if X < 3 then f(X < 2, true, true)     else g(false, false, X < 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("<", "X", "2"), "true", "true"), 
					new DefaultCompoundSyntaxTree("g", "false", "false", 
							new DefaultCompoundSyntaxTree("<", "X", "4"))));

			expression = "if X < 3 then f(X <= 2, X <= 3, X <= 4) else g(X <= 2, X <= 3, X <= 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("<=", "X", "2"), 
							new DefaultCompoundSyntaxTree("<=", "X", "3"), 
							new DefaultCompoundSyntaxTree("<=", "X", "4")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("<=", "X", "2"), 
							new DefaultCompoundSyntaxTree("<=", "X", "3"), 
							new DefaultCompoundSyntaxTree("<=", "X", "4"))));

			expression = "if X < 3 then f(X <= 2, true, true) else g(false, X <= 3, X <= 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("<=", "X", "2"), "true", "true"), 
					new DefaultCompoundSyntaxTree("g", "false", 
							new DefaultCompoundSyntaxTree("<=", "X", "3"), 
							new DefaultCompoundSyntaxTree("<=", "X", "4"))));

			expression = "if X < 3 then f(X > 2, X > 3, X > 4) else g(X > 2, X > 3, X > 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree(">", "X", "2"), 
							new DefaultCompoundSyntaxTree(">", "X", "3"), 
							new DefaultCompoundSyntaxTree(">", "X", "4")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree(">", "X", "2"), 
							new DefaultCompoundSyntaxTree(">", "X", "3"), 
							new DefaultCompoundSyntaxTree(">", "X", "4"))));

			expression = "if X < 3 then f(X > 2, false, false) else g(true, X > 3, X > 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree(">", "X", "2"), "false", "false"), 
					new DefaultCompoundSyntaxTree("g", "true", 
							new DefaultCompoundSyntaxTree(">", "X", "3"), 
							new DefaultCompoundSyntaxTree(">", "X", "4"))));

			expression = "if X < 3 then f(X >= 2, X >= 3, X >= 4) else g(X >= 2, X >= 3, X >= 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree(">=", "X", "2"), 
							new DefaultCompoundSyntaxTree(">=", "X", "3"), 
							new DefaultCompoundSyntaxTree(">=", "X", "4")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree(">=", "X", "2"), 
							new DefaultCompoundSyntaxTree(">=", "X", "3"), 
							new DefaultCompoundSyntaxTree(">=", "X", "4"))));

			expression = "if X < 3 then f(X >= 2, false, false) else g(true, true, X >= 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree(">=", "X", "2"), "false", "false"), 
					new DefaultCompoundSyntaxTree("g", "true", "true", 
							new DefaultCompoundSyntaxTree(">=", "X", "4"))));

			expression = "if X <= 3 then f(X < 2, X < 3, X < 4) else g(X < 2, X < 3, X < 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<=", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("<", "X", "2"), 
							new DefaultCompoundSyntaxTree("<", "X", "3"), 
							new DefaultCompoundSyntaxTree("<", "X", "4")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("<", "X", "2"), 
							new DefaultCompoundSyntaxTree("<", "X", "3"), 
							new DefaultCompoundSyntaxTree("<", "X", "4"))));

			expression = "if X <= 3 then f(X < 2, X < 3, true)     else g(false, false, X < 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<=", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("<", "X", "2"), 
							new DefaultCompoundSyntaxTree("<", "X", "3"), "true"), 
					new DefaultCompoundSyntaxTree("g", "false", "false", 
							new DefaultCompoundSyntaxTree("<", "X", "4"))));

			expression = "if X <= 3 then f(X <= 2, X <= 3, X <= 4) else g(X <= 2, X <= 3, X <= 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<=", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("<=", "X", "2"), 
							new DefaultCompoundSyntaxTree("<=", "X", "3"), 
							new DefaultCompoundSyntaxTree("<=", "X", "4")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("<=", "X", "2"), 
							new DefaultCompoundSyntaxTree("<=", "X", "3"), 
							new DefaultCompoundSyntaxTree("<=", "X", "4"))));

			expression = "if X <= 3 then f(X <= 2, true, true) else g(false, false, X <= 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<=", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("<=", "X", "2"), "true", "true"), 
					new DefaultCompoundSyntaxTree("g", "false", "false", 
							new DefaultCompoundSyntaxTree("<=", "X", "4"))));

			expression = "if X <= 3 then f(X > 2, X > 3, X > 4) else g(X > 2, X > 3, X > 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<=", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree(">", "X", "2"), 
							new DefaultCompoundSyntaxTree(">", "X", "3"), 
							new DefaultCompoundSyntaxTree(">", "X", "4")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree(">", "X", "2"), 
							new DefaultCompoundSyntaxTree(">", "X", "3"), 
							new DefaultCompoundSyntaxTree(">", "X", "4"))));

			expression = "if X <= 3 then f(X > 2, false, false) else g(true, true, X > 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<=", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree(">", "X", "2"), "false", "false"), 
					new DefaultCompoundSyntaxTree("g", "true", "true", 
							new DefaultCompoundSyntaxTree(">", "X", "4"))));

			expression = "if X <= 3 then f(X >= 2, X >= 3, X >= 4) else g(X >= 2, X >= 3, X >= 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<=", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree(">=", "X", "2"), 
							new DefaultCompoundSyntaxTree(">=", "X", "3"), 
							new DefaultCompoundSyntaxTree(">=", "X", "4")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree(">=", "X", "2"), 
							new DefaultCompoundSyntaxTree(">=", "X", "3"), 
							new DefaultCompoundSyntaxTree(">=", "X", "4"))));

			expression = "if X <= 3 then f(X >= 2, X >= 3, false) else g(true, true, X >= 4)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("<=", "X", "3"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree(">=", "X", "2"), 
							new DefaultCompoundSyntaxTree(">=", "X", "3"), "false"), 
					new DefaultCompoundSyntaxTree("g", "true", "true", 
							new DefaultCompoundSyntaxTree(">=", "X", "4"))));

			expression = "if even(X) then f(even(X)) else g(even(X))";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("even", "X"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("even", "X")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("even", "X"))));

			expression = "if even(X) then f(true) else g(false)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("even", "X"), 
					new DefaultCompoundSyntaxTree("f", "true"), 
					new DefaultCompoundSyntaxTree("g", "false")));

			expression = "if even(X) then f(even(Y)) else g(even(X))";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("even", "X"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("even", "Y")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("even", "X"))));

			expression = "if even(X) then f(if Y = X then true else even(Y)) else g(false)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("even", "X"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("if . then . else .", 
									new DefaultCompoundSyntaxTree("=", "Y", "X"), "true", 
									new DefaultCompoundSyntaxTree("even", "Y"))), 
					new DefaultCompoundSyntaxTree("g", "false")));

			expression = "if even(X) then {(on even(a)) f(even(Y))} else g(even(X))";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("even", "X"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("even", "a")), 
							new DefaultCompoundSyntaxTree("f", 
									new DefaultCompoundSyntaxTree("even", "Y")), null), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("even", "X"))));

			expression = "if even(X) then {(on even(a)) f(if Y != a and Y = X then true else even(Y))} else g(false)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("even", "X"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("even", "a")), 
							new DefaultCompoundSyntaxTree("f", 
									new DefaultCompoundSyntaxTree("if . then . else .", 
											new DefaultCompoundSyntaxTree("and", 
													new DefaultCompoundSyntaxTree("!=", "Y", "a"), 
													new DefaultCompoundSyntaxTree("=", "Y", "X")), "true", 
											new DefaultCompoundSyntaxTree("even", "Y"))), null), 
					new DefaultCompoundSyntaxTree("g", "false")));

			expression = "if X = Y then f(X = Y) else g(X = Y)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "Y"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("=", "X", "Y")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("=", "X", "Y"))));

			expression = "if X = Y then f(true) else g(false)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "Y"), 
					new DefaultCompoundSyntaxTree("f", "true"), 
					new DefaultCompoundSyntaxTree("g", "false")));

			expression = "if X = Y then f(X = Z) else g(X = Y)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "Y"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("=", "X", "Z")), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("=", "X", "Y"))));

			expression = "if X = Y then f(X = Z) else g(false)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "Y"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("=", "X", "Z")), 
					new DefaultCompoundSyntaxTree("g", "false")));

			expression = "if X = Y then {(on X = a) f(X = Y)} else g(X = Y)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "Y"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("=", "X", "a")), 
							new DefaultCompoundSyntaxTree("f", 
									new DefaultCompoundSyntaxTree("=", "X", "Y")), null), 
					new DefaultCompoundSyntaxTree("g", 
							new DefaultCompoundSyntaxTree("=", "X", "Y"))));

			expression = "if X = Y then {(on X = a) f(X = Y)} else g(false)";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "Y"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("=", "X", "a")), 
							new DefaultCompoundSyntaxTree("f", 
									new DefaultCompoundSyntaxTree("=", "X", "Y")), null), 
					new DefaultCompoundSyntaxTree("g", "false")));

			expression = "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "a"), 
									new DefaultCompoundSyntaxTree("q", "a", "b")), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "X"), 
									new DefaultCompoundSyntaxTree("q", "X", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("=", "Y", "b")), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "a"), 
									new DefaultCompoundSyntaxTree("q", "a", "b")), "E1", "E2"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("p", "X"), 
									new DefaultCompoundSyntaxTree("q", "X", "Y")), "E1", "E2")));

			expression = "sum({(on Person in World) if Person = rich then 2000 else 50 | Person = american})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("in", "Person", "World")), 
							new DefaultCompoundSyntaxTree("if . then . else .", 
									new DefaultCompoundSyntaxTree("=", "Person", "rich"), "2000", "50"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("=", "Person", "american")))));

			expression = 
					"sum({(on Person in World) 2000 | Person = american and Person = rich})" +
					"+" + 
					"sum({(on Person in World) 50 | Person = american and not (Person = rich)})";
			test(expression, new DefaultCompoundSyntaxTree("+", 
					new DefaultCompoundSyntaxTree("sum", 
							new DefaultCompoundSyntaxTree("{ . . . }", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("in", "Person", "World")), "2000", 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("and", 
													new DefaultCompoundSyntaxTree("=", "Person", "american"), 
													new DefaultCompoundSyntaxTree("=", "Person", "rich"))))), 
					new DefaultCompoundSyntaxTree("sum", 
							new DefaultCompoundSyntaxTree("{ . . . }", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("in", "Person", "World")), "50", 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("and", 
													new DefaultCompoundSyntaxTree("=", "Person", "american"), 
													new DefaultCompoundSyntaxTree("not", 
															new DefaultCompoundSyntaxTree("=", "Person", "rich"))))))));
			
			expression = "f(X)";
			test(expression, new DefaultCompoundSyntaxTree("f", "X"));

			expression = "g(X',X',X) and h(Y)";
			test(expression, new DefaultCompoundSyntaxTree("and", 
					new DefaultCompoundSyntaxTree("g", "X'", "X'", "X"), 
					new DefaultCompoundSyntaxTree("h", "Y")));

			expression = "f(X',X'',Y)";
			test(expression, new DefaultCompoundSyntaxTree("f", "X'", "X''", "Y"));

			expression = "{(on X) X | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), "X", 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) X | X != b }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), "X", 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "b"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) X | X != b }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), "X", 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "b"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X', Y) f(X',Y) | X' != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X'", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X'", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X'", "a"))));

			expression = "{(on X) f(X,Y) | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("f", "X", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) f(X)}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("f", "X"), null));

			expression = "{(on X) f(X,Y) | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("f", "X", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "f(X, Y)";
			test(expression, new DefaultCompoundSyntaxTree("f", "X", "Y"));

			expression = "{(on X') f(X',Y) | X' != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X'"), 
					new DefaultCompoundSyntaxTree("f", "X'", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X'", "a"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "f(X, Y)";
			test(expression, new DefaultCompoundSyntaxTree("f", "X", "Y"));

			expression = "{(on X', Y') f(X',Y') | X' != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X'", "Y'")), 
					new DefaultCompoundSyntaxTree("f", "X'", "Y'"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X'", "a"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on Y) f(X, Y) }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Y"), 
					new DefaultCompoundSyntaxTree("f", "X", "Y"), null));

			expression = "{(on X', Y) f(X',Y) | X' != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X'", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X'", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X'", "a"))));

			expression = "{(on X, X') f(X,X') | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "X'")), 
					new DefaultCompoundSyntaxTree("f", "X", "X'"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "f(X)";
			test(expression, new DefaultCompoundSyntaxTree("f", "X"));

			expression = "{(on X'', X') f(X'',X') | X'' != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X''", "X'")), 
					new DefaultCompoundSyntaxTree("f", "X''", "X'"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X''", "a"))));

			expression = "{(on X, X') f(X,X') | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "X'")), 
					new DefaultCompoundSyntaxTree("f", "X", "X'"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "f(X, X')";
			test(expression, new DefaultCompoundSyntaxTree("f", "X", "X'"));

			expression = "{(on X'', X''') f(X'',X''') | X'' != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X''", "X'''")), 
					new DefaultCompoundSyntaxTree("f", "X''", "X'''"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X''", "a"))));

			expression = "{(on Z) {(on X, Y) f(X,Y,Z) | X != a } | Z != c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Z"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
							new DefaultCompoundSyntaxTree("f", "X", "Y", "Z"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "Z", "c"))));

			expression = "{(on Y) f(X, Y, Z) }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Y"), 
					new DefaultCompoundSyntaxTree("f", "X", "Y", "Z"), null));

			expression = "{(on Z') {(on X', Y) f(X',Y,Z') | X' != a } | Z' != c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Z'"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", "X'", "Y")), 
							new DefaultCompoundSyntaxTree("f", "X'", "Y", "Z'"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X'", "a"))), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "Z'", "c"))));

			expression = "{(on Z) {(on Z', Y) f(X,Y,Z') | X != a } | Z != c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Z"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", "Z'", "Y")), 
							new DefaultCompoundSyntaxTree("f", "X", "Y", "Z'"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "Z", "c"))));

			expression = "{(on Y) f(X, Y, Z, Z') }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Y"), 
					new DefaultCompoundSyntaxTree("f", "X", "Y", "Z", "Z'"), null));

			expression = "{(on Z''') {(on Z'', Y) f(X,Y,Z'') | X != a } | Z''' != c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Z'''"), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", "Z''", "Y")), 
							new DefaultCompoundSyntaxTree("f", "X", "Y", "Z''"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "Z'''", "c"))));

			expression = "{(on X, p(Z)) f(X,Y) | X != a }";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", 
									new DefaultCompoundSyntaxTree("p", "Z"))), 
					new DefaultCompoundSyntaxTree("f", "X", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) f(X)}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("f", "X"), null));

			expression = "{(on X) p(X,Y) + 2}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X in {1,2,3}) p(X) | false}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("{ . }", 
											new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
					new DefaultCompoundSyntaxTree("p", "X"), 
					new DefaultCompoundSyntaxTree("|", "false")));

			expression = "{ }";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list")));

			expression = "{(on X in {1,2,3} - {1}, Y in {1,2} union {3}) p(X,Y) + 1 + 1 | true and Z}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("-", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")), 
													new DefaultCompoundSyntaxTree("{ . }", "1"))), 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("union", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "1", "2")), 
													new DefaultCompoundSyntaxTree("{ . }", "3"))))), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "1", "1"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", "true", "Z"))));

			expression = "{(on X in {2,3}, Y in {1,2,3}) p(X,Y) + 2 | Z}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "2", "3"))), 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), 
					new DefaultCompoundSyntaxTree("|", "Z")));

			expression = "{(on X in {1,2,3} - {1}, Y) p(X,Y) + 1 + 1}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("-", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")), 
													new DefaultCompoundSyntaxTree("{ . }", "1"))), "Y")), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "1", "1"), null));

			expression = "{(on X in {2,3}, Y) p(X,Y) + 2}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "2", "3"))), "Y")), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X,Y) p(X,Y) + 2}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "if V = (<X>) then {(on X) p(X,Y) + 2} else 0";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "V", 
							DefaultSymbol.createSymbol(DefaultSymbol.createSymbol("X"))), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("+", 
									new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), null), "0"));

			expression = "{(on X, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X, Z, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Z", 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "{(on X in set1, Z in set2, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", "set1"), 
									new DefaultCompoundSyntaxTree("in", "Z", "set2"), 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					new DefaultCompoundSyntaxTree("+", 
							new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), null));

			expression = "if V = (<X>) then {(on X, Y in {1,2,3}) p(X,Y) + 2} else 0";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "V", 
							DefaultSymbol.createSymbol(DefaultSymbol.createSymbol("X"))), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", "X", 
											new DefaultCompoundSyntaxTree("in", "Y", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
							new DefaultCompoundSyntaxTree("+", 
									new DefaultCompoundSyntaxTree("p", "X", "Y"), "2"), null), "0"));

			expression = "{(on X,Y) f(X) | Z = X}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
					new DefaultCompoundSyntaxTree("f", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("=", "Z", "X"))));

			expression = "sum({(on X in {1,2,3}) 0 | X != 2})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")))), "0", 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "2")))));

			expression = "product({(on X in {1,2,3}) 1 | X != 2})";
			test(expression, new DefaultCompoundSyntaxTree("product", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")))), "1", 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "2")))));

			expression = "sum(partition(A, B))";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("partition", "A", "B")));

			expression = "sum(partition())";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("partition")));

			expression = "{(on ) foo(X) | X != a and X != b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list")), 
					new DefaultCompoundSyntaxTree("foo", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "X", "b")))));

			expression = "if X != a and X != b then {foo(X)} else {}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("and", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"), 
							new DefaultCompoundSyntaxTree("!=", "X", "b")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("foo", "X")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list"))));

			expression = "{(on X in {1,2,3}) p(X) | false}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("{ . }", 
											new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
					new DefaultCompoundSyntaxTree("p", "X"), 
					new DefaultCompoundSyntaxTree("|", "false")));

			expression = "{ }";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list")));

			expression = "{(on X in {a,b,c}) foo(X) | X != a and X != b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("{ . }", 
											new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")))), 
					new DefaultCompoundSyntaxTree("foo", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "X", "b")))));

			expression = "partition(" +
					"if first({a,b,c}) != a and first({a,b,c}) != b then { foo(first({a,b,c})) } else {}," +
					"{(on X in rest({a,b,c})) foo(X) | X != a and X != b})";
			test(expression, new DefaultCompoundSyntaxTree("partition", 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("first", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))), "a"), 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("first", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))), "b")), 
							new DefaultCompoundSyntaxTree("{ . }", 
									new DefaultCompoundSyntaxTree("foo", 
											new DefaultCompoundSyntaxTree("first", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))))), 
							new DefaultCompoundSyntaxTree("{ . }", 
									new DefaultCompoundSyntaxTree("kleene list"))), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("rest", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))))), 
							new DefaultCompoundSyntaxTree("foo", "X"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"), 
											new DefaultCompoundSyntaxTree("!=", "X", "b"))))));


			expression = "{(on X in {a,b,c}, Y in {1,2,3}) foo(X,Y) | X != a and Y != 1}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))), 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
					new DefaultCompoundSyntaxTree("foo", "X", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "Y", "1")))));

			expression = "partition(" +
					"{(on                     Y in {1,2,3}) foo(first({a,b,c}),Y) | first({a,b,c}) != a and Y != 1}," +
					"{(on X in rest({a,b,c}), Y in {1,2,3}) foo(X,Y)              | X              != a and Y != 1})";
			test(expression, new DefaultCompoundSyntaxTree("partition", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
							new DefaultCompoundSyntaxTree("foo", 
									new DefaultCompoundSyntaxTree("first", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))), "Y"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", 
													new DefaultCompoundSyntaxTree("first", 
															new DefaultCompoundSyntaxTree("{ . }", 
																	new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))), "a"), 
											new DefaultCompoundSyntaxTree("!=", "Y", "1")))), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", 
											new DefaultCompoundSyntaxTree("in", "X", 
													new DefaultCompoundSyntaxTree("rest", 
															new DefaultCompoundSyntaxTree("{ . }", 
																	new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")))), 
											new DefaultCompoundSyntaxTree("in", "Y", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))))), 
							new DefaultCompoundSyntaxTree("foo", "X", "Y"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"), 
											new DefaultCompoundSyntaxTree("!=", "Y", "1"))))));


			expression = "{(on X in rest({a,b,c}), Y in {1,2,3}, Z) foo(X,Y) | X != a and Y != 1}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("rest", 
													new DefaultCompoundSyntaxTree("{ . }", 
															new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")))), 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))), "Z")), 
					new DefaultCompoundSyntaxTree("foo", "X", "Y"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "Y", "1")))));

			expression = "partition(" +
					"{(on X in rest({a,b,c}),                     Z) foo(X, first({1,2,3})) | X != a and first({1,2,3}) != 1}," +
					"{(on X in rest({a,b,c}), Y in rest({1,2,3}), Z) foo(X,Y)               | X != a and Y              != 1})";
			test(expression, new DefaultCompoundSyntaxTree("partition", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", 
											new DefaultCompoundSyntaxTree("in", "X", 
													new DefaultCompoundSyntaxTree("rest", 
															new DefaultCompoundSyntaxTree("{ . }", 
																	new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")))), "Z")), 
							new DefaultCompoundSyntaxTree("foo", "X", 
									new DefaultCompoundSyntaxTree("first", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"), 
											new DefaultCompoundSyntaxTree("!=", 
													new DefaultCompoundSyntaxTree("first", 
															new DefaultCompoundSyntaxTree("{ . }", 
																	new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))), "1")))), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", 
											new DefaultCompoundSyntaxTree("in", "X", 
													new DefaultCompoundSyntaxTree("rest", 
															new DefaultCompoundSyntaxTree("{ . }", 
																	new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")))), 
											new DefaultCompoundSyntaxTree("in", "Y", 
													new DefaultCompoundSyntaxTree("rest", 
															new DefaultCompoundSyntaxTree("{ . }", 
																	new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")))), "Z")), 
							new DefaultCompoundSyntaxTree("foo", "X", "Y"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"), 
											new DefaultCompoundSyntaxTree("!=", "Y", "1"))))));

			expression = "{(on X in {}) foo(X) | X != a and X != b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("{ . }", 
											new DefaultCompoundSyntaxTree("kleene list")))), 
					new DefaultCompoundSyntaxTree("foo", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{(on X in {1,2,3}, Y in {a,b,c}) p(X,Y)}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))), 
									new DefaultCompoundSyntaxTree("in", "Y", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))))), 
					new DefaultCompoundSyntaxTree("p", "X", "Y"), null));

			expression = "{p(1,a), p(1,b), p(1,c), p(2,a), p(2,b), p(2,c), p(3,a), p(3,b), p(3,c)}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", 
							new DefaultCompoundSyntaxTree("p", "1", "a"), 
							new DefaultCompoundSyntaxTree("p", "1", "b"), 
							new DefaultCompoundSyntaxTree("p", "1", "c"), 
							new DefaultCompoundSyntaxTree("p", "2", "a"), 
							new DefaultCompoundSyntaxTree("p", "2", "b"), 
							new DefaultCompoundSyntaxTree("p", "2", "c"), 
							new DefaultCompoundSyntaxTree("p", "3", "a"), 
							new DefaultCompoundSyntaxTree("p", "3", "b"), 
							new DefaultCompoundSyntaxTree("p", "3", "c"))));

			expression = "{(on X) if X = 2 then p(X) else q(X) | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("=", "X", "2"), 
							new DefaultCompoundSyntaxTree("p", "X"), 
							new DefaultCompoundSyntaxTree("q", "X")), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "partition({(on X) p(X) | X != a and X = 2}, {(on X) q(X) | X != a and not(X = 2)})";
			test(expression, new DefaultCompoundSyntaxTree("partition", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("p", "X"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"), 
											new DefaultCompoundSyntaxTree("=", "X", "2")))), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("q", "X"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"), 
											new DefaultCompoundSyntaxTree("not", 
													new DefaultCompoundSyntaxTree("=", "X", "2")))))));


			expression = "{(on X) if Y = 2 then p(X) else q(X) | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("=", "Y", "2"), 
							new DefaultCompoundSyntaxTree("p", "X"), 
							new DefaultCompoundSyntaxTree("q", "X")), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));


			expression = "sum({(on Person in World) if Person = rich then 2000 else 50 | Person = american})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("in", "Person", "World")), 
							new DefaultCompoundSyntaxTree("if . then . else .", 
									new DefaultCompoundSyntaxTree("=", "Person", "rich"), "2000", "50"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("=", "Person", "american")))));

			expression = 
					"sum(" +
					"partition(" + 
					"{(on Person in World) 2000 | Person = american and Person = rich}," + 
					"{(on Person in World) 50 | Person = american and not (Person = rich)}))";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("partition", 
							new DefaultCompoundSyntaxTree("{ . . . }", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("in", "Person", "World")), "2000", 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("and", 
													new DefaultCompoundSyntaxTree("=", "Person", "american"), 
													new DefaultCompoundSyntaxTree("=", "Person", "rich")))), 
							new DefaultCompoundSyntaxTree("{ . . . }", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("in", "Person", "World")), "50", 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("and", 
													new DefaultCompoundSyntaxTree("=", "Person", "american"), 
													new DefaultCompoundSyntaxTree("not", 
															new DefaultCompoundSyntaxTree("=", "Person", "rich"))))))));
		
			expression = "{(on X) if X = 2 and something then p(X) else q(X) | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("=", "X", "2"), "something"), 
							new DefaultCompoundSyntaxTree("p", "X"), 
							new DefaultCompoundSyntaxTree("q", "X")), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "partition({(on X) if something then p(X) else q(X) | X != a and X = 2}, {(on X) q(X) | X != a and not(X = 2)})";
			test(expression, new DefaultCompoundSyntaxTree("partition", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("if . then . else .", "something", 
									new DefaultCompoundSyntaxTree("p", "X"), 
									new DefaultCompoundSyntaxTree("q", "X")), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"), 
											new DefaultCompoundSyntaxTree("=", "X", "2")))), 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("q", "X"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"), 
											new DefaultCompoundSyntaxTree("not", 
													new DefaultCompoundSyntaxTree("=", "X", "2")))))));

			expression = "{(on X in {(on Y) foo(Y)}) bar(X) | X != a and X != b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("{ . . . }", 
											new DefaultCompoundSyntaxTree("( on . )", "Y"), 
											new DefaultCompoundSyntaxTree("foo", "Y"), null))), 
					new DefaultCompoundSyntaxTree("bar", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{(on Y) bar(foo(Y)) | foo(Y) != a and foo(Y) != b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Y"), 
					new DefaultCompoundSyntaxTree("bar", 
							new DefaultCompoundSyntaxTree("foo", "Y")), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("foo", "Y"), "a"), 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("foo", "Y"), "b")))));

			expression = "{(on X in {(on Y) foo(Y) | Y != c}) bar(X) | X != a and X != b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("{ . . . }", 
											new DefaultCompoundSyntaxTree("( on . )", "Y"), 
											new DefaultCompoundSyntaxTree("foo", "Y"), 
											new DefaultCompoundSyntaxTree("|", 
													new DefaultCompoundSyntaxTree("!=", "Y", "c"))))), 
					new DefaultCompoundSyntaxTree("bar", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{(on Y) bar(foo(Y)) | foo(Y) != a and foo(Y) != b and Y != c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Y"), 
					new DefaultCompoundSyntaxTree("bar", 
							new DefaultCompoundSyntaxTree("foo", "Y")), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("foo", "Y"), "a"), 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("foo", "Y"), "b"), 
									new DefaultCompoundSyntaxTree("!=", "Y", "c")))));

			expression = "{(on X in {(on Y in {(on Z) Z}) foo(Y) | Y != c}) bar(X) | X != a and X != b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("{ . . . }", 
											new DefaultCompoundSyntaxTree("( on . )", 
													new DefaultCompoundSyntaxTree("in", "Y", 
															new DefaultCompoundSyntaxTree("{ . . . }", 
																	new DefaultCompoundSyntaxTree("( on . )", "Z"), "Z", null))), 
											new DefaultCompoundSyntaxTree("foo", "Y"), 
											new DefaultCompoundSyntaxTree("|", 
													new DefaultCompoundSyntaxTree("!=", "Y", "c"))))), 
					new DefaultCompoundSyntaxTree("bar", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{(on Z) bar(foo(Z)) | foo(Z) != a and foo(Z) != b and Z != c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Z"), 
					new DefaultCompoundSyntaxTree("bar", 
							new DefaultCompoundSyntaxTree("foo", "Z")), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("foo", "Z"), "a"), 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("foo", "Z"), "b"), 
									new DefaultCompoundSyntaxTree("!=", "Z", "c")))));

			expression = "{(on X in {(on Y in {(on Z) Z}) foo(Y) | Y != c}) bar(X) | X != a and X != b and 'for all'({(on Z) blah(Z)})}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("{ . . . }", 
											new DefaultCompoundSyntaxTree("( on . )", 
													new DefaultCompoundSyntaxTree("in", "Y", 
															new DefaultCompoundSyntaxTree("{ . . . }", 
																	new DefaultCompoundSyntaxTree("( on . )", "Z"), "Z", null))), 
											new DefaultCompoundSyntaxTree("foo", "Y"), 
											new DefaultCompoundSyntaxTree("|", 
													new DefaultCompoundSyntaxTree("!=", "Y", "c"))))), 
					new DefaultCompoundSyntaxTree("bar", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "X", "b"), 
									new DefaultCompoundSyntaxTree("for all", 
											new DefaultCompoundSyntaxTree("{ . . . }", 
													new DefaultCompoundSyntaxTree("( on . )", "Z"), 
													new DefaultCompoundSyntaxTree("blah", "Z"), null))))));

			expression = "{(on Z) bar(foo(Z)) | foo(Z) != a and foo(Z) != b and 'for all'({(on Z) blah(Z)}) and Z != c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "Z"), 
					new DefaultCompoundSyntaxTree("bar", 
							new DefaultCompoundSyntaxTree("foo", "Z")), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("foo", "Z"), "a"), 
									new DefaultCompoundSyntaxTree("!=", 
											new DefaultCompoundSyntaxTree("foo", "Z"), "b"), 
									new DefaultCompoundSyntaxTree("for all", 
											new DefaultCompoundSyntaxTree("{ . . . }", 
													new DefaultCompoundSyntaxTree("( on . )", "Z"), 
													new DefaultCompoundSyntaxTree("blah", "Z"), null)), 
									new DefaultCompoundSyntaxTree("!=", "Z", "c")))));

			expression = "{(on X in 'some set') p(X) | X != a} - {somethingElse}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("in", "X", "some set")), 
							new DefaultCompoundSyntaxTree("p", "X"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))), 
					new DefaultCompoundSyntaxTree("{ . }", "somethingElse")));

			expression = "{(on X in 'some set') p(X) | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", "some set")), 
					new DefaultCompoundSyntaxTree("p", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) p(X) | X != a} - {p(Y)}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("p", "X"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("p", "Y"))));

			expression = "{(on X) p(X) | X != a and X != Y}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("p", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "X", "Y")))));

			expression = "{(on X) p(X) | X != a} - {p(a)}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("p", "X"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("p", "a"))));

			expression = "{(on X) p(X) | X != a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("p", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("!=", "X", "a"))));

			expression = "{(on X) p(X) | X != a} - {p(b)}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . . . }", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("p", "X"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"))), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("p", "b"))));

			expression = "{(on X) p(X) | X != a and X != b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", "X"), 
					new DefaultCompoundSyntaxTree("p", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("!=", "X", "a"), 
									new DefaultCompoundSyntaxTree("!=", "X", "b")))));

			expression = "{ }";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list")));

			expression = "{a, b, c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")));

			expression = "{a, a, b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "a", "a", "b")));

			expression = "{a, b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "a", "b")));

			expression = "{a, b, b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "a", "b", "b")));

			expression = "{a, b, b, a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "a", "b", "b", "a")));

			expression = "{a, b, b, a, b}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "a", "b", "b", "a", "b")));

			expression = "{X, Y}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "X", "Y")));

			expression = "if X = Y then {X} else {X,Y}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "Y"), 
					new DefaultCompoundSyntaxTree("{ . }", "X"), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y"))));

			expression = "{X, Y, a}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "X", "Y", "a")));

			expression = 
					"if X = Y " +
					"then if X = a then {X} else {X,a} " +
					"else if X = a then {X,Y} else if Y = a then {X,Y} else {X,Y,a}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "Y"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("{ . }", "X"), 
							new DefaultCompoundSyntaxTree("{ . }", 
									new DefaultCompoundSyntaxTree("kleene list", "X", "a"))), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("=", "X", "a"), 
							new DefaultCompoundSyntaxTree("{ . }", 
									new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
							new DefaultCompoundSyntaxTree("if . then . else .", 
									new DefaultCompoundSyntaxTree("=", "Y", "a"), 
									new DefaultCompoundSyntaxTree("{ . }", 
											new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
									new DefaultCompoundSyntaxTree("{ . }", 
											new DefaultCompoundSyntaxTree("kleene list", "X", "Y", "a"))))));

			expression = "partition({}, X, {}, Y)";
			test(expression, new DefaultCompoundSyntaxTree("partition", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list")), "X", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list")), "Y"));

			expression = "partition(X, Y)";
			test(expression, new DefaultCompoundSyntaxTree("partition", "X", "Y"));

			expression = "partition({}, X, {1}, Y, {2,3})";
			test(expression, new DefaultCompoundSyntaxTree("partition", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list")), "X", 
					new DefaultCompoundSyntaxTree("{ . }", "1"), "Y", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "2", "3"))));

			expression = "partition({1,2,3}, X, Y)";
			test(expression, new DefaultCompoundSyntaxTree("partition", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")), "X", "Y"));

			expression = "partition({}, {})";
			test(expression, new DefaultCompoundSyntaxTree("partition", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list"))));

			expression = "{}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list")));

			expression = "partition(X)";
			test(expression, new DefaultCompoundSyntaxTree("partition", "X"));

			expression = "X";
			test(expression, DefaultSymbol.createSymbol("X"));

			expression = "(1, 2)";
			test(expression, new DefaultCompoundSyntaxTree("( . )", 
					new DefaultCompoundSyntaxTree("kleene list", "1", "2")));

			expression = "(X, Y) = (X, Y, Z)";
			test(expression, new DefaultCompoundSyntaxTree("=", 
					new DefaultCompoundSyntaxTree("( . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
					new DefaultCompoundSyntaxTree("( . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y", "Z"))));

			expression = "tuple(X, Y) = (X, Y, Z)";
			test(expression, new DefaultCompoundSyntaxTree("=", 
					new DefaultCompoundSyntaxTree("tuple", "X", "Y"), 
					new DefaultCompoundSyntaxTree("( . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y", "Z"))));

			expression = "(X, Y) = tuple(X, Y, Z)";
			test(expression, new DefaultCompoundSyntaxTree("=", 
					new DefaultCompoundSyntaxTree("( . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
					new DefaultCompoundSyntaxTree("tuple", "X", "Y", "Z")));

			expression = "(X, Y, Z) = (a, b, c)";
			test(expression, new DefaultCompoundSyntaxTree("=", 
					new DefaultCompoundSyntaxTree("( . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y", "Z")), 
					new DefaultCompoundSyntaxTree("( . )", 
							new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))));

			expression = "X = a and Y = b and Z = c";
			test(expression, new DefaultCompoundSyntaxTree("and", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("=", "Y", "b"), 
					new DefaultCompoundSyntaxTree("=", "Z", "c")));

			expression = "tuple(X, Y, Z) = tuple(a, b, c)";
			test(expression, new DefaultCompoundSyntaxTree("=", 
					new DefaultCompoundSyntaxTree("tuple", "X", "Y", "Z"), 
					new DefaultCompoundSyntaxTree("tuple", "a", "b", "c")));

			expression = "tuple(X, Y, Z) = (a, b, c)";
			test(expression, new DefaultCompoundSyntaxTree("=", 
					new DefaultCompoundSyntaxTree("tuple", "X", "Y", "Z"), 
					new DefaultCompoundSyntaxTree("( . )", 
							new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))));

			expression = "(X, Y, Z) = tuple(a, b, c)";
			test(expression, new DefaultCompoundSyntaxTree("=", 
					new DefaultCompoundSyntaxTree("( . )", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "Y", "Z")), 
					new DefaultCompoundSyntaxTree("tuple", "a", "b", "c")));

			expression = "first(list(a,b,c))";
			test(expression, new DefaultCompoundSyntaxTree("first", 
					new DefaultCompoundSyntaxTree("list", "a", "b", "c")));

			expression = "first(list(a))";
			test(expression, new DefaultCompoundSyntaxTree("first", 
					new DefaultCompoundSyntaxTree("list", "a")));

			expression = "rest(list(a,b,c))";
			test(expression, new DefaultCompoundSyntaxTree("rest", 
					new DefaultCompoundSyntaxTree("list", "a", "b", "c")));

			expression = "list(b,c)";
			test(expression, new DefaultCompoundSyntaxTree("list", "b", "c"));

			expression = "rest(list(a))";
			test(expression, new DefaultCompoundSyntaxTree("rest", 
					new DefaultCompoundSyntaxTree("list", "a")));

			expression = "list()";
			test(expression, new DefaultCompoundSyntaxTree("list"));

			expression = "size(list(a,b,c))";
			test(expression, new DefaultCompoundSyntaxTree("size", 
					new DefaultCompoundSyntaxTree("list", "a", "b", "c")));

			expression = "size(list(a))";
			test(expression, new DefaultCompoundSyntaxTree("size", 
					new DefaultCompoundSyntaxTree("list", "a")));

			expression = "size(list())";
			test(expression, new DefaultCompoundSyntaxTree("size", 
					new DefaultCompoundSyntaxTree("list")));


			expression = "{x} union {y}";
			test(expression, new DefaultCompoundSyntaxTree("union", 
					new DefaultCompoundSyntaxTree("{ . }", "x"), 
					new DefaultCompoundSyntaxTree("{ . }", "y")));

			expression = "{x, y}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "x", "y")));

			expression = "{x, y} union {z}";
			test(expression, new DefaultCompoundSyntaxTree("union", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "x", "y")), 
					new DefaultCompoundSyntaxTree("{ . }", "z")));

			expression = "{x, y, z}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "x", "y", "z")));

			expression = "{x, y} union {}";
			test(expression, new DefaultCompoundSyntaxTree("union", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "x", "y")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list"))));

			expression = "{} union {x, y} union {}";
			test(expression, new DefaultCompoundSyntaxTree("union", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "x", "y")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list"))));

			expression = "A union {x, y} union {}";
			test(expression, new DefaultCompoundSyntaxTree("union", "A", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "x", "y")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list"))));

			expression = "A union {x, y}";
			test(expression, new DefaultCompoundSyntaxTree("union", "A", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "x", "y"))));

			expression = "A union {x, y} union {z} union B";
			test(expression, new DefaultCompoundSyntaxTree("union", "A", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "x", "y")), 
					new DefaultCompoundSyntaxTree("{ . }", "z"), "B"));

			expression = "A union {x, y, z} union B";
			test(expression, new DefaultCompoundSyntaxTree("union", "A", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "x", "y", "z")), "B"));


			expression = "sum({{(on X) c}})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), "c", null)));

			expression = "sum({{(on X) c}})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), "c", null)));

			expression = "c * |{{(on X) c}}|";
			test(expression, new DefaultCompoundSyntaxTree("*", "c", 
					new DefaultCompoundSyntaxTree("| . |", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "X"), "c", null))));

			expression = "product({{(on X) c}})";
			test(expression, new DefaultCompoundSyntaxTree("product", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), "c", null)));

			expression = "c ^ |{{(on X) c}}|";
			test(expression, new DefaultCompoundSyntaxTree("^", "c", 
					new DefaultCompoundSyntaxTree("| . |", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "X"), "c", null))));

			expression = "sum({{(on X) f(Y)}})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("f", "Y"), null)));

			expression = "f(Y) * |{{(on X) f(Y)}}|";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("f", "Y"), 
					new DefaultCompoundSyntaxTree("| . |", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "X"), 
									new DefaultCompoundSyntaxTree("f", "Y"), null))));

			expression = "sum({{(on X) f(X)}})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("f", "X"), null)));

			expression = "sum({{1,2,3}})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{{ . }}", 
							new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))));


			expression = "{}-{d}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list")), 
					new DefaultCompoundSyntaxTree("{ . }", "d")));

			expression = "{}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list")));

			expression = "{d}-{}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . }", "d"), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list"))));

			expression = "{d}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", "d"));

			expression = "{a,b,c}-{d}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					new DefaultCompoundSyntaxTree("{ . }", "d")));

			expression = "{a,b,c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")));

			expression = "{a,b,c}-{b}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					new DefaultCompoundSyntaxTree("{ . }", "b")));

			expression = "{a,c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", 
					new DefaultCompoundSyntaxTree("kleene list", "a", "c")));

			expression = "{a,b,c}-{b,a}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "b", "a"))));

			expression = "{c}";
			test(expression, new DefaultCompoundSyntaxTree("{ . }", "c"));

			expression = "{a,b,c}-{a,b,c}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c"))));

			expression = "{{a,b,c}}-{{a,X,c}}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{{ . }}", 
							new DefaultCompoundSyntaxTree("kleene list", "a", "b", "c")), 
					new DefaultCompoundSyntaxTree("{{ . }}", 
							new DefaultCompoundSyntaxTree("kleene list", "a", "X", "c"))));

			expression = "if X = b then {} else { b }";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "b"), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list")), 
					new DefaultCompoundSyntaxTree("{ . }", "b")));

			expression = "{{X,a,b,Y,Y}}-{{X,X,Y,a}}";
			test(expression, new DefaultCompoundSyntaxTree("-", 
					new DefaultCompoundSyntaxTree("{{ . }}", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "a", "b", "Y", "Y")), 
					new DefaultCompoundSyntaxTree("{{ . }}", 
							new DefaultCompoundSyntaxTree("kleene list", "X", "X", "Y", "a"))));

			expression = "if X = b then { Y } else if X = Y then { b } else {{ b, Y }}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "b"), 
					new DefaultCompoundSyntaxTree("{ . }", "Y"), 
					new DefaultCompoundSyntaxTree("if . then . else .", 
							new DefaultCompoundSyntaxTree("=", "X", "Y"), 
							new DefaultCompoundSyntaxTree("{ . }", "b"), 
							new DefaultCompoundSyntaxTree("{{ . }}", 
									new DefaultCompoundSyntaxTree("kleene list", "b", "Y")))));

			expression = "{(on X in {1,2,3}, Y in D) f(X) | X = Z}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))), 
									new DefaultCompoundSyntaxTree("in", "Y", "D"))), 
					new DefaultCompoundSyntaxTree("f", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("=", "X", "Z"))));

			expression = "{(on Y in D) f(Z)}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "Y", "D")), 
					new DefaultCompoundSyntaxTree("f", "Z"), null));

			expression = "{(on X in {1,2,3}, Y in D) f(X) | X = Z and h(X)}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("kleene list", 
									new DefaultCompoundSyntaxTree("in", "X", 
											new DefaultCompoundSyntaxTree("{ . }", 
													new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3"))), 
									new DefaultCompoundSyntaxTree("in", "Y", "D"))), 
					new DefaultCompoundSyntaxTree("f", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("=", "X", "Z"), 
									new DefaultCompoundSyntaxTree("h", "X")))));

			expression = "{(on Y in D) f(Z) | h(Z)}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "Y", "D")), 
					new DefaultCompoundSyntaxTree("f", "Z"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("h", "Z"))));

			expression = "{(on X in {1,2,3}) f(X) | X = Z and h(X)}";
			test(expression, new DefaultCompoundSyntaxTree("{ . . . }", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "X", 
									new DefaultCompoundSyntaxTree("{ . }", 
											new DefaultCompoundSyntaxTree("kleene list", "1", "2", "3")))), 
					new DefaultCompoundSyntaxTree("f", "X"), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("and", 
									new DefaultCompoundSyntaxTree("=", "X", "Z"), 
									new DefaultCompoundSyntaxTree("h", "X")))));

			expression = "if h(Z) then {f(Z)} else {}";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("h", "Z"), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("f", "Z")), 
					new DefaultCompoundSyntaxTree("{ . }", 
							new DefaultCompoundSyntaxTree("kleene list"))));

			expression = "{{ ( on Y in People ) sum({{ ( on p(a1, Y) ) p(a1, Y) }}) | Y = a2 }}";
			test(expression, new DefaultCompoundSyntaxTree("{{ . . . }}", 
					new DefaultCompoundSyntaxTree("( on . )", 
							new DefaultCompoundSyntaxTree("in", "Y", "People")), 
					new DefaultCompoundSyntaxTree("sum", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("p", "a1", "Y")), 
									new DefaultCompoundSyntaxTree("p", "a1", "Y"), null)), 
					new DefaultCompoundSyntaxTree("|", 
							new DefaultCompoundSyntaxTree("=", "Y", "a2"))));

			expression = "{{ sum({{ ( on p(a1, a2) ) p(a1, a2) }}) }}";
			test(expression, new DefaultCompoundSyntaxTree("{{ . }}", 
					new DefaultCompoundSyntaxTree("sum", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("p", "a1", "a2")), 
									new DefaultCompoundSyntaxTree("p", "a1", "a2"), null))));

			expression = "if Z = a then f(lambda Z : g(Z)) else 0";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "Z", "a"), 
					new DefaultCompoundSyntaxTree("f", 
							new DefaultCompoundSyntaxTree("lambda . : .", "Z", 
									new DefaultCompoundSyntaxTree("g", "Z"))), "0"));

			expression = "'scoped variables'(lambda Z : Z)";
			test(expression, new DefaultCompoundSyntaxTree("scoped variables", 
					new DefaultCompoundSyntaxTree("lambda . : .", "Z", "Z")));

			expression = "list(Z)";
			test(expression, new DefaultCompoundSyntaxTree("list", "Z"));

			expression = "'scoped variables'(lambda f(X) : f(X))";
			test(expression, new DefaultCompoundSyntaxTree("scoped variables", 
					new DefaultCompoundSyntaxTree("lambda . : .", 
							new DefaultCompoundSyntaxTree("f", "X"), 
							new DefaultCompoundSyntaxTree("f", "X"))));

			expression = "list(f(X))";
			test(expression, new DefaultCompoundSyntaxTree("list", 
					new DefaultCompoundSyntaxTree("f", "X")));

			expression = "if X = a then lambda f(X) : f(X) else 1";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("lambda . : .", 
							new DefaultCompoundSyntaxTree("f", "X"), 
							new DefaultCompoundSyntaxTree("f", "X")), "1"));

			expression = "if X = a then lambda f(a) : f(a) else 1";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("lambda . : .", 
							new DefaultCompoundSyntaxTree("f", "a"), 
							new DefaultCompoundSyntaxTree("f", "a")), "1"));

			expression = "(lambda f(X) : 2 + f(X))(1)";
			test(expression, new DefaultCompoundSyntaxTree(
					new DefaultCompoundSyntaxTree("lambda . : .", 
							new DefaultCompoundSyntaxTree("f", "X"), 
							new DefaultCompoundSyntaxTree("+", "2", 
									new DefaultCompoundSyntaxTree("f", "X"))), "1"));

			expression = "product({{(on X) f(X, Y) | X != a}}) * product({{(on Z) g(Z) | Z != b}})";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "X"), 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "X", "a")))), 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "Z"), 
									new DefaultCompoundSyntaxTree("g", "Z"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "Z", "b"))))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y}}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("and", 
													new DefaultCompoundSyntaxTree("!=", "X", "a"), 
													new DefaultCompoundSyntaxTree("=", "X", "Y"))))), 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "Z"), 
									new DefaultCompoundSyntaxTree("g", "Z"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "Z", "a"))))));

			expression = "product({{ ( on Y ) (f(Y, Y) * g(Y)) | Y != a }})";
			test(expression, new DefaultCompoundSyntaxTree("product", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "Y"), 
							new DefaultCompoundSyntaxTree("*", 
									new DefaultCompoundSyntaxTree("f", "Y", "Y"), 
									new DefaultCompoundSyntaxTree("g", "Y")), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "Y", "a")))));

			expression = "product({{(on X, Y) f(X, Y) | X != a}}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "X", "a")))), 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "Z"), 
									new DefaultCompoundSyntaxTree("g", "Z"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "Z", "a"))))));

			expression = "product({{ (on X) (product({{(on Y) f(X, Y)}}) * g(X))  | X != a}})";
			test(expression, new DefaultCompoundSyntaxTree("product", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("*", 
									new DefaultCompoundSyntaxTree("product", 
											new DefaultCompoundSyntaxTree("{{ . . . }}", 
													new DefaultCompoundSyntaxTree("( on . )", "Y"), 
													new DefaultCompoundSyntaxTree("f", "X", "Y"), null)), 
									new DefaultCompoundSyntaxTree("g", "X")), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));

			expression = "product({{(on X) f(X, Y)}}) * product({{(on Z) g(Z)}})";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "X"), 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), null)), 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "Z"), 
									new DefaultCompoundSyntaxTree("g", "Z"), null))));

			expression = "product({{ (on X) (f(X, Y) * g(X)) }})";
			test(expression, new DefaultCompoundSyntaxTree("product", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("*", 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), 
									new DefaultCompoundSyntaxTree("g", "X")), null)));

			expression = "product({{(on X, Y, V) f(X, Y, V) | X != a}}) * product({{(on W, Z) g(Z, W) | Z != a}})";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("kleene list", "X", "Y", "V")), 
									new DefaultCompoundSyntaxTree("f", "X", "Y", "V"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "X", "a")))), 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("kleene list", "W", "Z")), 
									new DefaultCompoundSyntaxTree("g", "Z", "W"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "Z", "a"))))));

			expression = "product({{ ( on X ) (product({{ ( on Y ) (product({{ ( on V ) f(X, Y, V) }}) * g(X, Y)) }})) | X != a }})";
			test(expression, new DefaultCompoundSyntaxTree("product", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("product", 
									new DefaultCompoundSyntaxTree("{{ . . . }}", 
											new DefaultCompoundSyntaxTree("( on . )", "Y"), 
											new DefaultCompoundSyntaxTree("*", 
													new DefaultCompoundSyntaxTree("product", 
															new DefaultCompoundSyntaxTree("{{ . . . }}", 
																	new DefaultCompoundSyntaxTree("( on . )", "V"), 
																	new DefaultCompoundSyntaxTree("f", "X", "Y", "V"), null)), 
													new DefaultCompoundSyntaxTree("g", "X", "Y")), null)), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));

			expression = "sum({{(on X) f(X, Y) * g(Y) | X != a}})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("*", 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), 
									new DefaultCompoundSyntaxTree("g", "Y")), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));

			expression = "g(Y) * sum({{(on X) f(X, Y) | X != a}})";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("g", "Y"), 
					new DefaultCompoundSyntaxTree("sum", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "X"), 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"))))));

			expression = "sum({{(on X) f(X, Y) * g(Y) * h() | X != a}})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("*", 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), 
									new DefaultCompoundSyntaxTree("g", "Y"), 
									new DefaultCompoundSyntaxTree("h")), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));

			expression = "g(Y) * h() * sum({{(on X) f(X, Y) | X != a}})";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("g", "Y"), 
					new DefaultCompoundSyntaxTree("h"), 
					new DefaultCompoundSyntaxTree("sum", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "X"), 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"))))));

			expression = "sum({{(on X) g(Y) | X != a}})";
			test(expression, new DefaultCompoundSyntaxTree("sum", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", "X"), 
							new DefaultCompoundSyntaxTree("g", "Y"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("!=", "X", "a")))));

			expression = "g(Y) * | type(X) - { a } |";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("g", "Y"), 
					new DefaultCompoundSyntaxTree("| . |", 
							new DefaultCompoundSyntaxTree("-", 
									new DefaultCompoundSyntaxTree("type", "X"), 
									new DefaultCompoundSyntaxTree("{ . }", "a")))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y }}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, new DefaultCompoundSyntaxTree("*", 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
									new DefaultCompoundSyntaxTree("f", "X", "Y"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("and", 
													new DefaultCompoundSyntaxTree("!=", "X", "a"), 
													new DefaultCompoundSyntaxTree("=", "X", "Y"))))), 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", "Z"), 
									new DefaultCompoundSyntaxTree("g", "Z"), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("!=", "Z", "a"))))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y and W = a}})";
			test(expression, new DefaultCompoundSyntaxTree("product", 
					new DefaultCompoundSyntaxTree("{{ . . . }}", 
							new DefaultCompoundSyntaxTree("( on . )", 
									new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
							new DefaultCompoundSyntaxTree("f", "X", "Y"), 
							new DefaultCompoundSyntaxTree("|", 
									new DefaultCompoundSyntaxTree("and", 
											new DefaultCompoundSyntaxTree("!=", "X", "a"), 
											new DefaultCompoundSyntaxTree("=", "X", "Y"), 
											new DefaultCompoundSyntaxTree("=", "W", "a"))))));

			expression = "X + product({{(on X, Y) |{(on W) W }|  |  X != a and X = Y}})";
			test(expression, new DefaultCompoundSyntaxTree("+", "X", 
					new DefaultCompoundSyntaxTree("product", 
							new DefaultCompoundSyntaxTree("{{ . . . }}", 
									new DefaultCompoundSyntaxTree("( on . )", 
											new DefaultCompoundSyntaxTree("kleene list", "X", "Y")), 
									new DefaultCompoundSyntaxTree("| . |", 
											new DefaultCompoundSyntaxTree("{ . . . }", 
													new DefaultCompoundSyntaxTree("( on . )", "W"), "W", null)), 
									new DefaultCompoundSyntaxTree("|", 
											new DefaultCompoundSyntaxTree("and", 
													new DefaultCompoundSyntaxTree("!=", "X", "a"), 
													new DefaultCompoundSyntaxTree("=", "X", "Y")))))));

			expression = "if X = a then type(X) else nothing";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("type", "X"), "nothing"));

			expression = "if X = a then f(X, type(X)) else nothing";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("f", "X", 
							new DefaultCompoundSyntaxTree("type", "X")), "nothing"));

			expression = "if X = a then f(a, type(X)) else nothing";
			test(expression, new DefaultCompoundSyntaxTree("if . then . else .", 
					new DefaultCompoundSyntaxTree("=", "X", "a"), 
					new DefaultCompoundSyntaxTree("f", "a", 
							new DefaultCompoundSyntaxTree("type", "X")), "nothing"));

			expression = "f(X,g())";
			test(expression, new DefaultCompoundSyntaxTree("f", "X", 
					new DefaultCompoundSyntaxTree("g")));

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
