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

import static com.sri.ai.util.Util.list;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultSyntaxLeaf;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.test.grinder.AbstractParserTest;

public class AntlrGrinderParserTest extends AbstractParserTest {

	private Collection<FunctionSignature> oldParserFunctionSignatures;

	public AntlrGrinderParserTest () {
		parser = new AntlrGrinderParserWrapper();
	}
	
	/**
	 * 
	 */
	protected void popParserFunctionSignatures() {
		((AntlrGrinderParserWrapper) parser).setRandomPredicatesSignatures(oldParserFunctionSignatures);
	}

	/**
	 * @param newParserFunctionSignatures
	 */
	protected void pushParserFunctionSignatures(List<FunctionSignature> newParserFunctionSignatures) {
		oldParserFunctionSignatures = ((AntlrGrinderParserWrapper) parser).getRandomPredicatesSignatures();
		((AntlrGrinderParserWrapper) parser).setRandomPredicatesSignatures(newParserFunctionSignatures);
	}

	@Test
	public void testSymbol () {
		String string;
		string = "";
		test(string, null);

		string = "e";
		test(string, Expressions.makeSymbol("e"));

		string = "3";
		test(string, Expressions.makeSymbol(3));

		string = "3e7";
		test(string, Expressions.makeSymbol(30000000));

		string = "3e-1";
		test(string, Expressions.makeSymbol("0.30"));

		string = ".3e7";
		test(string, Expressions.makeSymbol(3000000));

		string = ".3e-1";
		test(string, Expressions.makeSymbol("0.030"));

		string = "1.3e7";
		test(string, Expressions.makeSymbol(13000000));

		string = "1.3e-1";
		test(string, Expressions.makeSymbol("0.130"));

		string = "false";
		test(string, Expressions.makeSymbol(false));

		string = "keyword";
		test(string, Expressions.makeSymbol("keyword"));

		string = "Keyword";
		test(string, Expressions.makeSymbol("Keyword"));

		string = "foo";
		test(string, Expressions.makeSymbol("foo"));

		string = "foo1";
		test(string, Expressions.makeSymbol("foo1"));

		string = "foo10bar";
		test(string, Expressions.makeSymbol("foo10bar"));

		string = "1foo";
		test(string, Expressions.makeSymbol("1foo"));

		string = "'foo1'";
		test(string, Expressions.makeSymbol("foo1"));

		string = "foo1'";
		test(string, Expressions.makeSymbol("foo1'"));

		string = "foo1'''";
		test(string, Expressions.makeSymbol("foo1'''"));

		string = "'This is a test.'";
		test(string, Expressions.makeSymbol("This is a test."));

		string = "\"This is a test.\"";
		test(string, Expressions.makeSymbol("This is a test."));

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
		test(string, Expressions.makeSymbol("Test a"));
		
		string = "\"Test \\u0061\"";
		test(string, Expressions.makeSymbol("Test a"));

		string = "'Testing the  preservation \\t of	whitespace\\ncharacters.'";
		test(string, Expressions.makeSymbol("Testing the  preservation 	 of	whitespace\ncharacters."));

		string = "\"Testing the  preservation \\t of	whitespace\\ncharacters.\"";
		test(string, Expressions.makeSymbol("Testing the  preservation 	 of	whitespace\ncharacters."));

		string = "'This is a test *()#@$%!-_=+<>,./?:;\\'\"\\\"\\\\'";
		test(string, Expressions.makeSymbol("This is a test *()#@$%!-_=+<>,./?:;'\"\"\\"));

		string = "\"This is a test *()#@$%!-_=+<>,./?:;\\''\\\"\\\"\\\\\"";
		test(string, Expressions.makeSymbol("This is a test *()#@$%!-_=+<>,./?:;''\"\"\\"));
		
		string = "foo(bar1', 'bar2\\'', bar3''')";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "bar1'", "bar2'", "bar3'''"));
	}
	
	@Test 
	public void testComment () {
		String string;
		string = "3";
		test(string, Expressions.makeSymbol(3));

		string = "3 // This is a test.\n";
		test(string, Expressions.makeSymbol(3));

		string = "// This is a test.\n 3";
		test(string, Expressions.makeSymbol(3));

		string = "3 // This is a test.";
		test(string, Expressions.makeSymbol(3));

		string = "3 // This is a test.\n + 4";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 3, 4));

		string = "// Test\n 3 // This is a test.\n + 4 // Test";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 3, 4));

		string = "3 /* This is a test. */";
		test(string, Expressions.makeSymbol(3));

		string = "/* This is a test. */ 3";
		test(string, Expressions.makeSymbol(3));

		string = "3 /* This is a test. */ + 4";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 3, 4));
	}

	@Test
	public void testParen () {
		String string;
		string = "(foo)";
		test(string, Expressions.makeSymbol("foo"));

		string = "(foo1)";
		test(string, Expressions.makeSymbol("foo1"));

		string = "(foo10bar)";
		test(string, Expressions.makeSymbol("foo10bar"));

		string = "(1foo)";
		test(string, Expressions.makeSymbol("1foo"));

		string = "('foo1')";
		test(string, Expressions.makeSymbol("foo1"));

		string = "(foo1')";
		test(string, Expressions.makeSymbol("foo1'"));

		string = "(foo1''')";
		test(string, Expressions.makeSymbol("foo1'''"));

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
	public void testCartesianProduct() {
		String string;
		string = "A x B";
		test(string, Expressions.apply("x", "A", "B"));
		
		string = "A x B x C";
		test(string, Expressions.apply("x", "A", "B", "C"));

		string = "A x x";
		testFail(string);
	}
	
	@Test
	public void testFunctionType() {
		String string;
		string = "A -> B";
		test(string, Expressions.apply("->", "A", "B"));
		
		string = "A x B -> C";
		test(string, Expressions.apply("->", Expressions.apply("x", "A", "B"), "C"));

		string = "A -> B x C";
		testFail(string);
	}
	
	
	@Test
	public void testFunction () {
		String string;
		string = "foo()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo"));

		string = "foo(1)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", 1));

		string = "foo(1, 2, 3)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", 1, 2, 3));

		string = "foo(bar)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "bar"));

		string = "foo(bar1, bar2, bar3)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "bar1", "bar2", "bar3"));

		string = "foo(1+2, bar in hello)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 1, 2),
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "bar", "hello")));

		string = "foo(1+2, (bar in hello))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 1, 2),
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "bar", "hello")));

		string = "'foo bar'()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo bar"));

		string = "'foo bar'(a)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo bar", "a"));

		string = "'foo bar'(a, b, c)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo bar", "a", "b", "c"));
		
		string = "foo(bar(X))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", "X")));
		
		string = "foo(bar(X), baz(Y))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("baz", "Y")));
		
		string = "foo(W, bar(X), Y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "W", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), "Y"));

		string = "(lambda x : y)(a, b, c)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", "x", "y"), "a", "b", "c"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar")));

		string = "(x, y, z)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z")));

		string = "(a in b, x + y + z, i, j, k)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "a", "b"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y", "z"), 
						"i", "j", "k")));

		string = "'( . )'()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )"));

		string = "'( . )'(a)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", "a"));

		string = "'( . )'(a, b, c)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", "a", "b", "c"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2"));

		string = "1 + 2 + 3";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3"));

		string = "1 + 2 + 3 + 4";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4"));

		string = "1 + 2 + 3 + 4 + 5";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4", "5"));

		string = "1 + 2 + 3 + 4 + 5 + 6";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4", "5", "6"));

		string = "1 + 2 + 3 + 4 + 5 + 6 + 7";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4", "5", "6", "7"));
	}

	@Test
	public void testAssocTimes() throws IOException {
		String string;
		string = "1 * 2";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2"));

		string = "1 * 2 * 3";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3"));

		string = "1 * 2 * 3 * 4";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3", "4"));

		string = "1 * 2 * 3 * 4 * 5";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3", "4", "5"));

		string = "1 * 2 * 3 * 4 * 5 * 6";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3", "4", "5", "6"));

		string = "1 * 2 * 3 * 4 * 5 * 6 * 7";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocUnion() throws IOException {
		String string;
		string = "1 union 2";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2"));

		string = "1 union 2 union 3";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3"));

		string = "1 union 2 union 3 union 4";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3", "4"));

		string = "1 union 2 union 3 union 4 union 5";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3", "4", "5"));

		string = "1 union 2 union 3 union 4 union 5 union 6";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3", "4", "5", "6"));

		string = "1 union 2 union 3 union 4 union 5 union 6 union 7";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocEqual() throws IOException {
		String string;
		string = "1 = 2";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2"));

		string = "1 = 2 = 3";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3"));

		string = "1 = 2 = 3 = 4";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3", "4"));

		string = "1 = 2 = 3 = 4 = 5";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3", "4", "5"));

		string = "1 = 2 = 3 = 4 = 5 = 6";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3", "4", "5", "6"));

		string = "1 = 2 = 3 = 4 = 5 = 6 = 7";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocAnd() throws IOException {
		String string;
		string = "1 and 2";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2"));

		string = "1 and 2 and 3";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3"));

		string = "1 and 2 and 3 and 4";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3", "4"));

		string = "1 and 2 and 3 and 4 and 5";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3", "4", "5"));

		string = "1 and 2 and 3 and 4 and 5 and 6";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3", "4", "5", "6"));

		string = "1 and 2 and 3 and 4 and 5 and 6 and 7";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "1", "2", "3", "4", "5", "6", "7"));
	}	

	@Test
	public void testAssocOr() throws IOException {
		String string;
		string = "1 or 2";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2"));

		string = "1 or 2 or 3";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3"));

		string = "1 or 2 or 3 or 4";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4"));

		string = "1 or 2 or 3 or 4 or 5";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4", "5"));

		string = "1 or 2 or 3 or 4 or 5 or 6";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4", "5", "6"));

		string = "1 or 2 or 3 or 4 or 5 or 6 or 7";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4", "5", "6", "7"));
	}
	
	@Test
	public void testAssoc () {
		String string;
		string = "(1 + 2 + 3 + 4) * (5 + 6 + 7 + 8) * (9 + 10 + 11 + 12)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "5", "6", "7", "8"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "9", "10", "11", "12")));

		string = "(1 + 2 + 3 + 4) * (5 + 6 + 7 + 8) * (9 + 10 + 11 + (12 * 13 * 14 * 15))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2", "3", "4"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "5", "6", "7", "8"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "9", "10", "11", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "12", "13", "14", "15"))));

		string = "(1 or 2 or 3 or 4) and (5 or 6 or 7 or 8) and (9 or 10 or 11 or (12 and 13 and 14 and 15))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "1", "2", "3", "4"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "5", "6", "7", "8"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "9", "10", "11", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "12", "13", "14", "15"))));

		// Testing to make sure the associative node walker doesn't get confused by the function form of
		// the operators.
		string = "+(+(b, c))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "b", "c")));

		string = "+(+(b))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "b")));

		string = "+(a, +(+(b)))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "a", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "b"))));

		string = "+(a, +(+(b, c)))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "a", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "b", "c"))));

		string = "+({a}, +(+({b}, {c})))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")))));

		string = "union({a}, union(union({b}, {c})))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")))));

		string = "union({a}, union(union({b}, {c}), {d}))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"))));

		string = "union({a}, union(union({b}, {c}), {d}, {E}))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "E"))));

		string = "union({a}, union(union({b}, {c}), {d}, {E}, {f}))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c")), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "E"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "f"))));
	}

	@Test
	public void testExpressionSymbol () {
		String string;
		
		boolean oldValue = DefaultSyntaxLeaf.setDontAcceptSymbolValueToBeExpression(false);

		string = "<x>";
		test(string, Expressions.makeSymbol(Expressions.makeSymbol("x")));

		string = "<foo''>";
		test(string, Expressions.makeSymbol(Expressions.makeSymbol("foo''")));

		string = "<x in y>";
		test(string, Expressions.makeSymbol(
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y")));

		string = "<{(on x, y) f(x,y) | y}>";
		test(string, Expressions.makeSymbol(
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "x", "y"), 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "y"))));

		string = "< x < y >";
		test(string, Expressions.makeSymbol(
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "x", "y")));

		string = "<x>y>";
		test(string, Expressions.makeSymbol(
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y")));

		string = "< x > y >";
		test(string, Expressions.makeSymbol(
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y")));

		string = "< if x > y then x else y >";
		test(string, Expressions.makeSymbol(
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y"), "x", "y")));
		
		string = "if <x> > <y> then x else y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">",
						Expressions.makeSymbol(Expressions.makeSymbol("x")),
						Expressions.makeSymbol(Expressions.makeSymbol("y"))
						),
				"x", "y"));

		// Test for illegal strings.
		string = "< x";
		testFail(string);

		string = " x >";
		testFail(string);

		DefaultSyntaxLeaf.setDontAcceptSymbolValueToBeExpression(oldValue);
	}

	@Test
	public void testCardinality () {
		String string;
		
		string = "|foo|";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", "foo"));

		string = "| foo in bar |";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "foo", "bar")));

		string = "| {} |";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

		string = "| { foo, bar } |";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar"))));

		string = "| ({ foo, bar }) |";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar"))));

		string = "| 1 + 2 |";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 1, 2)));

		string = "'| . |'()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |"));

		string = "'| . |'(a)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", "a"));

		string = "'| . |'(a, b, c)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", "a", "b", "c"));

		// Testing illegal strings.
		string = "| 1, 2, |";
		testFail(string);

		string = "| 1, 2 | 3";
		testFail(string);
	}

	@Test
	public void testMultiset () {
		List<FunctionSignature> functionSignatures = list();
		pushParserFunctionSignatures(functionSignatures);
		
		String string;
		
		string = "{{ foo | bar }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", null, "foo", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "bar")));

		string = "{{ ( on ) ([ if X then 1 else 0 ]) }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", AntlrGrinderParserTest.makeScopingSyntaxTree(new ArrayList<Expression>()), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
						functionSignatures,
						"[ . ]",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "X", "1", "0")), null));
		
		string = "{{ foo }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "foo"));

		string = "{{ (on foo) bar }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "foo"), "bar", null));

		string = "{{ (on x in y) z }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y")), "z", null));

		string = "{{ foo | bar }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", null, "foo", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "bar")));

		string = "{{f(X) | false}}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", null, 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "false")));
		
		string = "{{ [if p(a) then 1 else 0] | true }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", null, 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
						functionSignatures,
						"[ . ]", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a"), "1", "0")), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "true")));

		string = "{{ (on foo, fooz) bar }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "fooz")), 
						"bar", null));

		string = "{{ (on foo, fooz) bar | barz }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "fooz")), 
						"bar", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "barz")));

		string = "{{ foo, bar, foo + bar }}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "foo", "bar"))));

		string = "{{}}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

		string = "'{{ . }}'()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}"));

		string = "'{{ . }}'(a)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "a"));

		string = "'{{ . }}'(a, b, c)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "a", "b", "c"));

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

		popParserFunctionSignatures();
	}

	@Test
	public void testSet () {
		String string;	
		
		string = "{ ( on ) p(X, X) | true }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", AntlrGrinderParserTest.makeScopingSyntaxTree(new ArrayList<Expression>()), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "X"), Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, "true")));

		string = "{ ( on ) X | true }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", AntlrGrinderParserTest.makeScopingSyntaxTree(new ArrayList<Expression>()),  
				"X", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, "true")));
				
		string = "{ a | true}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", null, "a", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(IntensionalSet.CONDITION_LABEL, "true")));
		
		string = "{ foo }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "foo"));

		string = "{ (on foo) bar }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "foo"), "bar", null));

		string = "{ (on x in y) z }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y")), "z", null));

		string = "{ foo | bar }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", null, "foo", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "bar")));

		string = "{f(X) | false}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", null, 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "false")));
		
		List<FunctionSignature> functionSignatures = list(new FunctionSignature("p/1"));
		pushParserFunctionSignatures(functionSignatures);
		string = "{ [if p(a) then 1 else 0] | true }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", null, 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
						functionSignatures,
						"[ . ]", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a"), "1", "0")), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "true")));
		popParserFunctionSignatures();

		string = "{ (on foo, fooz) bar }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "fooz")), 
						"bar", null));

		string = "{ (on foo, fooz) bar | barz }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "fooz")), 
						"bar", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "barz")));

		string = "{ foo, bar, foo + bar }";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "foo", "bar", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "foo", "bar"))));

		string = "{}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

		string = "'{ . }'()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }"));

		string = "'{ . }'(a)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a"));

		string = "'{ . }'(a, b, c)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a", "b", "c"));

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

		List<FunctionSignature> functionSignatures = list();
		pushParserFunctionSignatures(functionSignatures);

		String string;
		string = "[ x in y ]";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
				functionSignatures,
				"[ . ]", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y")));

		string = "[ x+1 ]";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
				functionSignatures,
				"[ . ]", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", 1)));

		string = "[ (x, y) ]";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
				functionSignatures,
				"[ . ]", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));

		// The "function application" forms of bracketed expressions below are a relic of the time when
		// there was confusion between syntax trees and function applications,
		// and which AntlrGrinderParserWrapper still uses.
		// In time, this should not be valid anymore.

		string = "'[ . ]'(a)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
				functionSignatures,
				"[ . ]", "a"));

		string = "'[ . ]'(a, b, c)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
				functionSignatures,
				"[ . ]", "a", "b", "c"));

		// Testing illegal strings.
		string = "[]";
		testFail(string);

		string = "[";
		testFail(string);

		string = "[ x";
		testFail(string);

		string = "[ x, y ]";
		testFail(string);

		popParserFunctionSignatures();
	}

	@Test
	public void testNeighborFactor () {
		String string;
		string = "neighbors of factor x";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", "x"));

		List<FunctionSignature> functionSignatures = list();
		pushParserFunctionSignatures(functionSignatures);
		string = "neighbors of factor [x]";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
						functionSignatures,
						"[ . ]", "x")));
		popParserFunctionSignatures();

		string = "neighbors of factor {x}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x")));

		string = "neighbors of factor {{x}}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "x")));

		string = "'neighbors of factor'(a)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", "a"));

		string = "neighbors of factor neighbors of factor x";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of factor", "x")));
	}
	
	@Test
	public void testNeighborVariable () {
		String string;
		string = "neighbors of variable x";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", "x"));

		List<FunctionSignature> functionSignatures = list(new FunctionSignature("x/0"));
		pushParserFunctionSignatures(functionSignatures);
		string = "neighbors of variable [x]";
		test(string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
						"neighbors of variable", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(
								functionSignatures, "[ . ]", "x")));
		popParserFunctionSignatures();

		string = "neighbors of variable {x}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x")));

		string = "neighbors of variable {{x}}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "x")));

		string = "'neighbors of variable'(a)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", "a"));

		string = "neighbors of variable neighbors of variable x";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of variable", "x")));
	}

	@Test
	public void testNeighborOf () {
		String string;
		string = "neighbors of x from y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", "x", "y"));

		List<FunctionSignature> functionSignatures = list(new FunctionSignature("x/0"));
		pushParserFunctionSignatures(functionSignatures);
		string = "neighbors of [x] from y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTreesWithRandomPredicatesSignatures(functionSignatures, "[ . ]", "x"), "y"));
		popParserFunctionSignatures();

		string = "neighbors of {x} from y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "neighbors of {{x}} from y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", "x"), "y"));

		string = "'neighbors of . from .'(a, b)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", "a", "b"));

		string = "neighbors of neighbors of x from y from neighbors of a from b";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", "x", "y"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("neighbors of . from .", "a", "b")));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"));

		string = "not (x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"));

		string = "not";
		test(string, Expressions.makeSymbol("not"));

		string = "not()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not"));

		string = "not(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));

		string = "not {x, y, z}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z"))));

		string = "not (x + y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")));

		string = "not x + y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"), "y"));
	}

	@Test
	public void testNegative () {
		String string;
		
		string = "-3";
		test(string, Expressions.makeSymbol("-3"));
		
		string = "-3e7";
		test(string, Expressions.makeSymbol(-30000000));

		string = "-3e-1";
		test(string, Expressions.makeSymbol("-0.30"));

		string = "-.3e7";
		test(string, Expressions.makeSymbol("-3000000"));

		string = "-.3e-1";
		test(string, Expressions.makeSymbol("-0.030"));

		string = "-1.3e7";
		test(string, Expressions.makeSymbol(-13000000));

		string = "-1.3e-1";
		test(string, Expressions.makeSymbol("-0.130"));
		
		string = "-x";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

		string = "- x";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

		string = "--x";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x")));

		string = "-(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

		string = "-";
		test(string, Expressions.makeSymbol("-"));

		string = "-()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-"));

		string = "-(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));

		string = "-x - y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x"), "y"));

		string = "x - -y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "y")));

		string = "--x";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x")));

		string = "-(-x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x")));

		string = "- {x, y, z}";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z"))));

		string = "- x + y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x"), "y"));

		string = "- (x + y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")));

		string = "(- x) + y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x"), "y"));
	}

	@Test
	public void testExponentiation() {
		String string;
		string = "x ^ y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "x", "y"));

		string = "x^y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "x", "y"));

		string = "w ^ x ^ y ^ z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "w",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^",  "x",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "y", "z"))));

		string = "x ^ y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "x", "y"), "z"));

		string = "{x} ^ y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} ^ y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")),	"y"));

		string = "^(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "x"));

		string = "^";
		test(string, Expressions.makeSymbol("^"));

		string = "^()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^"));

		string = "^(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", "x", "y"));

		string = "x/y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", "x", "y"));
		
		string = "w / x / y / z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/",  
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", "w", "x"), "y"), "z"));

		string = "x / y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", "x", "y"), "z"));

		string = "{x} / y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} / y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")),
				"y"));

		string = "/(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", "x"));

		string = "/";
		test(string, Expressions.makeSymbol("/"));

		string = "/()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/"));

		string = "/(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"));

		string = "x*y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"));

		string = "w * x * y * z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
				"w", "x", "y", "z"));

		string = "x * y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"), "z"));

		string = "{x} * y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} * y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "*(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x"));

		string = "*";
		test(string, Expressions.makeSymbol("*"));

		string = "*()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*"));

		string = "*(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x", "y"));
		
		string = "x-y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x", "y"));
		
		string = "2-0";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "2", "0"));
		
		string = "w - x - y - z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",  
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "w", "x"), "y"), "z"));

		string = "x - y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x", "y"), "z"));

		string = "{x} - y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} - y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "-(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

		string = "-";
		test(string, Expressions.makeSymbol("-"));

		string = "-()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-"));

		string = "-(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));
	}

	@Test
	public void testPlus () {
		String string;
		string = "x + y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y"));

		string = "x+y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y"));

		string = "1+2";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2"));

		string = "1+-2";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", 
				Expressions.makeSymbol("-2")));

		string = "w + x + y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				"w", "x", "y", "z"));

		string = "{x} + y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} + y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "+(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x"));

		string = "+";
		test(string, Expressions.makeSymbol("+"));

		string = "+()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+"));

		string = "+(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("intersection", "x", "y"));

		string = "w intersection x intersection y intersection z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("intersection", "w", "x", "y", "z"));

		string = "{x} intersection y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("intersection", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x intersection y} intersection y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("intersection", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("intersection", "x", "y")), "y"));

		string = "intersection(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("intersection", "x"));

		string = "intersection";
		test(string, Expressions.makeSymbol("intersection"));
		
		string = "intersection * 8";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "intersection", "8"));

		string = "intersection()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("intersection"));

		string = "intersection(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("intersection", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "x", "y"));

		string = "w union x union y union z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "w", "x", "y", "z"));

		string = "x union y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} union y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} union y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "union(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "x"));

		string = "union";
		test(string, Expressions.makeSymbol("union"));

		string = "union()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union"));

		string = "union(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y"));
		
		string = "w in x in y in z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in",  
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "w", "x"), "y"), "z"));

		string = "x in y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} in y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} in y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "in(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x"));

		string = "in";
		test(string, Expressions.makeSymbol("in"));

		string = "in()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in"));

		string = "in(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "x", "y"));
		
		string = "x<=y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "x", "y"));
		
		string = "w <= x <= y <= z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=",  
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "w", "x"), "y"), "z"));

		string = "x <= y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} <= y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} <= y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "<=(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "x"));

		string = "<=";
		test(string, Expressions.makeSymbol("<="));

		string = "<=()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<="));

		string = "<=(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "x", "y"));
		
		string = "x<y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "x", "y"));
		
		string = "w < x < y < z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<",  
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "w", "x"), "y"), "z"));

		string = "x < y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} < y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} < y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "'<'(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "x"));

		string = "'<'";
		test(string, Expressions.makeSymbol("<"));

		string = "'<'()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<"));

		string = "'<'(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "x", "y"));
		
		string = "x>=y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "x", "y"));
		
		string = "w >= x >= y >= z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=",  
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "w", "x"), "y"), "z"));

		string = "x >= y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} >= y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} >= y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")),
				"y"));

		string = ">=(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "x"));

		string = ">=";
		test(string, Expressions.makeSymbol(">="));

		string = ">=()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">="));

		string = ">=(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y"));
		
		string = "x>y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y"));
		
		string = "w > x > y > z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">",  
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "w", "x"), "y"), "z"));

		string = "x > y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} > y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} > y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "'>'(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "x"));

		string = "'>'";
		test(string, Expressions.makeSymbol(">"));

		string = "'>'()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">"));

		string = "'>'(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "x", "y"));
		
		string = "x!=y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "x", "y"));
		
		string = "w != x != y != z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=",  
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "w", "x"), "y"), "z"));

		string = "x != y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} != y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} != y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "!=(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "x"));

		string = "!=";
		test(string, Expressions.makeSymbol("!="));

		string = "!=()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!="));

		string = "!=(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "x", "y"));

		string = "x=y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "x", "y"));

		string = "w = x = y = z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
				"w", "x", "y", "z"));

		string = "x = y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} = y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} = y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "=(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "x"));

		string = "=";
		test(string, Expressions.makeSymbol("="));

		string = "=()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("="));

		string = "=(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "y", "z")));
		
		string = "x and y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x", "y"));

		string = "w and x and y and z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
				"w", "x", "y", "z"));

		string = "x and y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "x in y and y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y"), "y"));

		string = "{x} and y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} and y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "and(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x"));

		string = "and";
		test(string, Expressions.makeSymbol("and"));

		string = "and()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and"));

		string = "and(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "y", "z")));
		
		string = "x or y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y"));

		string = "w or x or y or z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
				"w", "x", "y", "z"));

		string = "x or y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "x in y or y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x", "y"), "y"));

		string = "{x} or y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} or y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "or(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x"));

		string = "or";
		test(string, Expressions.makeSymbol("or"));

		string = "or()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or"));

		string = "or(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>", "x", "y"));
		
		string = "x<=>y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>", "x", "y"));
		
		string = "w <=> x <=> y <=> z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>", "w", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>",  "x", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>", "y", "z"))));

		string = "x <=> y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} <=> y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} <=> y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")), "y"));

		string = "<=>(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>", "x"));

		string = "<=>";
		test(string, Expressions.makeSymbol("<=>"));

		string = "<=>()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>"));

		string = "<=>(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=>", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", "x", "y"));
		
		string = "x=>y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", "x", "y"));
		
		string = "w => x => y => z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", "w", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", "x", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", "y", "z"))));

		string = "x => y + z";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		string = "{x} => y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), "y"));

		string = "{x + y} => y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y")),
				"y"));

		string = "=>(x)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", "x"));

		string = "=>";
		test(string, Expressions.makeSymbol("=>"));

		string = "=>()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>"));

		string = "=>(x, y)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", "x", "y"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "a", "b"));
		
		string = "there exists a : there exists b : c";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "a", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "b", "c")));
		
		string = "'there exists . : .'(a, b)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "a", "b"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "x", "y"));

		string = "for all x = 5 : a";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "x", "5"), "a"));

		string = "for all x : for all y : for all z : true";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "x", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "y", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "z", "true"))));

		string = "for all Y : (for all X : ((X != Y) => (there exists W : (there exists Z : ((Z != a) => (Alpha = Beta))))))";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "Y", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "X", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "W", 
										Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "Z", 
												Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
														Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"), 
														Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => there exists W : there exists Z : (Z != a => Alpha = Beta)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "Y", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "X", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "W", 
										Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "Z", 
												Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
														Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"), 
														Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => (there exists W : there exists Z : Z != a => Alpha = Beta)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "Y", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "X", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "W", 
										Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "Z", 
												Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
														Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"), 
														Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Alpha", "Beta"))))))));

		string = "for all Y : for all X : X != Y => there exists W : there exists Z : Z != a => Alpha = Beta";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "Y", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all . : .", "X", 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y"), 
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "W", 
										Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("there exists . : .", "Z", 
												Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=>", 
														Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"), 
														Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Alpha", "Beta"))))))));
	}

	@Test
	public void testIfThenElse () {
		String string;
		string = "if a then b else c";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				"a", "b", "c"));

		string = "'if . then . else .'(a, b, c)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "a", "b", "c"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", "x", "a"));

		string = "lambda x, y, z : a";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z"), "a"));

		string = "lambda x, y, z : lambda a : b";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", "a", "b")));

		// Testing illegal strings
		string = "lambda a :";
		testFail(string);
	}

	@Test
	public void testMessage () {
		String string;
		string = "message to a from b";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("message to . from .", "a", "b"));

		string = "'message to . from .'(a, b)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("message to . from .", "a", "b"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("previous message to . from .", "a", "b"));

		string = "'previous message to . from .'(a, b)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("previous message to . from .", "a", "b"));

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
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "a", "a"), "a"));

		string = "a*a+a";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "a", "a"), "a"));

		string = "a+a*a";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "a",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "a", "a")));

		string = "-a*a+a";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "a"), "a"), "a"));

		string = "-a*-a-a";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "a"),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "a")), "a"));

		string = "-a*-a^a-a";
		test(string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "a"),
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^",
										Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "a"), "a")), "a"));

		string = "-a*-a^-a";
		test(string, 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "a"),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^",
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "a"),
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "a"))));
	}

	@Test
	public void testFunctionsAndSequences() {
		String string;
		string = "x+y";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+","x","y"));

		string = "f()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f"));

		string = "f(10)";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 10));

		string = "f(10 + a)";
		test(string, 								
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
								"+", 10, "a")));

		string = "f(10+a) - f(a, b)";
		test(string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f",
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 10, "a")),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")));
		
		string = "f(10)-f()";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 10),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f")));

		string = "10 + 10";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 10, 10));

		string = "10 + x*20";
		test(string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 10, 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x",20)));

		string = "f(10, 20, 30, f (x), f, (x))";
		test(string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 10, 20, 30, 
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "x"), "f", "x"));
	}
	
	
	@Test
	public void testGrinder () {
		String expression;
		expression = "if X = 1 then 0.0003 + 0.000000000001 else 0.150004 + 0.1 + 0.776699029126213691398561";
		test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "1"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "0.0003", "0.000000000001"), 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "0.150004", "0.1", "0.776699029126213691398561")));


			expression = "3";
			test(expression, Expressions.makeSymbol(3));

			expression = "x + y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y"));

			expression = "1 + 2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "2"));

			expression = "x + 2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "2"));

			expression = "+(x, 2, y, 6)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "2", "y", "6"));

			expression = "+(x, +(1, y), 11)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 1, "y"), "11"));

			expression = "+(x, 2, 1 + 2, 1 + y, 6)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", 2, 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 1, 2), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 1, "y"), "6"));

			expression = "+(x, 2, 3)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", 2, 3));

			expression = "+(x)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x"));

			expression = "+";
			test(expression, Expressions.makeSymbol("+"));

			expression = "1";
			test(expression, Expressions.makeSymbol(1));

			expression = "x";
			test(expression, Expressions.makeSymbol("x"));

			expression = "+()";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+"));

			expression = "3 - 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 3, 1));

			expression = "1 - 3";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 1, 3));

			expression = "X - Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "X", "Y"));

			expression = "X - 0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "X", 0));

			expression = "0 - X";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 0, "X"));

			expression = "-1";
			test(expression, Expressions.makeSymbol("-1"));

			expression = "-x";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", "x"));

			expression = "3";
			test(expression, Expressions.makeSymbol(3));

			expression = "x * y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", "y"));

			expression = "2 * 2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 2, 2));

			expression = "x * 2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", 2));

			expression = "*(x, 2, y, 6)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", 2, "y", 6));

			expression = "*(x, 0, y, 6)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", 0, "y", 6));

			expression = "*(x, 2, 1 * 2, 1 * y, 6)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", 2, 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 1, 2),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 1, "y"), 6));

			expression = "*(x, 2, 3)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", 2, 3));

			expression = "*(x)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x"));

			expression = "*";
			test(expression, Expressions.makeSymbol("*"));

			expression = "*()";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*"));

			expression = "3^2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", 3, 2));

			expression = "x^1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "x", 1));

			expression = "x^0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "x", 0));

			expression = "x^0.0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "x", "0.0"));

			expression = "1^n";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", 1, "n"));

			expression = "2^n";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", 2, "n"));

			expression = "4/2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 4, 2));

			expression = "4.0/2.0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", "4.0", "2.0"));

			expression = "4.0/3.0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", "4.0", "3.0"));

			expression = "a/b";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", "a", "b"));

			expression = "4/0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 4, 0));

			expression = "4/2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 4, 2));

			expression = "(4*2)/(2*3)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 4, 2),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 2, 3)));

			expression = "(4*2)/2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 4, 2), 2));

			expression = "2/2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("/", 2, 2));

			expression = "3 > 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 3, 1));

			expression = "3 > 3";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 3, 3));

			expression = "1 > 3";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 1, 3));

			expression = "1 > y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 1, "y"));

			expression = "1 > false";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 1, "false"));

			expression = "3 >= 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", 3, 1));

			expression = "3 >= 3";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", 3, 3));

			expression = "1 >= 3";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", 1, 3));

			expression = "1 >= y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", 1, "y"));

			expression = "1 >= false";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", 1, "false"));

			expression = "not (3 > 1)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 3, 1)));

			expression = "not(3 > 3)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 3, 3)));

			expression = "true and x";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "true", "x"));

			expression = "true";
			test(expression, Expressions.makeSymbol("true"));

			expression = "x and y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x", "y"));

			expression = "true and false";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "true", "false"));

			expression = "x and false";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x", "false"));

			expression = "x and false and y and true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
					"x", "false", "y", "true"));

			expression = "and(x, false, false and true, false and y, false)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x", "false", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "false", "true"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "false", "y"),
					"false"));

			expression = "and(x, true, false)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x", "true", "false")); 


			expression = "and(x)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x"));

			expression = "and";
			test(expression, Expressions.makeSymbol("and"));

			expression = "and()";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and")); 

			expression = "false or x";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "false", "x"));

			expression = "x or false";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", "false"));

			expression = "true";
			test(expression, Expressions.makeSymbol(true));

			expression = "x or y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y"));

			expression = "true or false";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "true", "false"));

			expression = "x or true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", "true"));

			expression = "x or false or y or true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
					"x", "false", "y", "true"));

			expression = "or()";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or")); 

			expression = "x or y or x or y or z";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
					"x", "y", "x", "y", "z"));

			expression = "x or x";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", "x"));

			expression = "not true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "true"));

			expression = "not false";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "false"));

			expression = "not x";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"));

			expression = "not x and y and x";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"), "y", "x"));

			expression = "not not x";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x")));

			expression = "'index of . in .'(b, f(a,b))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("index of . in .", "b", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")));

			expression = "'index of . in .'(c, f(a,b))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("index of . in .", "c", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")));

			expression = "'index of . in .'(c, f(a,b)) > 0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("index of . in .", "c", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")), "0"));

			expression = "x + 2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "2"));

			expression = "f(g(h(x)))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("h", "x"))));

			expression = "{(on x) x}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "x"), "x", null));

			expression = "x + {(on x) f(x)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "x"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "2 + {(on x) f(x)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 2,
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "x"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "x + {(on x in group(x)) f(x)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x",
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("group", "x"))),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "2 + {(on x in group(2)) f(x)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 2,
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "x",
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("group", 2))),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "x + {(on y) f(x)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "y"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "x"), null)));

			expression = "2 + {(on y) f(2)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 2,
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "y"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 2), null)));


			expression = "f(x)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "x"));

			expression = "f(f(x) + g(x))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "x"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "x"))));


			expression = "(1 + x + y)*(x + 3)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "1", "x", "y"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", 3)));

			expression = "1*x + 1*3 + x*x + x*3 + y*x + y*3";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 1, "x"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 1, 3),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", "x"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",	"x", 3),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "y", "x"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "y", 3)));

			// Testing case where one one of the operator applications is empty.
			// We use an evaluator with the distributive law alone so +() does not get evaluated to 0.
			expression = "+()*(x + 3)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", 3)));

			expression = "(1 * x * y)+(x * 3)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "1", "x", "y"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "x", 3)));

			expression = "not x";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"));

			expression = "not(x and y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "x", "y")));

			expression = "not x or not y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "y")));

			expression = "not(x or y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y")));

			expression = "not x and not y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "y")));

			expression = "not(x or y or z)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y", "z")));

			expression = "not x and not y and not z";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "y"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "z")));

			expression = "not(x or y or (z and w))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", "x", "y", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "z", "w"))));

			expression = "not x and not y and (not z or not w)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "x"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "y"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "z"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", "w"))));

			expression = "list(X)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "X"));

			expression = "list(X, Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "X", "Y"));

			expression = "list()";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list"));


			expression = "aConstantSymbol";
			test(expression, Expressions.makeSymbol("aConstantSymbol"));

			expression = "if A = B then aAndBEqual else aAndBNotEqual";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "A", "B"),
					"aAndBEqual", "aAndBNotEqual"));

			expression = "{(on X) X | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));



			expression = "if true then 1 else 2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					"true", 1, 2));

			expression = "if false then 1 else 2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					"false", 1, 2));

			expression = "if X then 1 else 2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					"X", 1, 2));


			expression = "f(a, b, c, if Y = 2 then X else X + 1, d, e, f)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b", "c",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", 2), "X",
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)), 
							"d", "e", "f"));
			expression = "if Y = 2 then f(a, b, c, X, d, e, f) else f(a, b, c, X + 1, d, e, f)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "2"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b", "c", "X", "d", "e", "f"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b", "c", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1), "d", "e", "f")));

			expression = "{(on X) if Y = 2 then X else X + 1 | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", 2), "X", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "if Y = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", 2),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "{(on X) if X = 2 then X else X + 1 | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", 2), "X", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) if p(Y) = 2 then X else X + 1 | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "Y"), 2), "X", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "if p(Y) = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "Y"), 2),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));


			expression = "{(on X) if p(X) = 2 then X else X + 1 | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 2), "X", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", 1)),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X in (if Y = a then Set1 else Set2)) p(X) | X != a}"; // lack of ()'s around if then else makes parse fail, not sure why. Entered in bug database.
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a"),
											"Set1", "Set2"))),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

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
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )",
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", "Set1")),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }",
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )",
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", "Set2")),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "{(on Y, X in (if Y = a then Set1 else Set2)) p(X) | X != a}"; // lack of ()'s around if then else makes parse fail, not sure why. Entered in bug database.
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "Y",
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a"),
													"Set1", "Set2")))),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "if X = a then X != a else X = Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"),
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")));

			expression = "if X = a then false else X = Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .",
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"),
					"false", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")));

			expression = "if X = a then f(X != a, 1, 2) else g(X = Y, a, b)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), "1", "2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), "a", "b")));

			expression = "if X = a then f(false, 1, 2) else g(X = Y, a, b)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "false", "1", "2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), "a", "b")));

			expression = "if X != a or Y != b then X != a else false";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), "false"));

			expression = "if X != a or Y != b then X != a or Y != b or Z != c else false";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c")), "false"));

			expression = "X != a or Y != b";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")));

			expression = "if X != a then X != a and Y = d else true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "d")), "true"));

			expression = "if X != a then Y = d else true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "d"), "true"));

			expression = "if X != a or Y != b then f(X != a or Y != b or Z != c) else true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c"))), "true"));

			expression = "if X != a or Y != b then f(true) else true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "true"), "true"));

			expression = "if X != a or Y != b then not(X != a or Y != b or Z != c) else true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c"))), "true"));

			expression = "if X != a or Y != b then false else true";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), "false", "true"));

			expression = "if A and B then if A then 1 else 0 else 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "A", "B"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "A", "1", "0"), "1"));

			expression = "1";
			test(expression, Expressions.makeSymbol("1"));

			expression = "if X = a then if p(X) then E1 else E2 else if p(X) then E1 else E2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), "E1", "E2")));

			expression = "if X = a then if p(a) then E1 else E2 else if p(X) then E1 else E2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a"), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X, Y) then E1 else E2 else if p(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a, b) then E1 else E2 else if p(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a", "b"), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X) and q(Y) then E1 else E2 else if p(X) and q(Y) then E1 else E2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "Y")), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a) and q(b) then E1 else E2 else if p(X) and q(Y) then E1 else E2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "b")), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(X) and q(X, Y) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2")));

			expression = ("if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2");
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "a", "b")), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2")));

			expression = "if even(X) then f(X) else X + 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1")));

			expression = "if X = a then 1 else 2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), "1", "2"));

			expression = "if X = a then f(X) else X + 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1")));

			expression = "if X = a then f(a) else X + 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1")));

			expression = "if X = a and Y = b then f(X,Y) else X + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = a and Y = b then f(a, b) else X + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a and Y = W then f(X,Y,Z) else X + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "W")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a and W = Y then f(a, W, a) else X + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "W", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a and W != Y then f(X,W,Z) else X + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a and W != Y then f(a, W, a) else X + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "W", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z and W != Y then f(X,W,Z) else X + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z and W != Y then f(X,W,Z) + {(on Z) foo(Z)} else Z + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), null)), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "Z", "Y")));

			expression = "if X = Z and W != Y then f(X, W, X) + {(on Z) foo(X)} else Z + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), null)), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "Z", "Y")));

			expression = "if X = Z = a or W != Y then f(X,W,Z) else X + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X = Z = a or W != Y then f(X,W,Z) else X + W";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "W")));

			expression = "if X != a then {(on X) X != a} else {(on Y) X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null)));

			expression = "if X != a then {(on X) X != a} else {(on Y) false}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), "false", null)));

			expression = "if X != a then X + 1 else 42";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1"), "42"));

			expression = "if X != a then X + 1 else f(X)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X")));

			expression = "if X != a then X + 1 else f(a)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "1"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a")));

			expression = "if X != a or Y != b then X + Y else f(X,Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y")));

			expression = "if X != a or Y != b then  X + Y else f(a, b)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", "b")));

			expression = "if X != Z or W != Y then f(X,W,Z) else foo(X, Z, W, Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Z"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Z", "W", "Y")));

			expression = "if X != Z or W != Y then f(X, W, Z) else foo(X, X, W, W)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("or", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Z"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "X", "W", "W")));

			expression = "if X != a and W != Y then f(X,W,Z) else X + Y";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "W", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "W", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", "Y")));

			expression = "if X != a then {(on X) X != a} else 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null), "1"));

			expression = "if X = a then {(on Y) X != a} else {(on X) X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null)));

			expression = "if X = a then {(on Y) false} else {(on X) X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), "false", null), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), null)));

			expression = "if X < 3 then f(X < 2, X < 3, X < 4) else g(X < 2, X < 3, X < 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4"))));

			expression = "if X < 3 then f(X < 2, true, true)     else g(false, false, X < 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), "true", "true"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false", "false", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4"))));

			expression = "if X < 3 then f(X <= 2, X <= 3, X <= 4) else g(X <= 2, X <= 3, X <= 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4"))));

			expression = "if X < 3 then f(X <= 2, true, true) else g(false, X <= 3, X <= 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), "true", "true"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4"))));

			expression = "if X < 3 then f(X > 2, X > 3, X > 4) else g(X > 2, X > 3, X > 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4"))));

			expression = "if X < 3 then f(X > 2, false, false) else g(true, X > 3, X > 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), "false", "false"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "true", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4"))));

			expression = "if X < 3 then f(X >= 2, X >= 3, X >= 4) else g(X >= 2, X >= 3, X >= 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4"))));

			expression = "if X < 3 then f(X >= 2, false, false) else g(true, true, X >= 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), "false", "false"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "true", "true", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4"))));

			expression = "if X <= 3 then f(X < 2, X < 3, X < 4) else g(X < 2, X < 3, X < 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4"))));

			expression = "if X <= 3 then f(X < 2, X < 3, true)     else g(false, false, X < 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "3"), "true"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false", "false", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<", "X", "4"))));

			expression = "if X <= 3 then f(X <= 2, X <= 3, X <= 4) else g(X <= 2, X <= 3, X <= 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4"))));

			expression = "if X <= 3 then f(X <= 2, true, true) else g(false, false, X <= 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "2"), "true", "true"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false", "false", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "4"))));

			expression = "if X <= 3 then f(X > 2, X > 3, X > 4) else g(X > 2, X > 3, X > 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4"))));

			expression = "if X <= 3 then f(X > 2, false, false) else g(true, true, X > 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "2"), "false", "false"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "true", "true", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">", "X", "4"))));

			expression = "if X <= 3 then f(X >= 2, X >= 3, X >= 4) else g(X >= 2, X >= 3, X >= 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4"))));

			expression = "if X <= 3 then f(X >= 2, X >= 3, false) else g(true, true, X >= 4)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("<=", "X", "3"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "3"), "false"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "true", "true", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(">=", "X", "4"))));

			expression = "if even(X) then f(even(X)) else g(even(X))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"))));

			expression = "if even(X) then f(true) else g(false)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "true"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if even(X) then f(even(Y)) else g(even(X))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"))));

			expression = "if even(X) then f(if Y = X then true else even(Y)) else g(false)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "X"), "true", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "Y"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if even(X) then {(on even(a)) f(even(Y))} else g(even(X))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "a")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "Y")), null), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"))));

			expression = "if even(X) then {(on even(a)) f(if Y != a and Y = X then true else even(Y))} else g(false)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "a")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "a"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "X")), "true", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("even", "Y"))), null), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if X = Y then f(X = Y) else g(X = Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))));

			expression = "if X = Y then f(true) else g(false)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "true"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if X = Y then f(X = Z) else g(X = Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))));

			expression = "if X = Y then f(X = Z) else g(false)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if X = Y then {(on X = a) f(X = Y)} else g(X = Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")), null), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))));

			expression = "if X = Y then {(on X = a) f(X = Y)} else g(false)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")), null), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "false")));

			expression = "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "a", "b")), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2")));

			expression = "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "a", "b")), "E1", "E2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X", "Y")), "E1", "E2")));

			expression = "sum({(on Person in World) if Person = rich then 2000 else 50 | Person = american})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"), "2000", "50"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american")))));

			expression = 
					"sum({(on Person in World) 2000 | Person = american and Person = rich})" +
					"+" + 
					"sum({(on Person in World) 50 | Person = american and not (Person = rich)})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), "2000", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), "50", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"))))))));
			
			expression = "f(X)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"));

			expression = "g(X',X',X) and h(Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "X'", "X'", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("h", "Y")));

			expression = "f(X',X'',Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "X''", "Y"));

			expression = "{(on X) X | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) X | X != b }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) X | X != b }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "X", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X', Y) f(X',Y) | X' != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X'", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))));

			expression = "{(on X) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) f(X)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), null));

			expression = "{(on X) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "f(X, Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"));

			expression = "{(on X') f(X',Y) | X' != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X'"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "f(X, Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"));

			expression = "{(on X', Y') f(X',Y') | X' != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X'", "Y'")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y'"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))));

			expression = "{(on X, Y) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on Y) f(X, Y) }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), null));

			expression = "{(on X', Y) f(X',Y) | X' != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X'", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))));

			expression = "{(on X, X') f(X,X') | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "X'")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "X'"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "f(X)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"));

			expression = "{(on X'', X') f(X'',X') | X'' != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X''", "X'")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X''", "X'"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X''", "a"))));

			expression = "{(on X, X') f(X,X') | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "X'")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "X'"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "f(X, X')";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "X'"));

			expression = "{(on X'', X''') f(X'',X''') | X'' != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X''", "X'''")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X''", "X'''"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X''", "a"))));

			expression = "{(on Z) {(on X, Y) f(X,Y,Z) | X != a } | Z != c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c"))));

			expression = "{(on Y) f(X, Y, Z) }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z"), null));

			expression = "{(on Z') {(on X', Y) f(X',Y,Z') | X' != a } | Z' != c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z'"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X'", "Y")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X'", "Y", "Z'"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X'", "a"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z'", "c"))));

			expression = "{(on Z) {(on Z', Y) f(X,Y,Z') | X != a } | Z != c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "Z'", "Y")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z'"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c"))));

			expression = "{(on Y) f(X, Y, Z, Z') }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z", "Z'"), null));

			expression = "{(on Z''') {(on Z'', Y) f(X,Y,Z'') | X != a } | Z''' != c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z'''"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "Z''", "Y")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "Z''"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z'''", "c"))));

			expression = "{(on X, p(Z)) f(X,Y) | X != a }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "Z"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) f(X)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), null));

			expression = "{(on X) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X in {1,2,3}) p(X) | false}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "false")));

			expression = "{ }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "{(on X in {1,2,3} - {1}, Y in {1,2} union {3}) p(X,Y) + 1 + 1 | true and Z}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "1"))), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2")), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "3"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "1", "1"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", "true", "Z"))));

			expression = "{(on X in {2,3}, Y in {1,2,3}) p(X,Y) + 2 | Z}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "2", "3"))), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "Z")));

			expression = "{(on X in {1,2,3} - {1}, Y) p(X,Y) + 1 + 1}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "1"))), "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "1", "1"), null));

			expression = "{(on X in {2,3}, Y) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "2", "3"))), "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X,Y) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			boolean oldValue = DefaultSyntaxLeaf.setDontAcceptSymbolValueToBeExpression(false);
			expression = "if V = (<X>) then {(on X) p(X,Y) + 2} else 0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "V", 
							Expressions.makeSymbol(Expressions.makeSymbol("X"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null), "0"));
			DefaultSyntaxLeaf.setDontAcceptSymbolValueToBeExpression(oldValue);

			expression = "{(on X, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X, Z, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Z", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			expression = "{(on X in set1, Z in set2, Y in {1,2,3}) p(X,Y) + 2}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", "set1"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Z", "set2"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null));

			oldValue = DefaultSyntaxLeaf.setDontAcceptSymbolValueToBeExpression(false);
			expression = "if V = (<X>) then {(on X, Y in {1,2,3}) p(X,Y) + 2} else 0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "V", 
							Expressions.makeSymbol(Expressions.makeSymbol("X"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), "2"), null), "0"));
			DefaultSyntaxLeaf.setDontAcceptSymbolValueToBeExpression(oldValue);

			expression = "{(on X,Y) f(X) | Z = X}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Z", "X"))));

			expression = "sum({(on X in {1,2,3}) 0 | X != 2})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), "0", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "2")))));

			expression = "product({(on X in {1,2,3}) 1 | X != 2})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), "1", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "2")))));

			expression = "sum(partition(A, B))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", "A", "B")));

			expression = "sum(partition())";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition")));

			expression = "{(on ) foo(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "if X != a and X != b then {foo(X)} else {}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{(on X in {1,2,3}) p(X) | false}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", "false")));

			expression = "{ }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "{(on X in {a,b,c}) foo(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "partition(" +
					"if first({a,b,c}) != a and first({a,b,c}) != b then { foo(first({a,b,c})) } else {}," +
					"{(on X in rest({a,b,c})) foo(X) | X != a and X != b})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("first", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("first", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), "b")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("first", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))))), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("rest", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))))), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b"))))));


			expression = "{(on X in {a,b,c}, Y in {1,2,3}) foo(X,Y) | X != a and Y != 1}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1")))));

			expression = "partition(" +
					"{(on                     Y in {1,2,3}) foo(first({a,b,c}),Y) | first({a,b,c}) != a and Y != 1}," +
					"{(on X in rest({a,b,c}), Y in {1,2,3}) foo(X,Y)              | X              != a and Y != 1})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("first", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), "Y"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("first", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))), "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("rest", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))))), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Y"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1"))))));


			expression = "{(on X in rest({a,b,c}), Y in {1,2,3}, Z) foo(X,Y) | X != a and Y != 1}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("rest", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), "Z")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1")))));

			expression = "partition(" +
					"{(on X in rest({a,b,c}),                     Z) foo(X, first({1,2,3})) | X != a and first({1,2,3}) != 1}," +
					"{(on X in rest({a,b,c}), Y in rest({1,2,3}), Z) foo(X,Y)               | X != a and Y              != 1})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("rest", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), "Z")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("first", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("first", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), "1")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("rest", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")))), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("rest", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), "Z")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X", "Y"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "1"))))));

			expression = "{(on X in {}) foo(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{(on X in {1,2,3}, Y in {a,b,c}) p(X,Y)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X", "Y"), null));

			expression = "{p(1,a), p(1,b), p(1,c), p(2,a), p(2,b), p(2,c), p(3,a), p(3,b), p(3,c)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "1", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "1", "b"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "1", "c"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "2", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "2", "b"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "2", "c"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "3", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "3", "b"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "3", "c"))));

			expression = "{(on X) if X = 2 then p(X) else q(X) | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "partition({(on X) p(X) | X != a and X = 2}, {(on X) q(X) | X != a and not(X = 2)})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2")))))));


			expression = "{(on X) if Y = 2 then p(X) else q(X) | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "2"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));


			expression = "sum({(on Person in World) if Person = rich then 2000 else 50 | Person = american})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"), "2000", "50"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american")))));

			expression = 
					"sum(" +
					"partition(" + 
					"{(on Person in World) 2000 | Person = american and Person = rich}," + 
					"{(on Person in World) 50 | Person = american and not (Person = rich)}))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), "2000", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich")))), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Person", "World")), "50", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "american"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Person", "rich"))))))));
		
			expression = "{(on X) if X = 2 and something then p(X) else q(X) | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2"), "something"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "partition({(on X) if something then p(X) else q(X) | X != a and X = 2}, {(on X) q(X) | X != a and not(X = 2)})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", "something", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("q", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("not", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "2")))))));

			expression = "{(on X in {(on Y) foo(Y)}) bar(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), null))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{(on Y) bar(foo(Y)) | foo(Y) != a and foo(Y) != b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), "b")))));

			expression = "{(on X in {(on Y) foo(Y) | Y != c}) bar(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "c"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{(on Y) bar(foo(Y)) | foo(Y) != a and foo(Y) != b and Y != c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), "b"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "c")))));

			expression = "{(on X in {(on Y in {(on Z) Z}) foo(Y) | Y != c}) bar(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), "Z", null))), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "c"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{(on Z) bar(foo(Z)) | foo(Z) != a and foo(Z) != b and Z != c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Z")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), "b"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c")))));

			expression = "{(on X in {(on Y in {(on Z) Z}) foo(Y) | Y != c}) bar(X) | X != a and X != b and 'for all'({(on Z) blah(Z)})}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), "Z", null))), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Y"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "c"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("blah", "Z"), null))))));

			expression = "{(on Z) bar(foo(Z)) | foo(Z) != a and foo(Z) != b and 'for all'({(on Z) blah(Z)}) and Z != c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("bar", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Z")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("foo", "Z"), "b"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("for all", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("blah", "Z"), null)), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "c")))));

			expression = "{(on X in 'some set') p(X) | X != a} - {somethingElse}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", "some set")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "somethingElse")));

			expression = "{(on X in 'some set') p(X) | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", "some set")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) p(X) | X != a} - {p(Y)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "Y"))));

			expression = "{(on X) p(X) | X != a and X != Y}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "Y")))));

			expression = "{(on X) p(X) | X != a} - {p(a)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a"))));

			expression = "{(on X) p(X) | X != a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))));

			expression = "{(on X) p(X) | X != a} - {p(b)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "b"))));

			expression = "{(on X) p(X) | X != a and X != b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "b")))));

			expression = "{ }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "{a, b, c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")));

			expression = "{a, a, b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "a", "b")));

			expression = "{a, b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b")));

			expression = "{a, b, b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "b")));

			expression = "{a, b, b, a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "b", "a")));

			expression = "{a, b, b, a, b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "b", "a", "b")));

			expression = "{X, Y}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")));

			expression = "if X = Y then {X} else {X,Y}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y"))));

			expression = "{X, Y, a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "a")));

			expression = 
					"if X = Y " +
					"then if X = a then {X} else {X,a} " +
					"else if X = a then {X,Y} else if Y = a then {X,Y} else {X,Y,a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "a"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "a"))))));

			expression = "partition({}, X, {}, Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")), "X", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")), "Y"));

			expression = "partition(X, Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", "X", "Y"));

			expression = "partition({}, X, {1}, Y, {2,3})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")), "X", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "1"), "Y", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "2", "3"))));

			expression = "partition({1,2,3}, X, Y)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")), "X", "Y"));

			expression = "partition({}, {})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "partition(X)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("partition", "X"));

			expression = "X";
			test(expression, Expressions.makeSymbol("X"));

			expression = "(1, 2)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2")));

			expression = "(X, Y) = (X, Y, Z)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "Z"))));

			expression = "tuple(X, Y) = (X, Y, Z)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("tuple", "X", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "Z"))));

			expression = "(X, Y) = tuple(X, Y, Z)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("tuple", "X", "Y", "Z")));

			expression = "(X, Y, Z) = (a, b, c)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "Z")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))));

			expression = "X = a and Y = b and Z = c";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "b"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Z", "c")));

			expression = "tuple(X, Y, Z) = tuple(a, b, c)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("tuple", "X", "Y", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("tuple", "a", "b", "c")));

			expression = "tuple(X, Y, Z) = (a, b, c)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("tuple", "X", "Y", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))));

			expression = "(X, Y, Z) = tuple(a, b, c)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "Z")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("tuple", "a", "b", "c")));

			expression = "first(list(a,b,c))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("first", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "a", "b", "c")));

			expression = "first(list(a))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("first", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "a")));

			expression = "rest(list(a,b,c))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("rest", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "a", "b", "c")));

			expression = "list(b,c)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "b", "c"));

			expression = "rest(list(a))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("rest", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "a")));

			expression = "list()";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list"));

			expression = "size(list(a,b,c))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("size", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "a", "b", "c")));

			expression = "size(list(a))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("size", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "a")));

			expression = "size(list())";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("size", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list")));


			expression = "{x} union {y}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "x"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "y")));

			expression = "{x, y}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")));

			expression = "{x, y} union {z}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "z")));

			expression = "{x, y, z}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z")));

			expression = "{x, y} union {}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{} union {x, y} union {}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "A union {x, y} union {}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "A", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "A union {x, y}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "A", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y"))));

			expression = "A union {x, y} union {z} union B";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "A", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "z"), "B"));

			expression = "A union {x, y, z} union B";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("union", "A", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "x", "y", "z")), "B"));


			expression = "sum({{(on X) c}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null)));

			expression = "sum({{(on X) c}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null)));

			expression = "c * |{{(on X) c}}|";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", "c", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null))));

			expression = "product({{(on X) c}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null)));

			expression = "c ^ |{{(on X) c}}|";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", "c", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), "c", null))));

			expression = "sum({{(on X) f(Y)}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "Y"), null)));

			expression = "f(Y) * |{{(on X) f(Y)}}|";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "Y"), null))));

			expression = "sum({{(on X) f(X)}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), null)));

			expression = "sum({{1,2,3}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))));


			expression = "{}-{d}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d")));

			expression = "{}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

			expression = "{d}-{}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{d}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d"));

			expression = "{a,b,c}-{d}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "d")));

			expression = "{a,b,c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")));

			expression = "{a,b,c}-{b}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b")));

			expression = "{a,c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "c")));

			expression = "{a,b,c}-{b,a}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "b", "a"))));

			expression = "{c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "c"));

			expression = "{a,b,c}-{a,b,c}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c"))));

			expression = "{{a,b,c}}-{{a,X,c}}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "b", "c")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "a", "X", "c"))));

			expression = "if X = b then {} else { b }";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "b"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b")));

			expression = "{{X,a,b,Y,Y}}-{{X,X,Y,a}}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "a", "b", "Y", "Y")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "X", "Y", "a"))));

			expression = "if X = b then { Y } else if X = Y then { b } else {{ b, Y }}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "b"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "b"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "b", "Y")))));

			expression = "{(on X in {1,2,3}, Y in D) f(X) | X = Z}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "D"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"))));

			expression = "{(on Y in D) f(Z)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "D")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "Z"), null));

			expression = "{(on X in {1,2,3}, Y in D) f(X) | X = Z and h(X)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3"))), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "D"))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("h", "X")))));

			expression = "{(on Y in D) f(Z) | h(Z)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "D")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("h", "Z"))));

			expression = "{(on X in {1,2,3}) f(X) | X = Z and h(X)}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "X", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "1", "2", "3")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("h", "X")))));

			expression = "if h(Z) then {f(Z)} else {}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("h", "Z"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "Z")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));

			expression = "{{ ( on Y in People ) sum({{ ( on p(a1, Y) ) p(a1, Y) }}) | Y = a2 }}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("in", "Y", "People")), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a1", "Y")), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a1", "Y"), null)), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Y", "a2"))));

			expression = "{{ sum({{ ( on p(a1, a2) ) p(a1, a2) }}) }}";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . }}", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a1", "a2")), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("p", "a1", "a2"), null))));

			expression = "if Z = a then f(lambda Z : g(Z)) else 0";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "Z", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", "Z", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Z"))), "0"));

			expression = "list(Z)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", "Z"));

			expression = "list(f(X))";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("list", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X")));

			expression = "if X = a then lambda f(X) : f(X) else 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X")), "1"));

			expression = "if X = a then lambda f(a) : f(a) else 1";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a")), "1"));

			expression = "(lambda f(X) : 2 + f(X))(1)";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("lambda . : .", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "2", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X"))), "1"));

			expression = "product({{(on X) f(X, Y) | X != a}}) * product({{(on Z) g(Z) | Z != b}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "b"))))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y}}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"))))));

			expression = "product({{ ( on Y ) (f(Y, Y) * g(Y)) | Y != a }})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "Y", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Y")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Y", "a")))));

			expression = "product({{(on X, Y) f(X, Y) | X != a}}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"))))));

			expression = "product({{ (on X) (product({{(on Y) f(X, Y)}}) * g(X))  | X != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), null)), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "X")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "product({{(on X) f(X, Y)}}) * product({{(on Z) g(Z)}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), null)), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), null))));

			expression = "product({{ (on X) (f(X, Y) * g(X)) }})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "X")), null)));

			expression = "product({{(on X, Y, V) f(X, Y, V) | X != a}}) * product({{(on W, Z) g(Z, W) | Z != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y", "V")), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "V"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "W", "Z")), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Z", "W"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"))))));

			expression = "product({{ ( on X ) (product({{ ( on Y ) (product({{ ( on V ) f(X, Y, V) }}) * g(X, Y)) }})) | X != a }})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Y"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
															Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "V"), 
																	Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y", "V"), null)), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "X", "Y")), null)), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "sum({{(on X) f(X, Y) * g(Y) | X != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Y")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "g(Y) * sum({{(on X) f(X, Y) | X != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))))));

			expression = "sum({{(on X) f(X, Y) * g(Y) * h() | X != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("h")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "g(Y) * h() * sum({{(on X) f(X, Y) | X != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("h"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"))))));

			expression = "sum({{(on X) g(Y) | X != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("sum", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "X"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a")))));

			expression = "g(Y) * | type(X) - { a } |";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Y"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("type", "X"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", "a")))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y }}) * product({{(on Z) g(Z) | Z != a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"))))), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g", "Z"), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "Z", "a"))))));

			expression = "product({{(on X, Y) f(X, Y) | X != a and X = Y and W = a}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", "Y"), 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y"), 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "W", "a"))))));

			expression = "X + product({{(on X, Y) |{(on W) W }|  |  X != a and X = Y}})";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "X", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("product", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{{ . . . }}", 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", "X", "Y")), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("| . |", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . . . }", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("( on . )", "W"), "W", null)), 
									Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("|", 
											Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("and", 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("!=", "X", "a"), 
													Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "Y")))))));

			expression = "if X = a then type(X) else nothing";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("type", "X"), "nothing"));

			expression = "if X = a then f(X, type(X)) else nothing";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("type", "X")), "nothing"));

			expression = "if X = a then f(a, type(X)) else nothing";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("if . then . else .", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("=", "X", "a"), 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "a", 
							Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("type", "X")), "nothing"));

			expression = "f(X,g())";
			test(expression, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f", "X", 
					Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("g")));

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

	/** Makes a scoping expression out of a list of scoping variables. */
	public static SyntaxTree makeScopingSyntaxTree(List<Expression> indexExpressions) {
		Expression kleeneListExpression = Expressions.makeKleeneListIfNeeded(indexExpressions);
		SyntaxTree kleeneListSyntaxTree = kleeneListExpression.getSyntaxTree();
		SyntaxTree result = SyntaxTrees.makeCompoundSyntaxTree(IntensionalSet.SCOPED_VARIABLES_LABEL, kleeneListSyntaxTree);
		return result;
	}
}
