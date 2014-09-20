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

import java.io.IOException;
import java.util.concurrent.TimeUnit;

import org.junit.Before;
import org.junit.Test;

import com.google.common.base.Stopwatch;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.brewer.core.DefaultGrammar;
import com.sri.ai.brewer.core.DefaultParsingProcess;
import com.sri.ai.brewer.core.DefaultParsingResult;
import com.sri.ai.brewer.core.MiniCommonGrammar;
import com.sri.ai.brewer.core.ParsingResult;
import com.sri.ai.brewer.parsingexpression.core.Disjunction;
import com.sri.ai.brewer.parsingexpression.core.Kleene;
import com.sri.ai.brewer.parsingexpression.core.NonTerminal;
import com.sri.ai.brewer.parsingexpression.core.Optional;
import com.sri.ai.brewer.parsingexpression.core.Sequence;
import com.sri.ai.brewer.parsingexpression.core.ParsingSymbol;
import com.sri.ai.brewer.parsingexpression.core.Terminal;
import com.sri.ai.brewer.parsingexpression.helper.AssociativeSequence;
import com.sri.ai.brewer.parsingexpression.helper.ParenthesizedNonTerminal;
import com.sri.ai.brewer.parsingexpression.helper.ParsingExpressionForFunctionApplications;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntaxLeaf;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.util.Util;

public class ParsingExpressionTest {

	private Grammar grammar = new DefaultGrammar();
	private ParsingExpression parsingExpression;
	private String string;

	@Before
	public void setUp() throws Exception {
	}

	@Test
	public void testSpeed() throws IOException {
		ParsingExpression parsingExpression;
		String string;
		grammar = new CommonGrammar();

		parsingExpression = new NonTerminal("Expression");
		string = "1 + 2";
		parse(parsingExpression, string);
	}

	@Test
	public void testSpeedOfSingletonSet() throws IOException {
		ParsingExpression parsingExpression;
		String string;
		grammar = new MiniCommonGrammar();

		parsingExpression = new NonTerminal("Expression");
		string = "{ f(X) }";
		parse(parsingExpression, string);
	}
	
//	@Test
	public void testSpeedOfSetDefinition() throws IOException {
		ParsingExpression parsingExpression;
		String string;
		grammar = new CommonGrammar();

		parsingExpression = new NonTerminal("Expression");
		string = "{{ ( on X ) X | X != bob }}";
		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
	}
	
//	@Test
	public void testSpeedOfSetDefinition2() throws IOException {
		ParsingExpression parsingExpression;
		String string;
		grammar = new CommonGrammar();

		parsingExpression = new NonTerminal("Expression");
		string = "{{ ( on X ) X }}";
		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
	}
	
//	@Test
	public void testSpeedOfSetDefinition3() throws IOException {
		ParsingExpression parsingExpression;
		String string;
		grammar = new CommonGrammar();

		parsingExpression = new NonTerminal("Expression");
		string = "{{ X, Y, f(X, Y, Z), g() }}";
		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
	}
	
	@Test
	public void testSpeedOfSetDefinition4() throws IOException {
//		ParsingExpression parsingExpression;
//		String string;
//		grammar = new CommonGrammar();
//
//		parsingExpression = new NonTerminal("Expression");
//		string = "1-2";
////		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
//
//		string = "9E-7";
////		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
//
//		string = "if <x> > <y> then x else y";
////		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
//
//		string = "<x in y>";
////		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
//
//		string = "(1,2)";
////		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
//
//		string = "{ x | y }";
////		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
//
//		string = "(lambda f(X) : 2 + f(X))(1)";
////		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
//
//		string = "person_1";
////		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));
//
//		string = "case x x:2, y:3, default:12";
////		parse(parsingExpression, string);
//		System.out.println("The parsed output of " + string + " is:  " + parse(parsingExpression, string));

	}

	@Test
	public void testEmptySequence() throws IOException {
		ParsingExpression parsingExpression;
		String string;
		grammar.clear();
		grammar.put("Expression", new Disjunction(new Terminal(","), new Sequence()));

		parsingExpression = new NonTerminal("Expression");
		string = "";
		test(parsingExpression, string, null);
	}

	@Test
	public void testSet() throws IOException {
		ParsingExpression parsingExpression;
		String string;
		grammar.clear();
		grammar.put("Expression", new Disjunction(
				new Sequence(new Terminal("{"), new Kleene(new NonTerminal("Expression"), new Optional(new Terminal(","))), new Terminal("}")),
				new ParsingSymbol("Expression")
		));

		parsingExpression = new NonTerminal("Expression");
		string = "{ 1, 2, 3 }";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("{ . }", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", 1, 2, 3)));
	}
	
	@Test
	public void testBasicParsing() throws IOException {
		ParsingExpression parsingExpression;
		String string;
		grammar.clear();
		grammar.put("Expression", new Disjunction(
				new Sequence(new Terminal("add"), new NonTerminal("Expression"), new Terminal("to"), new NonTerminal("Expression")),
				new Sequence(new Terminal("items"), new NonTerminal("Expression"), new NonTerminal("Expression"), new Terminal("with"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("+"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("-"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("++")),
				new Sequence(new Terminal("++"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new NonTerminal("Expression"), new Terminal("doublepostfix")),
				new ParsingSymbol("Expression")
		));

		// DEBUGGING
		parsingExpression = new Sequence(new ParsingSymbol("Expression"), new Terminal("-th"), new Terminal("root"), new Terminal("of"), new ParsingSymbol("Expression"));
		string = "3-th root of 4";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-th root of", SyntaxTrees.makeSyntaxLeaf(3), SyntaxTrees.makeSyntaxLeaf(4)));


		
		
		parsingExpression = new NonTerminal("Expression");
		string = "3";
		test(parsingExpression, string, Expressions.makeSymbol(3));

		parsingExpression = new NonTerminal("Expression");
		string = "false";
		test(parsingExpression, string, Expressions.makeSymbol(false));

		parsingExpression = new NonTerminal("Expression");
		string = "add x to y";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("add . to .","x","y"));

		parsingExpression = new NonTerminal("Expression");
		string = "items x y with z";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("items . . with .","x","y","z"));

		parsingExpression = new NonTerminal("Expression");
		string = "add x to y";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("add . to .","x","y"));

		parsingExpression = new NonTerminal("Expression");
		string = "x+y";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+","x","y"));

		parsingExpression = new NonTerminal("Expression");
		string = "++x";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("++","x"));

		parsingExpression = new NonTerminal("Expression");
		string = "x++";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(". ++","x"));

		parsingExpression = new NonTerminal("Expression");
		string = "x y doublepostfix";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(". . doublepostfix","x","y"));

		parsingExpression = new Sequence();
		string = "";
		test(parsingExpression, string, null);

		parsingExpression = new Terminal("keyword");
		string = "keyword";
		test(parsingExpression, string, Expressions.makeSymbol("keyword"));

		parsingExpression = new Terminal("Keyword");
		string = "Keyword";
		test(parsingExpression, string, Expressions.makeSymbol("Keyword"));

		parsingExpression = new Terminal("keyword");
		string = "keyword2";
		test(parsingExpression, string, null);

		parsingExpression = new Disjunction(new Terminal("keyword"), new Terminal("keyword2"));
		string = "keyword2";
		test(parsingExpression, string, Expressions.makeSymbol("keyword2"));

		parsingExpression = new Sequence(new Terminal("keyword1"), new Terminal("keyword2"));
		string = "keyword1 keyword2";
		test(parsingExpression, string, Expressions.makeSymbol("keyword1 keyword2"));

		parsingExpression = new Sequence(new Terminal("keyword1"), new Terminal(" "), new Terminal("keyword2"));
		string = "keyword1 somethingElse";
		test(parsingExpression, string, null);

		parsingExpression = new Optional(new Terminal("k1"));
		string = "k2";
		test(parsingExpression, string, null);

		parsingExpression = new Optional(new Terminal("k1"));
		string = "k1";
		test(parsingExpression, string, Expressions.makeSymbol("k1"));

		parsingExpression = new Sequence(new Terminal("k1"), new Optional(new Sequence(new Terminal("k2"))), new Terminal("k3"));
		string = "k1 k3";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("k1 . k3", new Object[]{null}));

		parsingExpression = new Sequence(new Terminal("k1"), new Optional(new Sequence(new Terminal("k2"))), new Terminal("k3"));
		string = "k1 k2 k3";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("k1 . k3", SyntaxTrees.makeSyntaxLeaf("k2")));

		parsingExpression = new Sequence(new Optional(new Terminal("*")), new Terminal("/"));
		string = "+/";
		test(parsingExpression, string, null);

		parsingExpression = new Sequence(new Terminal("+"), new Optional(new Terminal("*")), new Terminal("/"));
		string = "+*/";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+ . /", SyntaxTrees.makeSyntaxLeaf("*")));

		parsingExpression = new Sequence(new Terminal("log"), new ParsingSymbol("Expression"));
		string = "log 10";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("log", SyntaxTrees.makeSyntaxLeaf(10)));

		parsingExpression = new Sequence(new ParsingSymbol("Expression"), new Terminal("+"), new ParsingSymbol("Expression"));
		string = "2+2";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", SyntaxTrees.makeSyntaxLeaf(2), SyntaxTrees.makeSyntaxLeaf(2)));

		parsingExpression = new Sequence(new ParsingSymbol("Expression"), new Terminal("-th"), new Terminal("root"), new Terminal("of"), new ParsingSymbol("Expression"));
		string = "3-th root of 4";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-th root of", SyntaxTrees.makeSyntaxLeaf(3), SyntaxTrees.makeSyntaxLeaf(4)));

		parsingExpression = new Sequence(new Terminal("ship"), new Terminal("with"), new ParsingSymbol("Expression"), new Terminal("ton"), new Terminal("capacity"));
		string = "ship with 10 ton capacity";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("ship with . ton capacity", SyntaxTrees.makeSyntaxLeaf(10)));
	}

	@Test
	public void testDisjunction() throws IOException {
		parsingExpression = new Disjunction(
				new Sequence(new Terminal("keyword1"), new Terminal("keyword2")),
				new Sequence(new Terminal("keyword1"), new Terminal("keyword3")));
		string = "keyword1 keyword3";
		test(parsingExpression, string,  Expressions.makeSymbol("keyword1 keyword3"));

		parsingExpression = new Disjunction(new Terminal("keyword105"), new Sequence());
		string = "";
		test(parsingExpression, string, null);
	}

	@Test
	public void testKleene() throws IOException {
		parsingExpression = new Kleene(new Terminal("keyword105"), new Optional(new Terminal(",")));
		string = "";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"));

		parsingExpression = new Sequence(
				new Terminal("keyword34"),
				new Kleene(new Terminal("keyword105"), new Optional(new Terminal(","))));
		string = "keyword34";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("keyword34", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")));

		parsingExpression = new Kleene(new Terminal("keyword"), new Optional(new Terminal(",")));
		string = "keyword";
		test(parsingExpression, string, Expressions.makeSymbol("keyword"));

		parsingExpression = new Kleene(new Terminal("keyword"), new Optional(new Terminal(",")));
		string = "keyword";
		test(parsingExpression, string, Expressions.makeSymbol("keyword"));

		parsingExpression = new Kleene(new Terminal("keyword"), new Optional(new Terminal(",")));
		string = "keyword keyword keyword";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", SyntaxTrees.makeSyntaxLeaf("keyword"), SyntaxTrees.makeSyntaxLeaf("keyword"), SyntaxTrees.makeSyntaxLeaf("keyword")));

		parsingExpression = new Kleene(new Terminal("keyword"), new Optional(new Terminal(",")));
		string = "keyword, keyword, keyword";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", SyntaxTrees.makeSyntaxLeaf("keyword"), SyntaxTrees.makeSyntaxLeaf("keyword"), SyntaxTrees.makeSyntaxLeaf("keyword")));

		// the following tests whether something perceived as a separator turns out not to be and needs to be put back in the token stream.
		parsingExpression = new Sequence(new Kleene(new Terminal("keyword"), new Optional(new Terminal(","))), new Terminal(","), new Terminal("anotherkeyword"));
		string = "keyword, keyword, anotherkeyword"; // the second comma is taken by Kleene as a separator, but since the next item is not part of the repeating pattern, it really is not and needs to be returned.
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(". , anotherkeyword", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", SyntaxTrees.makeSyntaxLeaf("keyword"), SyntaxTrees.makeSyntaxLeaf("keyword"))));

		// testing Kleene with a minimum number of elements -- success
		parsingExpression = new Sequence(new Kleene(new Terminal("keyword"), new Optional(new Terminal(",")), 2), new Terminal(","), new Terminal("anotherkeyword"));
		string = "keyword, keyword, anotherkeyword"; // the second comma is taken by Kleene as a separator, but since the next item is not part of the repeating pattern, it really is not and needs to be returned.
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(". , anotherkeyword", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list", SyntaxTrees.makeSyntaxLeaf("keyword"), SyntaxTrees.makeSyntaxLeaf("keyword"))));

		// testing Kleene with a minimum number of elements -- failure
		parsingExpression = new Sequence(new Kleene(new Terminal("keyword"), new Optional(new Terminal(",")), 3), new Terminal(","), new Terminal("anotherkeyword"));
		string = "keyword, keyword, anotherkeyword"; // the second comma is taken by Kleene as a separator, but since the next item is not part of the repeating pattern, it really is not and needs to be returned.
		test(parsingExpression, string, null);

		// testing Kleene with a minimum number of elements -- failure followed by alternative successful parsing in sequence is still a failure because the Kleene parsing is a failure.
		parsingExpression = new Sequence(new Kleene(new Terminal("keyword"), new Optional(new Terminal(",")), 3), new Terminal("keyword"), new Terminal(","), new Terminal("keyword"), new Terminal(","), new Terminal("anotherkeyword"));
		string = "keyword, keyword, anotherkeyword";
		test(parsingExpression, string, null);

		// testing Kleene with a minimum number of elements -- failure followed by alternative successful parsing in disjunction is a success
		parsingExpression = new Disjunction(new Kleene(new Terminal("keyword"), new Optional(new Terminal(",")), 3), new Sequence(new Terminal("keyword"), new Terminal(","), new Terminal("keyword"), new Terminal(","), new Terminal("anotherkeyword")));
		string = "keyword, keyword, anotherkeyword";
		test(parsingExpression, string, Expressions.makeSymbol("keyword , keyword , anotherkeyword"));
	}

	@Test
	public void testAssociativeSequence() throws IOException {
		grammar.clear();
		grammar.put("Expression", new Disjunction(
				new Sequence(new NonTerminal("Expression"), new Terminal("+"), new NonTerminal("Expression")),
				new ParsingSymbol("Expression")
		));

		parsingExpression = new NonTerminal("Expression");
		string = "x + y + z";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "y", "z")));

		grammar.clear();
		grammar.put("Expression", new Disjunction(
				new AssociativeSequence(new NonTerminal("Expression"), new Terminal("+"), new NonTerminal("Expression")),
				new ParsingSymbol("Expression")
		));

		parsingExpression = new NonTerminal("Expression");
		string = "x + y + z";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", "x", "y", "z"));
	}

	@Test
	public void testPrecedence() throws IOException {
		grammar.clear();

		grammar.put("Ship", new Sequence(new Terminal("ship"), new Terminal("with"), new NonTerminal("Expression"), new Terminal("ton"), new Terminal("capacity")));
		grammar.put("Expression", new Disjunction(
				new Sequence(new Terminal("log"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("+"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("-"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("*"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("^"), new NonTerminal("Expression")),
				new Sequence(new Terminal("-"), new NonTerminal("Expression")),
				new NonTerminal("Ship"),
				new Terminal("a")));

		parsingExpression = new NonTerminal("Expression");
		string = "log a";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("log", SyntaxTrees.makeSyntaxLeaf("a")));

		parsingExpression = new NonTerminal("Expression");
		string = "log a-a";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("log",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
								SyntaxTrees.makeSyntaxLeaf("a"),
								SyntaxTrees.makeSyntaxLeaf("a"))));

		string = "a^a-a";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^", SyntaxTrees.makeSyntaxLeaf("a"), SyntaxTrees.makeSyntaxLeaf("a")),
				SyntaxTrees.makeSyntaxLeaf("a")));

		parsingExpression = new NonTerminal("Expression");
		string = "a^log a-a";
		test(parsingExpression, string, Expressions.makeSymbol("a")); // this one is a bit tricky. The ^ on log does not parse because log is supposed to be of smaller precedence than ^, so it cannot be its argument. The non-intuitiveness of the example comes from log being typically of higher precedence.

		string = "a*a+a";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", SyntaxTrees.makeSyntaxLeaf("a"), SyntaxTrees.makeSyntaxLeaf("a")),
				SyntaxTrees.makeSyntaxLeaf("a")));

		string = "a+a*a";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+",
				SyntaxTrees.makeSyntaxLeaf("a"),
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", SyntaxTrees.makeSyntaxLeaf("a"), SyntaxTrees.makeSyntaxLeaf("a"))));

		string = "-a*a+a";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", SyntaxTrees.makeSyntaxLeaf("a")),
						SyntaxTrees.makeSyntaxLeaf("a")),
						SyntaxTrees.makeSyntaxLeaf("a")));

		string = "-a*-a-a";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", SyntaxTrees.makeSyntaxLeaf("a")),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", SyntaxTrees.makeSyntaxLeaf("a"))),
						SyntaxTrees.makeSyntaxLeaf("a")));

		string = "-a*-a^a-a";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", SyntaxTrees.makeSyntaxLeaf("a")),
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^",
										Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", SyntaxTrees.makeSyntaxLeaf("a")),
										SyntaxTrees.makeSyntaxLeaf("a"))),
										SyntaxTrees.makeSyntaxLeaf("a")));

		string = "-a*-a^-a";
		test(parsingExpression, string, 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", SyntaxTrees.makeSyntaxLeaf("a")),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("^",
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", SyntaxTrees.makeSyntaxLeaf("a")),
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-", SyntaxTrees.makeSyntaxLeaf("a")))));

		string = "ship with a ton capacity - ship with a ton capacity";
		test(parsingExpression, string, 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("ship with . ton capacity", SyntaxTrees.makeSyntaxLeaf("a")),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("ship with . ton capacity", SyntaxTrees.makeSyntaxLeaf("a"))));

		string = "ship with a + a ton capacity - ship with a ton capacity";
		test(parsingExpression, string, 
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("-",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("ship with . ton capacity", Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", SyntaxTrees.makeSyntaxLeaf("a"), SyntaxTrees.makeSyntaxLeaf("a"))),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("ship with . ton capacity", SyntaxTrees.makeSyntaxLeaf("a"))));
	}

	@Test
	public void testFunctionsAndSequences() throws IOException {
		grammar.clear();
		grammar.put("Ship", new Sequence(
				new Terminal("ship"),
				new NonTerminal("Expression")));
		grammar.put("Dock", new Sequence(new Terminal("dock"), new NonTerminal("Ship"), new Terminal("at"), new ParsingSymbol("Expression")));
		grammar.put("Expression", new Disjunction(
				new Kleene(new NonTerminal("Expression"), /* separator: */ new Disjunction(new Terminal(","), new Terminal(";"))),
				//				new Sequence(new NonTerminal("Expression"), new Terminal("="), new Terminal(">"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("+"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("-"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("*"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("/"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("^"), new NonTerminal("Expression")),
				new ParsingExpressionForFunctionApplications(new NonTerminal("Expression")),
				new ParenthesizedNonTerminal("Expression"),
				new Sequence(new Terminal("log"), new NonTerminal("Expression")),
				new NonTerminal("Dock"),
				new NonTerminal("Ship"),
				new ParsingSymbol("Expression")
		));

		parsingExpression = new NonTerminal("Expression");
		string = "x+y";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+","x","y"));

		parsingExpression = new NonTerminal("Expression");
		string = "f()";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
						SyntaxTrees.makeSyntaxLeaf("f"),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list")));
		// this one may seem strange, but it makes sense because "Expression" can be an empty
		// kleene list, which is what the empty space between parentheses is:
		// a one-element kleene list, with the element being an empty kleene list.
		// In typical applications we will not have Kleene at the top of Expression.

		parsingExpression = new NonTerminal("Expression");
		string = "f(10)";
		test(parsingExpression, string, Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(SyntaxTrees.makeSyntaxLeaf("f"), SyntaxTrees.makeSyntaxLeaf(10)));

		parsingExpression = new NonTerminal("Expression");
		string = "f(10 + a)";
		test(parsingExpression, string, 								
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
								"+",
								SyntaxTrees.makeSyntaxLeaf(10),
								SyntaxTrees.makeSyntaxLeaf("a"))));

		parsingExpression = new NonTerminal("Expression");
		string = "f(10+a) - f(a, b)";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
						"-",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
								SyntaxTrees.makeSyntaxLeaf("f"),
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
										"+",
										SyntaxTrees.makeSyntaxLeaf(10),
										SyntaxTrees.makeSyntaxLeaf("a")
								)),
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
										SyntaxTrees.makeSyntaxLeaf("f"),
										Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list",
												SyntaxTrees.makeSyntaxLeaf("a"),
												SyntaxTrees.makeSyntaxLeaf("b")))));
		// this is weird but makes sense since "a,b" can be seen as a *single* expression
		// which is a kleene list, since "Expression" starts with a Kleene parsing expression.
		
		parsingExpression = new NonTerminal("Expression");
		string = "f(10)-f()";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
						"-",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
								SyntaxTrees.makeSyntaxLeaf("f"),
								SyntaxTrees.makeSyntaxLeaf(10)
						),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("f",
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list"))));
		// this one may seem strange, but it makes sense because "Expression" can be an empty
		// kleene list, which is what the empty space between parentheses is:
		// a one-element kleene list, with the element being an empty kleene list.
		// In typical applications we will not have Kleene at the top of Expression.

		parsingExpression = new NonTerminal("Expression");
		string = "10 + 10";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", SyntaxTrees.makeSyntaxLeaf(10), SyntaxTrees.makeSyntaxLeaf(10)));

		parsingExpression = new NonTerminal("Expression");
		string = "10 + x*20";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+", SyntaxTrees.makeSyntaxLeaf(10), Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*", SyntaxTrees.makeSyntaxLeaf("x"),SyntaxTrees.makeSyntaxLeaf(20))));

		parsingExpression = new NonTerminal("Expression");
		string = "ship Carpatia + x*log 20";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("+",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("ship", SyntaxTrees.makeSyntaxLeaf("Carpatia")),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("*",
								SyntaxTrees.makeSyntaxLeaf("x"),
								Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("log", SyntaxTrees.makeSyntaxLeaf(20)))));

		parsingExpression = new NonTerminal("Expression");
		string = "dock ship Carpatia at NY";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("dock . at .",
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("ship", SyntaxTrees.makeSyntaxLeaf("Carpatia")),
						SyntaxTrees.makeSyntaxLeaf("NY")));

		parsingExpression = new NonTerminal("Expression");
		string = "10, 20; 30, f (x), f, (x)";
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list",
						SyntaxTrees.makeSyntaxLeaf(10),
						SyntaxTrees.makeSyntaxLeaf(20),
						SyntaxTrees.makeSyntaxLeaf(30),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(SyntaxTrees.makeSyntaxLeaf("f"), SyntaxTrees.makeSyntaxLeaf("x")),
						SyntaxTrees.makeSyntaxLeaf("f"),
						SyntaxTrees.makeSyntaxLeaf("x")));

		grammar.clear();
		grammar.put("Expression", new Disjunction(
				new Kleene(new NonTerminal("Expression"), /* separator: */ new Optional(new Disjunction(new Terminal(","), new Terminal(";")))),
				new Sequence(new NonTerminal("Expression"), new Terminal("+"), new NonTerminal("Expression")),
				new Sequence(new NonTerminal("Expression"), new Terminal("*"), new NonTerminal("Expression")),
				new ParsingExpressionForFunctionApplications(new NonTerminal("Expression")),
				new ParenthesizedNonTerminal("Expression"),
				new ParsingSymbol("Expression")
		));

		parsingExpression = new NonTerminal("Expression");
		string = "10 20; 30 f (x) f, (x)"; // commas are optional separators; we can use them to show that the second f is not being applied
		test(parsingExpression, string,
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees("kleene list",
						SyntaxTrees.makeSyntaxLeaf(10),
						SyntaxTrees.makeSyntaxLeaf(20),
						SyntaxTrees.makeSyntaxLeaf(30),
						Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(SyntaxTrees.makeSyntaxLeaf("f"), SyntaxTrees.makeSyntaxLeaf("x")),
						SyntaxTrees.makeSyntaxLeaf("f"),
						SyntaxTrees.makeSyntaxLeaf("x")));
	}

	@Test
	public void testQuotedTerms() throws IOException {
		grammar = new CommonGrammar();

		boolean oldValue = DefaultSyntaxLeaf.setDontAcceptSymbolValueToBeExpression(false);
		
		parsingExpression = new NonTerminal("Expression");
		string = "<x>";
		test(parsingExpression, string, Expressions.makeSymbol(Expressions.makeSymbol("x")));

		parsingExpression = new NonTerminal("Expression");
		string = "<x>1>"; // tricky, huh? Of course, we could have written <x > 1> or <(x > 1)> for clarity.
		test(parsingExpression, string, Expressions.makeSymbol(Expressions.apply(">", "x", 1)));

		parsingExpression = new NonTerminal("Expression");
		string = "(<x>) < (<y>)";
		test(parsingExpression, string, Expressions.apply("<", Expressions.makeSymbol(Expressions.makeSymbol("x")), Expressions.makeSymbol(Expressions.makeSymbol("y"))));

		DefaultSyntaxLeaf.setDontAcceptSymbolValueToBeExpression(oldValue);
	}
	
	private void test(ParsingExpression parsingExpression, String string, Expression expectedParse)
	throws IOException {
		Expression actualParse = parse(parsingExpression, string);
		assertEquals(expectedParse, actualParse);
	}

	private Expression parse(ParsingExpression parsingExpression, String string)
			throws IOException {
		ParsingResult actual;
		ParsingProcess process = new DefaultParsingProcess(string, grammar);

		System.out.println("Parsing " + string);
		Stopwatch stopwatch = Stopwatch.createStarted();
		actual = parsingExpression.parsingResult(process);
		long time = stopwatch.elapsed(TimeUnit.MILLISECONDS);
		Expression actualParse = DefaultParsingResult.isSuccessful(actual)? actual.getParse() : null;
		System.out.println(string + " ------> " + actualParse);
		System.out.println("Time: " + time + " ms");
		if (process.hasNextTokenAccordingToCurrentConditions()) {
			System.out.println("Remaining tokens: " + Util.join(";", process.remainingTokens()) + ".");
		}
		return actualParse;
	}
}
