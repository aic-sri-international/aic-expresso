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
package com.sri.ai.test.grinder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.Library;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.DefaultLibrary;
import com.sri.ai.grinder.core.DefaultRewriterLookup;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.core.ExhaustiveRewriter;
import com.sri.ai.grinder.core.OpenInterpretationModule;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.library.AbsorbingElement;
import com.sri.ai.grinder.library.Associative;
import com.sri.ai.grinder.library.CommonLibrary;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Distributive;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.NewSubstitute;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.Substitute;
import com.sri.ai.grinder.library.Unification;
import com.sri.ai.grinder.library.Variables;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.controlflow.IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch;
import com.sri.ai.grinder.library.controlflow.IfThenElseExternalization;
import com.sri.ai.grinder.library.controlflow.IfThenElseSubExpressionsAndImposedConditionsProvider;
import com.sri.ai.grinder.library.controlflow.ImposedConditionsModule;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CompleteSimplify;
import com.sri.ai.grinder.library.equality.injective.DisequalityOnInjectiveSubExpressions;
import com.sri.ai.grinder.library.equality.injective.DisequalityOnMutuallyExclusiveCoDomainExpressions;
import com.sri.ai.grinder.library.equality.injective.EqualityOnInjectiveSubExpressions;
import com.sri.ai.grinder.library.equality.injective.EqualityOnMutuallyExclusiveCoDomainExpressions;
import com.sri.ai.grinder.library.function.InjectiveModule;
import com.sri.ai.grinder.library.function.MutuallyExclusiveCoDomainsModule;
import com.sri.ai.grinder.library.lambda.Lambda;
import com.sri.ai.grinder.library.lambda.LambdaApplication;
import com.sri.ai.grinder.library.number.Division;
import com.sri.ai.grinder.library.number.Exponentiation;
import com.sri.ai.grinder.library.number.GreaterThan;
import com.sri.ai.grinder.library.number.GreaterThanOrEqualTo;
import com.sri.ai.grinder.library.number.Minus;
import com.sri.ai.grinder.library.number.NestedArithmeticOperation;
import com.sri.ai.grinder.library.number.NotOnGreaterThan;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.number.Times;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.grinder.library.set.Partition;
import com.sri.ai.grinder.library.set.extensional.EqualityOfExtensionalUniSets;
import com.sri.ai.grinder.library.set.extensional.NormalizeExtensionalUniSet;
import com.sri.ai.grinder.library.set.extensional.UnionOnExtensionalSets;
import com.sri.ai.grinder.library.set.intensional.EqualityOfIntensionalUniSets;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.grinder.library.set.intensional.IntensionalSetSubExpressionsAndImposedConditionsProvider;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.test.grinder.library.equality.cardinality.CountsDeclaration;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.math.Rational;

public class GrinderTest extends AbstractGrinderTest {

	@Override
	public CommonGrammar makeGrammar() {
		return new CommonGrammar();
	}
	
	@Test
	public void testRounding() {
		Expression expression;
		Expression rounded;
		Expression expected;

		expression = parse("if X = 1 then 0.0003 + 0.000000000001 else 0.150004 + 0.1 + 0.776699029126213691398561");
		rounded    = Expressions.roundToAGivenPrecision(expression, 2);
		expected   = parse("if X = 1 then 0.0003 + 0.000000000001 else 0.15 + 0.1 + 0.78");
		assertEquals(expected, rounded);
	}
	
	@Test
	public void testMakeStringValuedSymbolParseSafe() {
		String s;
		
		// No change
		s = DefaultSymbol.makeStringValuedSymbolParseSafe("aSymbol");
		Assert.assertEquals("aSymbol", s);
		
		// spaces
		s = DefaultSymbol.makeStringValuedSymbolParseSafe("I have a space");
		Assert.assertEquals("'I have a space'", s);
		
		// ' not escaped
		s = DefaultSymbol.makeStringValuedSymbolParseSafe("I have'nt a space");
		Assert.assertEquals("'I have\\'nt a space'", s);
		
		// ' is escaped
		s = DefaultSymbol.makeStringValuedSymbolParseSafe("I have\\'nt a space");
		Assert.assertEquals("'I have\\'nt a space'", s);
		
		// ' not escaped
		s = DefaultSymbol.makeStringValuedSymbolParseSafe("I have\\\\'nt a space");
		Assert.assertEquals("'I have\\\\\\'nt a space'", s);
		
		// ' not escaped
		s = DefaultSymbol.makeStringValuedSymbolParseSafe("i'have");
		Assert.assertEquals("'i\\'have'", s);
		
		// ' is escaped
		s = DefaultSymbol.makeStringValuedSymbolParseSafe("i\\'have");
		Assert.assertEquals("'i\\'have'", s);
		
		// ' not escaped
		s = DefaultSymbol.makeStringValuedSymbolParseSafe("i\\\\'have");
		Assert.assertEquals("'i\\\\\\'have'", s);
	}
	
	@Test
	public void testMakeUniqueVariable() {
		Library library = new DefaultLibrary(
				new Plus(),
				new Associative("+"));
		evaluator = new ExhaustiveRewriter(library);
		
		Expression topExpression = Expressions.make(FunctorConstants.EQUAL, DefaultSymbol.createSymbol("V"), "2");
		RewritingProcess process = new DefaultRewritingProcess(topExpression, evaluator);
		
		Expression var = Expressions.makeUniqueVariable("X", topExpression, process);
		assertEquals(DefaultSymbol.createSymbol("X"), var);
		
		var = Expressions.makeUniqueVariable("x", topExpression, process);
		assertEquals(DefaultSymbol.createSymbol("X"), var);
		
		var = Expressions.makeUniqueVariable("X1", topExpression, process);	
		assertEquals(DefaultSymbol.createSymbol("X1"), var);
		
		var = Expressions.makeUniqueVariable("x1", topExpression, process);
		assertEquals(DefaultSymbol.createSymbol("X1"), var);
		
		var = Expressions.makeUniqueVariable("V", topExpression, process);
		assertEquals(DefaultSymbol.createSymbol("V'"), var);
		
		var = Expressions.makeUniqueVariable("v", topExpression, process);
		assertEquals(DefaultSymbol.createSymbol("V'"), var);
		
		var = Expressions.makeUniqueVariable("V1", topExpression, process);
		assertEquals(DefaultSymbol.createSymbol("V1"), var);
		
		var = Expressions.makeUniqueVariable("v1", topExpression, process);
		assertEquals(DefaultSymbol.createSymbol("V1"), var);
	}

	@Ignore // need to include asserts in this test
	@Test
	public void testEquivalencyCache() {
		Library library = new DefaultLibrary(
				new Plus(),
				new Associative("+"),
				new Times(),
				new Associative("*")
				);
		evaluator = new TotalRewriter(library);
		
		expressionString = "((1 + 2) * (1 + 2)) + ((1 + 2) * (1 + 2))"; // should reuse equivalency results
		expected   = DefaultSymbol.createSymbol(18);
		evaluationTest();

	}
	
	@Test
	public void testPlus() {
		Library library = new DefaultLibrary(
				new Plus(),
				new Associative("+"));
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "3";
		expected   = DefaultSymbol.createSymbol(3);
		evaluationTest();

		expressionString = "x + y";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "1 + 2";
		expected   = DefaultSymbol.createSymbol(3);
		evaluationTest();
		
		expressionString = "x + 2";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "+(x, 2, y, 6)";
		expected   = parse("+(x, 8, y)");
		evaluationTest();
		
		expected   = parse("+(x, +(1, y), 11)");
		expressionString = "+(x, +(1, y), 11)";
		expected   = parse("+(x, 12, y)");
		evaluationTest();
		
		expressionString = "+(x, 2, 1 + 2, 1 + y, 6)";
		expected   = parse("+(x, 12, y)");
		evaluationTest();
		
		expressionString = "+(x, 2, 3)";
		expected   = parse("x + 5");
		evaluationTest();
		
		expressionString = "+(x)";
		expected = parse("x");
		evaluationTest();
		
		expressionString = "+";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "1";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "x";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "+()";
		expected = parse("0");
		evaluationTest();
	}

	@Test
	public void testMinus() {
		Library library = new DefaultLibrary(
				new Minus());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "3 - 1";
		expected   = DefaultSymbol.createSymbol(2);
		evaluationTest();
		
		expressionString = "1 - 3";
		expected   = DefaultSymbol.createSymbol(-2);
		evaluationTest();
		
		expressionString = "X - Y";
		expected   = parse(expressionString);
		evaluationTest();
		
		expressionString = "X - 0";
		expected   = DefaultSymbol.createSymbol("X");
		evaluationTest();
		
		expressionString = "0 - X";
		expected   = parse("-X");
		evaluationTest();
	}
	
	@Test
	public void testUnaryMinus() {
		Library library = new DefaultLibrary(
				new UnaryMinus());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "-1";
		expected   = DefaultSymbol.createSymbol(-1);
		evaluationTest();
		
		expressionString = "-x";
		expected   = parse(expressionString);
		evaluationTest();
	}
	
	@Test
	public void testTimes() {
		Library library = new DefaultLibrary(
				new AbsorbingElement("*", 0),
				new Associative("*"),
				new Times());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "3";
		expected   = DefaultSymbol.createSymbol(3);
		evaluationTest();

		expressionString = "x * y";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "2 * 2";
		expected   = DefaultSymbol.createSymbol(4);
		evaluationTest();
		
		expressionString = "x * 2";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "*(x, 2, y, 6)";
		expected   = parse("*(x, 12, y)");
		evaluationTest();
		
		expressionString = "*(x, 0, y, 6)";
		expected   = parse("0");
		evaluationTest();
		
		expressionString = "*(x, 2, 1 * 2, 1 * y, 6)";
		expected   = parse("*(x, 24, y)");
		evaluationTest();
		
		expressionString = "*(x, 2, 3)";
		expected   = parse("x * 6");
		evaluationTest();
		
		expressionString = "*(x)";
		expected = parse("x");
		evaluationTest();
		
		expressionString = "*";
		expected = parse(expressionString);
		evaluationTest();

		expressionString = "*()";
		expected = parse("1");
		evaluationTest();
	}

	@Test
	public void testExponentiation() {
		Library library = new DefaultLibrary(
				new Exponentiation());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "3^2";
		expected   = DefaultSymbol.createSymbol(9);
		evaluationTest();
		
		expressionString = "x^1";
		expected   = DefaultSymbol.createSymbol("x");
		evaluationTest();
		
		expressionString = "x^0";
		expected   = DefaultSymbol.createSymbol(1);
		evaluationTest();
		
		expressionString = "x^0.0";
		expected   = DefaultSymbol.createSymbol(1);
		evaluationTest();
		
		expressionString = "1^n";
		expected   = DefaultSymbol.createSymbol(1);
		evaluationTest();
		
		expressionString = "2^n";
		expected   = parse(expressionString);
		evaluationTest();
	}
	
	@Test
	public void testExponentiationPrecision() {
		Library library = new DefaultLibrary(
				new Minus(),
				new Exponentiation());
		evaluator = new ExhaustiveRewriter(library);
		
		Rational nonZeroMinAbsValue = new Rational(1).divide(new Rational(10).pow(324));
		
		expressionString = "10^1022";
		expected   = DefaultSymbol.createSymbol(new Rational(10).pow(1022));
		evaluationTest();
		
		expressionString = "10^1023";
		expected   = DefaultSymbol.createSymbol(Double.MAX_VALUE);
		evaluationTest();
		
		expressionString = "10^1024";
		expected   = DefaultSymbol.createSymbol(Double.MAX_VALUE);
		evaluationTest();
		
		//
		expressionString = "0.1^324";
		expected   = DefaultSymbol.createSymbol(new Rational(10).pow(-324));
		evaluationTest();
		
		expressionString = "0.1^325";
		expected   = DefaultSymbol.createSymbol(nonZeroMinAbsValue);
		evaluationTest();
		
		expressionString = "0.1^1022";
		expected   = DefaultSymbol.createSymbol(nonZeroMinAbsValue);
		evaluationTest();
		
		expressionString = "0.1^1023";
		expected   = DefaultSymbol.createSymbol(nonZeroMinAbsValue);
		evaluationTest();
		
		expressionString = "0.1^2000";
		expected   = DefaultSymbol.createSymbol(nonZeroMinAbsValue);
		evaluationTest();
		
		//
		expressionString = "10^(0-324)";
		expected   = DefaultSymbol.createSymbol(new Rational(10).pow(-324));
		evaluationTest();
		
		expressionString = "10^(0-325)";
		expected   = DefaultSymbol.createSymbol(nonZeroMinAbsValue);
		evaluationTest();
		
		expressionString = "10^(0-1022)";
		expected   = DefaultSymbol.createSymbol(nonZeroMinAbsValue);
		evaluationTest();
		
		expressionString = "10^(0-1023)";
		expected   = DefaultSymbol.createSymbol(nonZeroMinAbsValue);
		evaluationTest();
		
		expressionString = "10^(0-2000)";
		expected   = DefaultSymbol.createSymbol(nonZeroMinAbsValue);
		evaluationTest();
	}
	
	@Test
	public void testDivision() {
		Library library = new DefaultLibrary(
				new Division());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "4/2";
		expected   = DefaultSymbol.createSymbol(2);
		evaluationTest();
		
		expressionString = "4.0/2.0";
		expected   = DefaultSymbol.createSymbol(2);
		evaluationTest();
		
		// Note: Arbitrary precision supported by Rationals
		// in this case you get 1.3333333....
		expressionString = "4.0/3.0";
		expected   = DefaultSymbol.createSymbol(new Rational("4/3"));
		evaluationTest();
		
		expressionString = "a/b";
		expected   = parse(expressionString);
		evaluationTest();
		
		expressionString = "4/0";
		expected   = parse(expressionString);
		evaluationTest();
	}
	
	@Test
	public void testNestedArithmetic() {
		Library library = new DefaultLibrary(
				new NestedArithmeticOperation());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "(A-B)-C";
		expected   = parse("A-(B+C)");
		evaluationTest();
		
		expressionString = "A-(B-C)";
		expected   = parse("(A+C)-B");
		evaluationTest();
		
		expressionString = "(A1 + C2 + A2 + A3 + C1) - (C1 + B1 + C2)";
		expected   = parse("(A1 + A2 + A3) - (B1)");
		evaluationTest();
		
		expressionString = "(A/B)/C";
		expected   = parse("A/(B*C)");
		evaluationTest();
		
		expressionString = "A/(B/C)";
		expected   = parse("(A*C)/B");
		evaluationTest();
		
		expressionString = "(A1 * C2 * A2 * A3 * C3) / (C2 * B1 * B2 * C3)";
		expected   = parse("(A1 * A2 * A3) / (B1 * B2)");
		evaluationTest();
		
		expressionString = "(A1 + A2 + A3 + (B1 - C1) + (Bk - Ck))";
		expected   = parse("(A1 + A2 + A3 + B1 + Bk) - (C1 + Ck)");
		evaluationTest();
		
		expressionString = "((Bk / Ck) * A1 * A2 * A3 * (B1 / C1))";
		expected   = parse("(Bk * A1 * A2 * A3 * B1) / (Ck * C1)");
		evaluationTest();
		
	}	
	
	@Test
	public void testGreaterThan() {
		Library library = new DefaultLibrary(
				new GreaterThan());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "3 > 1";
		expected   = DefaultSymbol.createSymbol(true);
		evaluationTest();

		expressionString = "3 > 3";
		expected   = DefaultSymbol.createSymbol(false);
		evaluationTest();

		expressionString = "1 > 3";
		expected   = DefaultSymbol.createSymbol(false);
		evaluationTest();

		expressionString = "1 > y";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "1 > false";
		expected = parse(expressionString);
		evaluationTest();
	}

	@Test
	public void testGreaterThanOrEqualTo() {
		Library library = new DefaultLibrary(
				new GreaterThanOrEqualTo());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "3 >= 1";
		expected   = DefaultSymbol.createSymbol(true);
		evaluationTest();
		
		expressionString = "3 >= 3";
		expected   = DefaultSymbol.createSymbol(true);
		evaluationTest();

		expressionString = "1 >= 3";
		expected   = DefaultSymbol.createSymbol(false);
		evaluationTest();

		expressionString = "1 >= y";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "1 >= false";
		expected = parse(expressionString);
		evaluationTest();
	}

	@Test
	public void testNotOnGreaterThanTo() {
		Library library = new DefaultLibrary(
				new NotOnGreaterThan());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "not (3 > 1)";
		expected = parse("3 <= 1");
		evaluationTest();
		
		expressionString = "not(3 > 3)";
		expected = parse("3 <= 3");
		evaluationTest();
	}

	@Test
	public void testAnd() {
		Library library = new DefaultLibrary(
				new AbsorbingElement("and", "false"),
				new And(),
				new Associative("and"));
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "true and x";
		expected   = DefaultSymbol.createSymbol("x");
		evaluationTest();

		expressionString = "true";
		expected   = DefaultSymbol.createSymbol(true);
		evaluationTest();

		expressionString = "x and y";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "true and false";
		expected   = DefaultSymbol.createSymbol(false);
		evaluationTest();
		
		expressionString = "x and false";
		expected = parse("false");
		evaluationTest();
		
		expressionString = "x and false and y and true";
		expected   = parse("false");
		evaluationTest();
		
		expressionString = "and(x, false, false and true, false and y, false)";
		expected   = parse("false");
		evaluationTest();
		
		expressionString = "and(x, true, false)";
		expected   = parse("false");
		evaluationTest();
		
		expressionString = "and(x)";
		expected = parse("x");
		evaluationTest();
		
		expressionString = "and";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "and()";
		expected = parse("true");
		evaluationTest();
	}

	@Test
	public void testAndWithoutAbsorbingElement() {
		Library library = new DefaultLibrary(
				new And(),
				new Associative("and"));
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "true";
		expected   = DefaultSymbol.createSymbol(true);
		evaluationTest();
	
		expressionString = "x and y";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "true and false";
		expected   = DefaultSymbol.createSymbol(false);
		evaluationTest();
		
		expressionString = "x and false";
		expected = parse("x and false");
		evaluationTest();
		
		expressionString = "x and false and y and true";
		expected   = parse("x and false and y");
		evaluationTest();
		
		expressionString = "and(x, false, false and true, false and y, false)";
		expected   = parse("and(x, false, y)");
		evaluationTest();
		
		expressionString = "and(x, true, false)";
		expected   = parse("x and false");
		evaluationTest();
		
		expressionString = "x";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "and";
		expected = parse(expressionString);
		evaluationTest();
	
		expressionString = "and()";
		expected = parse("true");
		evaluationTest();
	}

	@Test
	public void testOr() {
		Library library = new DefaultLibrary(
				new AbsorbingElement("or", "true"),
				new Or(),
				new Associative("or"));
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "false or x";
		expected   = DefaultSymbol.createSymbol("x");
		evaluationTest();

		expressionString = "x or false";
		expected   = DefaultSymbol.createSymbol("x");
		evaluationTest();

		expressionString = "true";
		expected   = DefaultSymbol.createSymbol(true);
		evaluationTest();
	
		expressionString = "x or y";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "true or false";
		expected   = DefaultSymbol.createSymbol(true);
		evaluationTest();
		
		expressionString = "x or true";
		expected = parse("true");
		evaluationTest();
		
		expressionString = "x or false or y or true";
		expected   = parse("true");
		evaluationTest();

		expressionString = "or()";
		expected = parse("false");
		evaluationTest();
	}
	
	@Test
	public void testIdempotentBooleanCommutativeAssociative() {
		Library library = new DefaultLibrary(
				new Or());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "x or y or x or y or z";
		expected   = parse("x or y or z");
		evaluationTest();
		
		expressionString = "x or x";
		expected   = DefaultSymbol.createSymbol("x");
		evaluationTest();
	}
	
	@Test
	public void testNot() {
		Library library = new DefaultLibrary(
				new Not());
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "not true";
		expected   = DefaultSymbol.createSymbol(false);
		evaluationTest();
		
		expressionString = "not false";
		expected   = DefaultSymbol.createSymbol(true);
		evaluationTest();
		
		expressionString = "not x";
		expected = parse(expressionString);
		evaluationTest();
		
		expressionString = "not x and y and x";
		expected = Expressions.FALSE;
		evaluationTest();
		
		expressionString = "not not x";
		expected = parse("x");
		evaluationTest();
	}

	@Test
	public void testSubstitution() {
		Library library = new DefaultLibrary(
				new ScopedVariables(),
				new ExpressionKnowledgeModule(),
				new ImposedConditionsModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider());
		evaluator = new ExhaustiveRewriter(library);

		Expression result;
		
		RewritingProcess process = newRewritingProcessWithCardinalityAndCounts(evaluator);

		Map<Expression, Expression> replacements= Util.map(parse("y"), parse("1"));
		result = Substitute.replaceAll(
				parse("x + 2"),
				replacements, process);
		assertEquals(parse("x + 2"), result);
		
		replacements = Util.map(parse("x"), parse("1"));
		result = Substitute.replaceAll(
				parse("x + 2"),
				replacements, process);
		assertEquals(parse("1 + 2"), result);
		
		replacements = Util.map();
		result = Substitute.replaceAll(
				parse("x + 2"),
				replacements, process);
		assertEquals(parse("x + 2"), result);
		
		replacements = Util.map(
				parse("x"), parse("2"),
				parse("2"), parse("10"));
		result = Substitute.replaceAll(
				parse("x + 2"),
				replacements, process);
		assertEquals(parse("2 + 10"), result);
		
		replacements = Util.map(parse("x"), parse("2"));
		result = Substitute.replaceAll(
				parse("f(g(h(x)))"),
				replacements, process);
		assertEquals(parse("f(g(h(2)))"), result);
		
		replacements =Util.map(parse("x"), parse("2"));
		result = Substitute.replaceAll(
				parse("{(on x) x}"),
				replacements, process);
		assertEquals(parse("{(on x) x}"), result); // should respect scoped symbols
		
		replacements = Util.map(parse("x"), parse("2"));
		result = Substitute.replaceAll(
				parse("x + {(on x) f(x)}"),
				replacements, process);
		assertEquals(parse("2 + {(on x) f(x)}"), result); // should respect scoped symbols
		
		replacements = Util.map(parse("x"), parse("2"));
		result = Substitute.replaceAll(
				parse("x + {(on x in group(x)) f(x)}"),
				replacements, process);
		assertEquals(parse("2 + {(on x in group(2)) f(x)}"), result); //  should respect scoped symbols; group(x) is not scoped by inner x
		
		replacements = Util.map(parse("x"), parse("2"));
		result = Substitute.replaceAll(
				parse("x + {(on y) f(x)}"),
				replacements, process);
		assertEquals(parse("2 + {(on y) f(2)}"), result); // x is not scoped here
		
	}
	
	@Test
	public void testNewSubstitute() {
		Library library = new DefaultLibrary(
				new ScopedVariables(),
				new IntensionalSet(),
				new ExpressionKnowledgeModule(),
				new ImposedConditionsModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new CompleteSimplify(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider());
		evaluator = new ExhaustiveRewriter(library);

		Expression result;
		Map<Expression, Expression> replacements;

		RewritingProcess process = newRewritingProcessWithCardinalityAndCounts(evaluator);
		
		//
		// IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch failure condition (i.e. keeps expanding else branch in the manner below), 
		// when calling NewSubstitute.
		replacements = Util.map(parse("W = 10"), parse("false"));
		result = NewSubstitute.replaceAll(
				parse("if false then false else 10 = | type(X) |"),
				replacements, process);
		assertEquals(parse("if false then false else (if W = 10 then false else 10 = | type(X) |)"), result);

		//
		replacements = Util.map(parse("y"), parse("1"));
		result = NewSubstitute.replaceAll(
				parse("x + 2"),
				replacements, process);
		assertEquals(parse("x + 2"), result);
		
		replacements = Util.map(parse("x"), parse("1"));
		result = NewSubstitute.replaceAll(
				parse("x + 2"),
				replacements, process);
		assertEquals(parse("1 + 2"), result);
		
		replacements = Util.map();
		result = NewSubstitute.replaceAll(
				parse("x + 2"),
				replacements, process);
		assertEquals(parse("x + 2"), result);
		
		replacements = Util.map(
				parse("x"), parse("2"),
				parse("2"), parse("10"));
		result = NewSubstitute.replaceAll(
				parse("x + 2"),
				replacements, process);
		assertEquals(parse("2 + 10"), result);
		
		replacements = Util.map(parse("x"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("f(g(h(x)))"),
				replacements, process);
		assertEquals(parse("f(g(h(2)))"), result);
		
		replacements = Util.map(parse("X"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{(on X) X}"),
				replacements, process);
		assertEquals(parse("{(on X) X}"), result); // should respect scoped symbols
		
		replacements = Util.map(parse("x"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("x + {(on x) f(x)}"),
				replacements, process);
		assertEquals(parse("2 + {(on x) f(x)}"), result); // should respect scoped symbols
		
		replacements = Util.map(parse("x"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("x + {(on x in group(x)) f(x)}"),
				replacements, process);
		assertEquals(parse("2 + {(on x in group(2)) f(x)}"), result); //  should respect scoped symbols; group(x) is not scoped by inner x
		
		replacements = Util.map(parse("x"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("x + {(on y) f(x)}"),
				replacements, process);
		assertEquals(parse("2 + {(on y) f(2)}"), result); // x is not scoped here
		
		replacements = Util.map(parse("p(a)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{(on q(a)) p(a)}"),
				replacements, process);
		assertEquals(parse("{(on q(a)) 2}"), result);
		
		replacements = Util.map(parse("p(a)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{(on p(a)) p(a)}"),
				replacements, process);
		assertEquals(parse("{(on p(a)) p(a)}"), result);
		
		replacements = Util.map(parse("p(a)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{(on p(X)) p(a)}"),
				replacements, process);
		assertEquals(parse("{(on p(X)) if X != a then 2 else p(a)}"), result);
		
		replacements = Util.map(parse("p(X)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{(on p(a)) p(X)}"),
				replacements, process);
		assertEquals(parse("{(on p(a)) if X != a then 2 else p(X)}"), result);
		
		replacements = Util.map(parse("p(Y)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("p(X)"),
				replacements, process);
		assertEquals(parse("if X = Y then 2 else p(X)"), result);
		
		replacements = Util.map(parse("p(X)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{(on p(a), p(b), p(c)) p(X)}"),
				replacements, process);
		assertEquals(parse("{(on p(a), p(b), p(c)) if X != a and X != b and X != c then 2 else p(X)}"), result);
		
		replacements = Util.map(parse("p(X,Y)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{(on p(Y,X)) p(X,Y)}"),
				replacements, process);
		assertEquals(parse("{(on p(Y,X)) if X != Y then 2 else p(X,Y)}"), result);
		
		replacements = Util.map(parse("p(X,Y)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{(on p(W,Z)) p(Y,X)}"),
				replacements, process);
		assertEquals(parse("{(on p(W,Z)) if (W != X or Z != Y) and X = Y then 2 else p(Y,X)}"), result);
		
		replacements = Util.map(parse("p(X,Y)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{(on p(Y,X)) p(Y,X)}"),
				replacements, process);
		assertEquals(parse("{(on p(Y,X)) p(Y,X)}"), result);

		replacements = Util.map(parse("p(X,Y)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("if W != X and Z != Y then p(W,Z) else p(a,Y)"),
				replacements, process);
		assertEquals(parse("if W != X and Z != Y then p(W,Z) else if X = a then 2 else p(a,Y)"), result);

		replacements = Util.map(parse("p(X,Y)"), parse("2"));
		result = NewSubstitute.replaceAll(
				parse("{{ (on p(a,Y)) {{ (on p(c,Y)) p(X,Y) }} or p(X,Y) | X != b }}"),
				replacements, process);
		assertEquals(parse("{{ (on p(a,Y))" +
				               "{{ (on p(c,Y)) if X != a and X != c then 2 else p(X,Y) }} or (if X != a then 2 else p(X,Y))" +
				               "| X != b }}"), result);
}
	
	@Test
	public void testDepthFirstIterator() {
		Expression expression;
		List<Expression> expected;
		
		expression = parse("f");
		expected = Util.list(expression);
		Assert.assertEquals(expected, Util.listFrom(new SubExpressionsDepthFirstIterator(expression)));
		
		expression = parse("f(x)");
		expected = Util.list(
				parse("f(x)"),
				parse("f"),
				parse("x")
				);
		Assert.assertEquals(expected, Util.listFrom(new SubExpressionsDepthFirstIterator(expression)));
		
		expression = parse("f(f(x) + g(x))");
		expected = Util.list(
				parse("f(f(x) + g(x))"),
				parse("f"),
				parse("f(x) + g(x)"),
				parse("+"),
				parse("f(x)"),
				parse("f"),
				parse("x"),
				parse("g(x)"),
				parse("g"),
				parse("x")
				);
		Assert.assertEquals(expected, Util.listFrom(new SubExpressionsDepthFirstIterator(expression)));
	}
	
	@Test
	public void testDistributive() {
		Library library = new DefaultLibrary(
				new Distributive("*", "+"),
				new Associative("+", "*", "and"));
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "(1 + x + y)*(x + 3)";
		expected = parse("1*x + 1*3 + x*x + x*3 + y*x + y*3");
		evaluationTest();

		// Testing case where one one of the operator applications is empty.
		// We use an evaluator with the distributive law alone so +() does not get evaluated to 0.
		expressionString = "+()*(x + 3)";
		expected = parse("+()"); // Cartesian product of arguments of + in first expression is empty, so the list of resulting applications of * is also empty.
		evaluationTest(new ExhaustiveRewriter(new Distributive("*", "+")));

		expressionString = "(1 * x * y)+(x * 3)"; // + does not distribute over *
		expected = parse("(1 * x * y)+(x * 3)");
		evaluationTest();
	}
	
	@Test
	public void testScopedVariables() {
		Library library = new DefaultLibrary(
				new ExpressionKnowledgeModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSet(),
				new ScopedVariables());
		
		evaluator = new ExhaustiveRewriter(library);

		expressionString = "'scoped variables'({(on X) p(X) | X != a})";
		expected = parse("list(X)");
		evaluationTest();

		expressionString = "'scoped variables'({(on X,Y) p(X,Y) | X != a})";
		expected = parse("list(X,Y)");
		evaluationTest();

		expressionString = "'scoped variables'(f(X,Y))";
		expected = parse("list()");
		evaluationTest();
	}
	
	@Test
	public void testConditionsThatExpressionImposesModule() {
		Library library = new DefaultLibrary(
				new ImposedConditionsModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider());
		
		evaluator = new ExhaustiveRewriter(library);
		
		Expression expression = parse("aConstantSymbol");
		RewritingProcess process = new DefaultRewritingProcess(expression, evaluator);
		
		List<Pair<Expression, List<Integer>>> conditions = ImposedConditionsModule.get(expression, 
				process);
		Assert.assertNull(conditions);
		
		expression = parse("if A = B then aAndBEqual else aAndBNotEqual");
		conditions = ImposedConditionsModule.get(expression, process);
		Assert.assertNotNull(conditions);
		Assert.assertEquals(2, conditions.size());
		Assert.assertEquals("A = B", conditions.get(0).first.toString());
		Assert.assertEquals(IfThenElse.getPathToThen(), conditions.get(0).second);
		Assert.assertEquals("not (A = B)", conditions.get(1).first.toString());
		Assert.assertEquals(IfThenElse.getPathToElse(), conditions.get(1).second);
		
		expression = parse("{(on X) X | X != a}");
		conditions = ImposedConditionsModule.get(expression, process);
		Assert.assertNotNull(conditions);
		Assert.assertEquals(1, conditions.size());
		Assert.assertEquals("X != a", conditions.get(0).first.toString());
		Assert.assertEquals(IntensionalSet.getPathToHead(), conditions.get(0).second);
	}
	
	@Test
	public void testConditionsThatExpressionImposesViaExpressionKnowledgeModule() {
		Library library = new DefaultLibrary(
				new ExpressionKnowledgeModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider());
		
		evaluator = new ExhaustiveRewriter(library);
		
		List<ExpressionAndContext> expressionsAndContext = null;
		Expression expression = parse("aConstantSymbol");
		RewritingProcess process = new DefaultRewritingProcess(expression, evaluator);
		
		expression = parse("if A = B then aAndBEqual else aAndBNotEqual");
		expressionsAndContext = Util.listFrom(
				ExpressionKnowledgeModule.getKnowledgeBasedImmediateSubExpressionsAndContextIteratorAfterBookkeeping(expression, process));
		Assert.assertEquals(4, expressionsAndContext.size());
		//
		Assert.assertEquals("'if . then . else .'", expressionsAndContext.get(0).getExpression().toString());
		Assert.assertEquals("true", expressionsAndContext.get(0).getConstrainingCondition().toString());
		Assert.assertEquals(IfThenElse.getPathToFunctor(), expressionsAndContext.get(0).getPath());
		//
		Assert.assertEquals("A = B", expressionsAndContext.get(1).getExpression().toString());
		Assert.assertEquals("true", expressionsAndContext.get(1).getConstrainingCondition().toString());
		Assert.assertEquals(IfThenElse.getPathToCondition(), expressionsAndContext.get(1).getPath());
		//
		Assert.assertEquals("aAndBEqual", expressionsAndContext.get(2).getExpression().toString());
		Assert.assertEquals("A = B", expressionsAndContext.get(2).getConstrainingCondition().toString());
		Assert.assertEquals(IfThenElse.getPathToThen(), expressionsAndContext.get(2).getPath());
		//
		Assert.assertEquals("aAndBNotEqual", expressionsAndContext.get(3).getExpression().toString());
		Assert.assertEquals("not (A = B)", expressionsAndContext.get(3).getConstrainingCondition().toString());
		Assert.assertEquals(IfThenElse.getPathToElse(), expressionsAndContext.get(3).getPath());
		
		expression = parse("{(on X) X | X != a}");
		expressionsAndContext = Util.listFrom(
				ExpressionKnowledgeModule.getKnowledgeBasedImmediateSubExpressionsAndContextIteratorAfterBookkeeping(expression, process));
		Assert.assertEquals(2, expressionsAndContext.size());
		Assert.assertEquals("X", expressionsAndContext.get(0).getExpression().toString());
		Assert.assertEquals("X != a", expressionsAndContext.get(0).getConstrainingCondition().toString());
		Assert.assertEquals(IntensionalSet.getPathToHead(), expressionsAndContext.get(0).getPath());
		Assert.assertEquals("X != a", expressionsAndContext.get(1).getExpression().toString());
		Assert.assertEquals("true", expressionsAndContext.get(1).getConstrainingCondition().toString());
		Assert.assertEquals(IntensionalSet.getPathToCondition(), expressionsAndContext.get(1).getPath());
	}
	
	@Test
	public void testIfThenElse() {
		Library library = new DefaultLibrary(
				new IfThenElse());
		
		evaluator = new ExhaustiveRewriter(library);

		expressionString = "if true then 1 else 2";
		expected = parse("1");
		evaluationTest();

		expressionString = "if false then 1 else 2";
		expected = parse("2");
		evaluationTest();

		expressionString = "if X then 1 else 2";
		expected = parse("if X then 1 else 2");
		evaluationTest();
	}
	
	@Test
	public void testIfThenElseExternalization() {
		Library library = new DefaultLibrary(
				new ExpressionKnowledgeModule(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSet(),
				new ScopedVariables(),
				new IfThenElseExternalization());
		
		evaluator = new ExhaustiveRewriter(library);

		expressionString = "f(a, b, c, if Y = 2 then X else X + 1, d, e, f)";
		expected = parse("if Y = 2 then f(a, b, c, X, d, e, f) else f(a, b, c, X + 1, d, e, f)");
		evaluationTest();

		expressionString = "{(on X) if Y = 2 then X else X + 1 | X != a}";
		expected = parse("if Y = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}");
		evaluationTest();

		expressionString = "{(on X) if X = 2 then X else X + 1 | X != a}";
		expected = parse("{(on X) if X = 2 then X else X + 1 | X != a}");
		evaluationTest();

		expressionString = "{(on X) if p(Y) = 2 then X else X + 1 | X != a}";
		expected = parse("if p(Y) = 2 then {(on X) X | X != a} else {(on X) X + 1 | X != a}");
		evaluationTest();

		expressionString = "{(on X) if p(X) = 2 then X else X + 1 | X != a}";
		expected = parse("{(on X) if p(X) = 2 then X else X + 1 | X != a}");
		evaluationTest();

		// the next two tests depends on knowledge-based sub-expression provisions.
		// no scoping problems:
		expressionString = "{(on X in (if Y = a then Set1 else Set2)) p(X) | X != a}"; // lack of ()'s around if then else makes parse fail, not sure why. Entered in bug database.
		expected = parse("if Y = a then {(on X in Set1) p(X) | X != a} else {(on X in Set2) p(X) | X != a}");
		evaluationTest();

		// scoping problems:
		expressionString = "{(on Y, X in (if Y = a then Set1 else Set2)) p(X) | X != a}";
		expected = parse("{(on Y, X in (if Y = a then Set1 else Set2)) p(X) | X != a}");
		evaluationTest();
	}
	
	@Test
	public void testIfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch() {
		Library library = new DefaultLibrary(
				new IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch(),
				new Equality(),
				new Disequality(),
				new IfThenElse(),
				new And(),
				new Or(),
				// Required Modules
				new ScopedVariables(),
				new IntensionalSet(),
				new ExpressionKnowledgeModule(),
				new ImposedConditionsModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider());
		
		evaluator = new ExhaustiveRewriter(library);

		expressionString = "if even(X) then f(even(X)) else g(even(X))";
		expected = parse("if even(X) then f(true) else g(false)");
		evaluationTest();

		expressionString = "if even(X) then f(even(Y)) else g(even(X))";
		expected = parse("if even(X) then f(if Y = X then true else even(Y)) else g(false)");
		evaluationTest();

		expressionString = "if even(X) then {(on even(a)) f(even(Y))} else g(even(X))";
		expected = parse("if even(X) then {(on even(a)) f(if Y != a and Y = X then true else even(Y))} else g(false)");
		evaluationTest();
	}
	
	
	@Ignore("Not currently supported.")
	@Test
	public void testIfThenElseConditionIsTrueInThenBranchAndFalseInElseBranchOnEquality() {
		Library library = new DefaultLibrary(
				new IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch(),
				new Equality(),
				new Disequality(),
				new IfThenElse(),
				new And(),
				new Or(),
				// Required Modules
				new ScopedVariables(),
				new IntensionalSet(),
				new ExpressionKnowledgeModule(),
				new ImposedConditionsModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider());
		
		evaluator = new ExhaustiveRewriter(library);
		
		RewritingProcess process = newRewritingProcessWithCardinalityAndCounts(evaluator);
		
		// substitutions on = and != only take place when they are non-trivial.
		expressionString = "if X = Y then f(X = Y) else g(X = Y)";
		expected =   parse("if X = Y then f(true) else g(false)");
		evaluationTest(process);

		expressionString = "if X = Y then f(X = Z) else g(X = Y)";
		expected =   parse("if X = Y then f(X = Z) else g(false)");
		evaluationTest(process);

		expressionString = "if X = Y then {(on X = a) f(X = Y)} else g(X = Y)";
		expected =   parse("if X = Y then {(on X = a) f(X = Y)} else g(false)");
		evaluationTest(process);
		
		// Test For: ALBP-78
		expressionString = "if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2";
		expected =   parse("if X = a and Y = b then if p(a) and q(a, b) then E1 else E2 else if p(X) and q(X, Y) then E1 else E2");
		evaluationTest(process);
	}
	
	@Test
	public void testStandardizedApartWithoutAssumingImplicitQuantificationOfAllVariablesInFirstExpression() {
		Library library = new CommonLibrary();
		
		evaluator = new ExhaustiveRewriter(library);

		Expression expression1;
		Expression expression2;
		
		expression1 = parse("f(X)");
		expression2 = parse("f(X)");
		expected    = expression1;
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);
		
		expression1 = parse("g(X',X',X) and h(Y)");
		expression2 = parse("f(X',X'',Y)");
		expected    = expression1;
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);
		
		expression1 = parse("{(on X) X | X != a }");
		expression2 = parse("{(on X) X | X != b }");
		expected    = parse("{(on X') X' | X' != a }");
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on X, Y) f(X,Y) | X != a }");
		expression2 = parse("{(on X) X | X != b }");
		expected    = parse("{(on X', Y) f(X',Y) | X' != a }");
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on X, Y) f(X,Y) | X != a }");
		expression2 = parse("f(X)");
		expected    = parse("{(on X', Y) f(X',Y) | X' != a }"); // Y does not collide with a free variable in expression2
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on X) f(X,Y) | X != a }");
		expression2 = parse("{(on X) f(X)}");
		expected    = parse("{(on X') f(X',Y) | X' != a }");
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on X) f(X,Y) | X != a }");
		expression2 = parse("f(X, Y)");
		expected    = parse("{(on X') f(X',Y) | X' != a }"); // Y is a free variable
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on X, Y) f(X,Y) | X != a }");
		expression2 = parse("f(X, Y)");
		expected    = parse("{(on X', Y') f(X',Y') | X' != a }"); // Y is scoping variable and collides with free variable
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on X, Y) f(X,Y) | X != a }");
		expression2 = parse("{(on Y) f(X, Y) }"); // Y is not free variable
		expected    = parse("{(on X', Y') f(X',Y') | X' != a }");
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on X, X') f(X,X') | X != a }");
		expression2 = parse("f(X)");
		expected    = parse("{(on X'', X') f(X'',X') | X'' != a }"); // first standardized apart variable collides with already existing one.
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on X, X') f(X,X') | X != a }");
		expression2 = parse("f(X, X')");
		expected    = parse("{(on X'', X''') f(X'',X''') | X'' != a }"); // first standardized apart variable collides with already existing one that is also being replaced.
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on Z) {(on X, Y) f(X,Y,Z) | X != a } | Z != c}");
		expression2 = parse("{(on Y) f(X, Y, Z) }");
		expected    = parse("{(on Z') {(on X', Y') f(X',Y',Z') | X' != a } | Z' != c}"); // testing standardization apart of sub-expressions
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on Z) {(on Z', Y) f(X,Y,Z') | X != a } | Z != c}");
		expression2 = parse("{(on Y) f(X, Y, Z, Z') }");
		expected    = parse("{(on Z''') {(on Z'', Y') f(X,Y',Z'') | X != a } | Z''' != c}"); // testing standardization apart of sub-expressions when a new variable collides with a nested one.
		testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);

		expression1 = parse("{(on X, p(Z)) f(X,Y) | X != a }");
		expression2 = parse("{(on X) f(X)}");
		expected    = null; // no SA for function application indices
		try {
			testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(expression1, expression2);
			fail("Should have thrown a StandardizedApartFrom.StandardizingApartOnFunctionApplicationsNotSupported");
		} catch (StandardizedApartFrom.StandardizingApartOnScopingFunctionApplicationsNotSupported error) {
			
		}
	}

	private void testStandardizationApartWithoutAssumingImplicitQuantificationOfAllVariables(Expression expression1, Expression expression2) {
		Expression result;
		result = StandardizedApartFrom.standardizedApartFrom(expression1, expression2, new DefaultRewritingProcess(expression1, evaluator));
		System.out.println("Standardization apart of " + expression1 + "  wrt " + expression2 + " (not assuming implicit quantification):\n                         " + result + "\n               Expected: " + expected);
		assertEquals(result, expected);
	}
	
	@Test
	public void testIntensionalSet() {
		Library library = new DefaultLibrary(
				new ScopedVariables(),
				new ExpressionKnowledgeModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSet(),
				new UnionOnExtensionalSets(),
				new Plus(),
				new And(),
				new Equality(),
				new IfThenElse());
		
		evaluator = new ExhaustiveRewriter(library);

		expressionString = "{(on value of <X>) p(X,Y) + 1 + 1}";
		expected = parse("{(on value of <X>) p(X,Y) + 2}");
		evaluationTest();
		
		expressionString = "{(on X in {1,2,3}) p(X) | false}";
		expected = parse("{ }");
		evaluationTest();

		expressionString = "{(on X in {1,2,3} - {1}, Y in {1,2} union {3}) p(X,Y) + 1 + 1 | true and Z}";
		expected = parse("{(on X in ({1,2,3} - {1}), Y in {1,2,3}) p(X,Y) + 2 | Z}");
		evaluationTest();

		expressionString = "{(on X in {1,2,3} - {1}, Y) p(X,Y) + 1 + 1}";
		expected = parse("{(on X in ({1,2,3} - {1}), Y) p(X,Y) + 2}");
		evaluationTest();

		expressionString = "{(on value of {<X>,<Y>}) p(X,Y) + 1 + 1}";
		expected = parse("{(on value of {<X>,<Y>}) p(X,Y) + 2}");
		evaluationTest();

		expressionString = "if V = (<X>) then {(on value of V) p(X,Y) + 1 + 1} else 0";
		expected = parse("if V = (<X>) then {(on value of V) p(X,Y) + 2} else 0");
		evaluationTest();
		
		expressionString = "{(on value of <X>, Y in {1,2} union {3}) p(X,Y) + 1 + 1}";
		expected = parse("{ ( on value of <X>, Y in { 1, 2, 3 } ) (p(X, Y) + 2) }");
		evaluationTest();
	}
	
	@Test
	public void testCannotIndirectlyMutateExpression() {
		Library library = new DefaultLibrary(
				new ScopedVariables(),
				new ExpressionKnowledgeModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider(),
				new IntensionalSet(),
				new UnionOnExtensionalSets(),
				new Plus(),
				new And(),
				new Equality(),
				new IfThenElse());
		
		evaluator = new ExhaustiveRewriter(library);
		
		Expression S = parse("{(on X,Y) f(X) | Z = X}");	
		String expectedS = S.toString();
		List<Expression> indexExpressions = IntensionalSet.getIndexExpressions(S);
		try {
			indexExpressions.remove(0);
			Assert.fail("An exception should have thrown");
		} catch (Exception ex) {
			// Expected
		}
		Assert.assertEquals(expectedS, S.toString());
	}

	
	@Test
	public void testNormalizeExtensionalUniSet() {
		
		evaluator = new NormalizeExtensionalUniSet();

		expressionString = "{ }";
		expected = parse("{ }");
		evaluationTest();

		expressionString = "{a, b, c}";
		expected = parse("{a, b, c}");
		evaluationTest();

		expressionString = "{a, a, b}";
		expected = parse("{a, b}");
		evaluationTest();

		expressionString = "{a, b, b}";
		expected = parse("{a, b}");
		evaluationTest();

		expressionString = "{a, b, b, a}";
		expected = parse("{a, b}");
		evaluationTest();

		expressionString = "{a, b, b, a, b}";
		expected = parse("{a, b}");
		evaluationTest();

		expressionString = "{X, Y}";
		expected = parse("if X = Y then {X} else {X,Y}");
		evaluationTest();

		expressionString = "{X, Y, a}";
		expected = parse(
				"if X = Y "+ 
		        "then if X = a "+
			    "     then { X } "+
		        "     else { X, a } "+
			    "else if X = a " +
		        "     then { X, Y } "+
			    "     else if Y = a " +
		        "          then {X,Y} " +
			    "          else {X,Y,a}");
						
		evaluationTest();
	}
	
	@Test
	public void testPartition() {
		Library library = new DefaultLibrary(
				new Partition());
		
		evaluator = new ExhaustiveRewriter(library);

		expressionString = "partition({}, X, {}, Y)";
		expected = parse("partition(X, Y)");
		evaluationTest();

		expressionString = "partition({}, X, {1}, Y, {2,3})";
		expected = parse("partition({1,2,3}, X, Y)");
		evaluationTest();

		expressionString = "partition({}, {})";
		expected = parse("{}");
		evaluationTest();

		expressionString = "partition(X)";
		expected = parse("X");
		evaluationTest();
	}

	@Test
	public void testEqualityOfExtensionalSets() {
		Library library = new DefaultLibrary(
				new EqualityOfExtensionalUniSets());
		
		evaluator = new ExhaustiveRewriter(library);

		expressionString = "{} = {}";
		expected = parse("true");
		evaluationTest();

		expressionString = "{X} = {}";
		expected = parse("false");
		evaluationTest();

		expressionString = "{} = {X}";
		expected = parse("false");
		evaluationTest();

		expressionString = "{X, Y, Z} = {U, V}";
		expected = parse("(X = U or X = V) and (Y = U or Y = V) and (Z = U or Z = V) and (U = X or U = Y or U = Z) and (V = X or V = Y or V = Z)");
		evaluationTest();

		expressionString = "{X} = {U, V} = Alpha";
		expected = parse("(X = U or X = V) and U = X and V = X and { U, V } = Alpha");
		evaluationTest();

		expressionString = "Alpha = {X} = {U, V} = Beta";
		expected = parse("(X = U or X = V) and U = X and V = X and { U, V } = Alpha = Beta");
		evaluationTest();

		expressionString = "{X} = Alpha = {U, V}";
		expected = parse("(X = U or X = V) and U = X and V = X and {U, V} = Alpha");
		evaluationTest();

		expressionString = "{{ X }} = Alpha = { U, V }";
		expected = parse(expressionString);
		evaluationTest();

		expressionString = "{ X } = { Y } = Alpha = { Z }";
		expected = parse("X = Y and Y = X and (Y = Z and Z = Y and { Z } = Alpha)");
		evaluationTest();
	}

	@Test
	public void testEqualityOfIntensionalSets() {
		Library library = new DefaultLibrary(
				new EqualityOfIntensionalUniSets(),
				new IntensionalSet(),
				new IntensionalSetSubExpressionsAndImposedConditionsProvider(),
				new ExpressionKnowledgeModule(),
				new ScopedVariables());
		
		evaluator = new ExhaustiveRewriter(library);

		expressionString = "{ (on X, Y) Alpha | X != Y } = { (on Z, W) Beta | Z != a }";
		expected = parse("(for all X : (for all Y : X != Y => (there exists Z : (there exists W : Z != a and Alpha = Beta)))) and (for all Z : (for all W : Z != a => (there exists X : (there exists Y : X != Y and Beta = Alpha))))");
		evaluationTest();

		expressionString = "{ (on X, Y) Alpha | X != Y } = { (on X, Y) Beta | X != a }";
		expected = parse("(for all X : (for all Y : X != Y => (there exists X' : (there exists Y' : X' != a and Alpha = Beta)))) and (for all X' : (for all Y' : X' != a => (there exists X : (there exists Y : X != Y and Beta = Alpha))))");
		evaluationTest();

		expressionString = "{ (on X, Y) Alpha | X != Y } = { (on X, Y) Beta | X != a } = Gamma";
		expected = parse("(for all X : (for all Y : X != Y => (there exists X' : (there exists Y' : X' != a and Alpha = Beta)))) and (for all X' : (for all Y' : X' != a => (there exists X : (there exists Y : X != Y and Beta = Alpha)))) and { (on X, Y) Beta | X != a } = Gamma");
		evaluationTest();
	}

	@Test
	public void testTuple() {
		
		Library library = new DefaultLibrary(
				new ExpressionKnowledgeModule(),
				new Equality(),
				new Disequality(),
				
				new EqualityOnMutuallyExclusiveCoDomainExpressions(),
				new DisequalityOnMutuallyExclusiveCoDomainExpressions(),
				new MutuallyExclusiveCoDomainsModule(),
				
				new EqualityOnInjectiveSubExpressions(),
				new DisequalityOnInjectiveSubExpressions(),
				new ScopedVariables(),
				new InjectiveModule(),
				
				new Tuple()
		);

		Expression expression;
		
		expression = parse("(1, 2)");
		assertEquals(2, Tuple.size(expression));
		assertEquals(parse("2"), Tuple.get(expression, 1));

		evaluator = new ExhaustiveRewriter(library);

		expressionString = "(X, Y) = (X, Y, Z)";
		expected = parse("false");
		evaluationTest();

		expressionString = "tuple(X, Y) = (X, Y, Z)";
		expected = parse("false");
		evaluationTest();

		expressionString = "(X, Y) = tuple(X, Y, Z)";
		expected = parse("false");
		evaluationTest();

		expressionString = "(X, Y, Z) = (a, b, c)";
		expected = parse("X = a and Y = b and Z = c");
		evaluationTest();

		expressionString = "tuple(X, Y, Z) = tuple(a, b, c)";
		expected = parse("X = a and Y = b and Z = c");
		evaluationTest();

		expressionString = "tuple(X, Y, Z) = (a, b, c)";
		expected = parse("X = a and Y = b and Z = c");
		evaluationTest();

		expressionString = "(X, Y, Z) = tuple(a, b, c)";
		expected = parse("X = a and Y = b and Z = c");
		evaluationTest();
	}

	@Test
	public void testUnionOnExtensionalSets() {
		Library library = new DefaultLibrary(
				new UnionOnExtensionalSets(),
				new Associative("union"));
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "3";
		expected   = DefaultSymbol.createSymbol(3);
		evaluationTest();
	
		expressionString = "{x} union {y}";
		expected = parse("{x, y}");
		evaluationTest();
		
		expressionString = "{x, y} union {z}";
		expected = parse("{x, y, z}");
		evaluationTest();
		
		expressionString = "{x, y} union {}";
		expected = parse("{x, y}");
		evaluationTest();
		
		expressionString = "{} union {x, y} union {}";
		expected = parse("{x, y}");
		evaluationTest();
		
		expressionString = "A union {x, y} union {}";
		expected = parse("A union {x, y}");
		evaluationTest();
		
		expressionString = "A union {x, y} union {z} union B";
		expected = parse("A union {x, y, z} union B");
		evaluationTest();
	}
	
	@Test
	public void testLambda() {
		List<Rewriter> rewriters = new DefaultLibrary(
				new Equality(),
				new ExpressionKnowledgeModule(),
				new IfThenElseSubExpressionsAndImposedConditionsProvider(),
				new Lambda(),
				new LambdaApplication(),
				new ScopedVariables()
		);

		evaluator = new ExhaustiveRewriter(rewriters);

		expressionString = "if Z = a then f(lambda Z : g(Z)) else 0";
		expected = parse("if Z = a then f(lambda Z : g(Z)) else 0");
		evaluationTest();
		
		expressionString = "'scoped variables'(lambda Z : Z)";
		expected = parse("list(Z)");
		evaluationTest();
		
		expressionString = "'scoped variables'(lambda f(X) : f(X))";
		expected = parse("list(f(X))");
		evaluationTest();
		
		expressionString = "if X = a then lambda f(X) : f(X) else 1";
		expected = parse("if X = a then lambda f(X) : f(X) else 1");
		evaluationTest();
		
		expressionString = "(lambda f(X) : 2 + f(X))(1)";
		expected = parse("2 + 1");
		evaluationTest();
	}
	
	@Test
	public void testFreeVariables() {
		Library library = new CommonLibrary();
		evaluator = new ExhaustiveRewriter(library);
//		evaluator = new ExhaustiveRewriter(library, writer);
		RewritingProcess process;
		Set<Expression> expected;
		Set<Expression> freeVariables;
		
		expressionString = "product({{(on X, Y) f(X, Y) | X != a and X = Y }}) * product({{(on Z) g(Z) | Z != a}})";
		expected = new HashSet<Expression>();
		expression = parse(expressionString);
		process = new DefaultRewritingProcess(expression, evaluator);
		freeVariables = Variables.freeVariables(expression, process);
		System.out.println("Computed free variables of " + expression + ": " + Util.join(freeVariables));
		process.notifyEndOfRewritingProcess();
		assertEquals(expected, freeVariables);
		
		expressionString = "product({{(on X, Y) f(X, Y) | X != a and X = Y and W = a}})";
		expected = Util.set(parse("W"));
		expression = parse(expressionString);
		process = new DefaultRewritingProcess(expression, evaluator);
		freeVariables = Variables.freeVariables(expression, process);
		System.out.println("Computed free variables of " + expression + ": " + Util.join(freeVariables));
		process.notifyEndOfRewritingProcess();
		assertEquals(expected, freeVariables);
		
		expressionString = "X + product({{(on X, Y) |{(on W) W }|  |  X != a and X = Y}})";
		expected = Util.set(parse("X"));
		expression = parse(expressionString);
		process = new DefaultRewritingProcess(expression, evaluator);
		freeVariables = Variables.freeVariables(expression, process);
		System.out.println("Computed free variables of " + expression + ": " + Util.join(freeVariables));
		process.notifyEndOfRewritingProcess();
		assertEquals(expected, freeVariables);
	}
	
	@Test
	public void testSyntacticVariables() {
		Library library = new CommonLibrary();
		evaluator = new ExhaustiveRewriter(library);
		
		expressionString = "if X = a then type(X) else nothing";
		expected   = parse("if X = a then type(X) else nothing");
		evaluationTest();
		
		expressionString = "if X = a then f(X, type(X)) else nothing";
		expected   = parse("if X = a then f(X, type(X)) else nothing");
		evaluationTest();
	}

	//@Test
	public void testProcessorSpeed() {
		for (int i = 0; i != 1500000000; i++) {
			if (i % 1000 == 0) {
				System.out.println("...");
			}
		}
		System.out.println("done");
	}
	
	@Test
	public void testCache() {
		CacheTest rewriter = new CacheTest();
		Library library = new CommonLibrary(rewriter);
		evaluator = new ExhaustiveRewriter(library);
		
		Expression subExpression = parse("g()");
		Expression expression = Expressions.apply("f", subExpression, subExpression, subExpression);
		evaluator.rewrite(expression);
		assertEquals(1, rewriter.count); // rewriter must have been used only once
	}
	
	private static class CacheTest extends AbstractRewriter {
		public int count = 0;
		@Override
		public Expression rewriteAfterBookkeeping(
				Expression expression,
				RewritingProcess process) {
			if (expression.hasFunctor("g")) {
				count++;
				return DefaultSymbol.createSymbol("foo");
			}
			return expression;
		}
	}
	
	@Test
	public void testOpenInterpretation() {
		Library library = new CommonLibrary(
				new GDependsOnHOpenInterpretationModuleProvider(),
				new AlwaysFalseOpenInterpretationModuleProvider());
		evaluator = new ExhaustiveRewriter(library);
		RewritingProcess process;
		process = new DefaultRewritingProcess(expression, evaluator);

		expression = parse("f(X,g())");
//		assertEquals(OpenInterpretationModule.
//				findModuleAndIndicateOpenInterpretationWithRespectTo(expression, parse("X"), process),
//				true);
		assertEquals(OpenInterpretationModule.
				findModuleAndIndicateOpenInterpretationWithRespectTo(expression, parse("h()"), process),
				true);
		assertEquals(OpenInterpretationModule.
				findModuleAndIndicateOpenInterpretationWithRespectTo(expression, parse("a"), process),
				false);
	}
	
	private static class GDependsOnHOpenInterpretationModuleProvider
	extends AbstractRewriter
	implements OpenInterpretationModule.Provider {

		@Override
		public boolean isOpenInterpretationExpressionWithRespectTo(
				Expression expression, Expression anotherExpression,
				RewritingProcess process) {
			if (expression.hasFunctor("g")) {
				return anotherExpression.hasFunctor("h");
			}
			return false;
		}

		@Override
		public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
			return expression;
		}
		@Override
		public void rewritingProcessInitiated(RewritingProcess process) {
			OpenInterpretationModule openInterpretationModule =
				(OpenInterpretationModule) process.findModule(OpenInterpretationModule.class);
			if (openInterpretationModule != null) {
				openInterpretationModule.register(this);
			}
		}
	}
	
	private static class AlwaysFalseOpenInterpretationModuleProvider
	extends AbstractRewriter
	implements OpenInterpretationModule.Provider {

		@Override
		public boolean isOpenInterpretationExpressionWithRespectTo(
				Expression expression, Expression anotherExpression,
				RewritingProcess process) {
			return false;
		}

		@Override
		public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
			return expression;
		}
		@Override
		public void rewritingProcessInitiated(RewritingProcess process) {
			OpenInterpretationModule openInterpretationModule =
				(OpenInterpretationModule) process.findModule(OpenInterpretationModule.class);
			if (openInterpretationModule != null) {
				openInterpretationModule.register(this);
			}
		}
	}

	@Test
	public void testUnification() {
		Library library = new CommonLibrary();
		evaluator = new ExhaustiveRewriter(library);
		
		Expression expression1;
		Expression expression2;
		Expression expected;
		
		expression1 = parse("f(a,a)");
		expression2 = parse("f(a,a)");
		expected = Expressions.TRUE;
		testUnificationGivenInputs(expression1, expression2, expected);
		
		expression1 = parse("f(a,Y)");
		expression2 = parse("f(X,a)");
		expected = parse("a = X and Y = a");
		testUnificationGivenInputs(expression1, expression2, expected);
		
		expression1 = parse("U(a,Y)");
		expression2 = parse("V(X,Z)");
		expected = parse("U = V and a = X and Y = Z");
		testUnificationGivenInputs(expression1, expression2, expected);
		
		expression1 = parse("U(X,X)");
		expression2 = parse("V(a,b)");
		expected = parse("U = V and X = a and X = b"); // the condition provided is not a simplified one; if it were, this would be a contradiction.
		testUnificationGivenInputs(expression1, expression2, expected);
		
		expression1 = parse("f(a,Y)");
		expression2 = parse("f(b,a)");
		expected = Expressions.FALSE;
		testUnificationGivenInputs(expression1, expression2, expected);
	}

	private void testUnificationGivenInputs(Expression expression1,
			Expression expression2, Expression expected) {
		RewritingProcess process;
		Expression unificationCondition;
		process = new DefaultRewritingProcess(expression1, evaluator);
		unificationCondition = Unification.unificationCondition(expression1, expression2, process);
		assertEquals(expected, unificationCondition);
	}
	
	private RewritingProcess newRewritingProcessWithCardinalityAndCounts(Rewriter evaluator) {
		
		DefaultRewritingProcess process = new DefaultRewritingProcess(null, evaluator);
		process.setRewriterLookup(new DefaultRewriterLookup(DirectCardinalityComputationFactory.getCardinalityRewritersMap()));
		CountsDeclaration countsDeclaration = new CountsDeclaration(10);
		countsDeclaration.setup(process);
		
		return process;
	}
}
