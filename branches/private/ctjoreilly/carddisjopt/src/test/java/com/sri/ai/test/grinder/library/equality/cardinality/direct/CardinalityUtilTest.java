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
package com.sri.ai.test.grinder.library.equality.cardinality.direct;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.util.base.Pair;

public class CardinalityUtilTest extends AbstractGrinderTest {
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}
	
	@Test
	public void testILegalFormulaConstant() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		Assert.assertTrue(CardinalityUtil.isLegalFormulaConstant(parse("a"), process));
		Assert.assertTrue(CardinalityUtil.isLegalFormulaConstant(parse("ab"), process));
		Assert.assertTrue(CardinalityUtil.isLegalFormulaConstant(parse("aB"), process));
		Assert.assertTrue(CardinalityUtil.isLegalFormulaConstant(DefaultSymbol.createSymbol(false), process));
		Assert.assertTrue(CardinalityUtil.isLegalFormulaConstant(DefaultSymbol.createSymbol(true), process));
		Assert.assertTrue(CardinalityUtil.isLegalFormulaConstant(parse("1"), process));
		Assert.assertTrue(CardinalityUtil.isLegalFormulaConstant(parse("3.14"), process));
		Assert.assertTrue(CardinalityUtil.isLegalFormulaConstant(parse("<a>"), process));
		
		Assert.assertFalse(CardinalityUtil.isLegalFormulaConstant(parse("a = b"), process));
	}
	
	@Test
	public void testIsFiniteConstant() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		Assert.assertTrue(CardinalityUtil.isFiniteConstant(parse("a"), process));
		Assert.assertTrue(CardinalityUtil.isFiniteConstant(parse("ab"), process));
		Assert.assertTrue(CardinalityUtil.isFiniteConstant(parse("aB"), process));
		Assert.assertTrue(CardinalityUtil.isFiniteConstant(DefaultSymbol.createSymbol(false), process));
		Assert.assertTrue(CardinalityUtil.isFiniteConstant(DefaultSymbol.createSymbol(true), process));
		
		Assert.assertFalse(CardinalityUtil.isFiniteConstant(parse("<a>"), process));
		Assert.assertFalse(CardinalityUtil.isFiniteConstant(parse("1"), process));
		Assert.assertFalse(CardinalityUtil.isFiniteConstant(parse("3.14"), process));
		Assert.assertFalse(CardinalityUtil.isFiniteConstant(parse("a = b"), process));
	}
	
	@Test
	public void testIsFormula() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		//
		// the Boolean constants False and True are formulas;
		Assert.assertTrue(CardinalityUtil.isFormula(DefaultSymbol.createSymbol(false), process));
		Assert.assertTrue(CardinalityUtil.isFormula(DefaultSymbol.createSymbol(true), process));
		Assert.assertTrue(CardinalityUtil.isFormula(DefaultSymbol.createSymbol("false"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(DefaultSymbol.createSymbol("true"), process));
		//
		Assert.assertFalse(CardinalityUtil.isFormula(DefaultSymbol.createSymbol("a"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(DefaultSymbol.createSymbol("X"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(DefaultSymbol.createSymbol("1"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(DefaultSymbol.createSymbol("3.14"), process));
		//
		// if alpha and beta are variable or constant symbols of finite types,
		// then alpha = beta is a formula. 
		Assert.assertTrue(CardinalityUtil.isFormula(parse("a = X"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("a = b = X"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("X != a"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("a = b"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("true = false"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("a = b = c"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("a != b"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("true != false"), process));
		//
		Assert.assertTrue(CardinalityUtil.isFormula(parse("1 = true"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("1 = a"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("1 = X"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("true != 1"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("a != 1"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("X != 1"), process));
		//
		// if phi is a formula, then not(phi) is a formula
		Assert.assertTrue(CardinalityUtil.isFormula(parse("not(a = X)"), process));
		//
		Assert.assertTrue(CardinalityUtil.isFormula(parse("not(X = 1)"), process));
		//
		// if phi and phi' are formulas, then and(phi, phi'), and or(phi, phi') are formulas
		Assert.assertTrue(CardinalityUtil.isFormula(parse("and()"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("and(a = X)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("and(a = X, b = Y)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("and(a = X, b = Y, and(c = Z))"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("and(a = X, b = Y, or(c = Z))"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("or()"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("or(a = X)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("or(a = X, b = Y)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("or(a = X, b = Y, and(c = Z))"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("or(a = X, b = Y, or(c = Z))"), process));
		//
		Assert.assertTrue(CardinalityUtil.isFormula(parse("and(a = 1)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("and(a = X, b = 1)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("and(a = X, b = Y, and(c = 1))"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("and(a = X, b = Y, or(c = 1))"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("or(a = 1)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("or(a = X, b = 1)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("or(a = X, b = Y, and(c = 1))"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("or(a = X, b = Y, or(c = 1))"), process));
		//
		// if phi and phi' are formulas then phi => phi' and phi <=> phi' are formulas
		Assert.assertTrue(CardinalityUtil.isFormula(parse("(X = a) => (Y = b)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("(X = a) => (Y = b)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("(X = a) <=> (Y = b)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("(X = a) <=> (Y != b)"), process));
		//
		Assert.assertFalse(CardinalityUtil.isFormula(parse("a => b"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(parse("X => y"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(parse("a <=> b"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(parse("X <=> y"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("(X = a) => (Y = 1)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("(X = a) => (Y = 1)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("(X = a) <=> (Y = 1)"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("(X = a) <=> (Y != 1)"), process));
		//
		// if phi is a formula, then 'exists x phi' is a formula
		Assert.assertTrue(CardinalityUtil.isFormula(parse("there exists X : X = a"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("there exists X : there exists Y : X = a and Y = b"), process));
		//
		Assert.assertTrue(CardinalityUtil.isFormula(parse("there exists X : X = 1"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("there exists X : there exists Y : X = 1 and Y = b"), process));
		//
		// if phi is a formula, then 'for all x phi' is a formula 
		Assert.assertTrue(CardinalityUtil.isFormula(parse("for all X : X = a"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("for all X : for all Y : X = a and Y = b"), process));
		//
		Assert.assertTrue(CardinalityUtil.isFormula(parse("for all X : X = 1"), process));
		Assert.assertTrue(CardinalityUtil.isFormula(parse("for all X : for all Y : X = 1 and Y = b"), process));
		//
		// conditional expressions irrespective of their sub-expressions are not formulas.
		Assert.assertFalse(CardinalityUtil.isFormula(parse("if X = a then false else true"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(parse("if X = a then Y = b else Z = c"), process));
		//
		Assert.assertFalse(CardinalityUtil.isFormula(parse("if X = 1 then false else true"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(parse("if X = 1 then Y = b else Z = c"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(parse("if X = a then 1 else true"), process));
		Assert.assertFalse(CardinalityUtil.isFormula(parse("if X = a then Y = b else Z = 1"), process));
	}
	
	@Test
	public void testFindIndependentProblems() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		Expression f;
		List<Expression> indices;
		List<Pair<Set<Expression>, List<Expression>>> indProblems;
		
		// {(on X, Y) tuple(X, Y) | true }
		f           = parse("true");
		indices     = Tuple.getElements(parse("tuple(X, Y)"));
		indProblems = CardinalityUtil.findIndependentProblemsInConjunction(f, indices, process);
		// ([], [true]), ([X], []), ([Y], [])
		assertProblemsMatch(indProblems,
				"tuple()", "tuple(true)",
				"tuple(X)", "tuple()",
				"tuple(Y)", "tuple()");
		
		// {(on X, Y) tuple(X, Y) | Z = a }
		f           = parse("Z = a");
		indices     = Tuple.getElements(parse("tuple(X, Y)"));
		indProblems = CardinalityUtil.findIndependentProblemsInConjunction(f, indices, process);
		// ([], [Z = a]), ([X], []), ([Y], [])
		assertProblemsMatch(indProblems,
				"tuple()", "tuple(Z = a)",
				"tuple(X)", "tuple()",
				"tuple(Y)", "tuple()");

		// {(on X, Y) tuple(X, Y) | X != a and Y != a and Z = a }
		f           = parse("X != a and Y != a and Z = a");
		indices     = Tuple.getElements(parse("tuple(X, Y)"));
		indProblems = CardinalityUtil.findIndependentProblemsInConjunction(f, indices, process);
		// ([], [Z = a]), ([X], [X != a]), ([Y], [Y != a])
		assertProblemsMatch(indProblems,
				"tuple()", "tuple(Z = a)",
				"tuple(X)", "tuple(X != a)",
				"tuple(Y)", "tuple(Y != a)");
		
		f           = parse("X != a and Y != a and Z = a and W = a");
		indices     = Tuple.getElements(parse("tuple(X, Y)"));
		indProblems = CardinalityUtil.findIndependentProblemsInConjunction(f, indices, process);
		// ([], [Z = a and W = a]), ([X], [X != a]), ([Y], [Y != a])
		assertProblemsMatch(indProblems,
				"tuple()", "tuple(Z = a, W = a)",
				"tuple(X)", "tuple(X != a)",
				"tuple(Y)", "tuple(Y != a)");
		
		f           = parse("and((X != a and W != b), Y != a, Z = a, W != a)");
		indices     = Tuple.getElements(parse("tuple(X, Y)"));
		indProblems = CardinalityUtil.findIndependentProblemsInConjunction(f, indices, process);
		// ([], [Z = a]), ([X], [and(X != a, W != b), W != a]), ([Y], [Y != a])
		assertProblemsMatch(indProblems,
				"tuple()", "tuple(Z = a)",
				"tuple(X)", "tuple(and(X != a, W != b), W != a)",
				"tuple(Y)", "tuple(Y != a)");

		f           = parse("X = a or Y != b");
		indices     = Tuple.getElements(parse("tuple(X, Y)"));
		indProblems = CardinalityUtil.findIndependentProblemsInDisjunction(f, indices, process);
		// ([X], [X = a]), ([Y], [Y != b])
		assertProblemsMatch(indProblems,
				"tuple(X)", "tuple(X = a)",
				"tuple(Y)", "tuple(Y != b)");
	
		f           = parse("Z = a or X = a or Y != b");
		indices     = Tuple.getElements(parse("tuple(X, Y)"));
		indProblems = CardinalityUtil.findIndependentProblemsInDisjunction(f, indices, process);
		// ([], [Z = a]), ([X], [X = a]), ([Y], [Y != b])
		assertProblemsMatch(indProblems,
				"tuple()", "tuple(Z = a)",
				"tuple(X)", "tuple(X = a)",
				"tuple(Y)", "tuple(Y != b)");
		
		f           = parse("Z = a or X = a or Y != b or Y = Z");
		indices     = Tuple.getElements(parse("tuple(X, Y)"));
		indProblems = CardinalityUtil.findIndependentProblemsInDisjunction(f, indices, process);
		// ([], [Z = a]), ([X], [X = a]), ([Y], [Y != b])
		assertProblemsMatch(indProblems,
				//"tuple()", "tuple(Z = a)",
				"tuple(X)", "tuple(X = a)",
				"tuple(Y)", "tuple(Z = a, Y != b, Y = Z)");

		f           = parse("Z = a or X = a or Y != b");
		indices     = Tuple.getElements(parse("tuple(X, Y, Z)"));
		indProblems = CardinalityUtil.findIndependentProblemsInDisjunction(f, indices, process);
		// ([], [Z = a]), ([X], [X = a]), ([Y], [Y != b])
		assertProblemsMatch(indProblems,
				"tuple(Z)", "tuple(Z = a)",
				"tuple(Y, X)", "tuple(X = a, Y != b)");

		f           = parse("Z = a or X = a or Y != b or W != c");
		indices     = Tuple.getElements(parse("tuple(X, Y, Z)"));
		indProblems = CardinalityUtil.findIndependentProblemsInDisjunction(f, indices, process);
		// ([], [Z = a]), ([X], [X = a]), ([Y], [Y != b])
		assertProblemsMatch(indProblems,
				"tuple()", "tuple(W != c)",
				"tuple(Z)", "tuple(Z = a)",
				"tuple(Y, X)", "tuple(X = a, Y != b)");
	}
	
	//
	// PRIVATE METHODS
	//
	private void assertProblemsMatch(List<Pair<Set<Expression>, List<Expression>>> indProblems, 
			String... expectedPairs) {
		if (expectedPairs.length % 2 != 0) {
			Assert.fail("need expected to be pairs of tuples");
		}
		
		int expectedSize = expectedPairs.length / 2;
		Assert.assertEquals(expectedSize, indProblems.size());
		
 		Map<Set<Expression>, Expression> expectedIndProblems = new HashMap<Set<Expression>, Expression>();
 		for (int i = 0; i < expectedPairs.length; i += 2) {
 			Set<Expression> vars = new HashSet<Expression>(Tuple.getElements(parse(expectedPairs[i])));
 			expectedIndProblems.put(vars, And.make(Tuple.getElements(parse(expectedPairs[i+1]))));
 		}
 		
 		for (Pair<Set<Expression>, List<Expression>> problem : indProblems) {
 			Expression expectedConjunction = expectedIndProblems.get(problem.first);
 			if (expectedConjunction == null) {
 				Assert.fail("Unable to find expected variable set for:"+problem);
 			}
 			Expression actualConjunction = And.make(problem.second);
 			Assert.assertEquals(expectedConjunction, actualConjunction);
 		}
	}
}
