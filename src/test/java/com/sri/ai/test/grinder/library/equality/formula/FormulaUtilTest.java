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
package com.sri.ai.test.grinder.library.equality.formula;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.test.grinder.AbstractGrinderTest;

public class FormulaUtilTest extends AbstractGrinderTest {
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}
	
	@Test
	public void testILegalFormulaConstant() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		Assert.assertTrue(FormulaUtil.isLegalFormulaConstant(parse("a"), process));
		Assert.assertTrue(FormulaUtil.isLegalFormulaConstant(parse("ab"), process));
		Assert.assertTrue(FormulaUtil.isLegalFormulaConstant(parse("aB"), process));
		Assert.assertTrue(FormulaUtil.isLegalFormulaConstant(DefaultSymbol.createSymbol(false), process));
		Assert.assertTrue(FormulaUtil.isLegalFormulaConstant(DefaultSymbol.createSymbol(true), process));
		Assert.assertTrue(FormulaUtil.isLegalFormulaConstant(parse("1"), process));
		Assert.assertTrue(FormulaUtil.isLegalFormulaConstant(parse("3.14"), process));
		Assert.assertTrue(FormulaUtil.isLegalFormulaConstant(parse("<a>"), process));
		
		Assert.assertFalse(FormulaUtil.isLegalFormulaConstant(parse("a = b"), process));
	}
	
	@Test
	public void testIsFiniteConstant() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		Assert.assertTrue(FormulaUtil.isFiniteConstant(parse("a"), process));
		Assert.assertTrue(FormulaUtil.isFiniteConstant(parse("ab"), process));
		Assert.assertTrue(FormulaUtil.isFiniteConstant(parse("aB"), process));
		Assert.assertTrue(FormulaUtil.isFiniteConstant(DefaultSymbol.createSymbol(false), process));
		Assert.assertTrue(FormulaUtil.isFiniteConstant(DefaultSymbol.createSymbol(true), process));
		
		Assert.assertFalse(FormulaUtil.isFiniteConstant(parse("<a>"), process));
		Assert.assertFalse(FormulaUtil.isFiniteConstant(parse("1"), process));
		Assert.assertFalse(FormulaUtil.isFiniteConstant(parse("3.14"), process));
		Assert.assertFalse(FormulaUtil.isFiniteConstant(parse("a = b"), process));
	}
	
	@Test
	public void testIsFormula() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		//
		// the Boolean constants False and True are formulas;
		Assert.assertTrue(FormulaUtil.isFormula(DefaultSymbol.createSymbol(false), process));
		Assert.assertTrue(FormulaUtil.isFormula(DefaultSymbol.createSymbol(true), process));
		Assert.assertTrue(FormulaUtil.isFormula(DefaultSymbol.createSymbol("false"), process));
		Assert.assertTrue(FormulaUtil.isFormula(DefaultSymbol.createSymbol("true"), process));
		//
		Assert.assertFalse(FormulaUtil.isFormula(DefaultSymbol.createSymbol("a"), process));
		Assert.assertFalse(FormulaUtil.isFormula(DefaultSymbol.createSymbol("X"), process));
		Assert.assertFalse(FormulaUtil.isFormula(DefaultSymbol.createSymbol("1"), process));
		Assert.assertFalse(FormulaUtil.isFormula(DefaultSymbol.createSymbol("3.14"), process));
		//
		// if alpha and beta are variable or constant symbols of finite types,
		// then alpha = beta is a formula. 
		Assert.assertTrue(FormulaUtil.isFormula(parse("a = X"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("a = b = X"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("X != a"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("a = b"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("true = false"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("a = b = c"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("a != b"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("true != false"), process));
		//
		Assert.assertTrue(FormulaUtil.isFormula(parse("1 = true"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("1 = a"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("1 = X"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("true != 1"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("a != 1"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("X != 1"), process));
		//
		// if phi is a formula, then not(phi) is a formula
		Assert.assertTrue(FormulaUtil.isFormula(parse("not(a = X)"), process));
		//
		Assert.assertTrue(FormulaUtil.isFormula(parse("not(X = 1)"), process));
		//
		// if phi and phi' are formulas, then and(phi, phi'), and or(phi, phi') are formulas
		Assert.assertTrue(FormulaUtil.isFormula(parse("and()"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("and(a = X)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("and(a = X, b = Y)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("and(a = X, b = Y, and(c = Z))"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("and(a = X, b = Y, or(c = Z))"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("or()"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("or(a = X)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("or(a = X, b = Y)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("or(a = X, b = Y, and(c = Z))"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("or(a = X, b = Y, or(c = Z))"), process));
		//
		Assert.assertTrue(FormulaUtil.isFormula(parse("and(a = 1)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("and(a = X, b = 1)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("and(a = X, b = Y, and(c = 1))"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("and(a = X, b = Y, or(c = 1))"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("or(a = 1)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("or(a = X, b = 1)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("or(a = X, b = Y, and(c = 1))"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("or(a = X, b = Y, or(c = 1))"), process));
		//
		// if phi and phi' are formulas then phi => phi' and phi <=> phi' are formulas
		Assert.assertTrue(FormulaUtil.isFormula(parse("(X = a) => (Y = b)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("(X = a) => (Y = b)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("(X = a) <=> (Y = b)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("(X = a) <=> (Y != b)"), process));
		//
		Assert.assertFalse(FormulaUtil.isFormula(parse("a => b"), process));
		Assert.assertFalse(FormulaUtil.isFormula(parse("X => y"), process));
		Assert.assertFalse(FormulaUtil.isFormula(parse("a <=> b"), process));
		Assert.assertFalse(FormulaUtil.isFormula(parse("X <=> y"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("(X = a) => (Y = 1)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("(X = a) => (Y = 1)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("(X = a) <=> (Y = 1)"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("(X = a) <=> (Y != 1)"), process));
		//
		// if phi is a formula, then 'exists x phi' is a formula
		Assert.assertTrue(FormulaUtil.isFormula(parse("there exists X : X = a"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("there exists X : there exists Y : X = a and Y = b"), process));
		//
		Assert.assertTrue(FormulaUtil.isFormula(parse("there exists X : X = 1"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("there exists X : there exists Y : X = 1 and Y = b"), process));
		//
		// if phi is a formula, then 'for all x phi' is a formula 
		Assert.assertTrue(FormulaUtil.isFormula(parse("for all X : X = a"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("for all X : for all Y : X = a and Y = b"), process));
		//
		Assert.assertTrue(FormulaUtil.isFormula(parse("for all X : X = 1"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("for all X : for all Y : X = 1 and Y = b"), process));
		//
		// conditional expressions used to not be formulas, irrespective of their sub-expressions, but are now supported.
		Assert.assertTrue(FormulaUtil.isFormula(parse("if X = a then false else true"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("if X = a then Y = b else Z = c"), process));
		//
		Assert.assertTrue(FormulaUtil.isFormula(parse("if X = 1 then false else true"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("if X = 1 then Y = b else Z = c"), process));
		Assert.assertTrue(FormulaUtil.isFormula(parse("if X = a then Y = b else Z = 1"), process));
		//
		// conditional expressions with leaves that are not formulas are not formulas themselves
		Assert.assertFalse(FormulaUtil.isFormula(parse("if X = a then 1 else true"), process));
		Assert.assertFalse(FormulaUtil.isFormula(parse("if X = a then 1 else 2"), process));
	}
	
	@Test
	public void test_isQuantifierFreeFormula() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		//
		// the Boolean constants False and True are formulas;
		Assert.assertTrue(FormulaUtil.isQuantifierFreeFormula(DefaultSymbol.createSymbol(false), process));
		//
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(DefaultSymbol.createSymbol("a"), process));
		//
		// if alpha and beta are variable or constant symbols of finite types,
		// then alpha = beta is a formula. 
		Assert.assertTrue(FormulaUtil.isQuantifierFreeFormula(parse("a = X"), process));
		//
		Assert.assertTrue(FormulaUtil.isQuantifierFreeFormula(parse("1 = true"), process));
		//
		// if phi is a formula, then not(phi) is a formula
		Assert.assertTrue(FormulaUtil.isQuantifierFreeFormula(parse("not(a = X)"), process));
		//
		Assert.assertTrue(FormulaUtil.isQuantifierFreeFormula(parse("not(X = 1)"), process));
		//
		// if phi and phi' are formulas, then and(phi, phi'), and or(phi, phi') are formulas
		Assert.assertTrue(FormulaUtil.isQuantifierFreeFormula(parse("and()"), process));
		//
		Assert.assertTrue(FormulaUtil.isQuantifierFreeFormula(parse("and(a = 1)"), process));
		//
		// if phi and phi' are formulas then phi => phi' and phi <=> phi' are formulas
		Assert.assertTrue(FormulaUtil.isQuantifierFreeFormula(parse("(X = a) => (Y = b)"), process));
		//
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(parse("a => b"), process));
		//
		// if phi is a formula, then 'exists x phi' is a formula
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(parse("there exists X : X = a"), process));
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(parse("there exists X : there exists Y : X = a and Y = b"), process));
		//
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(parse("there exists X : X = 1"), process));
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(parse("there exists X : there exists Y : X = 1 and Y = b"), process));
		//
		// if phi is a formula, then 'for all x phi' is a formula 
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(parse("for all X : X = a"), process));
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(parse("for all X : for all Y : X = a and Y = b"), process));
		//
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(parse("for all X : X = 1"), process));
		Assert.assertFalse(FormulaUtil.isQuantifierFreeFormula(parse("for all X : for all Y : X = 1 and Y = b"), process));
	}
	
	@Test
	public void test_isLiteral() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		Assert.assertEquals(true, FormulaUtil.isLiteral(parse("X = a"), process));
		Assert.assertEquals(true, FormulaUtil.isLiteral(parse("a = X"), process));
		Assert.assertEquals(true, FormulaUtil.isLiteral(parse("X = Y"), process));
		Assert.assertEquals(true, FormulaUtil.isLiteral(parse("X = X"), process));
		
		Assert.assertEquals(true, FormulaUtil.isLiteral(parse("X != a"), process));
		Assert.assertEquals(true, FormulaUtil.isLiteral(parse("a != X"), process));
		Assert.assertEquals(true, FormulaUtil.isLiteral(parse("X != Y"), process));
		Assert.assertEquals(true, FormulaUtil.isLiteral(parse("X != X"), process));
		
		Assert.assertEquals(false, FormulaUtil.isLiteral(parse("X = Y = a"), process));
		Assert.assertEquals(false, FormulaUtil.isLiteral(parse("p(X) = Y"), process));
		Assert.assertEquals(false, FormulaUtil.isLiteral(parse("X = p(Y)"), process));
	}
	
	@Test
	public void test_isClause() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		Assert.assertEquals(true, FormulaUtil.isClause(parse("or(X = a)"), process));
		Assert.assertEquals(true, FormulaUtil.isClause(parse("or(X = a, Y = b)"), process));
		
		Assert.assertEquals(false, FormulaUtil.isClause(parse("or()"), process));
		Assert.assertEquals(false, FormulaUtil.isClause(parse("or(X = a, or(Y = b, Z = c))"), process));
		
		Assert.assertEquals(false, FormulaUtil.isClause(parse("and(X = a)"), process));
		Assert.assertEquals(false, FormulaUtil.isClause(parse("and(X = a, Y = b)"), process));				
	}
	
	@Test
	public void test_isConjunctionOfLiterals() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		Assert.assertEquals(true, FormulaUtil.isConjunctionOfLiterals(parse("and(X = a)"), process));
		Assert.assertEquals(true, FormulaUtil.isConjunctionOfLiterals(parse("and(X = a, Y = b, Z = c)"), process));
		
		Assert.assertEquals(false, FormulaUtil.isConjunctionOfLiterals(parse("and()"), process));	
		Assert.assertEquals(false, FormulaUtil.isConjunctionOfLiterals(parse("and(X = a, and(Z = c, W = d))"), process));
		Assert.assertEquals(false, FormulaUtil.isConjunctionOfLiterals(parse("and(X = a, Y = b, p(Z) = c)"), process));
	}
	
	@Test 
	public void test_isNNF() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		//
		// the Boolean constants False and True are formulas;
		Assert.assertTrue(FormulaUtil.isNNF(DefaultSymbol.createSymbol(false), process));
		//
		Assert.assertFalse(FormulaUtil.isNNF(DefaultSymbol.createSymbol("a"), process));
		//
		// if alpha and beta are variable or constant symbols of finite types,
		// then alpha = beta is a formula. 
		Assert.assertTrue(FormulaUtil.isNNF(parse("a = X"), process));
		//
		Assert.assertTrue(FormulaUtil.isNNF(parse("1 = true"), process));
		//
		// if phi is a formula, then not(phi) is a formula
		Assert.assertFalse(FormulaUtil.isNNF(parse("not(a = X)"), process));
		//
		Assert.assertFalse(FormulaUtil.isNNF(parse("not(X = 1)"), process));
		//
		// if phi and phi' are formulas, then and(phi, phi'), and or(phi, phi') are formulas
		Assert.assertTrue(FormulaUtil.isNNF(parse("and()"), process));
		//
		Assert.assertTrue(FormulaUtil.isNNF(parse("and(a = 1)"), process));
		//
		// if phi and phi' are formulas then phi => phi' and phi <=> phi' are formulas
		Assert.assertFalse(FormulaUtil.isNNF(parse("(X = a) => (Y = b)"), process));
		//
		Assert.assertFalse(FormulaUtil.isNNF(parse("a => b"), process));
		//
		// if phi is a formula, then 'exists x phi' is a formula
		Assert.assertFalse(FormulaUtil.isNNF(parse("there exists X : X = a"), process));
		Assert.assertFalse(FormulaUtil.isNNF(parse("there exists X : there exists Y : X = a and Y = b"), process));
		//
		Assert.assertFalse(FormulaUtil.isNNF(parse("there exists X : X = 1"), process));
		Assert.assertFalse(FormulaUtil.isNNF(parse("there exists X : there exists Y : X = 1 and Y = b"), process));
		//
		// if phi is a formula, then 'for all x phi' is a formula 
		Assert.assertFalse(FormulaUtil.isNNF(parse("for all X : X = a"), process));
		Assert.assertFalse(FormulaUtil.isNNF(parse("for all X : for all Y : X = a and Y = b"), process));
		//
		Assert.assertFalse(FormulaUtil.isNNF(parse("for all X : X = 1"), process));
		Assert.assertFalse(FormulaUtil.isNNF(parse("for all X : for all Y : X = 1 and Y = b"), process));
	}
	
	@Test
	public void test_isCNF() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
			
		Assert.assertEquals(true, FormulaUtil.isCNF(parse("and(or(X = a))"), process));
		Assert.assertEquals(true, FormulaUtil.isCNF(parse("and(or(X = a, Y = b), or(Z = c, W = d))"), process));
		
		Assert.assertEquals(false, FormulaUtil.isCNF(parse("and()"), process));
		Assert.assertEquals(false, FormulaUtil.isCNF(parse("or(X = a)"), process));	
		Assert.assertEquals(false, FormulaUtil.isCNF(parse("and(or(X = a, or(Z = c, W = d)))"), process));
	}
	
	@Test
	public void test_isDNF() {
		RewritingProcess process = new DefaultRewritingProcess(parse(""), new Basic());
		
		Assert.assertEquals(true, FormulaUtil.isDNF(parse("or(and(X = a))"), process));
		Assert.assertEquals(true, FormulaUtil.isDNF(parse("or(and(X = a, Y = b), and(Z = c, W = d))"), process));
		
		Assert.assertEquals(false, FormulaUtil.isDNF(parse("or()"), process));	
		Assert.assertEquals(false, FormulaUtil.isDNF(parse("or(X = a)"), process));	
		Assert.assertEquals(false, FormulaUtil.isDNF(parse("or(and(X = a, and(Z = c, W = d)))"), process));
	}
}
