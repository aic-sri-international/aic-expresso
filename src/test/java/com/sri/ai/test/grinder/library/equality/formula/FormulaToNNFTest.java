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
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.equality.formula.FormulaToNNF;
import com.sri.ai.test.grinder.AbstractGrinderTest;

public class FormulaToNNFTest extends AbstractGrinderTest {
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}
	
	@Test(expected=IllegalArgumentException.class)
	public void test_convertToNNF_notAFormula() {
		RewritingProcess process = newProcess();

		FormulaToNNF.convertToNNF(parse("or(f(a))"), process);
	}
	
	@Test
	public void test_convertToNNF_LiteralNormalization() {
		RewritingProcess process = newProcess();
		
		// a = X -> X = a
		Assert.assertEquals(parse("X = a"), FormulaToNNF.convertToNNF(parse("a = X"), process));
		
		// a != X -> X != a
		Assert.assertEquals(parse("X != a"), FormulaToNNF.convertToNNF(parse("a != X"), process));
		
		// B = A -> A = B
		Assert.assertEquals(parse("A = B"), FormulaToNNF.convertToNNF(parse("B = A"), process));
		
		// B != A -> A != B
		Assert.assertEquals(parse("A != B"), FormulaToNNF.convertToNNF(parse("B != A"), process));
		
		// X = X -> true
		Assert.assertEquals(parse("true"), FormulaToNNF.convertToNNF(parse("X = X"), process));
		Assert.assertEquals(parse("true"), FormulaToNNF.convertToNNF(parse("a = a"), process));
		
		// X != X -> false
		Assert.assertEquals(parse("false"), FormulaToNNF.convertToNNF(parse("X != X"), process));
		Assert.assertEquals(parse("false"), FormulaToNNF.convertToNNF(parse("a != a"), process));
		
		// a = b -> false
		Assert.assertEquals(parse("false"), FormulaToNNF.convertToNNF(parse("a = b"), process));
		
		// a != b -> true
		Assert.assertEquals(parse("true"), FormulaToNNF.convertToNNF(parse("a != b"), process));
		
		// X = Y = Z = -> X = Y and Y = Z
		Assert.assertEquals(parse("X = Y and Y = Z"), FormulaToNNF.convertToNNF(parse("X = Y = Z"), process));		
	}
	
	@Test
	public void test_convertToNNF_ImplicationsOut() {
		RewritingProcess process = newProcess();
		
		// F1 => F2  -> not(F1) or F2
		Assert.assertEquals(parse("or(X != Y, W = Z)"), FormulaToNNF.convertToNNF(parse("(X = Y) => (Z = W)"), process));
		
		// F1 <=> F2 -> (not(F1) or F2) and (F1 or not(F2))
		Assert.assertEquals(parse("and(or(X != Y, W = Z), or(X = Y, W != Z))"), FormulaToNNF.convertToNNF(parse("(X = Y) <=> (Z = W)"), process));
	}
	
	@Test
	public void test_convertToNNF_NegationsIn() {
		RewritingProcess process = newProcess();
		
		// not(X = Y) -> X != Y
		Assert.assertEquals(parse("X != Y"), FormulaToNNF.convertToNNF(parse("not(X = Y)"), process));
		
		// not(X != Y) -> X = Y
		Assert.assertEquals(parse("X = Y"), FormulaToNNF.convertToNNF(parse("not(X != Y)"), process));
		
		// not(not(F)) -> F
		Assert.assertEquals(parse("X = Y"), FormulaToNNF.convertToNNF(parse("not(not(X = Y))"), process));
		Assert.assertEquals(parse("X != Y"), FormulaToNNF.convertToNNF(parse("not(not(X != Y))"), process));
		
		// not(F1 and F2)          -> not(F1) or not(F2)
		Assert.assertEquals(parse("or(X != Y, W !=Z)"), FormulaToNNF.convertToNNF(parse("not(and(X = Y, W = Z))"), process));
		
		// not(F1 or F2)           -> not(F1) and not(F2)
		Assert.assertEquals(parse("and(X != Y, W != Z)"), FormulaToNNF.convertToNNF(parse("not(or(X = Y, W = Z))"), process));
		
		// not(for all X : F)      -> there exists X : not(F)
		Assert.assertEquals(parse("X != Y"), FormulaToNNF.convertToNNF(parse("not(for all X: X = Y)"), process));
		
		// not(there exists X : F) -> for all X : not(F)
		Assert.assertEquals(parse("X != Y"), FormulaToNNF.convertToNNF(parse("not(there exists X: X = Y)"), process));
	}
	
	@Test
	public void test_convertToNNF_StandardizeVariables() {
		RewritingProcess process = newProcess();
		
		Assert.assertEquals(parse("and(X != Y, X' = Z)"), FormulaToNNF.convertToNNF(parse("(for all X : X != Y) and (for all X : X = Z)"), process));
		Assert.assertEquals(parse("and(X != Y, X' = Z)"), FormulaToNNF.convertToNNF(parse("(there exists X : X != Y) and (there exists X : X = Z)"), process));
	}
	
	@Test
	public void test_convertToNNF_ExistentialsOut() {
		RewritingProcess process = newProcess();
		
		Assert.assertEquals(parse("X != Y"), FormulaToNNF.convertToNNF(parse("there exists X : X != Y"), process));
		Assert.assertEquals(parse("or(X != Y, X' = Z)"), FormulaToNNF.convertToNNF(parse("(there exists X : X != Y) or (there exists X : X = Z)"), process));
		
// TODO - tests related to skolem functions when logic supported.		
	}
	
	@Test
	public void test_convertToNNF_AllsOut() {
		RewritingProcess process = newProcess();
		
		Assert.assertEquals(parse("X != Y"), FormulaToNNF.convertToNNF(parse("for all X : X != Y"), process));
	}
	
	//
	// PRIVATE
	//
	private RewritingProcess newProcess() {
		return DirectCardinalityComputationFactory.newCardinalityProcess(Expressions.TRUE);
	}
}
