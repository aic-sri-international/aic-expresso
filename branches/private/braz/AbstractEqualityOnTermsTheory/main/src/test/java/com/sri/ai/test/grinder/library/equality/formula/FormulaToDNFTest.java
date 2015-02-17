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

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.equality.formula.FormulaToDNF;
import com.sri.ai.test.grinder.AbstractGrinderTest;

public class FormulaToDNFTest extends AbstractGrinderTest {
	
	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		return DirectCardinalityComputationFactory.newCardinalityProcess(topExpression);
	}

	@Test(expected=IllegalArgumentException.class)
	public void test_convertToDNF_notAFormula() {
		RewritingProcess process = newProcess();

		FormulaToDNF.convertToDNF(parse("or(f(a))"), process);
	}
	
	@Test
	public void test_convertToDNF_OrNormalization() {
		RewritingProcess process = newProcess();

		// or() -> false
		Assert.assertEquals(parse("false"), FormulaToDNF.convertToDNF(parse("or()"), process));
		
		//  or(..., true, ...) -> true
		Assert.assertEquals(parse("true"), FormulaToDNF.convertToDNF(parse("or(X = a, true, Y = b)"), process));
		
		// or(..., false, ...) -> or(..., ...)
		Assert.assertEquals(parse("or(and(X = a), and(Y = b))"), FormulaToDNF.convertToDNF(parse("or(X = a, false, Y = b)"), process));
		
		// or(X = Y, X = Y) -> or(X = Y)
		Assert.assertEquals(parse("or(and(X = Y))"), FormulaToDNF.convertToDNF(parse("or(X = Y, X = Y)"), process));

		// or(X = Y, ..., X != Y) -> true
		Assert.assertEquals(parse("true"), FormulaToDNF.convertToDNF(parse("or(X = Y, Z = a, X != Y)"), process));
	}
	
	@Test
	public void test_convertToDNF_AndNormalization() {
		RewritingProcess process = newProcess();
		// and() -> true
		Assert.assertEquals(parse("true"), FormulaToDNF.convertToDNF(parse("and()"), process));
		
		// and(..., true, ...) -> and(..., ...)
		Assert.assertEquals(parse("or(and(X = a, Y = b))"), FormulaToDNF.convertToDNF(parse("and(or(X = a), true, or(Y = b))"), process));
		
		// and(X = Y, X = Y) -> and(X = Y)
		Assert.assertEquals(parse("or(and(X = Y))"), FormulaToDNF.convertToDNF(parse("and(X = Y, X = Y)"), process));
		
		// and(..., false, ...) -> false
		Assert.assertEquals(parse("false"), FormulaToDNF.convertToDNF(parse("and(or(X = a), false, or(Y = b))"), process));
		
		// and(X = Y, ..., X != Y) -> false
		Assert.assertEquals(parse("false"), FormulaToDNF.convertToDNF(parse("and(X = Y, Z = a, X != Y)"), process));
	}
	
	@Test
	public void test_convertToDNF_LiteralNormalization() {
		RewritingProcess process = newProcess();
		
		// a = X -> X = a
		Assert.assertEquals(parse("or(and(X = a))"), FormulaToDNF.convertToDNF(parse("or(a = X)"), process));
		
		// a != X -> X != a
		Assert.assertEquals(parse("or(and(X != a))"), FormulaToDNF.convertToDNF(parse("or(a != X)"), process));
		
		// B = A -> A = B
		Assert.assertEquals(parse("or(and(A = B))"), FormulaToDNF.convertToDNF(parse("or(B = A)"), process));
		
		// B != A -> A != B
		Assert.assertEquals(parse("or(and(A != B))"), FormulaToDNF.convertToDNF(parse("or(B != A)"), process));
		
		// X = X -> true
		Assert.assertEquals(parse("true"), FormulaToDNF.convertToDNF(parse("or(X = X)"), process));
		Assert.assertEquals(parse("true"), FormulaToDNF.convertToDNF(parse("or(a = a)"), process));
		
		// X != X -> false
		Assert.assertEquals(parse("false"), FormulaToDNF.convertToDNF(parse("or(X != X)"), process));
		Assert.assertEquals(parse("false"), FormulaToDNF.convertToDNF(parse("or(a != a)"), process));
		
		// a = b -> false
		Assert.assertEquals(parse("false"), FormulaToDNF.convertToDNF(parse("or(a = b)"), process));
		
		// a != b -> true
		Assert.assertEquals(parse("true"), FormulaToDNF.convertToDNF(parse("or(a != b)"), process));
		
		// X = Y = Z = -> X = Y and Y = Z
		Assert.assertEquals(parse("or(and(X = Y, Y = Z))"), FormulaToDNF.convertToDNF(parse("or(X = Y = Z)"), process));		
	}
	
	@Test
	public void test_convertToDNF_singleLiteral() {
		RewritingProcess process = newProcess();
		
		// X = Y -> and(or(X = Y))
		Assert.assertEquals(parse("or(and(X = Y))"), FormulaToDNF.convertToDNF(parse("X = Y"), process));
		
		// X != Y -> and(or(X != Y))
		Assert.assertEquals(parse("or(and(X != Y))"), FormulaToDNF.convertToDNF(parse("X != Y"), process));
	}
	
	@Test
	public void test_convertToDNF_ImplicationsOut() {
		RewritingProcess process = newProcess();
		
		// F1 => F2  -> not(F1) or F2
		Assert.assertEquals(parse("or(and(X != Y), and(W = Z))"), FormulaToDNF.convertToDNF(parse("(X = Y) => (Z = W)"), process));
		
		// F1 <=> F2 -> (not(F1) or F2) and (F1 or not(F2))
		Assert.assertEquals(parse("or(and(X != Y, W != Z), and(W = Z, X = Y))"), FormulaToDNF.convertToDNF(parse("(X = Y) <=> (Z = W)"), process));
	}
	
	@Test
	public void test_convertToDNF_NegationsIn() {
		RewritingProcess process = newProcess();
		
		// not(X = Y) -> X != Y
		Assert.assertEquals(parse("or(and(X != Y))"), FormulaToDNF.convertToDNF(parse("not(X = Y)"), process));
		
		// not(X != Y) -> X = Y
		Assert.assertEquals(parse("or(and(X = Y))"), FormulaToDNF.convertToDNF(parse("not(X != Y)"), process));
		
		// not(not(F)) -> F
		Assert.assertEquals(parse("or(and(X = Y))"), FormulaToDNF.convertToDNF(parse("not(not(X = Y))"), process));
		Assert.assertEquals(parse("or(and(X != Y))"), FormulaToDNF.convertToDNF(parse("not(not(X != Y))"), process));
		
		// not(F1 and F2)          -> not(F1) or not(F2)
		Assert.assertEquals(parse("or(and(X != Y), and(W !=Z))"), FormulaToDNF.convertToDNF(parse("not(and(X = Y, W = Z))"), process));
		
		// not(F1 or F2)           -> not(F1) and not(F2)
		Assert.assertEquals(parse("or(and(X != Y, W != Z))"), FormulaToDNF.convertToDNF(parse("not(or(X = Y, W = Z))"), process));
		
		// not(for all X : F)      -> there exists X : not(F)
		Assert.assertEquals(parse("or(and(X != Y))"), FormulaToDNF.convertToDNF(parse("not(for all X: X = Y)"), process));
		
		// not(there exists X : F) -> for all X : not(F)
		Assert.assertEquals(parse("or(and(X != Y))"), FormulaToDNF.convertToDNF(parse("not(there exists X: X = Y)"), process));
	}
	
	@Test
	public void test_convertToDNF_StandardizeVariables() {
		RewritingProcess process = newProcess();
		
		Assert.assertEquals(parse("or(and(X != Y, X' = Z))"), FormulaToDNF.convertToDNF(parse("(for all X : X != Y) and (for all X : X = Z)"), process));
		Assert.assertEquals(parse("or(and(X != Y, X' = Z))"), FormulaToDNF.convertToDNF(parse("(there exists X : X != Y) and (there exists X : X = Z)"), process));
	}
	
	@Test
	public void test_convertToDNF_ExistentialsOut() {
		RewritingProcess process = newProcess();
		
		Assert.assertEquals(parse("or(and(X != Y))"), FormulaToDNF.convertToDNF(parse("there exists X : X != Y"), process));
		Assert.assertEquals(parse("or(and(X != Y), and(X' = Z))"), FormulaToDNF.convertToDNF(parse("(there exists X : X != Y) or (there exists X : X = Z)"), process));
		
// TODO - tests related to skolem functions when logic supported.		
	}
	
	@Test
	public void test_convertToDNF_AllsOut() {
		RewritingProcess process = newProcess();
		
		Assert.assertEquals(parse("or(and(X != Y))"), FormulaToDNF.convertToDNF(parse("for all X : X != Y"), process));
	}
	
	@Test
	public void test_convertToDNF_DistributeAndOverOr() {
		RewritingProcess process = newProcess();
		
		// F1 and (F2 or F2)  -> (F1 and F2) or (F1 and F3)
		Assert.assertEquals(parse("or(and(A = B, B = C), and(A = B, C = D))"), FormulaToDNF.convertToDNF(parse("and(A = B, or(B = C, C = D))"), process));
		Assert.assertEquals(parse("or(and(A = B, E = F, B = C), and(A = B, E = F, C = D))"), FormulaToDNF.convertToDNF(parse("and(A = B, E = F, or(B = C, C = D))"), process));
		
		// (F1 or F2) and F3  -> (F1 and F3) or (F2 and F3)
		Assert.assertEquals(parse("or(and(A = B, B = C), and(A = B, C = D))"), FormulaToDNF.convertToDNF(parse("and(or(B = C, C = D), A = B)"), process));
		Assert.assertEquals(parse("or(and(A = B, E = F, B = C), and(A = B, E = F, C = D))"), FormulaToDNF.convertToDNF(parse("and(or(B = C, C = D), A = B, E = F)"), process));
	}
	
	@Test
	public void test_convertToDNF_DistributeAndOverAnd() {
		RewritingProcess process = newProcess();
		
		// F0 and (F1 and ... and Fn) -> (F0 and F1 and ... and Fn)
		Assert.assertEquals(parse("or(and(A = B, B = C, C = D))"), FormulaToDNF.convertToDNF(parse("and(A = B, and(B = C, C = D))"), process));
		
		// (F1 and ... and Fn) and F0 -> (F1 and ... and Fn and F0) 
		Assert.assertEquals(parse("or(and(B = C, C = D, A = B))"), FormulaToDNF.convertToDNF(parse("and(and(B = C, C = D), A = B)"), process));
	}
	
	@Test
	public void test_convertToDNF_DistributeOrOverOr() {
		RewritingProcess process = newProcess();
		
		// F0 or (F1 or ... or Fn)  -> (F0 or F1 or ... or Fn)
		Assert.assertEquals(parse("or(and(A = B), and(B = C), and(C = D))"), FormulaToDNF.convertToDNF(parse("or(A = B, or(B = C, C = D))"), process));
		
		// (F1 or ... or Fn) or F0    -> (F1 or ... or Fn or F0)
		Assert.assertEquals(parse("or(and(B = C), and(C = D), and(A = B))"), FormulaToDNF.convertToDNF(parse("or(or(B = C, C = D), A = B)"), process));
	}
	
	@Test
	public void test_convertToDNF_DisjunctionSingletonLiteralToConjunctionOfLiterals() {
		RewritingProcess process = newProcess();
		
		// L1 or L2 -> or(and(L1), and(L2))
		Assert.assertEquals(parse("or(and(A = B), and(B = C))"), FormulaToDNF.convertToDNF(parse("A = B or B = C"), process));
	}
	
	//
	// PRIVATE
	//
	private RewritingProcess newProcess() {
		return DirectCardinalityComputationFactory.newCardinalityProcess(Expressions.TRUE);
	}
}
