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
package com.sri.ai.test.grinder.library.equality.cardinality.plaindpll;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.Map;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.core.SGDPLLT;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.problemtype.Sum;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.theory.AtomsOnTheoryWithEquality;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.theory.EqualityTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.theory.term.FunctionalTermTheory;
import com.sri.ai.util.Util;

@Beta
public class AtomsAndEqualityOnTermsSumDPLLTest extends AbstractSymbolicSymbolEqualityDPLLTest {
	
	@Override
	protected Expression makeProblem(Expression expression, IndexExpressionsSet indexExpressions) {
		Expression set = new DefaultIntensionalMultiSet(indexExpressions, expression, TRUE);
		Expression problem = apply(FunctorConstants.SUM, set);
		return problem;
	}

	@Override
	protected Rewriter makeRewriter() {
		return new SGDPLLT(new AtomsOnTheoryWithEquality(new EqualityTheory(new FunctionalTermTheory())), new Sum());
	}

	@Test
	public void test() {
		
		Expression expression;
		Expression expected;
		Collection<String> indices;
		Map<Expression, Expression> freeSymbolsAndTypes;
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		// repeated for debugging
//		expression = parse("(if atom(X) then 1 else 0) + (if not atom(X) then 1 else 0)");
//		indices    = list();
//		expected   = parse("1");
//		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
//		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);

		expression = parse("if atom(X) then 1 else if not atom(X) then 2 else 0");
		indices    = list();
		expected   = parse("if atom(X) then 1 else 2");
		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);

		expression = parse("if atom(X) or not atom(X) then 1 else 0");
		indices    = list();
		expected   = parse("1");
		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);

		expression = parse("(if atom(X) then 1 else 0) + (if not atom(X) then 1 else 0)");
		indices    = list();
		expected   = parse("1");
		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);

		expression = parse("(if atom(X) then 0.3 else 0.7) + (if not atom(X) then 0.9 else 0.1)");
		indices    = list();
		expected   = parse("if atom(X) then 0.4 else 1.6");
		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);

		expression = parse("(if atom(X) then 0.3 else 0.7) * (if not atom(X) then 0.9 else 0.1)");
		indices    = list();
		expected   = parse("if atom(X) then 0.03 else 0.63");
		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);

		expression = parse("(if atom(X,Y) then 0.3 else 0.7) * (if not atom(X,Y) then 0.9 else 0.1)");
		indices    = list();
		expected   = parse("if atom(X, Y) then 0.03 else 0.63");
		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(x(Everything, Everything), Boolean)"));
		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);

		expression = parse("(if atom1(X) then 1 else 0) + (if not atom2(X) then 1 else 0)");
		indices    = list();
		expected   = parse("if atom1(X) then if atom2(X) then 1 else 2 else if atom2(X) then 0 else 1");
		freeSymbolsAndTypes = Util.map(
				parse("atom1"), parse("'->'(Everything, Boolean)"),
				parse("atom2"), parse("'->'(Everything, Boolean)"));
		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);

//		expression = parse("atom(X) or not atom(Y)");
//		indices    = list();
//		expected   = parse("if atom(X) then 1 else if atom(Y) then 0 else 1");
//		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
//		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);
//
//		expression = parse("X = Y and (atom(X) or not atom(Y))");
//		indices    = list();
//		expected   = parse("if X = Y then 1 else 0");
//		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
//		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);
//
//		expression = parse("X = Y => (atom(X) or not atom(Y))");
//		indices    = list();
//		expected   = parse("1");
//		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
//		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);
//
//		// tests whether equalities on boolean atoms still work; it sort of does, but equality theory doesn't know that atoms can have only two values, so we get this 
//		expression = parse("X = Y => (atom(X) = true or atom(Y) = false)");
//		indices    = list();
//		expected   = parse("if X = Y then if atom(Y) = true then 1 else if atom(Y) = false then 1 else 0 else 1");
//		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
//		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);
//
//		// tests whether equalities on boolean atoms work while mixed with regular atom use 
//		expression = parse("X = Y => (atom(X) or atom(Y) = false)");
//		indices    = list();
//		expected   = parse("1");
//		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(Everything, Boolean)"));
//		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);
//
//		// binary atom
//		expression = parse("atom(X,Y) or not atom(X,Y)");
//		indices    = list();
//		expected   = parse("1");
//		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(x(Everything, Everything), Boolean)"));
//		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);
//
//		// simplification
//		expression = parse("if atom(X,Y) then atom(Y,X) else atom(X,Y)");
//		indices    = list();
//		expected   = parse("if atom(X, Y) then if atom(Y, X) then 1 else 0 else 0");
//		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(x(Everything, Everything), Boolean)"));
//		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);
//
//		// mixing functions and atoms (predicates)
//		expression = parse("p(X) = a => (atom(p(X), b) <=> atom(a, b))");
//		indices    = list();
//		expected   = parse("1");
//		freeSymbolsAndTypes = Util.map(parse("atom"), parse("'->'(x(Everything, Everything), Boolean)"));
//		runSymbolicAndNonSymbolicTests(expression, indices, freeSymbolsAndTypes, expected);
//
//		// FROM HERE ON: tests repeated from EqualityOnTermsModelCountingDPLLTest, which should still be solved by this generalization
//		
//		// tests the most important property in the theoryWithEquality, that of functional congruence.
//		expression = parse("p(X) = a and p(Y) = b and X = Y");
//		indices    = list();
//		expected   = parse("0");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		// tests the most important property in the theoryWithEquality, that of functional congruence.
//		expression = parse("p(X,Y) != p(Z,W) => X != Z or Y != W");
//		indices    = list();
//		expected   = parse("1");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		// a non-intuitive one, because for X = a we have a contradiction, and yet X is never tested to be a.
//		// The trick is that p(X) = p(a) already implies X != a.
//		// The next example tried to add X = a afterwards, we the result is zero models
//		expression = parse("p(X) != p(a)");
//		indices    = list();
//		expected   = parse("if p(X) = p(a) then 0 else 1");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		// This is coupled with the previous example
//		expression = parse("p(X) != p(a) and X = a");
//		indices    = list();
//		expected   = parse("0");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		// This is coupled with the previous example
//		// Another example of the same lack of intuition
//		// It seems like the algorithm should consider the possibility that Z may be equal to a, in which case this is a contradiction
//		expression = parse("p(a) != p(Y) and (Y = Z) ");
//		indices    = list();
//		expected   = parse("if p(a) = p(Y) then 0 else if Y = Z then 1 else 0");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		expression = parse("X = Y and p(X) = p(Y)");
//		indices    = list();
//		expected   = parse("if X = Y then 1 else 0");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		expression = parse("X = Y => p(X) = p(Y)");
//		indices    = list();
//		expected   = parse("1");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		expression = parse("X = Y => p(X, Z) = p(Y, W)");
//		indices    = list();
//		expected   = parse("if X = Y then if p(Y, Z) = p(Y, W) then 1 else 0 else 1");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		expression = parse("p(X) = a and Z = b and p(Y) = Z => X != Y");
//		indices    = list();
//		expected   = parse("1");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		expression = parse("X = Y and (Z = T1 or Z = T2) and (T1 = W and T2 = W) and p(X,Z) != p(Y,W)");
//		indices    = list();
//		expected   = parse("0");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		// can only use indices at this point when disconnected from function applications.
//		expression = parse("X != Y and p(Z) != p(W)");
//		indices    = list("X", "Y");
//		expected   = parse("if p(Z) = p(W) then 0 else (| Everything | - 1) * | Everything |");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
//
//		//		expression = parse("X = Y and p(X) = p(Y)");
////		indices    = list("p(X)", "p(Y)");
////		expected   = parse("| Everything |");
////		runSymbolicAndNonSymbolicTests(expression, indices, expected);
////
////		expression = parse("for all X : for all Y : (q(X) = q(Y) => X = Y)");
////		indices    = list();
////		expected   = parse("1");
////		runSymbolicAndNonSymbolicTests(expression, indices, expected);
////
////		expression = parse("p(X) != p(Y) and (for all X : for all Y : (q(X) = q(Y) => X = Y)) => (q(X) != q(Y))");
////		indices    = list();
////		expected   = parse("1");
////		runSymbolicAndNonSymbolicTests(expression, indices, expected);
	}
}

