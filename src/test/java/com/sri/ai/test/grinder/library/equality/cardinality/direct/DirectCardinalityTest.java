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

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.ExhaustiveRewriter;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter.Quantification;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.AddConjunctAndTopSimplify;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.AddDisjunctAndTopSimplify;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.IncompleteLinearImplies;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.IsContradiction;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.IsTautology;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.PickCheapest;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.QuantifierEliminationWrapper;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.ReplaceConjunctAndTopSimplify;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.ReplaceDisjunctAndTopSimplify;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.SortPair;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.TopImpliedCertainty;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.WorstCaseNumberOfDisjuncts;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.test.grinder.TestData;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.base.Pair;

public class DirectCardinalityTest extends AbstractGrinderTest {
	
	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		return DirectCardinalityComputationFactory.newCardinalityProcess(topExpression);
	}

	@Test
	public void testCardinalityExtensionalSet() {
		class CardinalityExtensionalSetData extends TestData {
			private String E;
			private Expression exprE;
			private CountsDeclaration countsDeclaration = null;
			
			public CardinalityExtensionalSetData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
										.rewrite(CardinalityRewriter.R_cardExtensionalSet, exprE);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic Tests: no logical variables
			new CardinalityExtensionalSetData(false,
					"| {} |",
					new CountsDeclaration(10),
					"0"),
			new CardinalityExtensionalSetData(false,
					"| {a} |",
					new CountsDeclaration(10),
					"1"),
			new CardinalityExtensionalSetData(false,
					"| {a, a, a, b, b} |",
					new CountsDeclaration(10),
					"2"),
			//
			// Basic Tests: logical variables
			new CardinalityExtensionalSetData(false,
					"| {A} |",
					new CountsDeclaration(10),
					"1"),
			new CardinalityExtensionalSetData(false,
					"| {A, a} |",
					new CountsDeclaration(10),
					"if (A = a) then 1 else 2"),
			new CardinalityExtensionalSetData(false,
					"| {A, a, b} |",
					new CountsDeclaration(10),
					"if (A = a or A = b) then 2 else 3"),
			new CardinalityExtensionalSetData(false,
					"| {A, a, B} |",
					new CountsDeclaration(10),
					"if A = B or A = a then if B = a then 1 else 2 else if B = a then 2 else 3"),
			//
			// Basic: from com.sri.ai.test.grinder.library.equality.cardinality
			// CardinalityComputationTest.testCardinality()
			new CardinalityExtensionalSetData(false,
					"| {X, Y, X, Z, W, V, U, b, a, c, d, f} |",
					new CountsDeclaration(10),
					"if X = Y or X = Z or X = W or X = V or X = U or X = b or X = a or X = c or X = d or X = f then if Y = Z or Y = W or Y = V or Y = U or Y = b or Y = a or Y = c or Y = d or Y = f then if Z = W or Z = V or Z = U or Z = b or Z = a or Z = c or Z = d or Z = f then if W = V or W = U or W = b or W = a or W = c or W = d or W = f then if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 5 else 6 else if U = b or U = a or U = c or U = d or U = f then 6 else 7 else if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 6 else 7 else if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if W = V or W = U or W = b or W = a or W = c or W = d or W = f then if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 6 else 7 else if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if Z = W or Z = V or Z = U or Z = b or Z = a or Z = c or Z = d or Z = f then if W = V or W = U or W = b or W = a or W = c or W = d or W = f then if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 6 else 7 else if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if W = V or W = U or W = b or W = a or W = c or W = d or W = f then if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if U = b or U = a or U = c or U = d or U = f then 9 else 10 else if Y = Z or Y = W or Y = V or Y = U or Y = b or Y = a or Y = c or Y = d or Y = f then if Z = W or Z = V or Z = U or Z = b or Z = a or Z = c or Z = d or Z = f then if W = V or W = U or W = b or W = a or W = c or W = d or W = f then if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 6 else 7 else if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if W = V or W = U or W = b or W = a or W = c or W = d or W = f then if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if U = b or U = a or U = c or U = d or U = f then 9 else 10 else if Z = W or Z = V or Z = U or Z = b or Z = a or Z = c or Z = d or Z = f then if W = V or W = U or W = b or W = a or W = c or W = d or W = f then if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 7 else 8 else if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if U = b or U = a or U = c or U = d or U = f then 9 else 10 else if W = V or W = U or W = b or W = a or W = c or W = d or W = f then if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 8 else 9 else if U = b or U = a or U = c or U = d or U = f then 9 else 10 else if V = U or V = b or V = a or V = c or V = d or V = f then if U = b or U = a or U = c or U = d or U = f then 9 else 10 else if U = b or U = a or U = c or U = d or U = f then 10 else 11"),
			new CardinalityExtensionalSetData(false,
					"|{Y, Z, b, a}|",
					new CountsDeclaration(10),
					"if Y = Z or Y = b or Y = a then if Z = b or Z = a then 2 else 3 else if Z = b or Z = a then 3 else 4"),
			new CardinalityExtensionalSetData(false,
					"|{X, Y, X, Z, b, a}|",
					new CountsDeclaration(10),
					"if X = Y or X = Z or X = b or X = a then if Y = Z or Y = b or Y = a then if Z = b or Z = a then 2 else 3 else if Z = b or Z = a then 3 else 4 else if Y = Z or Y = b or Y = a then if Z = b or Z = a then 3 else 4 else if Z = b or Z = a then 4 else 5"),
			new CardinalityExtensionalSetData(false,
					"|{X, Y, X, a}|",
					new CountsDeclaration(10),
					"if X = Y or X = a then if Y = a then 1 else 2 else if Y = a then 2 else 3"),
			new CardinalityExtensionalSetData(false,
					"|{X, Y, X, a}|",
					new CountsDeclaration(10),
					"if X = Y or X = a then if Y = a then 1 else 2 else if Y = a then 2 else 3"),
			new CardinalityExtensionalSetData(false,
					"| {} |",
					new CountsDeclaration(10),
					"0"),
			new CardinalityExtensionalSetData(false,
					"|{a, b, c, c, b, c, a, a, c}|",
					new CountsDeclaration(10),
					"3"),
			new CardinalityExtensionalSetData(false,
					"|{a, b, c, X}|",
					new CountsDeclaration(10),
					"if X = a or X = b or X = c then 3 else 4"),
			new CardinalityExtensionalSetData(false,
					"|{X, a, b, c}|",
					new CountsDeclaration(10),
					"if X = a or X = b or X = c then 3 else 4"),
			//
			// Illegal Argument Tests
			new CardinalityExtensionalSetData(true,
					"A",
					new CountsDeclaration(10),
					"N/A"),
			new CardinalityExtensionalSetData(true,
					"| A |",
					new CountsDeclaration(10),
					"N/A"),
			new CardinalityExtensionalSetData(true,
					"| {{ a }} |",
					new CountsDeclaration(10),
					"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testIncompleteLinearImplies() {
		class IncompleteLinearImpliesData extends TestData {
			private String G, H;
			private Expression exprG, exprH;
			
			public IncompleteLinearImpliesData(String G, String H, String expected) {
				super(false, expected);
				this.G = G;
				this.H = H;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprG = parse(G);
				this.exprH = parse(H);
				
				return Tuple.make(exprG, exprH);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				// Note technically not a rewriter
				// but can test in the same way
				Expression result = Expressions.makeSymbol(IncompleteLinearImplies.implies(exprG, exprH, process));
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: G and H are the same expression
			new IncompleteLinearImpliesData(
					"a",
					"a",
					"true"),	
			new IncompleteLinearImpliesData(
					"A",
					"A",
					"true"),
			new IncompleteLinearImpliesData(
					"A = a",
					"A = a",
					"true"),	
			//
			// Basic: G is t1 = t2 and H is t2 = t1
			new IncompleteLinearImpliesData(
					"A = a",
					"a = A",
					"true"),	
			//
			// Basic: G is "t1 = c1" or G is "c1 = t1" with c1 a constant,
			//        and H is "not t1 = c2" or H is "not c2 = t1" or H is "t1 != c2" or H is "c2 != t1"
			//        with c2 a constant distinct from c1
			new IncompleteLinearImpliesData(
					"A = c1",
					"not(A = c2)",
					"true"),
			new IncompleteLinearImpliesData(
					"A = c1",
					"not(c2 = A)",
					"true"),
			new IncompleteLinearImpliesData(
					"A = c1",
					"A != c2",
					"true"),
			new IncompleteLinearImpliesData(
					"A = c1",
					"c2 != A",
					"true"),
			// More variables
			new IncompleteLinearImpliesData(
					"A = B = c1",
					"not(A = B = c2)",
					"true"),
			new IncompleteLinearImpliesData(
					"A = B = c1",
					"not(c2 = A = B)",
					"true"),
			//
			// Basic: G is t1 != t2 or "not (t1 = t2)" and H is "t2 != t1" or "not (t2 = t1)"
			new IncompleteLinearImpliesData(
					"A != a",
					"not(A = a)",
					"true"),	
			new IncompleteLinearImpliesData(
					"A != a",
					"not(a = A)",
					"true"),
			new IncompleteLinearImpliesData(
					"A != a",
					"a != A",
					"true"),
			new IncompleteLinearImpliesData(
					"not(A = a)",
					"A != a",
					"true"),	
			new IncompleteLinearImpliesData(
					"not(A = a)",
					"a != A",
					"true"),
			new IncompleteLinearImpliesData(
					"not(A = a)",
					"not(a = A)",
					"true"),
		    //
			// Basic: does not incomplete linear implies
			new IncompleteLinearImpliesData(
					"and(not(P),not(Q))",
					"not(or(P, Q))",
					"false"),
			new IncompleteLinearImpliesData(
					"a",
					"b",
					"false"),	
			new IncompleteLinearImpliesData(
					"A",
					"B",
					"false"),
			new IncompleteLinearImpliesData(
					"A = a",
					"B = a",
					"false"),	
			new IncompleteLinearImpliesData(
					"A = a",
					"b = A",
					"false"),	
			new IncompleteLinearImpliesData(
					"A = c1",
					"not(A = c1)",
					"false"),
			new IncompleteLinearImpliesData(
					"A = c1",
					"not(c1 = A)",
					"false"),
			new IncompleteLinearImpliesData(
					"A = c1",
					"A != c1",
					"false"),
			new IncompleteLinearImpliesData(
					"A = c1",
					"c1 != A",
					"false"),
			new IncompleteLinearImpliesData(
					"A != a",
					"not(B = a)",
					"false"),	
			new IncompleteLinearImpliesData(
					"A != a",
					"not(a = B)",
					"false"),
			//
			// Basic: ensure equalities on constants
			// do not linear implies
			new IncompleteLinearImpliesData(
					"b = b",
					"not(a = a)",
					"false"),
			new IncompleteLinearImpliesData(
					"a = a",
					"not(b = b)",
					"false"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testTopSimplifyDisjunction() {
		class TopSimplifyDisjunctionData extends TestData {
			private String E;
			private Expression exprE;
			
			public TopSimplifyDisjunctionData(boolean isIllegalArgumentTest, String E, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
											.rewrite(CardinalityRewriter.R_top_simplify_disjunction, exprE);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: if Fi is True or Alpha = Alpha return True
			new TopSimplifyDisjunctionData(false,
					"or(true)",
					"true"),
			new TopSimplifyDisjunctionData(false,
					"or(false, true)",
					"true"),
			new TopSimplifyDisjunctionData(false,
					"or(X = X)",
					"true"),
			new TopSimplifyDisjunctionData(false,
					"or(false, X = X)",
					"true"),
			//
			// Basic: if Fi is not "False" and Fi is not Alpha != Alpha add Fi to T
			new TopSimplifyDisjunctionData(false,
					"or(false)",
					"false"),	
			new TopSimplifyDisjunctionData(false,
					"or(false, A = a)",
					"A = a"),
			new TopSimplifyDisjunctionData(false,
					"or(false, A = a, B = b)",
					"or(A = a, B = b)"),
			new TopSimplifyDisjunctionData(false,
					"or(X != X)",
					"false"),	
			new TopSimplifyDisjunctionData(false,
					"or(X != X, A = a)",
					"A = a"),
			new TopSimplifyDisjunctionData(false,
					"or(X != X, A = a, B = b)",
					"or(A = a, B = b)"),
			//
			// Basic: if incomplete_linear_implies(Ti, Tj)
					new TopSimplifyDisjunctionData(false,
							"or(A = a, A != b)",
							"A != b"),
					new TopSimplifyDisjunctionData(false,
							"or(A = a, A != b, B = b)",
							"or(A != b, B = b)"),
			//
			// Basic: if incomplete_linear_implies(Tj, Ti)
					new TopSimplifyDisjunctionData(false,
							"or(A != b, A = a)",
							"A != b"),
			//
			// Basic: else if incomplete_linear_implies(not Ti, Tj)
			//                or incomplete_linear_implies(not Tj, Ti)
			// i.e. a tautology.
			new TopSimplifyDisjunctionData(false,
					"or(A = a, A != a)",
					"true"),
			new TopSimplifyDisjunctionData(false,
					"or(A != a, A = a)",
					"true"),
			//
			// Invalid Arguments
			new TopSimplifyDisjunctionData(false,
					"and(A = a, B = b)",
					"and(A = a, B = b)")
		};
		
		perform(tests);
	}
	
	@Test
	public void testTopSimplifyConjunction() {
		class TopSimplifyConjunctionData extends TestData {
			private String E;
			private Expression exprE;
			
			public TopSimplifyConjunctionData(boolean isIllegalArgumentTest, String E, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
										.rewrite(CardinalityRewriter.R_top_simplify_conjunction, exprE);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: if Fi is False or Alpha != Alpha return False
			new TopSimplifyConjunctionData(false,
					"and(false)",
					"false"),
			new TopSimplifyConjunctionData(false,
					"and(true, false)",
					"false"),
			new TopSimplifyConjunctionData(false,
					"X != X", // i.e. implied conjunction
					"false"),
			new TopSimplifyConjunctionData(false,
					"and(X != X)",
					"false"),
			new TopSimplifyConjunctionData(false,
					"and(X != X, false)",
					"false"),
			//
			// Basic: if Fi is not "True" and Fi is not Alpha = Alpha add Fi to T
			new TopSimplifyConjunctionData(false,
					"and(true)",
					"true"),	
			new TopSimplifyConjunctionData(false,
					"and(true, A = a)",
					"A = a"),
			new TopSimplifyConjunctionData(false,
					"and(true, A = a, B = b)",
					"and(A = a, B = b)"),
			new TopSimplifyConjunctionData(false,
					"and(X = X)",
					"true"),
			new TopSimplifyConjunctionData(false,
					"X = X", // i.e. implied conjunction
					"true"),
			new TopSimplifyConjunctionData(false,
					"and(X = X, A = a)",
					"A = a"),
			new TopSimplifyConjunctionData(false,
					"and(X = X, A = a, B = b)",
					"and(A = a, B = b)"),
			//
			// Basic: if incomplete_linear_implies(Tj, Ti)
			new TopSimplifyConjunctionData(false,
					"and(A = a, A = a)",
					"A = a"),
			//
			// Basic: if incomplete_linear_implies(Ti, Tj)
			new TopSimplifyConjunctionData(false,
					"and(A = a, A != b)",
					"A = a"),
			new TopSimplifyConjunctionData(false,
					"and(A = a, A != b, B = b)",
					"and(A = a, B = b)"),
			//
			// Basic: else if incomplete_linear_implies(Ti, not Tj)
			//                or incomplete_linear_implies(Tj, not Ti)
			// i.e. a contradiction.
			new TopSimplifyConjunctionData(false,
					"and(A != a, A = a)",
					"false"),
			new TopSimplifyConjunctionData(false,
					"and(A = a, A != a)",
					"false"),
			//
			new TopSimplifyConjunctionData(false,
					"a13 != X5 and X6 != X5 and true",
					"a13 != X5 and X6 != X5"),
			//
			// Invalid Arguments
			new TopSimplifyConjunctionData(false,
					"or(A = a, B = b)",
					"or(A = a, B = b)")
		};
		
		perform(tests);
	}
	
	@Test
	public void testTopSimplify() {
		class TopSimplifyData extends TestData {
			private String E;
			private Expression exprE;
			
			public TopSimplifyData(boolean isIllegalArgumentTest, String E, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
										.rewrite(CardinalityRewriter.R_top_simplify, exprE);
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: if F is a conjunction
			new TopSimplifyData(false,
				"and(true, A = a, B = b)",
				"and(A = a, B = b)"),
			//
			// Basic: if F is an implied conjunction
			new TopSimplifyData(false,
					"X = X",
					"true"),
			new TopSimplifyData(false,
					"X != X",
					"false"),
			//
			// Basic: if F is a disjunction
			new TopSimplifyData(false,
					"or(false, A = a, B = b)",
					"or(A = a, B = b)"),
			// Basic: Not a conjunction or disjunction
			new TopSimplifyData(false,
					"A <=> a",
					"A <=> a"),					
		};
		
		perform(tests);
	}
	
	@Test
	public void testMoveNotIn() {
		class MoveNotInData extends TestData {
			private String E;
			private Expression exprE;
			
			public MoveNotInData(boolean isIllegalArgumentTest, String E, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
										.rewrite(CardinalityRewriter.R_move_not_in, exprE);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: F is "not FALSE"
			new MoveNotInData(false,
				"not(false)",
				"true"),
			//
			// Basic: F is "not TRUE"
			new MoveNotInData(false,
				"not(true)",
				"false"),
			//
			// Basic: F is "not x = t"
			new MoveNotInData(false,
				"not(X = t)",
				"X != t"),
			// 
			// Basic: F is "not x != t"
			new MoveNotInData(false,
					"not(X != t)",
					"X = t"),		
			//
			// Basic: F is "not not G"
			new MoveNotInData(false,
					"not(not(X != t))",
					"X != t"),
			new MoveNotInData(false,
					"not(not(and(X = a, X = a)))",
					"X = a"),
			new MoveNotInData(false,
					"not(not(and(X = a, X = a, B = b)))",
					"and(X = a, B =b)"),
			//
			// Basic:  F is "not (G1 and ... and Gn)"
			new MoveNotInData(false,
					"not(and(X = a, X = a))",
					"not(X = a)"),
			new MoveNotInData(false,
					"not(and(X = a, X = a, B = b))",
					"or(not(X = a), not(B = b))"),
			//
			// Basic: F is "not (G1 or ... or Gn)"
			new MoveNotInData(false,
					"not(or(X = a, X = a))",
					"not(X = a)"),
			new MoveNotInData(false,
					"not(or(X = a, X = a, B = b))",
					"and(not(X = a), not(B = b))"),
			//
			// Basic: F is "not (G => H)"
			new MoveNotInData(false,
					"not((G = g) => (H = h))",
					"and(G = g, not(H = h))"),			
			//
			// Basic: F is "not (G <=> H)"
			new MoveNotInData(false,
					"not((G = g) <=> (H = h))",
					"or(and(G = g, not(H = h)), and(not(G = g), H = h))"),						
			//
			// Basic: F is "not (there exists x G)"
			new MoveNotInData(false,
					"not(there exists G : G = g)",
					"for all G : not(G = g)"),			
			//
			// Basic: F is "not (for all x G)"
			new MoveNotInData(false,
					"not(for all G : G = g)",
					"there exists G : not(G = g)"),						
			//
			// Illegal Argument Test
			new MoveNotInData(true,
					"X = t",
					"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testPickCheapest() {
		Expression cheapest;
		PickCheapest pickCheapest = new PickCheapest();
		
		cheapest = pickCheapest.pick(
					parse("(A = a, 0)")
				);
		Assert.assertEquals(parse("(A = a, 0)"), cheapest);
		
		cheapest = pickCheapest.pick(
				parse("(A = a, 0)"),
				parse("(a, 1)")
			);
		Assert.assertEquals(parse("(a, 1)"), cheapest);		
		cheapest = pickCheapest.pick(
				parse("(A = a, 0)"),
				parse("(or(a), 1)")
			);
		Assert.assertEquals(parse("(A = a, 0)"), cheapest);
		cheapest = pickCheapest.pick(
				parse("((a, b, c, d), 0)"),
				parse("(a => b, 1)")
			);
		Assert.assertEquals(parse("((a, b, c, d), 0)"), cheapest);
		cheapest = pickCheapest.pick(
				parse("((a, b, c, d), 0)"),
				parse("(a <=> b, 1)")
			);
		Assert.assertEquals(parse("((a, b, c, d), 0)"), cheapest);
		cheapest = pickCheapest.pick(
				parse("(and(a, b, c, d, e, f), 0)"),
				parse("(or(a, b, c), 1)")
			);
		Assert.assertEquals(parse("(and(a, b, c, d, e, f), 0)"), cheapest);	
		cheapest = pickCheapest.pick(
				parse("(and(a, b, c, d, e, f, g), 0)"),
				parse("(or(a, b, c), 1)")
			);
		Assert.assertEquals(parse("(and(a, b, c, d, e, f, g), 0)"), cheapest);
		cheapest = pickCheapest.pick(
				parse("(and(a, b, c, d, e, f, g, h), 0)"),
				parse("(or(a, b, c), 1)")
			);
		Assert.assertEquals(parse("(or(a, b, c), 1)"), cheapest);
		cheapest = pickCheapest.pick(
				parse("(and(X = a, X = b), 0)"),
				parse("(X = c, 1)")
			);
		Assert.assertEquals(parse("(X = c, 1)"), cheapest);		
		
		try {
			// Not a pair
			pickCheapest.pick(
					parse("(A = a)")
					);
			Assert.fail("IllegalArgumentException should have been thrown.");
		} catch (IllegalArgumentException iae) {
			// Expected
		}
	}
	
	@Test
	public void testSortPair() {
		Pair<Expression, Expression> sortedPair;
		SortPair sortPair = new SortPair();
		
		sortedPair = sortPair.sort(
				parse("and(a, b, c, d)"), 
				parse("a <=> b"));
		Assert.assertEquals(parse("and(a, b, c, d)"), sortedPair.first);
		Assert.assertEquals(parse("a <=> b"),         sortedPair.second);
		
		sortedPair = sortPair.sort(
				parse("a <=> b"),
				parse("and(a, b, c, d)"));
		Assert.assertEquals(parse("and(a, b, c, d)"), sortedPair.first);
		Assert.assertEquals(parse("a <=> b"),         sortedPair.second);
	}
	
	@Test
	public void testReplaceConjunctAndTopSimplify() {
		class ReplaceConjunctAndTopSimplifyData extends TestData {
			private String F, conjunction;
			int i;
			private Expression exprF, exprConjunction;
			
			public ReplaceConjunctAndTopSimplifyData(boolean isIllegalArgumentTest, String F, int i, String conjunction, String expected) {
				super(isIllegalArgumentTest, expected);
				this.F = F;
				this.i = i;
				this.conjunction = conjunction;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprF = parse(F);
				this.exprConjunction = parse(conjunction);
				return Tuple.make(exprF, i, exprConjunction);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				//Note: not a rewriter but can call in a similar way.
				Expression result = ReplaceConjunctAndTopSimplify.replaceConjunctAndTopSimplify(exprF, i, exprConjunction, process);
				
				return result;
			}
		}

		TestData[] tests = new TestData[] {
			//
			// Basic: if F is False
			new ReplaceConjunctAndTopSimplifyData(false,
					"false",
					0,
					"and(A = a, B = b, C = c)",
					"false"),
			//
			// Basic: incomplete_linear_implies(Fj, F), for some j != i
			new ReplaceConjunctAndTopSimplifyData(false,
					"A = a",
					1,
					"and(A = a, B = b, C = c)",
					"and(A = a, C = c)"),	
			new ReplaceConjunctAndTopSimplifyData(false,
					"A != c2",
					1,
					"and(A = c1, B = b, C = c)",
					"and(A = c1, C = c)"),	
			//
			// Basic: if incomplete_linear_implies(F, not Fj) or 
			//           incomplete_linear_implies(Fj, not F) for some j != i
			new ReplaceConjunctAndTopSimplifyData(false,
					"A = c1",
					1,
					"and(A = c2, B = b, C = c)",
					"false"),
			new ReplaceConjunctAndTopSimplifyData(false,
					"A = c1",
					1,
					"and(not(A = c1), B = b, C = c)",
					"false"),	
			//
			// Basic: C <- F1 and ... and Fi-1 and F and Fi+1 and ... and Fn
			new ReplaceConjunctAndTopSimplifyData(false,
					"D = d",
					1,
					"and(A = a, B = b, C = c)",
					"and(A = a, D = d, C = c)"),			
			//
			// Basic: C <- remove from C all conjuncts Fj, j != i, such that incomplete_linear_implies(F, Fj)
			new ReplaceConjunctAndTopSimplifyData(false,
					"D = d",
					1,
					"and(A = a, B = b, C = c, D != c)",
					"and(A = a, D = d, C = c)"),
			//
			// Basic: an implied conjunct.
			new ReplaceConjunctAndTopSimplifyData(false,
					"A = c",
					0,
					"or(A = a, B = b, C = c)",
					"A = c"),
			//
			// Illegal Argument tests
			// i out of range if not a conjunction
			new ReplaceConjunctAndTopSimplifyData(true,
					"A = a",
					1,
					"or(A = a, B = b, C = c)",
					"N/A"),
			// i out of range
			new ReplaceConjunctAndTopSimplifyData(true,
					"A = a",
					-1,
					"and(A = a, B = b, C = c)",
					"N/A"),
			// i out of range
			new ReplaceConjunctAndTopSimplifyData(true,
					"A = a",
					3,
					"and(A = a, B = b, C = c)",
					"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testAddConjunctAndTopSimplify() {
		class AddConjunctAndTopSimplifyData extends TestData {
			private String F, conjunction;
			private Expression exprF, exprConjunction;
			
			public AddConjunctAndTopSimplifyData(boolean isIllegalArgumentTest, String F, String conjunction, String expected) {
				super(isIllegalArgumentTest, expected);
				this.F = F;
				this.conjunction = conjunction;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprF = parse(F);
				this.exprConjunction = parse(conjunction);
				
				return Tuple.make(exprF, exprConjunction);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				// Note: Not a rewriter but can call in a similar way.
				Expression result = AddConjunctAndTopSimplify.addConjunctAndTopSimplify(exprF, exprConjunction, process);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: straight forward addition
			new AddConjunctAndTopSimplifyData(false,
					"D = d",
					"and(A = a, B = b, C = c)",
					"and(A = a, B = b, C = c, D = d)"),
			//
			// Basic: if F is False
			new AddConjunctAndTopSimplifyData(false,
					"false",
					"and(Y = b, Z = c)",
					"false"),
			//
			// Basic: if incomplete_linear_implies(Fi, F), for some i
			new AddConjunctAndTopSimplifyData(false,
					"A = a",
					"and(A = a, B = b, C = c)",
					"and(A = a, B = b, C = c)"),	
			new AddConjunctAndTopSimplifyData(false,
					"A != c2",
					"and(A = c1, B = b, C = c)",
					"and(A = c1, B = b, C = c)"),
			//
			// Basic: if incomplete_linear_implies(F, not Fi) or incomplete_linear_implies(Fi, not F) for some i
			new AddConjunctAndTopSimplifyData(false,
					"A = c1",
					"and(A = c2, B = b, C = c)",
					"false"),
			new AddConjunctAndTopSimplifyData(false,
					"A = c1",
					"and(not(A = c1), B = b, C = c)",
					"false"),
			//
			// Basic: C <- remove from F1 and ... and Fn all conjuncts Fi such that incomplete_linear_implies(F, Fi)
			new AddConjunctAndTopSimplifyData(false,
					"D = d",
					"and(A = a, B = b, C = c, D != c)",
					"and(A = a, B = b, C = c, D = d)"),
			//
			// Basic: conjuncts is an implied list
			new AddConjunctAndTopSimplifyData(false,
					"X = a",
					"Y = b",
					"and(Y = b, X = a)"),					
		};
		
		perform(tests);
	}
	
	@Test
	public void testReplaceDisjunctAndTopSimplify() {
		class ReplaceDisjunctAndTopSimplifyData extends TestData {
			private String F, disjunction;
			int i;
			private Expression exprF, exprDisjunction;
			
			public ReplaceDisjunctAndTopSimplifyData(boolean isIllegalArgumentTest, String F, int i, String disjunction, String expected) {
				super(isIllegalArgumentTest, expected);
				this.F = F;
				this.i = i;
				this.disjunction = disjunction;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprF = parse(F);
				this.exprDisjunction = parse(disjunction);
				return Tuple.make(exprF, i, exprDisjunction);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				// Note: Not a rewriter but can call in a similar way.
				Expression result = ReplaceDisjunctAndTopSimplify.replaceDisjunctAndTopSimplify(exprF, i, exprDisjunction, process);
				
				return result;
			}
		}

		TestData[] tests = new TestData[] {
			//
			// Basic: if F is True
			new ReplaceDisjunctAndTopSimplifyData(false,
					"true",
					0,
					"or(A = a, B = b, C = c)",
					"true"),
			//
			// Basic: if incomplete_linear_implies(F, Fj), for some j != i
			new ReplaceDisjunctAndTopSimplifyData(false,
					"A = a",
					1,
					"or(A = a, B = b, C = c)",
					"or(A = a, C = c)"),	
			new ReplaceDisjunctAndTopSimplifyData(false,
					"A = c1",
					1,
					"or(A != c2, B = b, C = c)",
					"or(A != c2, C = c)"),	
			//
			// Basic: if incomplete_linear_implies(not F, Fj) or 
			//           incomplete_linear_implies(not Fj, F) for some j != i
			new ReplaceDisjunctAndTopSimplifyData(false,
					"A = c1",
					1,
					"or(A != c1, B = b, C = c)",
					"true"),
			new ReplaceDisjunctAndTopSimplifyData(false,
					"not(A = c1)",
					1,
					"or(A = c1, B = b, C = c)",
					"true"),	
			//
			// Basic: D <- F1 or ... or Fi-1 or F or Fi+1 or ... or Fn
			new ReplaceDisjunctAndTopSimplifyData(false,
					"D = d",
					1,
					"or(A = a, B = b, C = c)",
					"or(A = a, D = d, C = c)"),			
			//
			// Basic: D <- remove from D all disjuncts Fj, j != i, such that incomplete_linear_implies(Fj, F)
			new ReplaceDisjunctAndTopSimplifyData(false,
					"D != d",
					1,
					"or(A = a, B = b, C = c, D = c)",
					"or(A = a, D != d, C = c)"),						
			//
			// Illegal Argument tests
			// 3rd argument not a disjunction
			new ReplaceDisjunctAndTopSimplifyData(true,
					"A = a",
					1,
					"and(A = a, B = b, C = c)",
					"N/A"),
			// i out of range
			new ReplaceDisjunctAndTopSimplifyData(true,
					"A = a",
					-1,
					"or(A = a, B = b, C = c)",
					"N/A"),
			// i out of range
			new ReplaceDisjunctAndTopSimplifyData(true,
					"A = a",
					3,
					"or(A = a, B = b, C = c)",
					"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testAddDisjunctAndTopSimplify() {
		class AddDisjunctAndTopSimplifyData extends TestData {
			private String F, disjunction;
			private Expression exprF, exprDisjunction;
			
			public AddDisjunctAndTopSimplifyData(boolean isIllegalArgumentTest, String F, String disjunction, String expected) {
				super(isIllegalArgumentTest, expected);
				this.F = F;
				this.disjunction = disjunction;
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprF = parse(F);
				this.exprDisjunction = parse(disjunction);
				
				return Tuple.make(exprF, exprDisjunction);
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				// Note: Not a rewriter but can call in a similar way.
				Expression result = AddDisjunctAndTopSimplify.addDisjunctAndTopSimplify(exprF, exprDisjunction, process);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: straight forward addition
			new AddDisjunctAndTopSimplifyData(false,
					"D = d",
					"or(A = a, B = b, C = c)",
					"or(A = a, B = b, C = c, D = d)"),
			//
			// Basic: if F is True
			new AddDisjunctAndTopSimplifyData(false,
					"true",
					"or(Y = b, Z = c)",
					"true"),
			//
			// Basic: if incomplete_linear_implies(F, Fi), for some i
			new AddDisjunctAndTopSimplifyData(false,
					"A = a",
					"or(A = a, B = b, C = c)",
					"or(A = a, B = b, C = c)"),	
			new AddDisjunctAndTopSimplifyData(false,
					"A = c2",
					"or(A != c1, B = b, C = c)",
					"or(A != c1, B = b, C = c)"),
			//
			// Basic: if incomplete_linear_implies(not F, Fi) or incomplete_linear_implies(not Fi, F) for some i
			new AddDisjunctAndTopSimplifyData(false,
					"A = c1",
					"or(A != c1, B = b, C = c)",
					"true"),
			new AddDisjunctAndTopSimplifyData(false,
					"not(A = c1)",
					"or(A = c1, B = b, C = c)",
					"true"),
			//
			// Basic: D <- remove from F1 or ... or Fn all disjuncts Fi such that incomplete_linear_implies(Fi, F)
			new AddDisjunctAndTopSimplifyData(false,
					"D != d",
					"or(A = a, B = b, C = c, D = c)",
					"or(A = a, B = b, C = c, D != d)"),
			//
			// Basic: adding to an implicit conjunction (i.e. passed a conjunct).
			new AddDisjunctAndTopSimplifyData(false,
					"X = a",
					"Y = b",
					"or(Y = b, X = a)"),					
		};
		
		perform(tests);
	}
	
	@Test
	public void testQuantifierElimination() {
		class QEData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CountsDeclaration           countsDeclaration = null;
			
			public QEData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
										.rewrite(CardinalityRewriter.R_quantifier_elimination, exprE);

				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: if F is "for all x: Y"
			new QEData(false,
					"for all X : X = X",
					new CountsDeclaration("X", "10"),
					"true"),
			new QEData(false,
					"for all X : X != X",
					new CountsDeclaration("X", "10"),
					"false"),
			new QEData(false,
					"for all X : X = X and Y = b",
					new CountsDeclaration("X", "10"),
					"Y = b"),
			new QEData(false,
					"for all X : X != X and Y = b",
					new CountsDeclaration("X", "10"),
					"false"),
			//
			// Basic: if F is "there exists x: Y"
			new QEData(false,
					"there exists X : X = X",
					new CountsDeclaration("X", "10"),
					"true"),
			new QEData(false,
					"there exists X : X != X",
					new CountsDeclaration("X", "10"),
					"false"),
			new QEData(false,
					"there exists X : X = X and Y = b",
					new CountsDeclaration("X", "10"),
					"Y = b"),
			new QEData(false,
					"there exists X : X != X and Y = b",
					new CountsDeclaration("X", "10"),
					"false"),
		    //
		    // Basic: no quantifiers to remove - do nothing
			new QEData(false,
					"not(a = a)",
					new CountsDeclaration(10),
					"not(a = a)"), // should not simplify if there were no removed quantifiers	
		    //
			// Basic: if F is "not G"
			new QEData(false,
					"not(for all X : X = X)",
					new CountsDeclaration("X", "10"),
					"false"),	
			new QEData(false,
					"not(for all X : X != X)",
					new CountsDeclaration("X", "10"),
					"true"),
			new QEData(false,
					"not(there exists X : X = X)",
					new CountsDeclaration("X", "10"),
					"false"),	
			new QEData(false,
					"not(there exists X : X != X)",
					new CountsDeclaration("X", "10"),
					"true"),			
			//
			// Basic: if F is G => H
			new QEData(false,
					"(for all X: X = X) => (for all Y: Y = Y)",
					new CountsDeclaration("X", "10", "Y", "10"),
					"true"),
			new QEData(false,
					"(for all X: X = X) => (for all Y: Y != Y)",
					new CountsDeclaration("X", "10", "Y", "10"),
					"false"),
			//
			// Basic: if F is G <=> H
			new QEData(false,
					"(for all X: X = X) <=> (for all Y: Y = Y)",
					new CountsDeclaration("X", "10", "Y", "10"),
					"true"),
			new QEData(false,
					"(for all X: X = X) <=> (for all Y: Y != Y)",
					new CountsDeclaration("X", "10", "Y", "10"),
					"false"),					
			//
			// Basic: if F is a conjunction F1 and ... and Fn
			new QEData(false,
					"(for all X: X = X) and (for all Y: Y = Y)",
					new CountsDeclaration("X", "10", "Y", "10"),
					"true"),
			new QEData(false,
					"(for all X: X = X) and (for all Y: Y != Y)",
					new CountsDeclaration("X", "10", "Y", "10"),
					"false"),
			//
			// Basic: if F is a disjunction F1 or ... or Fn
			new QEData(false,
					"(for all X: X = X) or (for all Y: Y = Y)",
					new CountsDeclaration("X", "10", "Y", "10"),
					"true"),
			new QEData(false,
					"(for all X: X = X) or (for all Y: Y != Y)",
					new CountsDeclaration("X", "10", "Y", "10"),
					"true"),
			new QEData(false,
					"(for all X: X != X) or (for all Y: Y != Y)",
					new CountsDeclaration("X", "10", "Y", "10"),
					"false"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testQuantifierEliminationWrapper() {
		class QuantifierEliminationWrapperData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CountsDeclaration           countsDeclaration = null;
			
			public QuantifierEliminationWrapperData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			public QuantifierEliminationWrapperData(boolean isIllegalArgumentTest, Expression contextualConstraint, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, contextualConstraint, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				TotalRewriter quantifierEliminationWrappers =
						new TotalRewriter(
								new QuantifierEliminationWrapper(KindAttribute.VALUE_THERE_EXISTS),
								new QuantifierEliminationWrapper(KindAttribute.VALUE_FOR_ALL),
								new QuantifierEliminationWrapper(FunctorConstants.AND),
								new QuantifierEliminationWrapper(FunctorConstants.OR),
								new QuantifierEliminationWrapper(FunctorConstants.NOT),
								new QuantifierEliminationWrapper(FunctorConstants.IMPLICATION),
								new QuantifierEliminationWrapper(FunctorConstants.EQUIVALENCE));

				RewritingProcess directCardinalityProcess = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process);
				Expression result = quantifierEliminationWrappers.rewrite(exprE, directCardinalityProcess);
	
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			new QuantifierEliminationWrapperData(false,
					"there exists X in People : X' != Y and (X' != dave and Y = dave or Y = dave and X' != dave) and not (X' = bob) and X'' != Y' and (X' = X'' and Y = Y' or X' = Y' and Y = X'') and (X'' != X' or Y' != Y)",
					new CountsDeclaration("X", "10"),
					/* order normalization: */
					GrinderUtil.usePlain?
							"(X' != dave) and (Y = dave) and (X' != bob) and (X' = Y') and (X'' = dave)"
//							"(X' != dave) and (Y = dave) and (X' != bob) and (Y' != dave) and (X' = Y') and (X'' = dave)"
//							"(Y = dave) and (Y' != dave) and (Y' != bob) and (X' = Y') and (X'' = dave)"
							: "if (Y = dave) and (X' != dave) and (X' != bob) and (X'' != Y') and ((X' = X'') and (Y' = dave) or (X' = Y') and (X'' = dave)) and ((X'' != X') or (Y' != dave)) then | People | > 0 else false"),
					/* without order normalization: */
					// "if X' != Y and (X' != dave and Y = dave or Y = dave and X' != dave) and X' != bob and X'' != Y' and (X' = X'' and Y = Y' or X' = Y' and Y = X'') and (X'' != X' or Y' != Y) then if | People | > 0 then | type(People) | > 0 else false else false"),
		};
		
		perform(tests);
	}

	@Test
	public void testIsTautology() {
		class TautologyData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CountsDeclaration           countsDeclaration = null;
			
			public TautologyData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				// Note: Not a rewriter but can call in a similar way.
				Expression result = Expressions.makeSymbol(IsTautology.isTautology(exprE, DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process)));
				
				return result;
			}
		}	
		
		TestData[] tests = new TestData[] {
				new TautologyData(false,
						"there exists Y : Y = Y and X = X",
						new CountsDeclaration("X", "11", "Y", "7"),
						"true"),
			//
			// Basic: if F has no free variables, is a tautology
			new TautologyData(false,
					"true",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"a = a",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"a != b",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"not(a != a)",
					new CountsDeclaration(10),
					"true"),					
			new TautologyData(false,
					"and(a = a, b = b)",
					new CountsDeclaration(10),
					"true"),		
			new TautologyData(false,
					"or(a = a, b = b)",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"true => true",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"false => true",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"false => false",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"true <=> true",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"false <=> false",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"there exists X : X = X",
					new CountsDeclaration("X", "10"),
					"true"),
			new TautologyData(false,
					"for all X : X = X",
					new CountsDeclaration("X", "10"),
					"true"),
			//
			// Basic: if F has no free variables, is not a tautology
			new TautologyData(false,
					"false",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"a = b",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"a != a",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"not(a = a)",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"and(a != a, b != b)",
					new CountsDeclaration(10),
					"false"),		
			new TautologyData(false,
					"or(a != a, b != b)",
					new CountsDeclaration(10),
					"false"),		
			new TautologyData(false,
					"true => false",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"true <=> false",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"false <=> true",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"there exists X : X != X",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"for all X : X != X",
					new CountsDeclaration(10),
					"false"),
			//
			// Basic: if F has free variables, is a tautology
			new TautologyData(false,
					"X = X",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"not(X != X)",
					new CountsDeclaration(10),
					"true"),					
			new TautologyData(false,
					"and(X = X, Y = Y)",
					new CountsDeclaration(10),
					"true"),		
			new TautologyData(false,
					"or(X = X, Y = Y)",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"X = X => Y = Y",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"X != X => Y = Y",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"X != X => Y != Y",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"X = X <=> Y = Y",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"X != X <=> Y != Y",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"there exists Y : Y = Y and X = X",
					new CountsDeclaration("X", "11", "Y", "7"),
					"true"),
			new TautologyData(false,
					"for all Y : Y = Y and X = X",
					new CountsDeclaration("X", "11", "Y", "7"),
					"true"),
			//
			// Basic: if F has free variables, is not a tautology
			new TautologyData(false,
					"X = a",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"not(X != a)",
					new CountsDeclaration(10),
					"false"),					
			new TautologyData(false,
					"and(X != X, Y = Y)",
					new CountsDeclaration(10),
					"false"),		
			new TautologyData(false,
					"or(X != X, Y != Y)",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"X = X => Y != Y",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"X = X <=> Y != Y",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"X != X <=> Y = Y",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"there exists Y : Y != Y and X = X",
					new CountsDeclaration("X", "11", "Y", "7"),
					"false"),
			new TautologyData(false,
					"for all Y : Y != Y and X = X",
					new CountsDeclaration("X", "11", "Y", "7"),
					"false"),
			//
			// Basic: Other
			new TautologyData(false,
					"0 = 0", // i.e. a numeric expression
					new CountsDeclaration(10),
					"true"),
			//
			// Formerly Illegal Arguments Test, now valid with support of if then else in formulas
			new TautologyData(false,
					"if a = a then true else false",
					new CountsDeclaration(10),
					"true"),
			new TautologyData(false,
					"if a != a then true else false",
					new CountsDeclaration(10),
					"false"),
			new TautologyData(false,
					"if X = X then true else false",
					new CountsDeclaration(10),
					"true"),	
			new TautologyData(false,
					"if X != X then true else false",
					new CountsDeclaration(10),
					"false"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testIsContradiction() {
		class ContradictionData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CountsDeclaration           countsDeclaration = null;
			
			public ContradictionData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				// Note: Not a rewriter but can call in a similar way.
				Expression result = Expressions.makeSymbol(IsContradiction.isContradiction(exprE, DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process)));
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: cases in which F has no free variables and is false, and therefore is a contradiction
			new ContradictionData(false,
					"false",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"a = b",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"a != a",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"not(a = a)",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"and(a != a, b != b)",
					new CountsDeclaration(10),
					"true"),		
			new ContradictionData(false,
					"or(a != a, b != b)",
					new CountsDeclaration(10),
					"true"),		
			new ContradictionData(false,
					"true => false",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"true <=> false",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"false <=> true",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"there exists X : X != X",
					new CountsDeclaration("X", "10"),
					"true"),
			new ContradictionData(false,
					"for all X : X != X",
					new CountsDeclaration("X", "10"),
					"true"),	
			//
			// Basic: cases in which F has no free variables and is true, and therefore is *not* a contradiction
			new ContradictionData(false,
					"true",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"a = a",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"a != b",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"not(a != a)",
					new CountsDeclaration(10),
					"false"),					
			new ContradictionData(false,
					"and(a = a, b = b)",
					new CountsDeclaration(10),
					"false"),		
			new ContradictionData(false,
					"or(a = a, b = b)",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"true => true",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"false => true",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"false => false",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"true <=> true",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"false <=> false",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"there exists X : X = X",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"for all X : X = X",
					new CountsDeclaration(10),
					"false"),
			//
			// Basic: if F has free variables, is a contradiction
			new ContradictionData(false,
					"X != X",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"not(X = X)",
					new CountsDeclaration(10),
					"true"),					
			new ContradictionData(false,
					"and(X != X, Y = Y)",
					new CountsDeclaration(10),
					"true"),		
			new ContradictionData(false,
					"or(X != X, Y != Y)",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"X = X => Y != Y",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"X = X <=> Y != Y",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"X != X <=> Y = Y",
					new CountsDeclaration(10),
					"true"),
			new ContradictionData(false,
					"there exists Y : Y != Y and X = X",
					new CountsDeclaration("X", "11", "Y", "7"),
					"true"),
			new ContradictionData(false,
					"for all Y : Y != Y and X = X",
					new CountsDeclaration("X", "11", "Y", "7"),
					"true"),
			//
			// Basic: if F has free variables, is not a contradiction
			new ContradictionData(false,
					"X = X",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"not(X != X)",
					new CountsDeclaration(10),
					"false"),					
			new ContradictionData(false,
					"and(X = X, Y = Y)",
					new CountsDeclaration(10),
					"false"),		
			new ContradictionData(false,
					"or(X = X, Y = Y)",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"X = X => Y = Y",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"X != X => Y = Y",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"X != X => Y != Y",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"X = X <=> Y = Y",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"X != X <=> Y != Y",
					new CountsDeclaration(10),
					"false"),
			new ContradictionData(false,
					"there exists Y : Y = Y and X = X",
					new CountsDeclaration("X", "11", "Y", "7"),
					"false"),
			new ContradictionData(false,
					"for all Y : Y = Y and X = X",
					new CountsDeclaration("X", "11", "Y", "7"),
					"false"),
			//
			// Basic: Other
			new ContradictionData(false,
					"0 = 0", 
					new CountsDeclaration(10),
					"false"),
			//
			// Formerly Illegal Arguments Test, now valid with support of if then else in formulas
			new ContradictionData(false,
					"if a != a then true else false",
					new CountsDeclaration(10),
					"true"), // result is negated because we are testing whether it is a contradiction
			new ContradictionData(false,
					"if a = a then true else false",
					new CountsDeclaration(10),
					"false"), // result is negated because we are testing whether it is a contradiction
			new ContradictionData(false,
					"if X != X then true else false",
					new CountsDeclaration(10),
					"true"), // result is negated because we are testing whether it is a contradiction
			new ContradictionData(false,
					"if X = X then true else false",
					new CountsDeclaration(10),
					"false"), // result is negated because we are testing whether it is a contradiction
		};
		
		perform(tests);
	}
	
	@Test
	public void testTopQuantifierElimination() {
		class TopQuantifierEliminationData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CountsDeclaration           countsDeclaration = null;
			
			public TopQuantifierEliminationData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
										.rewrite(CardinalityRewriter.R_top_quantifier_elimination, exprE);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: if Q is "for all"
			new TopQuantifierEliminationData(false,
					"for all X : X = X",
					new CountsDeclaration("X", "10"),
					"true"),
			new TopQuantifierEliminationData(false,
					"for all X : X != X",
					new CountsDeclaration("X", "10"),
					"false"),
			new TopQuantifierEliminationData(false,
					"for all X : X = a",
					new CountsDeclaration("X", "10"),
					"false"),
			new TopQuantifierEliminationData(false,
					"for all X : X = a",
					new CountsDeclaration("X", "1"),
					"true"),
			new TopQuantifierEliminationData(false,
					"for all X : X != a",
					new CountsDeclaration("X", "10"),
					"false"),
			new TopQuantifierEliminationData(false,
					"for all X : X = a or X = b or X = c",
					new CountsDeclaration("X", "4"),
					"false"),
			new TopQuantifierEliminationData(false,
					"for all X : X = a or X = b or X = c",
					new CountsDeclaration("X", "3"),
					"true"),
			new TopQuantifierEliminationData(false,
					"for all X : X = X and Y = b",
					new CountsDeclaration("X", "10"),
					"Y = b"),
			//
			// Basic: if Q is "there exists"
			new TopQuantifierEliminationData(false,
					"there exists X : X = X",
					new CountsDeclaration("X", "10"),
					"true"),
			new TopQuantifierEliminationData(false,
					"there exists X : X != X",
					new CountsDeclaration("X", "10"),
					"false"),
			new TopQuantifierEliminationData(false,
					"there exists X : X = a",
					new CountsDeclaration("X", "10"),
					"true"),
			new TopQuantifierEliminationData(false,
					"there exists X : X = a",
					new CountsDeclaration("X", "1"),
					"true"),
			new TopQuantifierEliminationData(false,
					"there exists X : X != a",
					new CountsDeclaration("X", "10"),
					"true"),
			new TopQuantifierEliminationData(false,
					"there exists X : X = a",
					new CountsDeclaration("X", "1"),
					"true"),
			new TopQuantifierEliminationData(false,
					"there exists X : X = a or X = b or X = c",
					new CountsDeclaration("X", "4"),
					"true"),
			new TopQuantifierEliminationData(false,
					"there exists X : X = a or X = b or X = c",
					new CountsDeclaration("X", "3"),
					"true"),
			new TopQuantifierEliminationData(false,
					"there exists X : X = X and Y = b",
					new CountsDeclaration("X", "10"),
					"Y = b"),
			// 
			// Illegal Argument Tests
			new TopQuantifierEliminationData(true,
					"X = a",
					new CountsDeclaration("X", "10"),
					"N/A"),			
		};
		
		perform(tests);
	}
	
	@Test
	public void testTopImpliedCertainty() {
		class TopImpliedCertaintyData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CountsDeclaration           countsDeclaration = null;
			
			public TopImpliedCertaintyData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				// Note: Implied Certainty is intended to be used in the context
				// of an exhaustive rewriter.
				ExhaustiveRewriter exhaustiveRewriter = new ExhaustiveRewriter(new ArrayList<Rewriter>(
						Arrays.asList(new Rewriter[] {
							new TopImpliedCertainty()
						})));
				
				Expression result = exhaustiveRewriter.rewrite(exprE, DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process));
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: if is_tautology(C => F), true
			new TopImpliedCertaintyData(false,
					"true",
					new CountsDeclaration(10),
					"true"),
			new TopImpliedCertaintyData(false,
					"a = a",
					new CountsDeclaration(10),
					"true"),
			new TopImpliedCertaintyData(false,
					"X = X",
					new CountsDeclaration(10),
					"true"),
			//
		    // Basic: if if is_tautology(C => not F)
			new TopImpliedCertaintyData(false,
					"false",
					new CountsDeclaration(10),
					"false"),
			new TopImpliedCertaintyData(false,
					"a != a",
					new CountsDeclaration(10),
					"false"),
			new TopImpliedCertaintyData(false,
					"X != X",
					new CountsDeclaration(10),
					"false"),				
			// 
			// Basic: sub-expressions replaced
			new TopImpliedCertaintyData(false,
					"if X = a then if X = a then 1 else 2 else 3",
					new CountsDeclaration(10),
					"if X = a then if true then 1 else whatever else 3"),
			new TopImpliedCertaintyData(false,
					"if X = a then if X = b then 1 else 2 else 3",
					new CountsDeclaration(10),
					"if X = a then if false then whatever else 2 else 3"),
			new TopImpliedCertaintyData(false,
					"if X = a then if X != b then 1 else 2 else 3",
					new CountsDeclaration(10),
					"if X = a then if true then 1 else whatever else 3"),
			new TopImpliedCertaintyData(false,
					"if X = a then if not(X = b) then 1 else 2 else 3",
					new CountsDeclaration(10),
					"if X = a then if true then 1 else whatever else 3"),
			new TopImpliedCertaintyData(false,
					"if X = a then if not(X != b) then 1 else 2 else 3",
					new CountsDeclaration(10),
					"if X = a then if false then whatever else 2 else 3"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testCardinality1Disjunction() {
		class Cardinality1DisjunctionData extends TestData {
			private String                              E;
			private Expression                          exprE;
			private CardinalityRewriter.Quantification quantification    = null;
			private CountsDeclaration                   countsDeclaration = null;
			
			public Cardinality1DisjunctionData(boolean isIllegalArgumentTest, String E, CardinalityRewriter.Quantification quantification, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = quantification;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
											.rewrite(CardinalityRewriter.R_card_disjunction, CardinalityUtil.argForCardinalityDisjunctionCall(exprE, quantification));
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: if quantification is "for all"
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a), and(X != a))} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a, X != Y), and(X != a, X != Z))} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a), and(X = a))} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"10"),
			//
			// Basic: N1 <- R_card(| F1 |_x, quantification)
			// if N1 = 0 return R_card(| F2 |_x, quantification) // | F1 and F2 |_x is 0
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b), and(X != X))} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"8"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b), and(X = c, X != c))} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"8"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b), and(X = c, X != c))} |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "8" : "10"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b))} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b), and(X = c, X != c))} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b), X = a, X = b)} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"10"),
			//
			// Basic: N1 <- R_card(| F1 |_x, quantification)
			// if N1 = |type(x)| return |type(x)| // | F2 |_x = | F1 and F2 |_x and cancel out
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b), and(X = X))} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"10"),	
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b), or(X = c, X != c))} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"10"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b), or(X = c, X != c))} |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"10"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a and X != b), or(X = c, X != c))} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"10"),
			// 
		    // Basic: N2 <- R_card(| F2 |_x, quantification)
			// if N2 = 0 return N1 // | F1 and F2 |_x is 0
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(X != a, and(and(X != a), and(X = a)))} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"9"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(X = a, and(and(X != a), and(X = a)))} |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"anyof(1, 10)"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(X = a, and(and(X != a), and(X = a)))} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			// 
		    // Basic: N2 <- R_card(| F2 |_x, quantification)
			// if N2 = |type(x)| return |type(x)| // N1 = | F1 |_x = | F1 and F2 |_x and cancel out
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(X != a, and(and(X = X), and(X = X)))} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"10"),	
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(X = a, and(and(X = X), and(X = X)))} |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"10"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(X = a, and(X = X))} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"10"),	
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(X = a, and(and(X = X), and(X = X)))} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"10"),	
			//
			// Basic: if quantification is "there exists"
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a), and(X != a))} |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(1),
					"0"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a), and(X != a))} |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(2),
					"2"),
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a, X != Y), and(X != a, X != Z))} |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(2),
					"anyof((if Y = a or Z = a then 2 else 0), (if Y = a then 1 else (if Y != Z then 1 else 0)))"
					//"if Y = a then 2 else (if Z = a then 2 else 0)"
					),
			//
			// Basic: N3 <- R_card_conjunction( | R_top_simplify_conjunction(F1 and F2) |_x, "none" )
			// return R_normalize(N1 + N2 - N3)
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(X = a, X = b)} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"2"),	
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(X != a, X != b)} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"10"),	
		    //
			// Basic: introduce free variables
			new Cardinality1DisjunctionData(false,
					"| {(on X) tuple(X) | or(and(X != a, X != Y), and(X != a, X != Z))} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(2),
					GrinderUtil.usePlain?
							  "if Y = a then 1 else if Z = a then 1 else 0"
							  : "anyof("+ // These seem to be incorrect:
							  "(if Y = a then 1 else (if Z = a then 1 else (if Y = Z then 0 else 1))),"+
					"(if Y = a then 1 else (if Y != Z then 1 else 0)))"),
			//
			// Illegal Argument Tests
			new Cardinality1DisjunctionData(true,
					"| {(on ) tuple(X) | or(X = a, X != b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"N/A"),
			new Cardinality1DisjunctionData(true,
					"| {(on X, Y) tuple(X) | or(X = a, X != b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"N/A"),
			new Cardinality1DisjunctionData(true,
					"| {(on X) tuple(X) | and(X = a, X = b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"N/A"),
		};
		
		perform(tests);
	}
	
	
	@Test
	public void testCardinalityConjunction() {
		class CardinalityConjunctionData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CardinalityRewriter.Quantification quantification    = null;
			private CountsDeclaration           countsDeclaration = null;
			
			public CardinalityConjunctionData(boolean isIllegalArgumentTest, String E, CardinalityRewriter.Quantification quantification, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = quantification;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			public CardinalityConjunctionData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = CardinalityRewriter.Quantification.NONE;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
							.rewrite(CardinalityRewriter.R_card_conjunction, CardinalityUtil.argForCardinalityConjunctionCall(exprE, quantification));
				
				return result;
			}
		}

		TestData[] tests = new TestData[] {
			new CardinalityConjunctionData(false,
				"| {(on X, Y) tuple(X, Y) | X = a and Y = b } |",
				new CountsDeclaration(10),
				"1"),
			new CardinalityConjunctionData(false,
				"| {(on X, Y) tuple(X, Y) | X = a and Y != b } |",
				new CountsDeclaration(10),
				"9"),
			new CardinalityConjunctionData(false,
				"| {(on X) tuple(X) | X != a and X != Z } |",
				new CountsDeclaration(10),
				"(if Z = a then 9 else 8)"),
			new CardinalityConjunctionData(false,
				"| {(on X, Y) tuple(X, Y) | Z = c and Y != b } |",
				new CountsDeclaration(10),
				"(if Z = c then 90 else 0)"),
			new CardinalityConjunctionData(false,
				"| {(on Y, Z) tuple(Y, Z) | Y != a and Z != Y and Z != b } |",
				new CountsDeclaration(10),
				"73"),
			new CardinalityConjunctionData(false,
				"| {(on X, Y) tuple(X, Y) | Y != X and Z != Y } |",
				new CountsDeclaration(10),
				"81"),
			new CardinalityConjunctionData(false,
				"| {(on X, Y, Z) tuple(X, Y, Z) | Y != X and Z != Y } |",
				new CountsDeclaration(10),
				"810"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testCardinalityDisjunction() {
		class CardinalityDisjunctionData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CardinalityRewriter.Quantification quantification    = null;
			private CountsDeclaration           countsDeclaration = null;
			
			public CardinalityDisjunctionData(boolean isIllegalArgumentTest, String E, CardinalityRewriter.Quantification quantification, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = quantification;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			public CardinalityDisjunctionData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = CardinalityRewriter.Quantification.NONE;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
							.rewrite(CardinalityRewriter.R_card_disjunction, CardinalityUtil.argForCardinalityDisjunctionCall(exprE, quantification));
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
				new CardinalityDisjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = a or Y = b } |",
					new CountsDeclaration(10),
					"19"),
				new CardinalityDisjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X != Y or Y != Z } |",
					new CountsDeclaration(10),
					"99"),
				new CardinalityDisjunctionData(false,
					"| {(on X1, Y1, X2, Y2, Y3) tuple(X1, Y1, X2, Y2, Y3) | X1 = a1 or Y1 = b1 or X2 = a2 or Y2 = b2 or Y3 = b3} |",
					new CountsDeclaration(10),
					"40951"),
				new CardinalityDisjunctionData(false,
					"| {(on X1, Y1, X2, Y2, Y3) tuple(X1, Y1, X2, Y2, Y3) | X1 != a1 or Y1 != b1 or X2 != a2 or Y2 != b2 or Y3 != b3} |",
					new CountsDeclaration(10),
					"99999"),
			};
			
		
		perform(tests);
	}
	
	@Test
	public void testCardinalityImplication() {
		class CardinalityImplicationData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CardinalityRewriter.Quantification quantification    = null;
			private CountsDeclaration           countsDeclaration = null;
			
			public CardinalityImplicationData(boolean isIllegalArgumentTest, String E, CardinalityRewriter.Quantification quantification, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = quantification;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			public CardinalityImplicationData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = CardinalityRewriter.Quantification.NONE;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
							.rewrite(CardinalityRewriter.R_card_implication, CardinalityUtil.argForCardinalityImplicationCall(exprE, quantification));
				
				return result;
			}
		}  	

		
		TestData[] tests = new TestData[] {
				new CardinalityImplicationData(false,
					"| {(on X) tuple(X) | (X = a) => (X != b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"10"),
				new CardinalityImplicationData(false,
					"| {(on X) tuple(X) | (X = a) => (Y != b) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "if Y = b then 9 else 10" : "if Y = b then 0 else 10"),
				new CardinalityImplicationData(false,
					"| {(on X) tuple(X) | (X = b) => (X != a) } |",
					new CountsDeclaration(10),
					"10"),
				new CardinalityImplicationData(false,
					"| {(on X) tuple(X) | (X = a) => (Y != b) } |",
					new CountsDeclaration(10),
					"if Y = b then 9 else 10"),
				new CardinalityImplicationData(false,
					"| {(on X) tuple(X) | (X = a) => (Y != b) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "if Y = b then 9 else 10" : "10"),
				new CardinalityImplicationData(false,
					"| {(on X) tuple(X) | (X = a) => (X != b) } |",
					new CountsDeclaration(10),
					"10"),
				new CardinalityImplicationData(false,
					"| {(on X, Y) tuple(X, Y) | X != a => Y != b } |",
					new CountsDeclaration("X", "11", "Y", "7"),
					"67"),
			};
		perform(tests);
	}
	
	@Test
	public void testCardinalityEquivalence() {
		class CardinalityEquivalenceData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CardinalityRewriter.Quantification quantification    = null;
			private CountsDeclaration           countsDeclaration = null;
			
			public CardinalityEquivalenceData(boolean isIllegalArgumentTest, String E, CardinalityRewriter.Quantification quantification, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = quantification;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			public CardinalityEquivalenceData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = CardinalityRewriter.Quantification.NONE;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
							.rewrite(CardinalityRewriter.R_card_equivalence, CardinalityUtil.argForCardinalityEquivalenceCall(exprE, quantification));
				
				return result;
			}
		}  	

		
		TestData[] tests = new TestData[] {
				new CardinalityEquivalenceData(false,
					"| {(on X) tuple(X) | (X = a) <=> (Y != b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if Y = b then 9 else 1"),
				new CardinalityEquivalenceData(false,
					"| {(on X) tuple(X) | (X = a) <=> (Y != b) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "if Y = b then 9 else 1" : "anyof( 10, if Y = b then 10 else 1 )"),
				new CardinalityEquivalenceData(false,
					"| {(on X) tuple(X) | (X = a) <=> (Y != b) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "if Y = b then 9 else 1"
							: "anyof( 0, if Y = b then 0 else 1 )"),
			};
		perform(tests);
	}
	
	@Test
	public void testCardinalityConjunctionOfDisequalities() {
		class CardinalityConjunctionOfDisequalitiesData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CardinalityRewriter.Quantification quantification    = null;
			private CountsDeclaration           countsDeclaration = null;
			
			public CardinalityConjunctionOfDisequalitiesData(boolean isIllegalArgumentTest, String E, CardinalityRewriter.Quantification quantification, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = quantification;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			public CardinalityConjunctionOfDisequalitiesData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = CardinalityRewriter.Quantification.NONE;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
							.rewrite(CardinalityRewriter.R_card_conjunction_of_disequalities, CardinalityUtil.argForCardinalityConjunctionOfDisequalitiesCall(exprE, quantification));
				
				return result;
			}
		}  	
		
		TestData[] tests = new TestData[] {
				new CardinalityConjunctionOfDisequalitiesData(false,
					"| {(on X, Y) tuple(X, Y) | X != a and Y != b } |",
					new CountsDeclaration(10),
					"81"),
				new CardinalityConjunctionOfDisequalitiesData(false,
					"| {(on X) tuple(X) | X != a and X != b } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"10"),
				new CardinalityConjunctionOfDisequalitiesData(false,
					"| {(on X) tuple(X) | X != a and X != b } |",
					new CountsDeclaration(10),
					"8"),
				new CardinalityConjunctionOfDisequalitiesData(false,
					"| {(on X) tuple(X) | X != a and X != X } |",
					new CountsDeclaration(10),
					"0"),
				new CardinalityConjunctionOfDisequalitiesData(false,
					"| {(on X) tuple(X) | X != a and X != b and X != Y } |",
					new CountsDeclaration(10),
					"if Y = a or Y = b then 8 else 7"),
				new CardinalityConjunctionOfDisequalitiesData(false,
					"| {(on X, Y) tuple(X, Y) | X != a and X != Y } |",
					new CountsDeclaration(10),
					"81"),
			};
		perform(tests);
	}
	
	
	@Test
	public void testCardinality1Conjunction() {
		class Cardinality1ConjunctionData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CardinalityRewriter.Quantification quantification    = null;
			private CountsDeclaration           countsDeclaration = null;
			
			public Cardinality1ConjunctionData(boolean isIllegalArgumentTest, String E, CardinalityRewriter.Quantification quantification, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = quantification;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
							.rewrite(CardinalityRewriter.R_card_conjunction, CardinalityUtil.argForCardinalityConjunctionCall(exprE, quantification));
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: if F is True or empty conjunction
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | true } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and() } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"10"),
			//
			// Basic: if F is False
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | false } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"0"),
			//
			// Basic: if x does not occur in F
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | Y = a } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),					
					"if Y = a then 10 else 0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | Y = a } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration("X", "10"),					
					"if Y = a then 10 else 0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | Y = a } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration("X", "10"),					
					"if Y = a then 10 else 0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | Y != a } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),					
					"if Y = a then 0 else 10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | Y != a } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration("X", "10"),					
					"if Y = a then 0 else 10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | Y != a } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration("X", "10"),					
					"if Y = a then 0 else 10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(for all Y : Y = Y) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10", "Y", "10"),
					"10"),	
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(there exists Y : Y = a and Y = b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10", "Y", "10"),
					"0"),
			//
			// Basic: if F is conjunction of x = t and Phi
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X = a } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"1"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X = X } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"10"),	
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X = X and Y = X } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"1"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X = a and Y = a } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if Y = a then 1 else 0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X = a and Y = a } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"if Y = a then 1 else 0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X = a and Y = a } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"if Y = a then 1 else 0"),
			//
			// Basic: if F is x != t1 and ... x != tk and Phi, where x does not occur in Phi - 'for all'
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and X !=b } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and X != Y } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and Y = a } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							"if Y = a then 9 else 0" // PlainCardinalityDPLL computes exact cardinality
							: "0"),
			// #21:
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and Y != a and Y != b} |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							"if Y != a and Y != b then 9 else 0" // PlainCardinalityDPLL computes exact cardinality
							: "0"),
			//
			// Basic: if F is x != t1 and ... x != tk and Phi, where x does not occur in Phi - 'there exists'
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and X != Y } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and X != b } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(2),
					"0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and X != Y } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(2),
					"if Y = a then 1 else 0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and Y = a } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							"if Y = a then 9 else 0" // PlainCardinalityDPLL computes exact cardinality
							: "if Y = a then 10 else 0"),
			// #27:
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and Y != a and Y != b} |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							"if (Y != a and Y != b) then 9 else 0" // PlainCardinalityDPLL computes exact cardinality
							: "if (Y != a and Y != b) then 10 else 0"),
			//
			// Basic: if F is x != t1 and ... x != tk and Phi, where x does not occur in Phi - 'none'
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"9"),	
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and X != b } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"8"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and X != Y } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if Y = a then 9 else 8"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and Y = a } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if Y = a then 9 else 0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and Y != a and Y != b} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if (Y != a and Y != b) then 9 else 0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | X != a and X != Y and Y != a and Y != b} |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if Y != a and Y != b then 8 else 0"),
			//
			// Basic: if Fi is "not G"
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(not(X = a)) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(not(X = a)) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(not(X = a)) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"9"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(not(X = a), not(X = b)) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(not(X = a), not(X = b)) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(not(X = a), not(X = b)) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"8"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(not(X != a)) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"1"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(not(not(X = a))) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"1"),
			//
			// Basic: if Fi is (nested) conjunction (G1 and ... and Gk)
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(and(X = a)) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"1"),
			// #43:
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(and(X != a, X != b)) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							"8" // PlainCardinalityDPLL computes exact cardinality
							: "0"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(and(X != a, X != b)) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							"8" // PlainCardinalityDPLL computes exact cardinality
							: "10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(and(X != a, X != b)) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"8"),					
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(and((X = b) <=> (X != Y))) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if Y = b then 0 else 2"),
			// #47:
			new Cardinality1ConjunctionData(false,
					// equivalent 'and(and((X = b) <=> (X != Y)))'
					"| {(on X) tuple(X) | and(and(or(not(X = b), X != Y), or(X = b, not(X != Y)))) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if Y = b then 0 else 2"),
		    //
			// Basic: if Fi is G1 => G2
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and((X = b) => (X != a)) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"10"),	
		    //
			// Basic: if there is i such that Fi is G1 <=> G2
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and((X = b) <=> (X != Y)) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if Y = b then 0 else 2"),	
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and((X != b) <=> (X != Y)) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"if Y = b then 10 else 8"),	
			//
			// Basic: if Fi is (F1 or F2)
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(or(not(X = b), X != a)) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"10"),	
			//
			// Basic: if there is i such that Fi is 'for all y : G'
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(for all Y : Y = X) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10", "Y", "10"),
					"0"),
			//
			// Basic: if there is i such that Fi is 'there exists y : G'				
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(there exists Y : Y = X) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10", "Y", "10"),
					"10"),
			new Cardinality1ConjunctionData(false,
					"| {(on X) tuple(X) | and(there exists Y : Y != X) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10", "Y", "10"),
					"10"),
			//
			// Illegal Argument Test
			new Cardinality1ConjunctionData(true,
					"| {(on ) tuple(X) | and(X = a, X != b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"N/A"),
			new Cardinality1ConjunctionData(true,
					"| {(on X, Y) tuple(X) | and(X = a, X != b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"N/A"),
			new Cardinality1ConjunctionData(true,
					"| {(on X) tuple(X) | or(X = a, X = b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testCardinality1() {
		class Cardinality1Data extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CardinalityRewriter.Quantification quantification    = null;
			private CountsDeclaration           countsDeclaration = null;
			
			public Cardinality1Data(boolean isIllegalArgumentTest, String E, CardinalityRewriter.Quantification quantification, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.quantification = quantification;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
							.rewrite(CardinalityRewriter.R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(exprE, quantification));
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: if x does not occur in F
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | Y = a } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"if Y = a then 10 else 0"),
			//
			// Basic: if F is a conjunction
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | X = a } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"1"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | and(X != a, X != b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"8"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | X = a } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"1"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | and(X != a, X != b) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "8" : "10"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | X = a } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"1"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | and(X != a, X != b) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "8" : "0"),
			//
			// Basic: if F is disjunction F1 or F2
				// OF INTEREST FOR TOP_SIMPLIFY
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | or(X != a, X = b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"9"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | or(X != a, X = b) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "9" : "10"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | or(X != a, X = b) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "9" : "0"),
			//
			// Basic: if F is "not G"
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | not(X = a) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"9"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | not(X != a) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration(10),
					"1"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | not(X = a) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "9" : "10"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | not(X != a) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration(10),
					"1"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | not(X = a) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					GrinderUtil.usePlain? "9" : "0"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | not(X != a) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration(10),
					"1"),
			// Basic if F is for all y : G
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | for all Y : Y = X } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10", "Y", "10"),
					"0"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | for all Y : Y = X } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration("X", "10", "Y", "10"),
					"0"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | for all Y : Y = X } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration("X", "10", "Y", "10"),
					"0"),
			//
			// Basic: if F is there exists y : G		
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | and(there exists Y : Y = X) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10", "Y", "10"),
					"10"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | and(there exists Y : Y != X) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10", "Y", "10"),
					"10"),	
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | and(there exists Y : Y = X) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration("X", "10", "Y", "10"),
					"10"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | and(there exists Y : Y != X) } |",
					CardinalityRewriter.Quantification.THERE_EXISTS,
					new CountsDeclaration("X", "10", "Y", "10"),
					"10"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | and(there exists Y : Y = X) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration("X", "10", "Y", "10"),
					"10"),
			new Cardinality1Data(false,
					"| {(on X) tuple(X) | and(there exists Y : Y != X) } |",
					CardinalityRewriter.Quantification.FOR_ALL,
					new CountsDeclaration("X", "10", "Y", "10"),
					"10"),
			//
			// Illegal Argument Test
			new Cardinality1Data(true,
					"| {(on ) tuple(X) | and(X = a, X != b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"N/A"),
			new Cardinality1Data(true,
					"| {(on X, Y) tuple(X) | and(X = a, X != b) } |",
					CardinalityRewriter.Quantification.NONE,
					new CountsDeclaration("X", "10"),
					"N/A"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testSumOverOneVariable() {
		class SumOverOneVariableData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CountsDeclaration           countsDeclaration = null;
			
			public SumOverOneVariableData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
										.rewrite(CardinalityRewriter.R_sum_over_one_variable, exprE);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: S is a numeric constant expression.
								
			new SumOverOneVariableData(false,
					"sum({{(on X) 1 | true}})",
					new CountsDeclaration("X", "10"),
					"10"),
			new SumOverOneVariableData(false,
					"sum({{(on X) 2 | true}})",
					new CountsDeclaration("X", "10"),
					"20"),
			new SumOverOneVariableData(false,
					"sum({{(on X) 2 | X = a}})",
					new CountsDeclaration("X", "10"),
					"2"),
			new SumOverOneVariableData(false,
					"sum({{(on X) 2 | X != a}})",
					new CountsDeclaration("X", "10"),
					"18"),
			new SumOverOneVariableData(false,
					"sum({{(on X) 2 | X != a and Y != b}})",
					new CountsDeclaration("X", "10"),
					"if Y != b then 18 else 0"),
			//
			// Basic: S is "if F then S1 else S2".
			new SumOverOneVariableData(false,
					"sum({{(on X) if X = a then 2 else 3 | true}})",
					new CountsDeclaration("X", "10"),
					"29"),		
			new SumOverOneVariableData(false,
					"sum({{(on X) if X = a then (if X = b then 2 else 3) else 4 | true}})",
					new CountsDeclaration("X", "10"),
					"39"),
			new SumOverOneVariableData(false,
					"sum({{(on X) if X = a then 2 else 3 | X != b}})",
					new CountsDeclaration("X", "10"),
					"26"),	
			//
			// Basic: from com.sri.ai.test.grinder.library.equality.cardinality
			// CardinalityComputationTest.testSumOverSingleIndex()
			new SumOverOneVariableData(false,
					// "sum({{(on X) if X=Z then if X=A then 9 else 8 else if X=A then 8 else if Z=A then 8 else 7}})"
					"sum({{(on X) if X=Z then if X=A then 9 else 8 else if X=A then 8 else if Z=A then 8 else 7 | true }})",
					new CountsDeclaration("X", "10", "A", "10", "Z", "10"),
					// "if A = Z then 9 + (| type(X) | - 1) * 8 else 16 + (| type(X) | - 2) * 7"
					"if Z = A then 81 else 72"),	
			new SumOverOneVariableData(false,
					// "sum({{(on X) 1}})"
					"sum({{(on X) 1 | true }})",
					new CountsDeclaration("X", "10"),
					// "sum({{(on X) 1}})"
					"10"),
			new SumOverOneVariableData(false,
					// "sum({{(on X) 3 | X!=Y and X!=a}})"
					"sum({{(on X) 3 | X != Y and X != a}})",
					new CountsDeclaration("X", "10"),
					// "if Y = a then (| type(X) | - 1) * 3 else (| type(X) | - 2) * 3"
					"if Y = a then 27 else 24"),
			new SumOverOneVariableData(false,
					// "sum({{(on X) 3 | X!=Y or X!=a}})"
					"sum({{(on X) 3 | X != Y or X != a}})",
					new CountsDeclaration("X", "10"),
					// "if Y = a then ((| type(X) | - 1 + | type(X) | - 1) - | type(X) | - 1) * 3 else ((| type(X) | - 1 + | type(X) | - 1) - | type(X) | - 2) * 3"
					"if a = Y then 27 else 30"),
			// CardinalityComputationTest.testSumOverSingleIndex()
			new SumOverOneVariableData(false,
					"sum({{(on X) n | X=Y}})",
					new CountsDeclaration("X", "10"),
					"n"),
			new SumOverOneVariableData(false,
					"sum({{(on X) g(t) | X!=Y}})",
					new CountsDeclaration("X", "10"),
					"9 * g(t)"),
			new SumOverOneVariableData(false,
					"sum({{(on X) 2 | true}})",
					new CountsDeclaration("X", "10"),
					"20"),
			//
			// Illegal Argument Tests
			new SumOverOneVariableData(true,
					"sum({{(on X) X | true}})", // X is not Easily-Summable with respect to X
					new CountsDeclaration("X", "10"),
					"N/A"),
			new SumOverOneVariableData(true,
					"sumit({{(on X) X | true}})", // wrong function
					new CountsDeclaration("X", "10"),
					"N/A"),
			new SumOverOneVariableData(true,
					"sumit({{(on X) 2 | true}})", // wrong function
					new CountsDeclaration("X", "10"),
					"N/A"),
			new SumOverOneVariableData(true,
					"sum({(on ) 2 | true})",  // should be multiset
					new CountsDeclaration("X", "10"),
					"N/A"),
		};
		
		perform(tests, 15, 16);
	}
	
	@Test
	public void testEqualityInConjunction() {
		class EqualityInConjunctionData extends TestData {
			private String E;
			private Expression exprE;
			private CountsDeclaration countsDeclaration = null;
			
			public EqualityInConjunctionData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
							.rewrite(CardinalityRewriter.R_equality_in_conjunction, CardinalityUtil.argForEqualityInConjunctionCall(exprE, Quantification.NONE));
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Basic: trivial cases
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = a and Y = b} |",
					new CountsDeclaration(10),
					"1"),	
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = X and Y = b} |",
					new CountsDeclaration(10),
					"10"),
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = X and Y = Y} |",
					new CountsDeclaration(10),
					"100"),
			new EqualityInConjunctionData(false,
					"| {(on X, Y, Z) tuple(X, Y, Z) | X = X and Y = Y and Z = Z} |",
					new CountsDeclaration(10),
					"1000"),
			//
			// Basic: cases where variables have different types/counts.
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = a and Y = b} |",
					new CountsDeclaration("X", "10", "Y", "15"),
					"1"),
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = X and Y = b} |",
					new CountsDeclaration("X", "10", "Y", "15"),
					"10"),
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = X and Y = b} |",
					new CountsDeclaration("X", "15", "Y", "10"),
					"15"),
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = X and Y = Y} |",
					new CountsDeclaration("X", "10", "Y", "15"),
					"150"),
			new EqualityInConjunctionData(false,
					"| {(on X, Y, Z) tuple(X, Y, Z) | X = X and Y = Y and Z = Z} |",
					new CountsDeclaration("X", "10", "Y", "15", "Z", "20"),
					"3000"),
			// 
			// Basic: degenerate cases
			new EqualityInConjunctionData(false,
					"| {(on X) tuple(X) | X = a } |",
					new CountsDeclaration(10),
					"1"),	
			new EqualityInConjunctionData(false,
					"| {(on X) tuple(X) | X = X } |",
					new CountsDeclaration(10),
					"10"),
			//
			// Basic: on equality expressions with > 2 arguments
			new EqualityInConjunctionData(false,
					"| {(on X) tuple(X) | X = X = Y } |",
					new CountsDeclaration(10),
					"1"),
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = X = Y } |",
					new CountsDeclaration("X", "11", "Y", "7"),
					"7"),
			new EqualityInConjunctionData(false,
					"| {(on X) tuple(X) | X = Y = Z} |",
					new CountsDeclaration(10),
					"if Z = Y then 1 else 0"),
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = Y = Z } |",
					new CountsDeclaration("X", "11", "Y", "7"),
					"1"),
			new EqualityInConjunctionData(false,
					"| {(on X) tuple(X) | X = Y = Z and W = a} |",
					new CountsDeclaration(10),
					"if W = a and Z = Y then 1 else 0"),
			new EqualityInConjunctionData(false,
					"| {(on X, Y) tuple(X, Y) | X = Y = Z and W = a} |",
					new CountsDeclaration("X", "11", "Y", "7", "Z", "10", "W", "10"),
					"if W = a then 1 else 0"),
			//
			// Illegal Argument:
			new EqualityInConjunctionData(true,
					"| {(on X, Y) tuple(Z, Y) | X != a and Y != b} |",
					new CountsDeclaration(10),
					"N/A"),	
		};
		
		perform(tests);
	}
	
	@Test
	public void testEdgeCasesFailingInRandomGeneratedFormulas() {
		class EdgeCasesFailingInRandomGeneratedFormulasData extends TestData {
			private String                      E;
			private Expression                  exprE;
			private CountsDeclaration           countsDeclaration = null;
			
			public EdgeCasesFailingInRandomGeneratedFormulasData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
										.rewrite(CardinalityRewriter.R_card, exprE);
				
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
		    //
			new EdgeCasesFailingInRandomGeneratedFormulasData(false,
					"| { ( on X18, X5, X6 ) ( X18, X5, X6 ) | a14 != X18 and (a13 != X5 and X6 != X5 and true) } |",
					new CountsDeclaration(10),
					"729"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testCardinality() {
		class CardinalityData extends TestData {
			private String E;
			private Expression exprE;
			private CountsDeclaration countsDeclaration = null;
			
			public CardinalityData(Expression contextualConstraint, boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, contextualConstraint, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			public CardinalityData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				this(Expressions.TRUE, isIllegalArgumentTest, E, countsDeclaration, expected);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				// NOTE:
				// We will test for both situations where the ASSUME_DOMAIN_ALWAYS_LARGE is
				// set and then not set.
				String defaultAssumeDomainAlwaysLarge = ""+GrinderConfiguration.isAssumeDomainsAlwaysLarge();

				Expression result1, result2;
				try {
					// Result 1 with ASSUME_DOMAIN_ALWAYS_LARGE set to true and no counts declarations made
					Configuration.setProperty(GrinderConfiguration.KEY_ASSUME_DOMAIN_ALWAYS_LARGE, "true");	
					RewritingProcess cardinalityProcess = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process);
					result1 = cardinalityProcess.rewrite(CardinalityRewriter.R_card, exprE);
					
					// Result 2 with ASSUME_DOMAIN_ALWAYS_LARGE set to false and counts declarations made
					countsDeclaration.setup(process);
					Configuration.setProperty(GrinderConfiguration.KEY_ASSUME_DOMAIN_ALWAYS_LARGE, "false");	
					cardinalityProcess = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process);
					result2 = cardinalityProcess.rewrite(CardinalityRewriter.R_card, exprE);
				} catch (IllegalArgumentException iae) {
					throw iae;
				} finally {
					// Reset the default.
					Configuration.setProperty(GrinderConfiguration.KEY_ASSUME_DOMAIN_ALWAYS_LARGE, defaultAssumeDomainAlwaysLarge);
				}
				return Tuple.make(result1, result2);
			}
		}
		
		TestData[] tests = new TestData[] {
			//
			// Scope shadowing tests 
			new CardinalityData(
					parse("X != a"),
					false,
					"| {(on X) tuple(X) | X = a } |",
					new CountsDeclaration(10),
					"(1, 1)"),
			new CardinalityData(
					parse("X != a"),
					false,
					"| {(on Z) tuple(Z) | X = a } |",
					new CountsDeclaration(10),
					"(0, 0)"),
			new CardinalityData(
					parse("Z != a"),
					false,
					"| {(on X) tuple(X) | X != a or Z = a } |",
					new CountsDeclaration(10),
					"( (| type(X) | - 1), 9 )"),
			new CardinalityData(
					parse("Z = a"),
					false,
					"| {(on X) tuple(X) | X != a or Z = a } |",
					new CountsDeclaration(10),
					"( (| type(X) |), 10 )"),
			new CardinalityData(
					parse("X = a"),
					false,
					"| {(on X in Type) tuple(X) | X != a } |",
					new CountsDeclaration(10),
					"(| Type |-1, 9)"),	
			//
			// Basic: Examples from paper: 
		    // 'Lifted Arbitrary Constraint Solving for Lifted Probabilistic Inference'.
			// #5: | X = a |_x
			new CardinalityData(false,
					"| {(on X) tuple(X) | X = a } |",
					new CountsDeclaration(10),
					"(1, 1)"),
			// #6: | X != a |_x
			new CardinalityData(false,
					"| {(on X) tuple(X) | X != a } |",
					new CountsDeclaration(10),
					"((| type(X) | - 1), 9)"),	
			// #7: | Y = b |_x,y
			new CardinalityData(false,
					"| {(on X, Y) tuple(X, Y) | Y = b } |",
					new CountsDeclaration("X", "10", "Y", "10"),
					"( (| type(X) |), 10 )"),
			// #8: | Y != b |_x,y
			new CardinalityData(false,
					"| {(on X, Y) tuple(X, Y) | Y != b } |",
					new CountsDeclaration("X", "10", "Y", "10"),
					"( anyof( ((| type(Y) | - 1) * | type(X) |), (| type(X) | * (| type(Y) | - 1)) ), 90 )"),
					//"( ((| type(Y) | - 1) * | type(X) |), 90 )"),
			// #9: | True |_x,y
			new CardinalityData(false,
					"| {(on X, Y) tuple(X, Y) | true } |",
					new CountsDeclaration("X", "10", "Y", "10"),
					"( (| type(X) | * | type(Y) |), 100 )"),		
			// #10: | False |_x,y
			new CardinalityData(false,
					"| {(on X, Y) tuple(X, Y) | false } |",
					new CountsDeclaration("X", "10", "Y", "10"),
					"(0, 0)"),
			// #11: | Z = c |
			new CardinalityData(false,
					"| {(on ) tuple() | Z = c } |",
					new CountsDeclaration(10),
					"( anyof((if Z = c then 1 else 0), (if Z != c then 0 else 1)), (if Z = c then 1 else 0) )"),
			// #12: | X = a |
			new CardinalityData(false,
					"| {(on ) tuple() | X = a } |",
					new CountsDeclaration(10),
					"( (if X = a then 1 else 0), (if X = a then 1 else 0) )"),
			// #13: | there exists X : X = Y and Z = a |_y
			new CardinalityData(false,
					"| {(on Y) tuple(Y) | there exists X : X = Y and Z = a } |",
					new CountsDeclaration("X", "10", "Y", "10"),
					"( (if Z = a then | type(Y) | else 0), (if Z = a then 10 else 0) )"),
			//
			// Basic: from com.sri.ai.test.grinder.library.equality.cardinality.original.
		    // CardinalityTest.testCardinalityOnSingleIndexAndConjunctionCondition()
			new CardinalityData(false,
					// "|{{(on Z) 1 | false}}|"
					"| {(on Z) tuple(Z) | false } |",
					new CountsDeclaration("Z", "10"),
					"(0, 0)"),	
			new CardinalityData(false,
					// "|{{(on Z) 1 | false}}|"
					"| {(on Z) tuple(Z) | (Z != a and (Z != b and (not (Z = c))) and ((not (not (not (Z = d)))))) } |",
					new CountsDeclaration("Z", "10"),
					"(| type(Z) | - 4, 6)"),	
			new CardinalityData(false,
					// "|{{(on Z) 1}}|"
					"| {(on Z) tuple(Z) | true } |",
					new CountsDeclaration("Z", "10"),
					"( (| type(Z) |), 10 )"),
			new CardinalityData(false,
					// "|{{(on Z) 1 | Z = a}}|"
					"| {(on Z) tuple(Z) | Z = a } |",
					new CountsDeclaration("Z", "10"),
					"(1, 1)"),
			new CardinalityData(false,
					// "|{{(on Z) 1 | Z != a}}|"
					"| {(on Z) tuple(Z) | Z != a } |",
					new CountsDeclaration("Z", "10"),
					"( (| type(Z) | - 1), 9 )"),
			new CardinalityData(false,
					// "|{{(on Z) 1 | Z != X}}|"
					"| {(on Z) tuple(Z) | Z != X } |",
					new CountsDeclaration("Z", "10"),
					"( (| type(Z) | - 1), 9 )"),
			new CardinalityData(false,
					// "|{{(on Z) 1 | X != a and X != Y and Z != Y and Z != b}}|"
					"| {(on Z) tuple(Z) | X != a and X != Y and Z != Y and Z != b } |",
					new CountsDeclaration("Z", "10", "X", "10", "Y", "10"),
					// "if X != a and X != Y then if Y = b then | type(Z) - {b} | else | type(Z) - {b,Y} | else 0"
					GrinderUtil.usePlain?
							"(if X = a then 0 else if X = Y then 0 else if Y = b then | type(Z) | - 1 else | type(Z) | - 2, if X = a then 0 else if X = Y then 0 else if Y = b then 9 else 8)"
//							  "(if X = a then 0 else if Y = a then | type(Z) | - 2 else if X = Y then 0 else if Y = b then | type(Z) | - 1 else | type(Z) | - 2, if X = a then 0 else if Y = a then 8 else if X = Y then 0 else if Y = b then 9 else 8)"
							: "(if (X != a) and (X != Y) then if Y = b then | type(Z) | - 1 else | type(Z) | - 2 else 0, if (X != a) and (X != Y) then if Y = b then 9 else 8 else 0)"),
			new CardinalityData(false,
					// "|{{(on Z) 1 | Z != a and X != Z}}|"
					"| {(on Z) tuple(Z) | Z != a and X != Z } |",
					new CountsDeclaration("Z", "10", "X", "10"),
					// "if X = a then | type(Z) - {a} | else | type(Z) - {a,X} |"
					"( (if X = a then | type(Z) | - 1 else | type(Z) | - 2), (if X = a then 9 else 8) )"),
			//
			// Basic: from com.sri.ai.test.grinder.library.equality.cardinality.original.
		    // CardinalityTest.testCardinalityOnSingleIndexAndDisjunctionCondition()	
			new CardinalityData(false,
					// "|{{(on Z) 1 | Z = a or Z = b}}|"
					"| {(on Z) tuple(Z) | Z = a or Z = b } |",
					new CountsDeclaration("Z", "10"),
					"( anyof( 2, (| type(Z) | - (| type(Z) | - 2)) ), 2 )"),
			// #23:
			new CardinalityData(false,
					// "|{{(on Z) 1 | Z = a <=> W = b}}|"
					"| {(on Z) tuple(Z) | Z = a <=> W = b } |",
					new CountsDeclaration("Z", "10", "W", "10"),
					// "if W != b then |type(Z) - {a}| else 1"
					"( (if W = b then 1 else | type(Z) | - 1), (if W = b then 1 else 9) )"),	
			//
			// Basic: from com.sri.ai.test.grinder.library.equality.cardinality.original.
		    // CardinalityTest.testRick()
			new CardinalityData(false,
					// "|{{ ( on VarInst18, VarInst2 ) 1 | VarInst18 = p6 and VarInst2 = c1 or VarInst18 = p4 and VarInst2 = c1 or VarInst18 = p2 and VarInst2 = c1 or VarInst18 = p8 and VarInst2 = c1 }}|"
					"| {(on VarInst18, VarInst2) tuple(VarInst18, VarInst2) | VarInst18 = p6 and VarInst2 = c1 or VarInst18 = p4 and VarInst2 = c1 or VarInst18 = p2 and VarInst2 = c1 or VarInst18 = p8 and VarInst2 = c1 } |",
					new CountsDeclaration(10),
					"(4, 4)"),	
			//
			// Basic: from com.sri.ai.test.grinder.library.equality.cardinality.original.
		    // CardinalityTest.testCardinalityOnMultipleIndices()	
			// TODO: first options are from PlainCardinalityDPLL; need to check which is correct, especially the numeric answer below.
			new CardinalityData(false,
					"| {(on X, Y, Z) tuple(X, Y, Z) | X != a and X != Y and Z != Y and Z != b } |",
					new CountsDeclaration(10),
					"( anyof( " + // first one is from PlainCardinalityDPLL
					"(| type(X) | - 1) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Y) | - 1) + (| type(X) | - 2) * (| type(Y) | - 2) * (| type(Z) | - 2), " +
					"((| type(X) | - 2) * (| type(Z) | - 1 + (| type(Y) | - 2) * (| type(Z) | - 2)) + (| type(Y) | - 1) * (| type(Z) | - 2)), " +
					"((| type(X) | - 2) * ((| type(Z) | + (| type(Y) | - 2) * (| type(Z) | - 2)) - 1) + (| type(Y) | - 1) * (| type(Z) | - 2)), " +
					"((| type(Y) | - 1) * (| type(Z) | - 2) + (| type(X) | - 2) * ((| type(Z) | + (| type(Y) | - 2) * (| type(Z) | - 2)) - 1)), " +
					"((| type(X) | - 2) * (| type(Z) | - 1) + | type(Z) | - 2 + (| type(X) | - 2) * (| type(Z) | - 2) + (| type(Y) | - 2) * ((| type(Z) | - 1) - 1) + (| type(Y) | - 2) * ((| type(X) | - 1) - 2) * ((| type(Z) | - 1) - 1)) ),"

					// first one is from PlainCardinalityDPLL
					+ "656 )"),	
			new CardinalityData(false,
					// "|{{(on Z, W) 1 | Z = a }}|"
					"| {(on Z, W) tuple(Z, W) | Z = a } |",
					new CountsDeclaration("Z", "20", "W", "10"),
					// "|type(W)|"
					"( (| type(W) |), 10 )"),	
			new CardinalityData(false,
					// "|{{(on Z, W) 1 | Z != a }}|"
					"| {(on Z, W) tuple(Z, W) | Z != a } |",
					new CountsDeclaration("Z", "20", "W", "10"),
					// "|type(W)| * (|type(Z)| - 1)"
					"( ((| type(Z) | - 1) * | type(W) |), 190 )"),	
			new CardinalityData(false,
					"| {(on Z, W) tuple(Z, W) | Z != W } |",
					new CountsDeclaration(10),
					"( anyof( (| type(W) | * (| type(Z) | - 1)), " +
					"(| type(Z) | * (| type(W) | - 1)) ), 90 )"),	
			new CardinalityData(false,
					// "|{{(on Z, W) 1 | Z != a and X != b }}|"
					"| {(on Z, W) tuple(Z, W) |  Z != a and X != b } |",
					new CountsDeclaration("Z", "10", "W", "5"),
					// "if X != b then |type(W)| * (|type(Z)| - 1) else 0"
					"( (if X = b then 0 else (| type(Z) | - 1) * | type(W) |), (if X = b then 0 else 45) )"),
			new CardinalityData(false,
					// "|{{(on X, Y) 1 | X != a and Y != b }}|"
					"| {(on X, Y) tuple(X, Y) |  X != a and Y != b } |",
					new CountsDeclaration("X", "11", "Y", "7"),
					// "(|type(Y)| - 1 ) * (|type(X)| - 1)"
					"( ((| type(X) | - 1) * (| type(Y) | - 1)), 60 )"),
			//
			// Basic: from com.sri.ai.test.grinder.library.equality.cardinality
			// CardinalityComputationTest.testCardinality()
			new CardinalityData(false,
					// "|{{(on Y) 5 | X!=Y and Y!=Z and Y!=A}}|"
					"| {(on Y) tuple(Y) |  X != Y and Y != Z and Y != A } |",
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							"(if X = Z then if Z = A then | type(Y) | - 1 else | type(Y) | - 2 else if X = A then | type(Y) | - 2 else if Z = A then | type(Y) | - 2 else | type(Y) | - 3, if X = Z then if Z = A then 9 else 8 else if X = A then 8 else if Z = A then 8 else 7)"  
							//"(if Z = X then if A = X then | type(Y) | - 1 else | type(Y) | - 2 else if A = X then | type(Y) | - 2 else if A = Z then | type(Y) | - 2 else | type(Y) | - 3, if Z = X then if A = X then 9 else 8 else if A = X then 8 else if A = Z then 8 else 7)"
							: "(if (X = Z) or (X = A) then if Z = A then | type(Y) | - 1 else | type(Y) | - 2 else if Z = A then | type(Y) | - 2 else | type(Y) | - 3, if (X = Z) or (X = A) then if Z = A then 9 else 8 else if Z = A then 8 else 7)"),
			new CardinalityData(false,
					// "| {{ ( on X' in People ) (if epidemic then 0.70 else 0.20) | ((X = mary and X != X') or (X = john and X != X')) and X' != bob and X' != mary and X' != john }} |"
					"| {(on X') tuple(X') | ((X = mary and X != X') or (X = john and X != X')) and X' != bob and X' != mary and X' != john } |",
					new CountsDeclaration("X'", "10"),
					// "if X = mary then | People | - 3 else if X = john then | People | - 3 else 0""
					"( (if X = mary then | type(X') | - 3 else if X = john then | type(X') | - 3 else 0), (if X = mary then 7 else if X = john then 7 else 0) )"),	
			// #33:
			new CardinalityData(false,
					// "|{{(on X, Y) 5 | not (X=Y=Z)}}|"
					"| {(on X, Y) tuple(X, Y) | not (X=Y=Z) } |",
					new CountsDeclaration("X", "11", "Y", "11"),
					"( anyof(" +
					"(| type(Y) | + -1 + (| type(X) | - 1) * | type(Y) |), " + // PlainCardinalityDPLL
					"((| type(X) | * (| type(Y) | - 1) + (| type(Y) | - 1) * | type(X) |) - (| type(Y) | - 1 + (| type(X) | - 1) * (| type(Y) | - 2))), " +
					"((| type(X) | * (| type(Y) | - 1) + | type(Y) |) - 1), " +
					"((| type(X) | * (| type(Y) | - 1) + | type(X) | * (| type(Y) | - 1)) - (| type(Y) | - 1 + (| type(X) | - 1) * (| type(Y) | - 2))), " +
					"((| type(Y) | * (| type(X) | - 1) + (| type(Y) | - 1) * | type(X) |) - (| type(Y) | - 1) * (| type(X) | - 1)), " +
					"((| type(X) | * (| type(Y) | - 1) + (| type(Y) | - 1) * | type(X) | + 1) - (| type(Y) | + (| type(X) | - 1) * (| type(Y) | - 2))), " + 
					"((| type(Y) | - 1 + | type(Y) | - 1) - (| type(Y) | - 1) + (| type(X) | - 1) * ((| type(Y) | - 1 + | type(Y) | - 1) - (| type(Y) | - 2)))"
					+ ""
					+ "), 120 )"),
			new CardinalityData(false,
					// "|{{(on X in People) 1 | for all Y: X=Y}}|"
					"| {(on X) tuple(X) |  for all Y: X = Y } |",
					new CountsDeclaration("X", "10", "Y", "10"),
					// "if |type(Y)| <= 1 then |People| else 0"
					GrinderUtil.usePlain?
					  "(0,0)" // uses GrinderConfiguration.isAssumeDomainsAlwaysLarge() to conclude that | type(Y) | > 1, as it should
					: "( (if | type(Y) | = 1 then | type(X) | else 0), 0 )"),
			new CardinalityData(false,
					// "|{{(on X in People) 1 | for all Y: X=Y}}|"
					"| {(on X) tuple(X) |  for all Y: X = Y } |",
					new CountsDeclaration("X", "10", "Y", "1"),
					// "if |type(Y)| <= 1 then |People| else 0"
					GrinderUtil.usePlain?
							  "(0,"  // uses GrinderConfiguration.isAssumeDomainsAlwaysLarge() to conclude that | type(Y) | > 1, as it should
							  + "10)"
							: "( (if | type(Y) | = 1 then | type(X) | else 0), 10 )"),
// Note: moved to testCardinalityKnownSizesRequired()
//			new CardinalityData(false,
//					"{(on X) tuple(X) | there exists Y : X != Y or Y != Z }",
//					new CountsDeclaration("X", "10", "Y", "3"),
//					"10"),
// Note: moved to testCardinalityKnownSizesRequired()
//			new CardinalityData(false,
//					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y or Y != Z}}|",
//					"{(on X) tuple(X) | there exists Y : there exists Z: X != Y or Y != Z }",
//					new CountsDeclaration("X", "10", "Y", "3", "Z", "2"),
//					// "if | type(Z) | > 1 and | type(Y) | > 0 or | type(Z) | > 0 and | type(Y) | > 1 then | People | else 0"
//					"10"),
// Note: moved to testCardinalityKnownSizesRequired()
//			new CardinalityData(false,
//					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y or Y != Z}}|",
//					"{(on X) tuple(X) | there exists Y : there exists Z: X != Y or Y != Z }",
//					new CountsDeclaration("X", "10", "Y", "1", "Z", "2"),
//					// "if | type(Z) | > 1 and | type(Y) | > 0 or | type(Z) | > 0 and | type(Y) | > 1 then | People | else 0"
//					"10"),
// Note: moved to testCardinalityKnownSizesRequired()
//			new CardinalityData(false,
//					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y or Y != Z}}|",
//					"{(on X) tuple(X) | there exists Y : there exists Z: X != Y or Y != Z }",
//					new CountsDeclaration("X", "10", "Y", "3", "Z", "1"),
//					// "if | type(Z) | > 1 and | type(Y) | > 0 or | type(Z) | > 0 and | type(Y) | > 1 then | People | else 0"
//					"10"),
// Note: moved to testCardinalityKnownSizesRequired()
//			new CardinalityData(false,
//					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y or Y != Z}}|",
//					"{(on X) tuple(X) | there exists Y : there exists Z: X != Y or Y != Z }",
//					new CountsDeclaration("X", "10", "Y", "1", "Z", "1"),
//					// "if | type(Z) | > 1 and | type(Y) | > 0 or | type(Z) | > 0 and | type(Y) | > 1 then | People | else 0"
//					"0"),
			// #36:
			new CardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y and Y = Z}}|"
					"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y = Z } |",
					new CountsDeclaration("X", "10", "Y", "3", "Z", "2"),
					// "if | type(Y) | > 1 then | People | else 0"
					// Note: first result looks wrong but is ok as we are working with ASSUME_DOMAIN_ALWAYS_LARGE assumption
					GrinderUtil.usePlain?
							  "(|type(X)|,"  // uses GrinderConfiguration.isAssumeDomainsAlwaysLarge() to conclude that | type(Y) | > 0, as it should
							  + "10)"
							: "( (if | type(Y) | > 0 then | type(X) | else 0), 10 )"),
			new CardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y and Y = Z}}|"
					"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y = Z } |",
					new CountsDeclaration("X", "10", "Y", "1", "Z", "2"),
					// "if | type(Y) | > 1 then | People | else 0"
					GrinderUtil.usePlain?
							  "(|type(X)|,"  // uses GrinderConfiguration.isAssumeDomainsAlwaysLarge() to conclude that | type(Y) | > 0, as it should
							  + "0)"
					// Note: first result looks wrong but is ok as we are working with ASSUME_DOMAIN_ALWAYS_LARGE assumption
							: "( (if | type(Y) | > 0 then | type(X) | else 0), 0 )"), // INCORRECT, should impose condition | type(Y) | > 1 because at some point it gets there exists Y : Y != X
			new CardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X = Y and Y = Z}}|"
					"| {(on X) tuple(X) | there exists Y : there exists Z: X = Y and Y = Z } |",
					new CountsDeclaration("X", "10", "Y", "1", "Z", "2"),
					// "if | type(Y) | > 0 then | People | else 0"
					"( (| type(X) |), 10 )"),
// Note: moved to testCardinalityKnownSizesRequired()
//			new CardinalityData(false,
//					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y and Y != Z}}|"
//					"{(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y != Z }",
//					new CountsDeclaration("X", "10", "Y", "3", "Z", "2"),
//					// "if | type(Z) | > 1 and | type(Y) | > 1 then | People | else 0"
//					"10"),
// Note: moved to testCardinalityKnownSizesRequired()
//			new CardinalityData(false,
//					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y and Y != Z}}|"
//					"{(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y != Z }",
//					new CountsDeclaration("X", "10", "Y", "1", "Z", "2"),
//					// "if | type(Z) | > 1 and | type(Y) | > 1 then | People | else 0"
//					"0"),
// Note: moved to testCardinalityKnownSizesRequired()
//			new CardinalityData(false,
//					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y and Y != Z}}|"
//					"{(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y != Z }",
//					new CountsDeclaration("X", "10", "Y", "3", "Z", "1"),
//					// "if | type(Z) | > 1 and | type(Y) | > 1 then | People | else 0"
//					"0"),
			// #39:
			new CardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : X != Y and Y != Z}}|"
					"| {(on X) tuple(X) | there exists Y : X != Y and Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "3", "Z", "1"),
					// "if X = Z then if | type(Y) | > 1 then | People | else 0 else if | type(Y) | > 2 then | People | else 0"
					GrinderUtil.usePlain?
							  "(|type(X)|,"  // uses GrinderConfiguration.isAssumeDomainsAlwaysLarge() to conclude that | type(Y) | > 0, as it should
							  + "10)"
							  // Note: first result looks wrong but is ok as we are working with ASSUME_DOMAIN_ALWAYS_LARGE assumption
							: "( (if | type(Y) | > 0 then | type(X) | else 0), 10 )"),
			new CardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : X != Y and Y != Z}}|"
					"| {(on X) tuple(X) | there exists Y : X != Y and Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "2", "Z", "1"),
					// "if X = Z then if | type(Y) | > 1 then | People | else 0 else if | type(Y) | > 2 then | People | else 0"
					GrinderUtil.usePlain?
							  "(|type(X)|,"  // uses GrinderConfiguration.isAssumeDomainsAlwaysLarge() to conclude that | type(Y) | > 0, as it should
							  + "1)" // tricky example: for there to exist a Y, Z and X must be same value, because type(Y) has only two elements.
							: 					// Note: first result looks wrong but is ok as we are working with ASSUME_DOMAIN_ALWAYS_LARGE assumption
					"( (if | type(Y) | > 0 then | type(X) | else 0), 1 )"),
			new CardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : X != Y and Y != Z}}|"
					"| {(on X) tuple(X) | there exists Y : X != Y and Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "1", "Z", "1"),
					// "if X = Z then if | type(Y) | > 1 then | People | else 0 else if | type(Y) | > 2 then | People | else 0"
					GrinderUtil.usePlain?
							  "(|type(X)|,"  // uses GrinderConfiguration.isAssumeDomainsAlwaysLarge() to conclude that | type(Y) | > 0, as it should
							  + "0)"
							: 					// Note: first result looks wrong but is ok as we are working with ASSUME_DOMAIN_ALWAYS_LARGE assumption
					"( (if | type(Y) | > 0 then | type(X) | else 0), 0 )"),
			new CardinalityData(false,
					// "|{{(on X, Y) 1 | X != a <=> Y != b}}|"
					"| {(on X, Y) tuple(X, Y) | X != a <=> Y != b} |",
					new CountsDeclaration("X", "11", "Y", "7"),
					// "(| type(X) | - 1) * (| type(Y) | - 1) + 1"
					"( anyof( " +
					"(1 + (| type(X) | - 1) * (| type(Y) | - 1)), " +
					"((| type(Y) | - 1) * (| type(X) | - 1) + 1), " +
					"((| type(X) | - 1) * (| type(Y) | - 1) + 1) ), 61 )"),
			// #43:
			new CardinalityData(false,
					// "| {{ ( on X' in People ) 1 | (X = X' = person1 or X = X' = person2 or X = X' = person3) and not (X = X') }} |"
					"| {(on X') tuple(X') | (X = X' = person1 or X = X' = person2 or X = X' = person3) and not (X = X') } |",
					new CountsDeclaration("X'", "10"),
					// "0"
					"(0, 0)"),
			new CardinalityData(false,
					// "| {{ ( on X in Cars, Y in People ) 1 }} |"
					"| {(on X, Y) tuple(X, Y) | true } |",
					new CountsDeclaration("X", "11", "Y", "7"),
					// "| Cars | * | People |"
					"( (| type(X) | * | type(Y) |), 77 )"),
			new CardinalityData(false,
					// "| {{ ( on X in People ) 1 }} |"
					"| {(on X) tuple(X) | true } |",
					new CountsDeclaration("X", "11"),
					// "| People |"
					"( (| type(X) |), 11 )"),
			new CardinalityData(false,
					// "| {{ ( on X in People ) 1 | not (X != bob and X != mary and X != john) and not (X = bob) }} |"
					"| {(on X) tuple(X) | not (X != bob and X != mary and X != john) and not (X = bob) } |",
					new CountsDeclaration("X", "11"),
					// "2"
					"( anyof( 2, (| type(X) | - (1 + | type(X) | - 3)) ), 2 )"),
			new CardinalityData(false,
					// "|{{(on X, Y) 1 | X != a}}|"
					"| {(on X, Y) tuple(X, Y) | X != a } |",
					new CountsDeclaration("X", "11", "Y", "7"),
					// "(| type(X) | - 1) * | type(Y) |"
					"( ((| type(X) | - 1) * | type(Y) |), 70 )"),
			new CardinalityData(false,
					// "|{{(on X) 1 | X = Y}}|", Util.map(parse("|type(X)|"), 3),
					"| {(on X) tuple(X) | X = Y } |",
					new CountsDeclaration("X", "3"),
					// "1"
					"(1, 1)"),
			new CardinalityData(false,
					// "|{{(on X) 1 | X = X}}|"
					"| {(on X) tuple(X) | X = X } |",
					new CountsDeclaration("X", "10"),
					// "| type(X) |"
					"( (| type(X) |), 10 )"),
			new CardinalityData(false,
					// "|{{(on X) 1 | X != X}}|"
					"| {(on X) tuple(X) | X != X } |",
					new CountsDeclaration("X", "10"),
					// "0"
					"(0, 0)"),
			new CardinalityData(false,
					// "|{{(on X) 1 | X != X}}|"
					"| {(on X) tuple(X) | X != X } |",
					new CountsDeclaration("X", "10"),
					// "0"
					"(0, 0)"),
			// #52:
			new CardinalityData(false,
					// "|{{(on X, Y) 5 | X=Y=Z}}|"
					"| {(on X, Y) tuple(X, Y) | X=Y=Z } |",
					new CountsDeclaration("X", "11", "Y", "7"),
					// "1"
					"(1, 1)"),
			new CardinalityData(false,
					// "|{{(on Y, X, Z) 1 | X != a and Y != Z}}|"
					"| {(on X, Y, Z) tuple(X, Y, Z) | X != a and Y != Z } |",
					new CountsDeclaration("X", "11", "Y", "7", "Z", "7"),
					// "| type(Y) | * (| type(X) | - 1) * (| type(Z) | - 1)"
					"( anyof("
					+ "((| type(X) | - 1) * (| type(Y) | - 1) * | type(Z) |),"
					+ "((| type(X) | - 1) * | type(Y) | * (| type(Z) | - 1))"
					+ "), "
					+ "420 )"),
			new CardinalityData(false,
					// "|{{(on Y, X) 1 | X != a and Y != Z}}|"
					"| {(on X, Y) tuple(X, Y) | X != a and Y != Z } |",
					new CountsDeclaration("X", "11", "Y", "7"),
					// "(| type(Y) | - 1) * (| type(X) | - 1)"
					"( ((| type(X) | - 1) * (| type(Y) | - 1)), 60 )"),
			new CardinalityData(false,
					// "|{{(on X) X | X != a}}|"
					"| {(on X) tuple(X) | X != a } |",
					new CountsDeclaration("X", "11"),
					// "|type(X)| - 1"
					"( (| type(X) | - 1), 10 )"),
			new CardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : X != Y}}|"
					"| {(on X) tuple(X) |there exists Y : X != Y } |",
					new CountsDeclaration("X", "11", "Y", "2"),
					// "if |type(Y)| > 1 then |People| else 0
					GrinderUtil.usePlain?
							  "(|type(X)|," // uses GrinderConfiguration.isAssumeDomainsAlwaysLarge() to conclude that | type(Y) | > 0, as it should
							  + "11)"
					// Note: first result looks wrong but is ok as we are working with ASSUME_DOMAIN_ALWAYS_LARGE assumption
							: "( (if | type(Y) | > 0 then | type(X) | else 0), 11 )"),
			// #57:
			new CardinalityData(false,
					"| {(on X, Y, Z) tuple(X, Y, Z) | X!=a and Y!=b and Z!=W and Y!=X and X!=Z } |",
					new CountsDeclaration(10),
					GrinderUtil.usePlain? // TODO: not sure if answers are equivalent
							  //"(if W = b then (| type(Y) | - 1) * (| type(Z) | - 1) + | type(X) | + -2 + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 4) * (| type(Y) | - 3) * (| type(Z) | - 2) else if W = a then (| type(Y) | - 1) * (| type(Z) | - 2) + | type(X) | + -2 + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 4) * (| type(Y) | - 3) * (| type(Z) | - 2) else (| type(Y) | - 1) * (| type(Z) | - 2) + | type(X) | + -4 + (| type(X) | - 3) * (| type(Y) | - 2) + | type(X) | + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 4) * (| type(Z) | - 3) + (| type(X) | - 3) * (| type(Z) | - 3) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 4) * (| type(Y) | - 4) * (| type(Z) | - 3), if W = b then 593 else if W = a then 584 else 592)"
							  "(  if W = a then (| type(X) | - 2) * (| type(Z) | - 1) + (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Y) | - 3) * (| type(Z) | - 2) else if W = b then | type(X) | + -1 + (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Y) | - 3) * (| type(Z) | - 2) else | type(X) | + -1 + (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Z) | - 3) + (| type(X) | - 2) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Y) | - 3) * (| type(Z) | - 3), if W = a then 584 else if W = b then 593 else 592)"
							: "( (if W = b then (| type(Y) | - 1) * (| type(Z) | - 1) + (| type(X) | - 2) * (| type(Y) | - 2) * (| type(Z) | - 2) else (if W = a then (| type(Y) | - 1) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Y) | - 2) * (| type(Z) | - 2) else (| type(Y) | - 2) * (| type(Z) | - 1) + (| type(Y) | - 1) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) * (| type(Z) | - 2))), (if W = b then 593 else (if W = a then 584 else 592)) )"),							
					
			new CardinalityData(false,
					"| { (on X, Y, Z) tuple(X, Y, Z) |  X != a and X != V and X != Y and Y != Z and X != Z } |",
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							  "(if V = a then (| type(X) | - 2) * (| type(Z) | - 1) + (| type(X) | - 2) * (| type(Y) | - 1) + (| type(X) | - 3) * (| type(Y) | - 2) * (| type(Z) | - 1) else | type(X) | + -4 + (| type(X) | - 3) * (| type(Z) | - 2) + | type(X) | + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 4) * (| type(Y) | - 3) * (| type(Z) | - 2), if V = a then 648 else 576)"
							//"(if V = a then (| type(X) | - 2) * (| type(Z) | - 1) + (| type(X) | - 2) * (| type(Y) | - 1) + (| type(X) | - 3) * (| type(Y) | - 2) * (| type(Z) | - 1) else | type(X) | + -4 + (| type(X) | - 3) * (| type(Z) | - 2) + | type(X) | + (| type(X) | - 3) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 4) * (| type(Y) | - 3) * (| type(Z) | - 2), if V = a then 648 else 576)"
							: "( (if V = a then (| type(X) | - 1) * (| type(Y) | - 1) * (| type(Z) | - 2) else (| type(X) | - 2) * (| type(Y) | - 1) * (| type(Z) | - 2)), (if V = a then 648 else 576) )"),

			new CardinalityData(false,
					"| { (on X, Y, Z) tuple(X, Y, Z) |  X != a and X != V and X != Y and Y != Z and Y != b and Z != v } |",
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							  "(if V = a then (| type(X) | - 1) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Y) | - 2) + (| type(X) | - 2) * (| type(Y) | - 2) + (| type(X) | - 2) * (| type(Y) | - 3) * (| type(Z) | - 3) else if V = v then (| type(X) | - 2) * (| type(Z) | - 1) + (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 4) * (| type(Z) | - 3) else if V = b then (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Y) | - 3) * (| type(Z) | - 3) else (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 4) * (| type(Z) | - 4), if V = a then 592 else if V = v then 528 else if V = b then 519 else 527)"
							//"(if V = a then (| type(X) | - 1) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Y) | - 2) + (| type(X) | - 2) * (| type(Y) | - 2) + (| type(X) | - 2) * (| type(Y) | - 3) * (| type(Z) | - 3) else if V = b then (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Y) | - 2) + (| type(X) | - 3) * (| type(Y) | - 3) * (| type(Z) | - 3) else if V = v then (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Z) | - 1) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 4) * (| type(Z) | - 3) else (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 3) + (| type(X) | - 3) * (| type(Y) | - 4) * (| type(Z) | - 4), if V = a then 592 else if V = b then 519 else if V = v then 528 else 527)"
							: "( anyof( (if V != b then if V = a or V = v then if V != c then | type(Z) | - 1 + (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Z) | - 1 + (| type(Y) | - 3) * (| type(Z) | - 2)) + (| type(Y) | - 2) * (| type(Z) | - 2) else | type(Z) | - 1 + (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * (| type(Z) | - 1 + (| type(Y) | - 3) * (| type(Z) | - 2)) else | type(Z) | - 1 + (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 4) * (| type(Z) | - 1 + (| type(Y) | - 3) * (| type(Z) | - 2)) + (| type(Y) | - 2) * (| type(Z) | - 2) else (if V != v then (| type(X) | - 3) * (| type(Z) | - 1 + (| type(Y) | - 3) * (| type(Z) | - 2)) + (| type(Y) | - 2) * (| type(Z) | - 2) else (| type(X) | - 3) * (| type(Z) | - 1 + (| type(Y) | - 3) * (| type(Z) | - 2)))), " +
					" (if V = v then (| type(Z) | + (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * ((| type(Z) | + (| type(Y) | - 3) * (| type(Z) | - 2)) - 1)) - 1 else (if V = b then (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * ((| type(Z) | + (| type(Y) | - 3) * (| type(Z) | - 2)) - 1) else (if V = a then ((| type(Y) | - 2) * (| type(Z) | - 2) + | type(Z) | + (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * ((| type(Z) | + (| type(Y) | - 3) * (| type(Z) | - 2)) - 1)) - 1 else ((| type(Y) | - 2) * (| type(Z) | - 2) + | type(Z) | + (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 4) * ((| type(Z) | + (| type(Y) | - 3) * (| type(Z) | - 2)) - 1)) - 1))), " +
					"(if V != b then if V = a or V = v then if V != v then (| type(Z) | + (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * ((| type(Z) | + (| type(Y) | - 3) * (| type(Z) | - 2)) - 1) + (| type(Y) | - 2) * (| type(Z) | - 2)) - 1 else (| type(Z) | + (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 3) * ((| type(Z) | + (| type(Y) | - 3) * (| type(Z) | - 2)) - 1)) - 1 else (| type(Z) | + (| type(Y) | - 2) * (| type(Z) | - 2) + (| type(X) | - 4) * ((| type(Z) | + (| type(Y) | - 3) * (| type(Z) | - 2)) - 1) + (| type(Y) | - 2) * (| type(Z) | - 2)) - 1 else (if V != v then (| type(X) | - 3) * ((| type(Z) | + (| type(Y) | - 3) * (| type(Z) | - 2)) - 1) + (| type(Y) | - 2) * (| type(Z) | - 2) else (| type(X) | - 3) * ((| type(Z) | + (| type(Y) | - 3) * (| type(Z) | - 2)) - 1))) " +
					" ), (if V = v then 528 else (if V = b then 519 else (if V = a then 592 else 527))))"),

			new CardinalityData(false,
					"| { (on X, Y) tuple(X, Y) |  X != a and ( X = Z <=> Y != b ) and Y != X } |",
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							"(if Z = b then | type(Y) | + -3 + | type(X) | else if Z = a then | type(X) | - 2 else | type(Y) | + -5 + | type(X) |, if Z = b then 17 else if Z = a then 8 else 15)"
							  //"(if Z = a then | type(X) | - 2 else if Z = b then | type(Y) | + -3 + | type(X) | else | type(Y) | + -5 + | type(X) |, if Z = a then 8 else if Z = b then 17 else 15)"
							: "( anyof( (if Z != a then if Z = b then | type(Y) | - 1 + | type(X) | - 2 else | type(Y) | - 2 + | type(X) | - 3 else | type(X) | - 2), " +
					"(if Z = a then | type(X) | - 2 else (if Z = b then (| type(Y) | + | type(X) |) - 3 else (| type(Y) | + | type(X) |) - 5)), " +
					"(if Z != a then if Z = b then (| type(Y) | + | type(X) |) - 3 else (| type(Y) | + | type(X) |) - 5 else | type(X) | - 2) ), (if Z = a then 8 else (if Z = b then 17 else 15)) )"),
			new CardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : X != Y}}|"
					"| {(on X) tuple(X) |there exists Y : X != Y } |",
					new CountsDeclaration("X", "11", "Y", "1"),
					// "if |type(Y)| > 1 then |People| else 0
					// Note: first result looks wrong but is ok as we are working with ASSUME_DOMAIN_ALWAYS_LARGE assumption
					GrinderUtil.usePlain?
							  "(|type(X)|," // uses GrinderConfiguration.isAssumeDomainsAlwaysLarge() to conclude that | type(Y) | > 0, as it should
							  + "0)"
					// Note: first result looks wrong but is ok as we are working with ASSUME_DOMAIN_ALWAYS_LARGE assumption
							: "( (if | type(Y) | > 0 then | type(X) | else 0), 0 )"),
			new CardinalityData(false,
					"| {(on X, Y, Z, V) tuple(X, Y, Z, V) | X != Y and W != X and Z != V and X'=Y'} |",
					new CountsDeclaration(10),
					GrinderUtil.usePlain?
							  "(if X' = Y' then (| type(X) | - 1) * | type(Z) | * (| type(V) | - 1) + (| type(X) | - 2) * (| type(Y) | - 1) * | type(Z) | * (| type(V) | - 1) else 0, if X' = Y' then 7290 else 0)"
							: "( anyof( (if X' = Y' then (| type(X) | - 1) * (| type(Y) | - 1) * | type(V) | * (| type(Z) | - 1) else 0), " +
					"(if X' = Y' then (| type(X) | - 1 + (| type(Y) | - 1) * (| type(X) | - 2)) * | type(V) | * (| type(Z) | - 1) else 0), " +
					"(if X' = Y' then (| type(X) | - 1) * (| type(Y) | - 1) * | type(Z) | * (| type(V) | - 1) else 0), " +
					"(if X' = Y' then (| type(Y) | - 1) * (| type(X) | - 1) * | type(V) | * (| type(Z) | - 1) else 0), " + 
					"(if X' = Y' then ((| type(X) | + (| type(Y) | - 1) * (| type(X) | - 2)) - 1) * | type(V) | * (| type(Z) | - 1) else 0), " +
					"(if X' = Y' then (| type(X) | - 1 + (| type(Y) | - 1) * ((| type(X) | - 1) - 1)) * | type(V) | * (| type(Z) | - 1) else 0), " +
					"(if X' = Y' then (| type(Y) | - 1) * | type(V) | * (| type(Z) | - 1) + (| type(X) | - 1) * (| type(Y) | - 2) * | type(V) | * (| type(Z) | - 1) else 0) ), (if X' = Y' then 7290 else 0) )"),
			//
			// Basic: Independent sub-problems tests
			new CardinalityData(false,
					"| {(on X, Y) tuple(X, Y) | true } |",
					new CountsDeclaration("X", "10", "Y", "10"),
					"( (| type(X) | * | type(Y) |), 100 )"),	
			new CardinalityData(false,
					"| {(on X, Y) tuple(X, Y) | Z = a } |",
					new CountsDeclaration("X", "10", "Y", "10"),
					"( (if Z = a then | type(X) | * | type(Y) | else 0), (if Z = a then 100 else 0) )"),
			new CardinalityData(false,
					"| {(on X, Y) tuple(X, Y) | X != a and Y != a and Z = a } |",
					new CountsDeclaration("X", "10", "Y", "10"),
					"( (if Z = a then (| type(X) | - 1) * (| type(Y) | - 1) else 0), (if Z = a then 81 else 0) )"),
		    //
			// Basic: Alternative forms of legal cardinality expressions
			new CardinalityData(false,
					"| {{(on X) tuple(X) | X != a }} |",
					new CountsDeclaration("X", "11"),
					// "|type(X)| - 1"
					"( (| type(X) | - 1), 10 )"),
			new CardinalityData(false,
					"| {{(on X) X | X != a }} |",
					new CountsDeclaration("X", "11"),
					// "|type(X)| - 1"
					"( (| type(X) | - 1), 10 )"),
			new CardinalityData(false,
					"| {{(on X) b | X != a }} |",
					new CountsDeclaration("X", "11"),
					// "|type(X)| - 1"
					"( (| type(X) | - 1), 10 )"),
			//
			// Illegal Argument Tests
			new CardinalityData(true,
					"| {(on X, Y) tuple(Z, Y) | X = a and Y = b} |",
					new CountsDeclaration(10),
					"N/A"),
			new CardinalityData(true,
					"| {(on X, Y) 1 | X = a and Y = b} |",
					new CountsDeclaration(10),
					"N/A"),
					
// Note: from CardinalityComputationTest.testCardinality(), which are not compatible with direct approach
//					new CardinalityData(true,
//									// "|{{(on X, Y, Z) 1 | g(X) != Y}}|",
//									"| {{(on X, Y, Z) 1 | g(X) != Y}} |",
//									new CountsDeclaration(10),
//									// "| type(X) | * (| type(Y) | - 1) * | type(Z) |"
//									"N/A"),	
		};
		
		perform(tests);
	}
	
	@Test
	public void testCardinalityKnownSizesRequired() {
		class KnownCardinalityData extends TestData {
			private String E;
			private Expression exprE;
			private CountsDeclaration countsDeclaration = null;
			
			public KnownCardinalityData(boolean isIllegalArgumentTest, String E, CountsDeclaration countsDeclaration, String expected) {
				super(isIllegalArgumentTest, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			}
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				countsDeclaration.setup(process);
				
				Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process) 
											.rewrite(CardinalityRewriter.R_card, exprE);
			
				return result;
			}
		}
		
		TestData[] tests = new TestData[] {
			new KnownCardinalityData(false,
					"| {(on X1, Y1, X2, Y2, X3, Y3) tuple(X1, Y1, X2, Y2, X3, Y3) | (X1 = a1 and Y1 = b1) or (X2 = a2 and Y2 = b2) or (X3 = a3 and Y3 = b3) } |",
					new CountsDeclaration(10),
					"29701"),
			new KnownCardinalityData(false,
					"| {(on X) tuple(X) |  Z = a or (Y != a and X != a) } |",
					new CountsDeclaration("X", "10", "Y", "3"),
					//"if Z = a and Y != a then 10 else if Z = a then if Y != a then 19 else 10 else if Y != a then 9 else 0"),
					"if Z = a then 10 else if Y = a then 0 else 9"),
			new KnownCardinalityData(false,
					"| {(on X) tuple(X) | there exists Y : X != Y or Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "3"),
					"10"),
			new KnownCardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y or Y != Z}}|",
					"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y or Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "3", "Z", "2"),
					// "if | type(Z) | > 1 and | type(Y) | > 0 or | type(Z) | > 0 and | type(Y) | > 1 then | People | else 0"
					"10"),
			new KnownCardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y or Y != Z}}|",
					"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y or Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "1", "Z", "2"),
					// "if | type(Z) | > 1 and | type(Y) | > 0 or | type(Z) | > 0 and | type(Y) | > 1 then | People | else 0"
					"10"),
			new KnownCardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y or Y != Z}}|",
					"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y or Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "3", "Z", "1"),
					// "if | type(Z) | > 1 and | type(Y) | > 0 or | type(Z) | > 0 and | type(Y) | > 1 then | People | else 0"
					"10"),
			new KnownCardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y or Y != Z}}|",
					"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y or Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "1", "Z", "1"),
					// "if | type(Z) | > 1 and | type(Y) | > 0 or | type(Z) | > 0 and | type(Y) | > 1 then | People | else 0"
					"0"),
			new KnownCardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y and Y != Z}}|"
					"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "3", "Z", "2"),
					// "if | type(Z) | > 1 and | type(Y) | > 1 then | People | else 0"
					"10"),
			new KnownCardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y and Y != Z}}|"
					"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "1", "Z", "2"),
					// "if | type(Z) | > 1 and | type(Y) | > 1 then | People | else 0"
					"0"),
			new KnownCardinalityData(false,
					// "|{{(on X in People) 1 | there exists Y : there exists Z: X != Y and Y != Z}}|"
					"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y != Z } |",
					new CountsDeclaration("X", "10", "Y", "3", "Z", "1"),
					// "if | type(Z) | > 1 and | type(Y) | > 1 then | People | else 0"
					"0"),
			//
			// Deep quantification tests
			new KnownCardinalityData(false,
					"| {(on X1, Y1, X2, Y2, Y3) tuple(X1, Y1, X2, Y2, Y3) | there exists X1 : there exists X2 : there exists Y1 : there exists Y2 : there exists Y3 : X1 = a1 or Y1 = b1 or X2 = a2 or Y2 = b2 or Y3 = b3} |",
					new CountsDeclaration(10),
					"100000"),
			new KnownCardinalityData(false,
					"| {(on X1, Y1, X2, Y2, Y3) tuple(X1, Y1, X2, Y2, Y3) | for all X1 : for all X2 : for all Y1 : for all Y2 : for all Y3 : X1 != a1 or Y1 != b1 or X2 != a2 or Y2 != b2 or Y3 != b3} |",
					new CountsDeclaration(10),
					"0"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testWorstCaseNumberOfDisjuncts() {
		String expressionString;
		int expected;

		expressionString = "X1 = a1 or X2 = a2 or X3 = a3 or X4 = a4 or X5 = a5";
		expected = 5;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "not(X1 = a1) or not(X2 = a2) or not(X3 = a3) or not(X4 = a4) or not(X5 = a5)";
		expected = 5;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "not (X1 = a1 or X2 = a2 or X3 = a3 or X4 = a4 or X5 = a5)";
		expected = 1;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "not (p1 or p2 or p3 or p4 or p5)";
		expected = 1;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "(p1 or p2 or p3 or p4 or p5) and (p1 or p2 or p3 or p4 or p5)";
		expected = 25;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "not((p1 or p2 or p3 or p4 or p5) and (p1 or p2 or p3 or p4 or p5))";
		expected = 2;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "not not((p1 or p2 or p3 or p4 or p5) and not not (p1 or p2 or p3 or p4 or p5))";
		expected = 25;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "(q1 and q2 and q3) or ((p1 or p2 or p3 or p4 or p5) and (p1 or p2 or p3 or p4 or p5))";
		expected = 26;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "not((q1 and q2 and q3) or ((p1 or p2 or p3 or p4 or p5) and (p1 or p2 or p3 or p4 or p5)))";
		expected = 6;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "there exists X : X1 = a1 or X2 = a2 or X3 = a3 or X4 = a4 or X5 = a5";
		expected = 5;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));

		expressionString = "for all X : X1 = a1 or X2 = a2 or X3 = a3 or X4 = a4 or X5 = a5";
		expected = 5;
		assertEquals(expected, WorstCaseNumberOfDisjuncts.get(parse(expressionString)));
	}
	
}
