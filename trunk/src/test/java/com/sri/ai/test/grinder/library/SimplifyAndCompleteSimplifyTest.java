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
package com.sri.ai.test.grinder.library;

import org.junit.Test;

import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.CardinalityTypeOfLogicalVariable;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.test.grinder.TestData;

public class SimplifyAndCompleteSimplifyTest extends AbstractGrinderTest {
	
	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}

	@Test
	public void testSimplifyPassesBasicTests() {

		TestData[] tests = new TestData[] {
				//
				// Replaces 0*E' by 0 
				new SimplifyTestData(
						"0*2",
						"0"),
				new SimplifyTestData(
						"0*p(a)",
						"0"),
				new SimplifyTestData(
						"2*0",
						"0"),
				new SimplifyTestData(
						"p(a)*0",
						"0"),
				//
				// Replaces 1*E' by E' 
				new SimplifyTestData(
						"1*2",
						"2"),
				new SimplifyTestData(
						"1*p(a)",
						"p(a)"),
				new SimplifyTestData(
						"2*1",
						"2"),
				new SimplifyTestData(
						"p(a)*1",
						"p(a)"),
				//
				// Replaces 0+E' by E' 
				new SimplifyTestData(
						"0+2",
						"2"),
				new SimplifyTestData(
						"0+p(a)",
						"p(a)"),
				new SimplifyTestData(
						"2+0",
						"2"),
				new SimplifyTestData(
						"p(a)+0",
						"p(a)"),
				//
				// Replaces 0^E' by 0
				new SimplifyTestData(
							"0^2",
							"0"),
				new SimplifyTestData(
							"0^p(a)",
							"0^p(a)"),
			    //
				// Replace 0^0 by 1
				// see: http://en.wikipedia.org/wiki/Exponentiation#Zero_to_the_zero_power
				// for discussion.
				new SimplifyTestData(
							"0^0",
							"1"),
				//
				// Replaces E'^0 by 1 
				new SimplifyTestData(
							"2^0",
							"1"),
				new SimplifyTestData(
							"p(a)^0",
							"1"),
				//
				// Replaces E'^1 by E'
				new SimplifyTestData(
							"2^1",
							"2"),
				new SimplifyTestData(
							"p(a)^1",
							"p(a)"),
				//
				// Replaces E'/1 by E' 
			    new SimplifyTestData(
							"2/1",
							"2"),
				new SimplifyTestData(
							"p(a)/1",
							"p(a)"),
				//
				// Replaces --E' by E' 
				// Test for resolved issue: JIRA ALBP-69
				new SimplifyTestData(
							"--2",
							"2"),
// TODO - need fix for ALBP-68, currently returns --p(a)
//				new SimplifyTestData(
//							"--p(a)",
//							"p(a)"),	
				//
				// Replaces E'-0 by E' 
				new SimplifyTestData(
							"2-0",
							"2"),
				new SimplifyTestData(
							"p(a)-0",
							"p(a)"),
				//
				// Replaces 0-E' by -E' 
                // Note: I use (-2) here instead of 2
				// as the result is the numeric valued
				// symbol -2. However, when I parse
				// "-2" directly I get the function appliction
				// -(2), which is correct but does not compare
				// correctly, so using (-2) gets around this.
				// Test for resolved issue: JIRA ALBP-69
				new SimplifyTestData(
							"0-(-2)",
							"2"),
				// Test for resolved issue: JIRA ALBP-69
				new SimplifyTestData(
							"0-p(a)",
							"-p(a)"),
				//
				// Replaces false and E' by false 
				new SimplifyTestData(
							"false and p(a)",
							"false"),
				//
				// Replaces true and E' by E'
				new SimplifyTestData(
							"true and p(a)",
							"p(a)"),
				//
				// Replaces false or E' by E' 
				new SimplifyTestData(
						"false or p(a)",
						"p(a)"),
				//
				// Replaces true or E' by true 
				new SimplifyTestData(
						"true or p(a)",
						"true"),
				//
				// Replaces not not E' by E' 
				new SimplifyTestData(
						"not not p(a)",
						"p(a)"),
				//
				// Replaces not true by false
				new SimplifyTestData(
						"not true",
						"false"),
				//
				// Replaces not false by true.
				new SimplifyTestData(
						"not false",
						"true"),
				//
				// Replaces if true then E' else E'' by E'
				new SimplifyTestData(
						"if true then p(a) else p(b)",
						"p(a)"),
				//
				// Replaces if false then E' else E'' by E''
				new SimplifyTestData(
						"if false then p(a) else p(b)",
						"p(b)"),
				//
				// Replaces if C then E' else E' by E'
				new SimplifyTestData(
						"if p(b) then p(a) else p(a)",
						"p(a)"),
				new SimplifyTestData(
						"if X = Y then p(a) else p(a)",
						"p(a)"),
				//
				// Replaces function applications of numeric operations on 
				// actual numbers by its result
				new SimplifyTestData(
						"4+2",
						"6"),
				new SimplifyTestData(
						"4-2",
						"2"),
				new SimplifyTestData(
						"4*2",
						"8"),
				new SimplifyTestData(
						"4/2",
						"2"),
				new SimplifyTestData(
						"4^2",
						"16"),
				//
				// Replaces function applications of boolean operations
				// on actual boolean values by its result
				// TODO - add tests
				//
				// Externalizes Conditionals
				// TODO - add tests
				// Tests for resolved issue: JIRA ALBP-31
				new SimplifyTestData(
						"{ B, (if A = B then C else D) }",
						"if A = B then { B, C } else { B, D }"),
				new SimplifyTestData(
						"{ if A = B then C else D }",
						"if A = B then { C } else { D }"),
				// Tests for resolved issue: JIRA ALBP-51
				new SimplifyTestData(
						"if A = B then if B = A then 1 else 2 else 3",
						"if A = B then 1 else 3"),
				// Tests for resolved issue: JIRA ALBP-51
				new SimplifyTestData(
						"if B = A then {C} else if A = B then {D} else {E}",
						"if B = A then {C} else {E}"),
				// TODO - test injective functions.
				// TODO - test exclusive ranges.
		};
		
		perform(tests);
	}
	
	@Test
	public void testSimplifyAsDescribedInPapers() {
		//
		// Tests based on how simplification should work, as described in:
		// /aic-smf/doc/papers/CP 2012/CP 2012 submitted version with revisions
		// /aic-smf/doc/papers/CP 2012/StaRAI-12
		TestData[] tests = new TestData[] {
			//
			// Basic: 1. if-then-elses are externalized.
			new SimplifyTestData(
				"and(A = a, (if B = b then C = c else C = d), E = e)",
				"A = a and (B = b and C = c or B != b and C = d) and E = e"),
				//"B = b and A = a and C = c and E = e or B != b and A = a and C = d and E = e"),
				// Note: before FromConditionalFormulaToFormula, used to be
				// if B = b then and(A = a, C = c, E = e) else and(A = a, C = d, E = e)
			//
			// Basic: 2. Operations on constants are performed.
			new SimplifyTestData(
				"2 + 2",
				"4"),
			new SimplifyTestData(
				"not(0 = 1)",
				"true"),
			new SimplifyTestData(
				"0 = 1",
				"false"),
			new SimplifyTestData(
				"a = a",
				"true"),
			new SimplifyTestData(
				"a = b",
				"false"),
			new SimplifyTestData(
				"not(false)",
				"true"),
			new SimplifyTestData(
				"not(true)",
				"false"),
			new SimplifyTestData(
				"and(false, true)",
				"false"),
			new SimplifyTestData(
				"or(false, true)",
				"true"),
			new SimplifyTestData(
				"if true then 1 else 2",
				"1"),
			new SimplifyTestData(
				"if false then 1 else 2",
				"2"),
			//
			// Basic: 3. Operations whose results can be defined
			// by a subset of their arguments equal to certain constants.
			new SimplifyTestData(
				"and(false, X = Y)",
				"false"),
			new SimplifyTestData(
				"0 + 5",
				"5"),
			new SimplifyTestData(
				"5 - 0",
				"5"),
			new SimplifyTestData(
				"0 * 5",
				"0"),
			new SimplifyTestData(
				"0 * | X != a |",
				"0"),
			new SimplifyTestData(
				"1 * 5",
				"5"),
			new SimplifyTestData(
				"if true then | X = a | else 2",
				"| X = a |"),
			new SimplifyTestData(
				"if X != a then true else true",
				"true"),
			new SimplifyTestData(
				"if X != a then false else false",
				"false"),
			//
			// Basic: 4 equalities and disequalities on formulas
			new SimplifyTestData(
				"X = X",
				"true"),
			new SimplifyTestData(
				"X != X",
				"false"),
			// Note: as we support normalization (i.e. an ordering)
		    // on equalities and disequalities the following
			// will simplify.
			new SimplifyTestData(
				"(X != x) = (x != X)",
				"true"),
			new SimplifyTestData(
				"(X != x) != (x != X)",
				"false"),
			//
			// Basic: 5 conjuncts with equality on different constants
			new SimplifyTestData(
				"and(X = a, X = b)",
				"false"),
				new SimplifyTestData(
				"and(X = a, b = X)",
				"false"),
			//
			// Basic: 6 conjuncts with equality and inequality on the same term
			new SimplifyTestData(
				"and(X = a, X != a)",
				"false"),
			new SimplifyTestData(
				"and(X = a, a != X)",
				"false"),
			new SimplifyTestData(
				"and(X = Y, X != Y)",
				"false"),
			new SimplifyTestData(
				"and(X = Y, Y != X)",
				"false"),
			//
			// Basic: 7 transitive equalities results in a contradiction 
			// Note: requires complete simplification.
				
			// Note: replacement of transitive equalities in conjuncts, i.e:
			//
			// and(X != a, X = Y, Y != b) -> and(X != a, X = Y, X != b)
			//
			// has been decided not to be needed as R_implied_certainty
			// will pick out if its a contradiction anyway. So the
			// following case will not simplify.
			new SimplifyTestData(
				"and(X != a, X = Y, Y != b)",
				"and(X != a, X = Y, Y != b)"),			
			//
			// Basic: 8. False if not satisfiable
			new SimplifyTestData(
					"true => false",
					"false"),
			new SimplifyTestData(
					"X = X => X != X",
					"false"),
			//
			// Basic: 9. True if not falsifiable
			new SimplifyTestData(
					"true => true",
					"true"),
			new SimplifyTestData(
					"X = X => X = X",
					"true"),
			//
			// Basic: 10. if-then-else, true false sub-formula replacement
			new SimplifyTestData(
					"if X = a then if X = b then 1 else 2 else 3",
					"if X = a then 2 else 3"),
			new SimplifyTestData(
					"if X = a then if X != b then 1 else 2 else 3",
					"if X = a then 1 else 3"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testSimplifyNonFormulaConditionalTests() {
		TestData[] tests = new TestData[] {
				//
				// Basic: 
				new SimplifyTestData(
					"+((if not query then 1 else 0) * 2, (if query then 1 else 0) * 3)",
					"if not query then if query then 5 else 2 else (if query then 3 else 0)"),
		};
		
		perform(tests);
	}
	
	@Test
	public void testCompleteSimplifyRequired() {
		TestData[] tests = new TestData[] {
			// Tests for resolved issue: JIRA ALBP-53
			new CompleteSimplifyTestData(
				"if A = C then {Z1} else if B = C then if A = B then {Z2} else {Z3} else {Z4}", 
				"if A = C then {Z1} else if B = C then {Z3} else {Z4}"),
			//
			// Basic: 7 transitive equalities results in a contradiction 
			// Note: requires complete simplification.
			new CompleteSimplifyTestData(
					"and(X = a, X = Y, Y != a)",
					"false"),
		};
		
		perform(tests);
	}
	
	//
	// PRIVATE METHODS
	//
	class SimplifyTestData extends TestData implements CardinalityTypeOfLogicalVariable.DomainSizeOfLogicalVariable {
		private String expressionString; 
		private Expression expression;
		
		public SimplifyTestData(String expressionString, String expected) {
			super(false, expected);
			this.expressionString = expressionString;
		};
		
		//
		// START-DomainSizeOfLogicalVariable
		@Override
		public Integer size(Expression logicalVariable, RewritingProcess process) {
			return 100; // Default to this
		}
		// END-DomainSizeOfLogicalVariable
		//
		
		@Override
		public Expression getTopExpression() {
			this.expression = parse(expressionString);
			return expression;
		}
		
		@Override
		public Expression callRewrite(RewritingProcess process) {
			
			// Ensure explicit counts added for all variable domains.
			CardinalityTypeOfLogicalVariable.registerDomainSizeOfLogicalVariableWithProcess(this, process);
				
			Expression result = DirectCardinalityComputationFactory.newCardinalityProcess(expression, process).rewrite(getSimplifyName(), expression);
			
			return result;
		}
		
		//
		// PROTECTED
		//
		protected String getSimplifyName() {
			return CardinalityRewriter.R_simplify;
		}
	};
	
	class CompleteSimplifyTestData extends SimplifyTestData {
		
		public CompleteSimplifyTestData(String expressionString, String expected) {
			super(expressionString, expected);
		};
		
		@Override
		protected String getSimplifyName() {
			return CardinalityRewriter.R_complete_simplify;
		}
	};
}
