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

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.test.grinder.TestData;

public class BasicTest extends AbstractGrinderTest {
	
	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		return new DefaultRewritingProcess(topExpression, new Basic());
	}

	@Test
	public void testBasic() {
		
		class BasicTestData extends TestData {
			private String expressionString; 
			private Expression expression;
			private Basic rBasic = new Basic();
			
			public BasicTestData(String expressionString, String expected) {
				super(false, expected);
				this.expressionString = expressionString;
			};
			
			@Override
			public Expression getTopExpression() {
				this.expression = parse(expressionString);
				return expression;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return rBasic.rewrite(expression, process);
			}
		};

		TestData[] tests = new TestData[] {
				// Replaces 0*E' by 0 
				new BasicTestData(
						"0*2",
						"0"),
				new BasicTestData(
						"0*p(a)",
						"0"),
				new BasicTestData(
						"2*0",
						"0"),
				new BasicTestData(
						"p(a)*0",
						"0"),
				// Replaces 1*E' by E' 
				new BasicTestData(
						"1*2",
						"2"),
				new BasicTestData(
						"1*p(a)",
						"p(a)"),
				new BasicTestData(
						"2*1",
						"2"),
				new BasicTestData(
						"p(a)*1",
						"p(a)"),
				// Replaces 0+E' by E' 
				new BasicTestData(
						"0+2",
						"2"),
				new BasicTestData(
						"0+p(a)",
						"p(a)"),
				new BasicTestData(
						"2+0",
						"2"),
				new BasicTestData(
						"p(a)+0",
						"p(a)"),
				// Replaces 0^E' by 0
				new BasicTestData(
							"0^2",
							"0"),
				new BasicTestData(
							"0^p(a)",
							"0^p(a)"),
			    // Replace 0^0 by 1
				// see: http://en.wikipedia.org/wiki/Exponentiation#Zero_to_the_zero_power
				// for discussion.
				new BasicTestData(
							"0^0",
							"1"),
				// Replaces E'^0 by 1 
				new BasicTestData(
							"2^0",
							"1"),
				new BasicTestData(
							"p(a)^0",
							"1"),
				// Replaces E'^1 by E'
				new BasicTestData(
							"2^1",
							"2"),
				new BasicTestData(
							"p(a)^1",
							"p(a)"),
				// Replaces E'/1 by E' 
			    new BasicTestData(
							"2/1",
							"2"),
				new BasicTestData(
							"p(a)/1",
							"p(a)"),
				// Replaces --E' by E' 
				// TODO - need fix for ALBP-69, currently parses --2 as - - 2
				// Note: Likely to do with ALBPGrammar.
				//new BasicTestData(
				//			"--2",
				//			"2"),
				// TODO - need fix for ALBP-68, currently returns --p(a)
				// Note: Likely to do with ALBPGrammar.
				//new BasicTestData(
				//			"--p(a)",
				//			"p(a)"),			
				// Replaces E'-0 by E' 
				new BasicTestData(
							"2-0",
							"2"),
				new BasicTestData(
							"p(a)-0",
							"p(a)"),
				// Replaces 0-E' by -E' 
				// TODO - need fix for ALBP-69, currently parses -2 as - 2
				// Note: Likely to do with ALBPGrammar.
				//new BasicTestData(
				//			"0-2",
				//			"-2"),
				// TODO - need fix for ALBP-69, currently parses - p(a)	
				// Note: Likely to do with ALBPGrammar.
				//new BasicTestData(
				//			"0-p(a)",
				//			"-p(a)"),
				// Replaces false and E' by false 
				new BasicTestData(
							"false and p(a)",
							"false"),
				// Replaces true and E' by E'
				new BasicTestData(
							"true and p(a)",
							"p(a)"),
				// Replaces false or E' by E' 
				new BasicTestData(
						"false or p(a)",
						"p(a)"),
				// Replaces true or E' by true 
				new BasicTestData(
						"true or p(a)",
						"true"),
				// Replaces not not E' by E' 
				new BasicTestData(
						"not not p(a)",
						"p(a)"),
				// Replaces not true by false
				new BasicTestData(
						"not true",
						"false"),
				// Replaces not false by true.
				new BasicTestData(
						"not false",
						"true"),
				// Replaces if true then E' else E'' by E'
				new BasicTestData(
						"if true then p(a) else p(b)",
						"p(a)"),
				// Replaces if false then E' else E'' by E''
				new BasicTestData(
						"if false then p(a) else p(b)",
						"p(b)"),
				// Replaces if C then E' else E' by E'
				new BasicTestData(
						"if p(b) then p(a) else p(a)",
						"p(a)"),
				new BasicTestData(
						"if X = Y then p(a) else p(a)",
						"p(a)"),
				// Replaces function applications of numeric operations on 
				// actual numbers by its result
				new BasicTestData(
						"4+2",
						"6"),
				new BasicTestData(
						"4-2",
						"2"),
				new BasicTestData(
						"4*2",
						"8"),
				new BasicTestData(
						"4/2",
						"2"),
				new BasicTestData(
						"4^2",
						"16"),
				// Replaces function applications of boolean operations
				// on actual boolean values by its result
				// TODO - add tests
				// Externalizes Conditionals
				// TODO - add tests
				// JIRA ALBP-31
				new BasicTestData(
						"{ B, (if A = B then C else D) }",
						"if A = B then { B, C } else { B, D }"),
				new BasicTestData(
						"{ if A = B then C else D }",
						"if A = B then { C } else { D }"),
				// JIRA ALBP-51
				new BasicTestData(
						"if A = B then if B = A then 1 else 2 else 3",
						"if A = B then if B = A then 1 else 2 else 3"),
				// JIRA ALBP-51
				new BasicTestData(
						"if B = A then {C} else if A = B then {D} else {E}",
						"if B = A then {C} else if A = B then {D} else {E}"),
				// TODO - fix JIRA ALBP-53
				// new BasicTestData("if A = C then {Z1} else if B = C then if A = B then {Z2} else {Z3} else {Z4}", "if A = C then {Z1} else if B = C then {Z3} else {Z4}"),
				// TODO - test injective functions.
				// TODO - test exclusive ranges.
		};
		
		perform(tests);
	}
}
