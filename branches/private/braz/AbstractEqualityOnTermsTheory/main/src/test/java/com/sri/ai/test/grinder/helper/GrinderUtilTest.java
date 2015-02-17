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
package com.sri.ai.test.grinder.helper;

import org.junit.Assert;
import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.test.grinder.TestData;

public class GrinderUtilTest extends AbstractGrinderTest {
	
	@Override
	public RewritingProcess makeRewritingProcess(Expression topExpression) {
		return new DefaultRewritingProcess(topExpression, new Basic());
	}

	// This is a key support routine.
	@Test
	public void testMakeSureConditionsOnLogicalVariablesAreSeparatedAndOnTop() {
		
		class LVTestData extends TestData {
			private String E; 
			private Expression exprE;
			
			public LVTestData(String E, String expected) {
				super(false, expected);
				this.E = E;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				return GrinderUtil.makeSureConditionsOnLogicalVariablesAreSeparatedAndOnTop(exprE, process);
			}
		};

		TestData[] tests = new TestData[] {
				//
				new LVTestData(// No change expected as the condition comprises only of a logical equality
						"if X = a then E1 else E2",
						"if X = a then E1 else E2"),	
				new LVTestData(// No change expected as the condition comprises only of a logical inequality
						"if X != a then E1 else E2",
						"if X != a then E1 else E2"),
				new LVTestData(// No change expected as the condition comprises only of two logical inequalities
						"if X != a and X != b then E1 else E2",
						"if X != a and X != b then E1 else E2"),
				new LVTestData(
						"if X = a and p(X) then E1 else E2",
						"if X = a then if p(X) then E1 else E2 else E2"),
				//
				new LVTestData(
						"if X = a and Y = b and q(X, Y) then E1 else E2",
						"if X = a and Y = b then if q(X, Y) then E1 else E2 else E2"),
				new LVTestData(
						"if X = a and r then E1 else E2",
						"if X = a then if r then E1 else E2 else E2"),
				new LVTestData(
						"if X = a and p(X) then if Y = b then E1 else E2 else if Y = c then E3 else E4",
						"if X = a then if p(X) then if Y = b then E1 else E2 else if Y = c then E3 else E4 else if Y = c then E3 else E4"),
				new LVTestData(
						"if X = a and Y = b and p(X) and q(X, Y) then E1 else E2",
						"if X = a and Y = b then if p(X) and q(X, Y) then E1 else E2 else E2"),
				new LVTestData(
						"if X = a and p(X) then if Y = b and q(X, Y) then E1 else E2 else if Y = c and q(X, Y) then E3 else E4",
						"if X = a then if p(X) then if Y = b then if q(X, Y) then E1 else E2 else E2 else if Y = c then if q(X, Y) then E3 else E4 else E4 else if Y = c then if q(X, Y) then E3 else E4 else E4"),
		};
		
		perform(tests);
	}
	
	// Another key support routine
	@Test
	public void testIsConditionalOnLogicalVariables() {
		class ICLVTestData extends TestData {
			private String E; 
			private Expression exprE;
			private boolean shouldBeTrue;
			
			public ICLVTestData(boolean shouldBeTrue, String E) {
				super(false, E);
				this.shouldBeTrue = shouldBeTrue;
				this.E = E;
			};
			
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				
				Assert.assertEquals(shouldBeTrue, GrinderUtil.isConditionalOnLogicalVariables(exprE, process));
				// No rewriting performed, just testing boolean condition in this test.
				return exprE;
			}
		};

		TestData[] tests = new TestData[] {
				//
				// Basic: true cases.
				new ICLVTestData(
						true,
						"if X = a then E1 else E2"),	
				new ICLVTestData(
						true,
						"if X != a then E1 else E2"),
				new ICLVTestData(
						true,
						"if X != a and X != b then E1 else E2"),
				new ICLVTestData(
						true,
						"if X = a and p(X) then E1 else E2"),
				new ICLVTestData(
						true,
						"if X = a and Y = b and q(X, Y) then E1 else E2"),
				new ICLVTestData(
						true,
						"if X = a and r then E1 else E2"),
				new ICLVTestData(
						true,
						"if X = a and p(X) then if Y = b then E1 else E2 else if Y = c then E3 else E4"),
				new ICLVTestData(
						true,
						"if X = a and Y = b and p(X) and q(X, Y) then E1 else E2"),
				new ICLVTestData(
						true,
						"if X = a and p(X) then if Y = b and q(X, Y) then E1 else E2 else if Y = c and q(X, Y) then E3 else E4"),		
				//
				// Basic: false cases.
				new ICLVTestData(
						false,	
						"if p(X) then E1 else E2"),
				new ICLVTestData(
						false,	
						"if q(X,Y) then E1 else E2"),
				new ICLVTestData(
						false,	
						"if r then E1 else E2"),
				new ICLVTestData(
						false,	
						"if p(X) and q(X,Y) then E1 else E2"),
				new ICLVTestData(
						false,	
						"if p(X) or q(X,Y) then E1 else E2"),
				new ICLVTestData(
						false,	
						"if p(X) and q(X,Y) and r then E1 else E2"),
		};
		
		perform(tests);
	}
}
