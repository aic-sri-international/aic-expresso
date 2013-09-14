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
package com.sri.ai.test.grinder.library.equality.cardinality;

import java.util.Map;

import org.junit.Test;

import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.DirectCardinalityComputationFactory;
import com.sri.ai.grinder.library.equality.cardinality.core.ConcurrentCardinality;
import com.sri.ai.test.grinder.AbstractGrinderTest;
import com.sri.ai.test.grinder.TestData;

public class ConcurrentCardinalityTest extends AbstractGrinderTest {

	@Override
	public Grammar makeGrammar() {
		return new CommonGrammar();
	}

	
	@Test
	public void testCardinality() {
		class CardinalityData extends TestData {
			private String E; 
			private Expression exprE;
			private Map<Object, Object> globalObjects;
			private CountsDeclaration countsDeclaration = null;

			public CardinalityData(String E, String expected, CountsDeclaration countsDeclaration) {
				super(false, expected);
				this.E = E;
				this.countsDeclaration = countsDeclaration;
				this.countsDeclaration.setParser(parser);
			};
	
			public CardinalityData(String E, Map<Object, Object> globalObjects, String expected) {
				super(false, expected);
				this.E = E;
				this.globalObjects = globalObjects;
			};
	
			@Override
			public Expression getTopExpression() {
				this.exprE = parse(E);
				
				return exprE;
			}
			
			@Override
			public Expression callRewrite(RewritingProcess process) {
				if (globalObjects != null) {
					process.getGlobalObjects().putAll(globalObjects);
				}
				countsDeclaration.setup(process);
				Expression result = ConcurrentCardinality.newCardinality().rewrite(exprE, DirectCardinalityComputationFactory.newCardinalityProcess(exprE, process));
				return result;
			}			
		}
		
		int n = 10;
		
		TestData[] tests = new TestData[] {
				new CardinalityData(
					"|{{(on X) tuple(X) | for all Y : Y != a => X=a}}|",
					"1",
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y, Z) 5 | X!=Y or X!=Z or Y!=Z}}|",
					str(n*n*n - n),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X) 1 | X != X}}|",
					str(0),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y) 5 | X=Y or X=Z}}|",
					str(2*n - 1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X1, Y1, X2, Y2, X3, Y3) tuple(X1, Y1, X2, Y2, X3, Y3) | (X1 = a1 and Y1 = b1) or (X2 = a2 and Y2 = b2) or (X3 = a3 and Y3 = b3) }}|",
					"29701",
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X) 1 | X = X}}|",
					str(n),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y != Z }}|",
					"10",
					new CountsDeclaration("X", "10", "Y", "3", "Z", "2")),
				new CardinalityData(
					"|{{(on X1, Y1, X2, Y2, X3, Y3) tuple(X1, Y1, X2, Y2, X3, Y3) | (X1 = a1 and Y1 = b1) or (X2 = a2 and Y2 = b2) or (X3 = a3 and Y3 = b3) }}|",
					"43561",
					new CountsDeclaration(n+1)),
				new CardinalityData(
                    "| {{ ( on X' in People ) 1 | (X = X' = person1 or X = X' = person2 or X = X' = person3) and not (X = X') }} |",
                    str(0),
                    new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X) 5 | X=Y <=> X=Z}}|",
					"if Y = Z then 10 else 8",
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{a, b, c, X}|",
					"if X = a or X = b or X = c then 3 else 4",
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{X, Y, X, Z, W, V, U, b, a, c, d, f}|",
					"if U = V or U = W or U = Z or U = Y or U = X or U = b or U = a or U = c or U = d or U = f then if X = Y or X = Z or X = W or X = V or X = b or X = a or X = c or X = d or X = f then if V = W or V = Z or V = Y or V = b or V = a or V = c or V = d or V = f then if Y = Z or Y = W or Y = b or Y = a or Y = c or Y = d or Y = f then if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 5 else 6 else if Z = b or Z = a or Z = c or Z = d or Z = f then 6 else 7 else if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 6 else 7 else if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if Y = Z or Y = W or Y = b or Y = a or Y = c or Y = d or Y = f then if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 6 else 7 else if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if V = W or V = Z or V = Y or V = b or V = a or V = c or V = d or V = f then if Y = Z or Y = W or Y = b or Y = a or Y = c or Y = d or Y = f then if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 6 else 7 else if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if Y = Z or Y = W or Y = b or Y = a or Y = c or Y = d or Y = f then if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if Z = b or Z = a or Z = c or Z = d or Z = f then 9 else 10 else if X = Y or X = Z or X = W or X = V or X = b or X = a or X = c or X = d or X = f then if V = W or V = Z or V = Y or V = b or V = a or V = c or V = d or V = f then if Y = Z or Y = W or Y = b or Y = a or Y = c or Y = d or Y = f then if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 6 else 7 else if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if Y = Z or Y = W or Y = b or Y = a or Y = c or Y = d or Y = f then if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if Z = b or Z = a or Z = c or Z = d or Z = f then 9 else 10 else if V = W or V = Z or V = Y or V = b or V = a or V = c or V = d or V = f then if Y = Z or Y = W or Y = b or Y = a or Y = c or Y = d or Y = f then if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 7 else 8 else if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if Z = b or Z = a or Z = c or Z = d or Z = f then 9 else 10 else if Y = Z or Y = W or Y = b or Y = a or Y = c or Y = d or Y = f then if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 8 else 9 else if Z = b or Z = a or Z = c or Z = d or Z = f then 9 else 10 else if W = Z or W = b or W = a or W = c or W = d or W = f then if Z = b or Z = a or Z = c or Z = d or Z = f then 9 else 10 else if Z = b or Z = a or Z = c or Z = d or Z = f then 10 else 11",
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y) 5 | X!=Y or X=Z}}|",
					str(n*n - n + 1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X) 5 | X=Y or X=Z}}|",
					"if Y = Z then 1 else 2",
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y) 5 | X=Y or X=Z}}|",
					str(2*n - 1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y) 5 | X!=Y or X!=Z}}|",
					str(n*n - 1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y, W) 5 | W=a or X!=Y or X!=Z}}|",
					str(n*n*n - n + 1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y) 5 | Y=Z or X!=Y or X!=Z}}|",
					str(n*n),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y) 1 | X != a => Y != b}}|",
					str(n*n - n + 1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on Y, X) 1 | X != a and Y != Z}}|",
					str((n-1) * (n-1)),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on Y, X, Z) 1 | X != a and Y != Z}}|",
					str(n*n*n - 2*n*n + n),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y) 1 | X != a <=> Y != b}}|",
					str((n-1) * (n-1) + 1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"| {{ ( on X in Cars, Y in People ) 1 }} |",
					str(n*n),
					new CountsDeclaration(n)),
				new CardinalityData(
					"| {{ ( on X in People ) 1 }} |",
					str(n),
					new CountsDeclaration(n)),
				new CardinalityData(
					"| {{ ( on X in People ) 1 | not (X != bob and X != mary and X != john) and not (X = bob) }} |",
					str(2),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y) 1 | X != a}}|",
					str(n*n - n),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X) 1 | X = Y}}|",
				    str(1),
				    new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y, Z) 1 | g(X) != Y}}|",
					str(n*n*n - n*n),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X, Y) 5 | X=Y=Z}}|",
					str(1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on X) X | X != a}}|",
					str(n-1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{(on Y) 5 | X!=Y and Y!=Z and Y!=A}}|",
					//"if Z = A then if X = Z or X = A then 9 else 8 else (if X = Z or X = A then 8 else 7)",
					"if X = Z or X = A then if Z = A then 9 else 8 else if Z = A then 8 else 7",
					new CountsDeclaration(n)),				
				new CardinalityData(
					"|{{(on X, Y) 5 | not (X=Y=Z)}}|",
					str(n*n - 1),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{X, Y, X, Z, b, a}|",
					"if Z = Y or Z = X or Z = b or Z = a then if X = Y or X = b or X = a then if Y = b or Y = a then 2 else 3 else if Y = b or Y = a then 3 else 4 else if X = Y or X = b or X = a then if Y = b or Y = a then 3 else 4 else if Y = b or Y = a then 4 else 5",
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{a, b, c, c, b, c, a, a, c}|",
					str(3),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{X, Y, X, a}|",
					"if Y = X or Y = a then if X = a then 1 else 2 else if X = a then 2 else 3",
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{}|",
					str(0),
					new CountsDeclaration(n)),
				new CardinalityData(
					"|{{a, b, c, a, c, c}}|",
					str(6),
					new CountsDeclaration(n)),
		};
		
		perform(tests);
	}
	
	private String str(int n) {
		return "" + n;
	}	
}

