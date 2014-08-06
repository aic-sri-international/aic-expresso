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

import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Cardinality;
import com.sri.ai.util.Util;

/**
 * A collection of cardinality stress tests.
 * @author oreilly
 *
 */
@Beta
public class DefaultCardinalityStressTest extends AbstractCardinalityRewritersStressTest {

	@Override
	public List<? extends Rewriter> makeRewriters() {
		List<? extends Rewriter> result = Util.list(new Cardinality());
		return result;
	}

	@Override
	public List<StressTestData> makeStressTestDataObjects() {
		List<StressTestData> result = new LinkedList<StressTestData>();

		// Note: An example of using expected on a GivenFormulaStressTest
		result.add(new GivenCardinalityStressTestData("Expected", new String[][] {
				// Cardinality Expression, Expected Result
				{"|{{(on X, Y) tuple(X, Y) | not (X=Y=Z) }}|",            "99"},
				{"|{{(on X) tuple(X) | Z = a or (Y != a and X != a) }}|", "if Z = a then 10 else (if Y != a then 9 else 0)"}
		}));

		// Note: An example of using expected on a Parametric Stress Test
		result.add(new ParametricCardinalityStressTestData(
				"DNF",
				"Equality",
				"or",
				"=",
				"and", 2, 4, 2, 0, new String[] {
						// Expected Result
						"199", 
						"29701",
				"3940399"}));

		result.add(new ParametricCardinalityStressTestData("DNF", "Equality",   "or",  "=",  "and", 2, 10, 2, 2));
		// Note: Concurrent Cardinality appears to choke for values > 5, Direct Cardinality runs till 15+
		result.add(new ParametricCardinalityStressTestData("DNF", "Inequality", "or",  "!=", "and", 2, 10, 2, 0));
		// Note: Concurrent Cardinality appears to choke for values > 9, Direct Cardinality runs till 15+
		result.add(new ParametricCardinalityStressTestData("CNF", "Equality",   "and", "=",  "or",  2, 10, 2, 0));
		// Note: Concurrent Cardinality appears to choke for values > 5, Direct Cardinality runs till 15+
		result.add(new ParametricCardinalityStressTestData("CNF", "Inequality", "and", "!=", "or",  2, 10, 2, 0));
		// Note: from Shahin's email 'Concurrent & Direct tested side by side
		result.add(new GivenCardinalityStressTestData("Side by Side", new String[] {
				"|{{(on X, Y) tuple(X, Y) | not (X=Y=Z) }}|",
				"|{{(on X) tuple(X) | Z = a or (Y != a and X != a) }}|",
				"|{{(on X) tuple(X) | for all Y : Y != a => X=a}}|",
				"|{{(on X, Y, Z) 5 | X!=Y or X!=Z or Y!=Z}}|",
				"|{{(on X) 1 | X != X}}|",
				"|{{(on X, Y) 5 | X=Y or X=Z}}|",
				"|{{(on X1, Y1, X2, Y2, X3, Y3) tuple(X1, Y1, X2, Y2, X3, Y3) | (X1 = a1 and Y1 = b1) or (X2 = a2 and Y2 = b2) or (X3 = a3 and Y3 = b3) }}|",
				"|{{(on X) 1 | X = X}}|",
				"|{{(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y != Z }}|",
				"|{{(on X, Y) 5 | X!=Y or X=Z}}|",
				"|{{(on X) 5 | X=Y or X=Z}}|",
				"|{{(on X, Y) 5 | X=Y or X=Z}}|",
				"|{{(on X, Y) 5 | X!=Y or X!=Z}}|",
				"|{{(on X, Y, W) 5 | W=a or X!=Y or X!=Z}}|",
				"|{{(on X, Y) 5 | Y=Z or X!=Y or X!=Z}}|",
				"|{{(on X, Y) 1 | X != a => Y != b}}|",
				"|{{(on Y, X) 1 | X != a and Y != Z}}|",
				"|{{(on Y, X, Z) 1 | X != a and Y != Z}}|",
				"|{{(on X, Y) 1 | X != a <=> Y != b}}|",
				"|{{(on X, Y) 1 | X != a}}|",
				"|{{(on X) 1 | X = Y}}|",
				"|{{(on X, Y) 5 | X=Y=Z}}|",
				"|{{(on X) X | X != a}}|",
				"|{{(on Y) 5 | X!=Y and Y!=Z and Y!=A}}|",
				"|{{(on X, Y) 5 | not (X=Y=Z)}}|"
		}));
		// Note: from DirectCardinalityTest.testCardinality()
		result.add(new GivenCardinalityStressTestData("DirectCardinalityTest.testCardinality()", new String[] {
				"| {(on X) tuple(X) | X = a } |",
				"| {(on X) tuple(X) | X != a } |",
				"| {(on X, Y) tuple(X, Y) | X = a and Y = b } |",
				"| {(on X, Y) tuple(X, Y) | X = a and Y != b } |",
				"| {(on X, Y) tuple(X, Y) | X != a and Y != b } |",
				"| {(on X, Y) tuple(X, Y) | Y = b } |",
				"| {(on X, Y) tuple(X, Y) | Y != b } |",
				"| {(on X, Y) tuple(X, Y) | true } |",
				"| {(on X, Y) tuple(X, Y) | false } |",
				"| {(on X) tuple(X) | X != a and X != Z } |",
				"| {(on X, Y) tuple(X, Y) | Z = c and Y != b } |",
				"| {(on ) tuple() | Z = c } |",
				"| {(on ) tuple() | X = a } |",
				"| {(on Y) tuple(Y) | there exists X : X = Y and Z = a } |",
				"| {(on Z) tuple(Z) | false } |",
				"| {(on Z) tuple(Z) | true } |",
				"| {(on Z) tuple(Z) | Z = a } |",
				"| {(on Z) tuple(Z) | Z != a } |",
				"| {(on Z) tuple(Z) | Z != X } |",
				"| {(on Z) tuple(Z) | X != a and X != Y and Z != Y and Z != b } |",
				"| {(on Z) tuple(Z) | Z != a and X != Z } |",
				"| {(on Z) tuple(Z) | Z = a or Z = b } |",
				"| {(on Z) tuple(Z) | Z = a <=> W = b } |",
				"| {(on VarInst18, VarInst2) tuple(VarInst18, VarInst2) | VarInst18 = p6 and VarInst2 = c1 or VarInst18 = p4 and VarInst2 = c1 or VarInst18 = p2 and VarInst2 = c1 or VarInst18 = p8 and VarInst2 = c1 } |",
				"| {(on X, Y) tuple(X, Y) | X != a and X != Y } |",
				"| {(on Y, Z) tuple(Y, Z) | Y != a and Z != Y and Z != b } |",
				"| {(on X, Y, Z) tuple(X, Y, Z) | Y != X and Z != Y } |",
				"| {(on X, Y, Z) tuple(X, Y, Z) | X != a and X != Y and Z != Y and Z != b } |",
				"| {(on Z, W) tuple(Z, W) | Z = a } |",
				"| {(on Z, W) tuple(Z, W) | Z != a } |",
				"| {(on Z, W) tuple(Z, W) | Z != W } |",
				"| {(on Z, W) tuple(Z, W) |  Z != a and X != b } |",
				"| {(on X, Y) tuple(X, Y) |  X != a and Y != b } |",
				"| {(on Y) tuple(Y) |  X != Y and Y != Z and Y != A } |",
				"| {(on X') tuple(X') | ((X = mary and X != X') or (X = john and X != X')) and X' != bob and X' != mary and X' != john } |",
				"| {(on X, Y) tuple(X, Y) | not (X=Y=Z) } |",
				"| {(on X) tuple(X) |  for all Y: X = Y } |",
				"| {(on X) tuple(X) | there exists Y : there exists Z: X != Y and Y = Z } |",
				"| {(on X) tuple(X) | there exists Y : X != Y and Y != Z } |",
				"| {(on X, Y) tuple(X, Y) | X != a <=> Y != b} |",
				"| {(on X') tuple(X') | (X = X' = person1 or X = X' = person2 or X = X' = person3) and not (X = X') } |",
				"| {(on X, Y) tuple(X, Y) | true } |",
				"| {(on X) tuple(X) | true } |",
				"| {(on X) tuple(X) | not (X != bob and X != mary and X != john) and not (X = bob) } |",
				"| {(on X, Y) tuple(X, Y) | X != a } |",
				"| {(on X) tuple(X) | X = Y } |",
				"| {(on X) tuple(X) | X = X } |",
				"| {(on X) tuple(X) | X != X } |",
				"| {(on X) tuple(X) | X != X } |",
				"| {(on X, Y) tuple(X, Y) | X != Y or X != Z } |",
				"| {(on X, Y) tuple(X, Y) | X=Y=Z } |",
				"| {(on X, Y) tuple(X, Y) | X != a => Y != b } |",
				"| {(on X, Y, Z) tuple(X, Y, Z) | X != a and Y != Z } |",
				"| {(on X, Y) tuple(X, Y) | X != a and Y != Z } |",
				"| {(on X) tuple(X) | X != a } |",
				"| {(on X) tuple(X) |there exists Y : X != Y } |",
				"| {(on X) tuple(X) |there exists Y : X != Y } |",
				"| {(on X, Y) tuple(X, Y) | true } |",
				"| {(on X, Y) tuple(X, Y) | Z = a } |",
				"| {(on X, Y) tuple(X, Y) | X != a and Y != a and Z = a } |",
				"| {(on X1, Y1, X2, Y2, Y3) tuple(X1, Y1, X2, Y2, Y3) | X1 = a1 or Y1 = b1 or X2 = a2 or Y2 = b2 or Y3 = b3} |",
				"| {(on X1, Y1, X2, Y2, Y3) tuple(X1, Y1, X2, Y2, Y3) | X1 != a1 or Y1 != b1 or X2 != a2 or Y2 != b2 or Y3 != b3} |"
		}));
		// Note: from LBPTest R_card calls.
		result.add(new GivenCardinalityStressTestData("LBPTest R_card calls", new String[] {
				"| { ( on A ) tuple( A ) | A != B } |",
				"| { ( on A ) tuple( A ) | A != X } |",
				"| { ( on A ) tuple( A ) } |",
				"| { ( on A' ) tuple( A' ) | A' != A } |",
				"| { ( on A' ) tuple( A' ) | A' != B } |",
				"| { ( on A' ) tuple( A' ) } |",
				"| { ( on A'' ) tuple( A'' ) } |",
				"| { ( on B ) tuple( B ) | B != A } |",
				"| { ( on B ) tuple( B ) | B != X } |",
				"| { ( on B ) tuple( B ) } |",
				"| { ( on B' ) tuple( B' ) | B' != B } |",
				"| { ( on B' ) tuple( B' ) } |",
				"| { ( on B'' ) tuple( B'' ) } |",
				"| { ( on X ) tuple( X ) | (X != a3 and not (X = a1)) and X = a2 } |",
				"| { ( on X ) tuple( X ) | (X != a3 and not (X = a1)) and not (X = a2) } |",
				"| { ( on X ) tuple( X ) | (X != ann and not (X != bob and X != mary and X != john)) and X = bob } |",
				"| { ( on X ) tuple( X ) | (X != ann and not (X != bob and X != mary and X != john)) and not (X = bob) } |",
				"| { ( on X ) tuple( X ) | (X != mary and not (X != bob and X != john)) and X = bob } |",
				"| { ( on X ) tuple( X ) | (X != mary and not (X != bob and X != john)) and not (X = bob) } |",
				"| { ( on X ) tuple( X ) | X != a1 and X = a2 } |",
				"| { ( on X ) tuple( X ) | X != a1 and not (X = a2) } |",
				"| { ( on X ) tuple( X ) | X != a2 and X = a1 } |",
				"| { ( on X ) tuple( X ) | X != a2 and not (X = a1) } |",
				"| { ( on X ) tuple( X ) | X != a3 and X = a1 } |",
				"| { ( on X ) tuple( X ) | X != ann and (X != bob and X != mary and X != john) } |",
				"| { ( on X ) tuple( X ) | X != ann and X = bob } |",
				"| { ( on X ) tuple( X ) | X != ann and not (X = bob) } |",
				"| { ( on X ) tuple( X ) | X != ann } |",
				"| { ( on X ) tuple( X ) | X != bob and (X != mary and X != john) } |",
				"| { ( on X ) tuple( X ) | X != bob and X != ann } |",
				"| { ( on X ) tuple( X ) | X != bob and X != mary and X != john } |",
				"| { ( on X ) tuple( X ) | X != bob and not (X != mary and X != john) } |",
				"| { ( on X ) tuple( X ) | X != bob } |",
				"| { ( on X ) tuple( X ) | X != mary and (X != bob and X != john) } |",
				"| { ( on X ) tuple( X ) | X = a or X = b } |",
				"| { ( on X ) tuple( X ) | X = a } |",
				"| { ( on X ) tuple( X ) | X = a1 } |",
				"| { ( on X ) tuple( X ) | X = bob } |",
				"| { ( on X ) tuple( X ) | X = person1 or X = person2 or X = person3 } |",
				"| { ( on X ) tuple( X ) | not (X != bob and X != mary and X != john) and X = bob } |",
				"| { ( on X ) tuple( X ) | not (X != bob and X != mary and X != john) and not (X = bob) } |",
				"| { ( on X ) tuple( X ) | not (X = a) } |",
				"| { ( on X ) tuple( X ) | not (X = a1) and X = a2 } |",
				"| { ( on X ) tuple( X ) | not (X = a1) and not (X = a2) } |",
				"| { ( on X ) tuple( X ) | not (X = bob) } |",
				"| { ( on X ) tuple( X ) | not (X = person1 or X = person2 or X = person3) } |",
				"| { ( on X ) tuple( X ) } |",
				"| { ( on X' ) tuple( X' ) | (X' != X and not (X' != bob and X' != mary and X' != john)) and X' = bob } |",
				"| { ( on X' ) tuple( X' ) | (X' != X and not (X' != bob and X' != mary and X' != john)) and not (X' = bob) } |",
				"| { ( on X' ) tuple( X' ) | (X' != X and not (X' = a1)) and X' = a2 } |",
				"| { ( on X' ) tuple( X' ) | (X' != X and not (X' = a1)) and not (X' = a2) } |",
				"| { ( on X' ) tuple( X' ) | X' != X and (X' != bob and X' != mary and X' != john) } |",
				"| { ( on X' ) tuple( X' ) | X' != X and X' = a1 } |",
				"| { ( on X' ) tuple( X' ) | X' != X and X' = bob } |",
				"| { ( on X' ) tuple( X' ) | X' != X and not (X' = bob) } |",
				"| { ( on X' ) tuple( X' ) | X' != X } |",
				"| { ( on X' ) tuple( X' ) | X' != a1 and X' = a2 } |",
				"| { ( on X' ) tuple( X' ) | X' != a1 and not (X' = a2) } |",
				"| { ( on X' ) tuple( X' ) | X' != a2 and X' = a1 } |",
				"| { ( on X' ) tuple( X' ) | X' != a2 and not (X' = a1) } |",
				"| { ( on X' ) tuple( X' ) | X' != bob and (X' != mary and X' != john) } |",
				"| { ( on X' ) tuple( X' ) | X' != bob and X' != X } |",
				"| { ( on X' ) tuple( X' ) | X' != bob and not (X' != mary and X' != john) } |",
				"| { ( on X' ) tuple( X' ) | X' != bob } |",
				"| { ( on X' ) tuple( X' ) } |",
				"| { ( on X'' ) tuple( X'' ) | X'' != X' } |",
				"| { ( on Y ) tuple( Y ) } |",
				"| { ( on Y' ) tuple( Y' ) | Y' != Y } |",
				"| { ( on Y'' ) tuple( Y'' ) | Y'' != Y' } |",
				"| { ( on Z ) tuple( Z ) | Z != Y } |",
				"| { ( on Z ) tuple( Z ) } |",
				"| { ( on Z' ) tuple( Z' ) | Z' != Z } |"
		}));

		return result;
	}
}
