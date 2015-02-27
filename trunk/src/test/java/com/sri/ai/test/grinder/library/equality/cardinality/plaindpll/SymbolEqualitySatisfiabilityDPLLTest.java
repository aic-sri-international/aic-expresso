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

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;

import java.util.Collection;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.SGDPLLT;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.EqualityTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.Satisfiability;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.SymbolTermTheory;

@Beta
public class SymbolEqualitySatisfiabilityDPLLTest extends AbstractSymbolicSymbolEqualityDPLLTest {
	
	@Test
	public void test() {
		
		Expression expression;
		Expression expected;
		Collection<String> indices;
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		// Repeated for debugging purposes
//		expression  = parse("(X != a) and (X = Y and Y != c)");
//		indices     = list();
//		expected    = parse("if X = a then false else if X = Y then if Y = c then false else true else false");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		
		// tests whether splitter negation, when applied, gets "translated" by inner splitters in then branches.
		// for example, if I apply X != a to if X = Y then if Y = a then true else false else false,
		// X != a needs to be translated to Y != a under X = Y, because in that then branch X does not exist,
		// and X != a is therefore meaningless.
		expression  = parse("(X != a) and (X = Y and Y != c)");
		indices     = list();
		expected    = parse("if X = a then false else if X = Y then if Y = c then false else true else false");
//		expected    = parse("if X = a then false else if Y = a then false else if X = Y then if Y = c then false else true else false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		// tests whether splitter negation, when applied, gets "translated" by inner splitters in then branches.
		// for example, if I apply X != a to if X = Y then if Y = a then true else false else false,
		// X != a needs to be translated to Y != a under X = Y, because in that then branch X does not exist,
		// and X != a is therefore meaningless.
		expression  = parse("(X != a) and (X = Y and Y != a)");
		indices     = list("Y");
		expected    = parse("if X = a then false else true");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		// tests the following:
		// at some point there will be an externalization of X = a over if X = Y then if W = Y then true else false else false
		// This tests whether the last Y gets replaced by a, even though the splitter X = a is in X.
		// This requires the algorithm to realize that X is represented by Y under X = Y.
		expression  = parse("(if X = a then true else false) and (if X = Y then if W = Y then true else false else false)");
		indices     = list();
		expected    = parse("if X = a then if Y = a then if W = a then true else false else false else false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		// tests whether splitter property that then branch does not contain first splitter variable holds
		// when X = Y is applied to solution under Y = a, algorithm needs to realize that X in W = X under Y = a needs to be replaced by a.
		// This happens because applying X = Y replaces all X by Y and then Y gets replaced by a.
		expression  = parse("(if X = Y then true else false) and (if Y = a then if W = X then true else false else false)");
		indices     = list();
		expected    = parse("if X = Y then if Y = a then if W = a then true else false else false else false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		
		
		expression = parse("true");
		indices    = null; // means all variables
		expected   = parse("true");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		expression = parse("false");
		indices    = null; // means all variables
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		// tests answer completeness
		expression  = parse("(Y = a and X = T) or (Y != a and X = T1 and T = T1)");
		indices     = list("Y");
		// original algorithm provided this incomplete solution due to incomplete condition-applying-on-solution algorithm used in externalization
		// expected = parse("if X = T then if T = T1 then if T = T1 then 10 else 1 else 1 else (if X = T1 then if T = T1 then 9 else 0 else 0)");
		expected    = parse("if X = T then true else false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		
		expression = parse("X != Y");
		indices    = list("X");
		expected   = parse("| Everything | - 1 > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != Y and X != a");
		indices    = list("X");
		expected   = parse("if Y = a then | Everything | - 1 > 0 else | Everything | - 2 > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != Y and X != Z and X != a");
		indices    = list("X");
		expected   = parse("if Y = Z then if Z = a then | Everything | - 1 > 0 else | Everything | - 2 > 0 else if Y = a then | Everything | - 2 > 0 else if Z = a then | Everything | - 2 > 0 else | Everything | - 3 > 0");
//		expected   = parse("if Z = Y then if Y = a then | Everything | - 1 > 0 else | Everything | - 2 > 0 else if Z = a then | Everything | - 2 > 0 else if Y = a then | Everything | - 2 > 0 else | Everything | - 3 > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("Y = a and X != Y and X != a");
		indices    = list("X");
		expected   = parse("if Y = a then | Everything | - 1 > 0 else false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		

		expression = parse("X1 != X2 and (X2 = X3 or X2 = X4) and X3 = X1 and X4 = X1");
		indices    = null; // means all variables
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X1 != X2 and X2 != X0 and X1 != X0");
		indices    = null; // means all variables
		expected   = parse("(| Everything | - 1) * | Everything | * (| Everything | - 2) > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("true");
		indices    = null; // means all variables
		expected   = parse("true");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("true");
		indices    = list("X", "Y");
		expected   = parse("| Everything |  * | Everything |  > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("false");
		indices    = null; // means all variables
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("false");
		indices    = list("X", "Y");
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		
		expression = parse("X = a");
		indices    = null; // means all variables
		expected   = parse("true");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a");
		indices    = null; // means all variables
		expected   = parse("| Everything | - 1 > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X = a");
		indices    = list("X", "Y");
		expected   = parse("| Everything | > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a");
		indices    = list("X", "Y");
		expected   = parse("(| Everything | - 1) * | Everything |  > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X = a and Y != b");
		indices    = list("X", "Y");
		expected   = parse("| Everything | - 1 > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a and Y != b");
		indices    = list("X", "Y");
		expected   = parse("(| Everything | - 1) * (| Everything | - 1) > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a or Y != b");
		indices    = list("X", "Y");
		expected   = parse("(| Everything | - 1 > 0) or ((| Everything | - 1) * | Everything | > 0)");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a and X != Y and Y != a");
		indices    = null;
		expected   = parse("(| Everything | - 2)*(| Everything | - 1) > 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
	}

	protected Expression makeProblem(Expression expression, IndexExpressionsSet indexExpressions) {
		Expression problem = new DefaultExistentiallyQuantifiedFormula(indexExpressions, expression);
		return problem;
	}

	protected Rewriter makeRewriter() {
		return new SGDPLLT(new EqualityTheory(new SymbolTermTheory()), new Satisfiability());
	}
}
