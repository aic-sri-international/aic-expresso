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
import com.sri.ai.expresso.core.DefaultUniversallyQuantifiedFormula;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.DPLLGeneralizedAndSymbolic;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.EqualityOnTermsTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.SymbolTermTheory;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.Tautologicality;

@Beta
public class SymbolEqualityTautologicalityDPLLTest extends AbstractSymbolicSymbolEqualityDPLLTest {
	
	@Test
	public void test() {
		
		Expression expression;
		Expression expected;
		Collection<String> indices;
		
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		expression = Expressions.parse("(X != Y or X = Y) and Y = Z");
		indices    = list("X");
		expected   = Expressions.parse("if Y = Z then true else false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X = Y <=> Y = X");
		indices    = null;
		expected   = parse("true");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		expression = parse("X = Y and Y = Z => X = Z");
		indices    = null;
		expected   = parse("true");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		expression = parse("X = V => for all Y : Y = X => Y = V");
		indices    = null;
		expected   = parse("true");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);

		expression = parse("X = V => for all Y : Y = X => Y = V and V = a");
		indices    = list("X"); // V is free
		expected   = parse("if V = a then true else false");
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
		expected    = parse("if X = T then if T = T1 then true else | Everything | - 1 = 0 else false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		
		expression = parse("X != Y");
		indices    = list("X");
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != Y and X != a");
		indices    = list("X");
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != Y and X != Z and X != a");
		indices    = list("X");
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("Y = a and X != Y and X != a");
		indices    = list("X");
		expected   = parse("if Y = a then false else | Everything | = 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		

//		// not sure the expected answer here is really correct, as it varied weirdly when making changes to algorithm that should not have changed it but did.
//		expression = parse("X1 != X2 and (X2 = X3 or X2 = X4) and X3 = X1 and X4 = X1");
//		indices    = null; // means all variables
//		expected   = parse("(| Everything | * | Everything | * | Everything | = 0) and ((| Everything | - 1) * | Everything | * | Everything | = 0) and ((| Everything | - 1) * (| Everything | - 1) * | Everything | = 0) and (| Everything | - 1 = 0) and ((| Everything | - 2) * | Everything | = 0) and ((| Everything | - 2) * (| Everything | - 1) * | Everything | = 0) and ((| Everything | - 1) * (| Everything | - 2) * (| Everything | - 1) * | Everything | = 0)");
//		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X1 != X2 and X2 != X0 and X1 != X0");
		indices    = null; // means all variables
		expected   = parse("(| Everything | * | Everything | = 0) and (| Everything | * (| Everything | - 1) = 0)");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("true");
		indices    = list("X", "Y");
		expected   = parse("true");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("false");
		indices    = list("X", "Y");
		expected   = parse("| Everything | * | Everything | = 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		
		expression = parse("X = a");
		indices    = null; // means all variables
		expected   = parse("| Everything | - 1 = 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a");
		indices    = null; // means all variables
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X = a");
		indices    = list("X", "Y");
		expected   = parse("(| Everything | - 1) * | Everything | = 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a");
		indices    = list("X", "Y");
		expected   = parse("| Everything | = 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X = a and Y != b");
		indices    = list("X", "Y");
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a and Y != b");
		indices    = list("X", "Y");
		expected   = parse("(| Everything | = 0) and (| Everything | - 1 = 0)");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a or Y != b");
		indices    = list("X", "Y");
		expected   = parse("false");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X != a and X != Y and Y != a");
		indices    = null;
		expected   = parse("(| Everything | = 0) and (| Everything | - 1 = 0)");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
		
		expression = parse("X = Y and Y = X");
		indices    = null;
		expected   = parse("(| Everything | - 1) * | Everything | = 0");
		runSymbolicAndNonSymbolicTests(expression, indices, expected);
	}

	protected Expression makeProblem(Expression expression, IndexExpressionsSet indexExpressions) {
		Expression problem = new DefaultUniversallyQuantifiedFormula(indexExpressions, expression);
		return problem;
	}

	protected DPLLGeneralizedAndSymbolic makeRewriter() {
		return new DPLLGeneralizedAndSymbolic(new EqualityOnTermsTheory(new SymbolTermTheory()), new Tautologicality());
	}
}
