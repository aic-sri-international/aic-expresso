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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.getAllVariables;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.makeIndexExpression;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.toArrayList;

import java.util.Collection;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultIntensionalMultiSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.plaindpll.PlainCardinalityDPLLWithFreeVariables;

@Beta
public class PlainCardinalityDPLLWithFreeVariablesTest {
	// TO BE TESTED
	private final static Expression everythingType        = makeSymbol("Everything");
	private final static Expression everythingCardinality = apply(CARDINALITY, everythingType);

	@Test
	public void test() {
		
//		DirectCardinalityComputationFactory.newCardinalityProcess(); // just so the right modules kick in. I am not proud!
		
		Expression expression;
		Expression expected;
		Collection<String> indices;
		
		GrinderUtil.setMinimumOutputForProfiling();
		
		expression = parse("true");
		indices    = null; // means all variables
		expected   = parse("1");
		runTest(expression, indices, expected);

		// tests answer completeness
		expression  = parse("(Y = a and X = T) or (Y != a and X = T1 and T = T1)");
		indices     = list("Y");
		// original algorithm provided this incomplete solution due to incomplete condition-applying-on-solution algorithm used in externalization
		// expected = parse("if X = T then if T = T1 then if T = T1 then 10 else 1 else 1 else (if X = T1 then if T = T1 then 9 else 0 else 0)");
		expected    = parse("if X = T then if T = T1 then 10 else 1 else (if X = T1 then 0 else 0)");
		runTest(expression, indices, expected);
		
		
		
		
		expression = parse("X != Y");
		indices    = list("X");
		expected   = parse("9");
		runTest(expression, indices, expected);
		
		expression = parse("X != Y and X != a");
		indices    = list("X");
		expected   = parse("if Y = a then 9 else 8");
		runTest(expression, indices, expected);
		
		expression = parse("X != Y and X != Z and X != a");
		indices    = list("X");
		expected   = parse("if Y = Z then if Z = a then 9 else 8 else (if Y = a then 8 else (if Z = a then 8 else 7))");
		runTest(expression, indices, expected);
		
		expression = parse("Y = a and X != Y and X != a");
		indices    = list("X");
		expected   = parse("if Y = a then 9 else 0");
		runTest(expression, indices, expected);
		

		
		
		expression = parse("X1 != X2 and (X2 = X3 or X2 = X4) and X3 = X1 and X4 = X1");
		indices    = null; // means all variables
		expected   = parse("0");
		runTest(expression, indices, expected);
		
		expression = parse("X1 != X2 and X2 != X0 and X1 != X0");
		indices    = null; // means all variables
		expected   = parse("720");
		runTest(expression, indices, expected);
		
		expression = parse("true");
		indices    = null; // means all variables
		expected   = parse("1");
		runTest(expression, indices, expected);
		
		expression = parse("true");
		indices    = list("X", "Y");
		expected   = parse("100");
		runTest(expression, indices, expected);
		
		expression = parse("false");
		indices    = null; // means all variables
		expected   = parse("0");
		runTest(expression, indices, expected);
		
		expression = parse("false");
		indices    = list("X", "Y");
		expected   = parse("0");
		runTest(expression, indices, expected);
		
		
		
		
		expression = parse("X = a");
		indices    = null; // means all variables
		expected   = parse("1");
		runTest(expression, indices, expected);
		
		expression = parse("X != a");
		indices    = null; // means all variables
		expected   = parse("9");
		runTest(expression, indices, expected);
		
		expression = parse("X = a");
		indices    = list("X", "Y");
		expected   = parse("10");
		runTest(expression, indices, expected);
		
		expression = parse("X != a");
		indices    = list("X", "Y");
		expected   = parse("90");
		runTest(expression, indices, expected);
		
		expression = parse("X = a and Y != b");
		indices    = list("X", "Y");
		expected   = parse("9");
		runTest(expression, indices, expected);
		
		expression = parse("X != a and Y != b");
		indices    = list("X", "Y");
		expected   = parse("81");
		runTest(expression, indices, expected);
		
		expression = parse("X != a or Y != b");
		indices    = list("X", "Y");
		expected   = parse("99");
		runTest(expression, indices, expected);
		
		expression = parse("X != a and X != Y and Y != a");
		indices    = null;
		expected   = parse("72");
		runTest(expression, indices, expected);
	}

	private void runTest(Expression expression, Collection<String> indicesStrings, Expression expected) {
		DefaultRewritingProcess process = new DefaultRewritingProcess(expression, null);
		CountsDeclaration countsDeclaration = new CountsDeclaration(10);
		countsDeclaration.setup(process);
		
		Collection<Expression> indices;
		if (indicesStrings != null) {
			indices = mapIntoArrayList(indicesStrings, new Parse());
		}
		else {
			indices = getAllVariables(expression, process);
		}
		
		process.putGlobalObject(everythingCardinality, makeSymbol(10));
		
		List<Expression> indexExpressions =
				indices
				.stream()
				.map(index -> makeIndexExpression(index, everythingType))
				.collect(toArrayList(indices.size()));
		
		Rewriter cardinalityRewriter = new PlainCardinalityDPLLWithFreeVariables(countsDeclaration);
		Expression set = new DefaultIntensionalMultiSet(indexExpressions, Expressions.ONE, expression);
		Expression cardinalityProblem = apply(FunctorConstants.CARDINALITY, set);
		System.out.println("Problem: " + cardinalityProblem);
//		RewritingProcess subProcess = extendContextualSymbols(fromFreeSymbolsToEverything(cardinalityProblem, process), process);
		RewritingProcess subProcess = process;
		Expression actual = cardinalityRewriter.rewrite(cardinalityProblem, subProcess);
		Assert.assertEquals(expected, actual);
	}
	
//	private Map<Expression, Expression> fromFreeSymbolsToEverything(Expression expression, RewritingProcess process) {
//		Map<Expression, Expression> result = new LinkedHashMap<Expression, Expression>();
//		Collection<Expression> freeSymbols = freeSymbols(expression, process);
//		freeSymbols.forEach(freeSymbol -> result.put(freeSymbol, everythingType));
//		return result;
//	}

	private static class Parse implements Function<String, Expression> {

		@Override
		public Expression apply(String input) {
			return parse(input);
		}
		
	}

}
