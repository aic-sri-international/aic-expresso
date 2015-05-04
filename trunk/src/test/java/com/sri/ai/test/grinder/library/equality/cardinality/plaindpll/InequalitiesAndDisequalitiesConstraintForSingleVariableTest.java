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
import static com.sri.ai.grinder.plaindpll.theory.InequalitiesAndDisequalitiesConstraintForSingleVariable.localInference;
import static com.sri.ai.util.Util.list;
import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.grinder.plaindpll.theory.InequalitiesAndDisequalitiesConstraintForSingleVariable.LocalInferenceConclusion;

@Beta
public class InequalitiesAndDisequalitiesConstraintForSingleVariableTest {
	
	@Test
	public void test() {
		
		Expression[] literals;
		boolean[] keep;
		List<Expression> addedLiterals;
		
		literals = parseExpressions("x < y", "x < z", "y > z");
		keep = new boolean[]{false, true, true};
		addedLiterals = list();
		runTest(literals, keep, addedLiterals);
		
		literals = parseExpressions("x > y", "x < z", "y > z");
		keep = null; // contradiction, because lower separator y is above upper separator z
		addedLiterals = list();
		runTest(literals, keep, addedLiterals);

		literals = parseExpressions("x >= y", "x <= z", "y >= z");
		keep = new boolean[]{false, false, false}; // lower separator greater or equal to upper separator, implying equality all over
		addedLiterals = list(parse("y = x"), parse("x = z"));
		runTest(literals, keep, addedLiterals);
		
		literals = parseExpressions("x != y", "x < z", "y > z");
		keep = new boolean[]{false, true, true}; // x != y is irrelevant because y is beyond upper separator z
		addedLiterals = list();
		runTest(literals, keep, addedLiterals);
		
		literals = parseExpressions("y > z", "x != y", "x < z");
		keep = new boolean[]{true, false, true}; // same as prior one, permuted
		addedLiterals = list();
		runTest(literals, keep, addedLiterals);
		
		literals = parseExpressions("3 > z", "x != 3", "x < z");
		keep = new boolean[]{true, false, true}; // same as prior one, with a constant
		addedLiterals = list();
		runTest(literals, keep, addedLiterals);

		literals = parseExpressions("x >= y", "x <= z", "y != z");
		keep = new boolean[]{true, true, true}; // do nothing, y != z prevents bounds to be equal, which they might otherwise
		addedLiterals = list();
		runTest(literals, keep, addedLiterals);

		literals = parseExpressions("x >= y", "x < z", "y != z");
		keep = new boolean[]{true, true, false}; // same as prior one, but now bounds can never be equal, so y != z is redundant
		addedLiterals = list();
		runTest(literals, keep, addedLiterals);
	}

	/**
	 * @param literals
	 * @param keep
	 * @param addedLiterals
	 */
	private void runTest(Expression[] literals, boolean[] keep, List<Expression> addedLiterals) {
		LocalInferenceConclusion expected;
		LocalInferenceConclusion conclusion;
		try {
			conclusion = localInference(literals);
			if (keep == null) {
				Assert.fail("Should have found a Contradiction but found " + conclusion);
			}
			expected = new LocalInferenceConclusion(literals, keep, addedLiterals);
			System.out.println("Literals  : " + literals);	
			System.out.println("Conclusion: " + conclusion);	
			assertEquals(expected, conclusion);
		}
		catch (Contradiction e) {
			if (keep != null) {
				expected = new LocalInferenceConclusion(literals, keep, addedLiterals);
				Assert.fail("Indicated Contradiction but expected " + expected);
			}
		}
	}
	
	private Expression[] parseExpressions(String... strings) {
		Expression[] result = new Expression[strings.length];
		for (int i = 0; i != strings.length; i++) {
			result[i] = parse(strings[i]);
		}
		return result;
	}
}
