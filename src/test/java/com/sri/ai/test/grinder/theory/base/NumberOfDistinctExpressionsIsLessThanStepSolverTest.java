/*
 * Copyright (c) 2014, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-4-Clause
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
package com.sri.ai.test.grinder.theory.base;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver.Step;
import com.sri.ai.grinder.tester.TheoryTestingSupport;
import com.sri.ai.grinder.theory.equality.EqualityTheory;
import com.sri.ai.grinder.theory.equality.NumberOfDistinctExpressionsIsLessThanStepSolver;

public class NumberOfDistinctExpressionsIsLessThanStepSolverTest  {
	
	public Random makeRandom() {
		return new Random();
	}
	
	@Test
	public void test() {
		
		TheoryTestingSupport theoryTestingSupport = 
				TheoryTestingSupport.make(
						makeRandom(), 
						new EqualityTheory(true, true));
		Context context = theoryTestingSupport.makeContextWithTestingInformation();
		
		String contextString = "X != Y and X != a and X != b and Y != b";
		List<String> elementsStrings = list("X", "Y", "a", "b", "c");
		int limit = 5;
		context = context.conjoin(parse(contextString), context);
		
		ArrayList<Expression> list = mapIntoArrayList(elementsStrings, Expressions::parse);
		NumberOfDistinctExpressionsIsLessThanStepSolver stepSolver = new NumberOfDistinctExpressionsIsLessThanStepSolver(limit, list);

		Step step = stepSolver.step(context);
		assertEquals(true, step.itDepends());
		assertEquals(parse("X = c"), step.getSplitter());
		
		ExpressionLiteralSplitterStepSolver stepSolverIfXEqualsC = step.getStepSolverForWhenSplitterIs(true);
		ExpressionLiteralSplitterStepSolver stepSolverIfXIsDifferentFromC = step.getStepSolverForWhenSplitterIs(false);

		// if X = c, the number of distinct values is at most 4, so it will never reach the limit
		step = stepSolverIfXEqualsC.step(context);
		assertEquals(false, step.itDepends());
		assertEquals(TRUE, step.getValue());

		// using again just to make sure it produces the same result
		step = stepSolverIfXEqualsC.step(context);
		assertEquals(false, step.itDepends());
		assertEquals(TRUE, step.getValue());
		

		// if X != c, the number of distinct values will now depend on Y = a
		step = stepSolverIfXIsDifferentFromC.step(context);
		assertEquals(true, step.itDepends());
		assertEquals(parse("Y = a"), step.getSplitter());

		// using again just to make sure it produces the same result
		step = stepSolverIfXIsDifferentFromC.step(context);
		assertEquals(true, step.itDepends());
		assertEquals(parse("Y = a"), step.getSplitter());
		

		ExpressionLiteralSplitterStepSolver stepSolverIfXIsDifferentFromCAndYEqualsA = step.getStepSolverForWhenSplitterIs(true);
		ExpressionLiteralSplitterStepSolver stepSolverIfXIsDifferentFromCAndYIsDifferentFromA = step.getStepSolverForWhenSplitterIs(false);

		// ok, moving on, assuming Y = a, limit will not be reached
		step = stepSolverIfXIsDifferentFromCAndYEqualsA.step(context);
		assertEquals(false, step.itDepends());
		assertEquals(TRUE, step.getValue());
		
		// if however Y != a, limit will depend on Y = c
		step = stepSolverIfXIsDifferentFromCAndYIsDifferentFromA.step(context);
		assertEquals(true, step.itDepends());
		assertEquals(parse("Y = c"), step.getSplitter());
		
		ExpressionLiteralSplitterStepSolver stepSolverIfXIsDifferentFromCAndYIsDifferentFromAAndYIsEqualToC = step.getStepSolverForWhenSplitterIs(true);
		ExpressionLiteralSplitterStepSolver stepSolverIfXIsDifferentFromCAndYIsDifferentFromAAndYIsDifferentFromC = step.getStepSolverForWhenSplitterIs(false);

		// if Y = c, then limit is not going to be reached
		step = stepSolverIfXIsDifferentFromCAndYIsDifferentFromAAndYIsEqualToC.step(context);
		assertEquals(false, step.itDepends());
		assertEquals(TRUE, step.getValue());
		
		// if Y != c, then limit is reached
		step = stepSolverIfXIsDifferentFromCAndYIsDifferentFromAAndYIsDifferentFromC.step(context);
		assertEquals(false, step.itDepends());
		assertEquals(FALSE, step.getValue());
	}
}
