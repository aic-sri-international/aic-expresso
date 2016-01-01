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
package com.sri.ai.test.grinder.sgdpll2.theory.base;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver.SolutionStep;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.NumberOfDistinctExpressionsStepSolver;

public class NumberOfDistinctExpressionsStepSolverTest  {
	
	@Test
	public void test() {
		
		EqualityConstraintTheory constraintTheory = new EqualityConstraintTheory(true, true);
		RewritingProcess process = constraintTheory.makeRewritingProcessWithTestingInformation();
		
		String contextualConstraintString = "X != Y and X != a and X != b and Y != b";
		List<String> elementsStrings = list("X", "Y", "a", "b", "c");
		Constraint2 contextualConstraint = CompleteMultiVariableConstraint.parse(contextualConstraintString, constraintTheory, process);
		ArrayList<Expression> list = mapIntoArrayList(elementsStrings, Expressions::parse);
		NumberOfDistinctExpressionsStepSolver stepSolver = new NumberOfDistinctExpressionsStepSolver(list);

		SolutionStep step = stepSolver.step(contextualConstraint, process);
		assertEquals(true, step.itDepends());
		assertEquals(parse("X = c"), step.getLiteral());
		
		ContextDependentProblemStepSolver stepSolverIfXEqualsC = step.getStepSolverForWhenExpressionIsTrue();
		ContextDependentProblemStepSolver stepSolverIfXIsDifferentFromC = step.getStepSolverForWhenExpressionIsFalse();

		// if X = c, the number of distinct values can be 3 or 4, depending on whether Y = a, or Y = b
		step = stepSolverIfXEqualsC.step(contextualConstraint, process);
		assertEquals(true, step.itDepends());
		assertEquals(parse("Y = a"), step.getLiteral());

		ContextDependentProblemStepSolver stepSolverIfXEqualsCAndYEqualsA = step.getStepSolverForWhenExpressionIsTrue();
		ContextDependentProblemStepSolver stepSolverIfXEqualsCAndYIsDifferentFromA = step.getStepSolverForWhenExpressionIsFalse();

		// if X = c and Y = a, the number of distinct values is 3 (a, b, c)
		step = stepSolverIfXEqualsCAndYEqualsA.step(contextualConstraint, process);
		assertEquals(false, step.itDepends());
		assertEquals(parse("3"), step.getValue());

		// if X = c and Y != a, the number of distinct values is 3 or 4, depending on Y = c
		step = stepSolverIfXEqualsCAndYIsDifferentFromA.step(contextualConstraint, process);
		assertEquals(true, step.itDepends());
		assertEquals(parse("Y = c"), step.getLiteral());

		ContextDependentProblemStepSolver stepSolverIfXEqualsCAndYIsDifferentFromAAndYEqualsC = step.getStepSolverForWhenExpressionIsTrue();
		ContextDependentProblemStepSolver stepSolverIfXEqualsCAndYIsDifferentFromAAndYIsDifferentFromC = step.getStepSolverForWhenExpressionIsFalse();

		// if X = c and Y != a and Y = c, the number of distinct values is 3
		step = stepSolverIfXEqualsCAndYIsDifferentFromAAndYEqualsC.step(contextualConstraint, process);
		assertEquals(false, step.itDepends());
		assertEquals(parse("3"), step.getValue());

		// if X = c and Y != a and Y != c, the number of distinct values is 4
		step = stepSolverIfXEqualsCAndYIsDifferentFromAAndYIsDifferentFromC.step(contextualConstraint, process);
		assertEquals(false, step.itDepends());
		assertEquals(parse("4"), step.getValue());

		// if X = c and Y = a, the number of distinct values is 3 (a, b, c)
		step = stepSolverIfXEqualsCAndYEqualsA.step(contextualConstraint, process);
		assertEquals(false, step.itDepends());
		assertEquals(parse("3"), step.getValue());

		// using again just to make sure it produces the same result
		step = stepSolverIfXEqualsCAndYEqualsA.step(contextualConstraint, process);
		assertEquals(false, step.itDepends());
		assertEquals(parse("3"), step.getValue());
		

		// if X != c, the number of distinct value will now depend on Y = a
		step = stepSolverIfXIsDifferentFromC.step(contextualConstraint, process);
		assertEquals(true, step.itDepends());
		assertEquals(parse("Y = a"), step.getLiteral());

		// using again just to make sure it produces the same result
		step = stepSolverIfXIsDifferentFromC.step(contextualConstraint, process);
		assertEquals(true, step.itDepends());
		assertEquals(parse("Y = a"), step.getLiteral());
		
		// if X != c, the number of distinct values can be 4 or 5, depending on whether Y = a, or Y = b
		step = stepSolverIfXIsDifferentFromC.step(contextualConstraint, process);
		assertEquals(true, step.itDepends());
		assertEquals(parse("Y = a"), step.getLiteral());

		ContextDependentProblemStepSolver stepSolverIfXIsDifferentFromCAndYEqualsA = step.getStepSolverForWhenExpressionIsTrue();
		ContextDependentProblemStepSolver stepSolverIfXIsDifferentFromCAndYIsDifferentFromA = step.getStepSolverForWhenExpressionIsFalse();

		step = stepSolverIfXIsDifferentFromCAndYEqualsA.step(contextualConstraint, process);
		assertEquals(false, step.itDepends());
		assertEquals(parse("4"), step.getValue());
		
		// if however Y != a, limit will depend on Y = c
		step = stepSolverIfXIsDifferentFromCAndYIsDifferentFromA.step(contextualConstraint, process);
		assertEquals(true, step.itDepends());
		assertEquals(parse("Y = c"), step.getLiteral());
		
		ContextDependentProblemStepSolver stepSolverIfXIsDifferentFromCAndYIsDifferentFromAAndYIsEqualToC = step.getStepSolverForWhenExpressionIsTrue();
		ContextDependentProblemStepSolver stepSolverIfXIsDifferentFromCAndYIsDifferentFromAAndYIsDifferentFromC = step.getStepSolverForWhenExpressionIsFalse();

		// if Y = c, then there are 4 distinct values
		step = stepSolverIfXIsDifferentFromCAndYIsDifferentFromAAndYIsEqualToC.step(contextualConstraint, process);
		assertEquals(false, step.itDepends());
		assertEquals(parse("4"), step.getValue());
		
		// if Y != c, then Y is also unique and the number of distinct values is 5
		step = stepSolverIfXIsDifferentFromCAndYIsDifferentFromAAndYIsDifferentFromC.step(contextualConstraint, process);
		assertEquals(false, step.itDepends());
		assertEquals(parse("5"), step.getValue());
	}
}
