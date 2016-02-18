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
package com.sri.ai.test.grinder.sgdpll.theory.inequality;

import static com.sri.ai.expresso.helper.Expressions.INFINITY;
import static com.sri.ai.expresso.helper.Expressions.MINUS_INFINITY;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.TypeContext;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.sgdpll.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.inequality.MaximumExpressionStepSolver;

@Beta
public class MaximumExpressionStepSolverTest {

	@Test
	public void test() {
		ConstraintTheory constraintTheory = new InequalityConstraintTheory(true, true);
		Context context = new TypeContext(constraintTheory);
		context = constraintTheory.extendWithTestingInformation(context);
		Context contextualConstraint = new CompleteMultiVariableConstraint(constraintTheory, context);
		
		context = contextualConstraint;

		List<String> expressionStrings;
		String order;
		Expression orderMinimum;
		Expression orderMaximum;
		Expression expected;
		
		expressionStrings = list("I", "J");
		expected = parse("if I < J then J else I");
		order = LESS_THAN;
		orderMinimum = MINUS_INFINITY;
		orderMaximum = INFINITY;
		runTest(expressionStrings, order, orderMinimum, orderMaximum, expected, contextualConstraint, context);	
		
		expressionStrings = list("I", "J");
		expected = parse("if I > J then J else I");
		order = GREATER_THAN;
		orderMinimum = INFINITY;
		orderMaximum = MINUS_INFINITY;
		runTest(expressionStrings, order, orderMinimum, orderMaximum, expected, contextualConstraint, context);	
		
		expressionStrings = list("2", "3", "J");
		expected = parse("if 3 < J then J else 3");
		order = LESS_THAN;
		orderMinimum = MINUS_INFINITY;
		orderMaximum = INFINITY;
		runTest(expressionStrings, order, orderMinimum, orderMaximum, expected, contextualConstraint, context);	
		
		expressionStrings = list("2", "I", "3", "J");
		expected = parse("if 2 < I then if I < J then J else I else if 3 < J then J else 3");
		order = LESS_THAN;
		orderMinimum = MINUS_INFINITY;
		orderMaximum = INFINITY;
		runTest(expressionStrings, order, orderMinimum, orderMaximum, expected, contextualConstraint, context);	
		
		expressionStrings = list("1", "2");
		expected = parse("2");
		order = LESS_THAN;
		orderMinimum = MINUS_INFINITY;
		orderMaximum = INFINITY;
		runTest(expressionStrings, order, orderMinimum, orderMaximum, expected, contextualConstraint, context);	
		
		expressionStrings = list("1", "2");
		expected = parse("1");
		order = GREATER_THAN;
		orderMinimum = INFINITY;
		orderMaximum = MINUS_INFINITY;
		runTest(expressionStrings, order, orderMinimum, orderMaximum, expected, contextualConstraint, context);	
		
		expressionStrings = list("1", "-infinity");
		expected = parse("1");
		order = LESS_THAN;
		orderMinimum = MINUS_INFINITY;
		orderMaximum = INFINITY;
		runTest(expressionStrings, order, orderMinimum, orderMaximum, expected, contextualConstraint, context);	
		
		expressionStrings = list("1", "infinity");
		expected = parse("infinity");
		order = LESS_THAN;
		orderMinimum = MINUS_INFINITY;
		orderMaximum = INFINITY;
		runTest(expressionStrings, order, orderMinimum, orderMaximum, expected, contextualConstraint, context);	
	}

	private void runTest(List<String> expressions, String order, Expression orderMinimum, Expression orderMaximum, Expression expected, Context contextualConstraint, Context context) {
		ContextDependentProblemStepSolver<Expression> stepSolver =
				new MaximumExpressionStepSolver(
						mapIntoArrayList(expressions, Expressions::parse),
						makeSymbol(order),
						orderMinimum,
						orderMaximum);

		Expression solution = ContextDependentExpressionProblemSolver.solve(stepSolver, contextualConstraint);
		System.out.println("Maximum of " + expressions + " for order " + order + ": " + solution);
		assertEquals(expected, solution);
	}
}