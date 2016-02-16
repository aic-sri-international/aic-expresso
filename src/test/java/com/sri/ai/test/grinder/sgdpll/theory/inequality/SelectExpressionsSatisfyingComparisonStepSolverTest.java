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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
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
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.sgdpll.theory.base.ExpressionWrapperStepSolver;
import com.sri.ai.grinder.sgdpll.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.inequality.SelectExpressionsSatisfyingComparisonStepSolver;

@Beta
public class SelectExpressionsSatisfyingComparisonStepSolverTest {

	@Test
	public void test() {
		ConstraintTheory constraintTheory = new InequalityConstraintTheory(true, true);
		Context context = new DefaultRewritingProcess();
		context = constraintTheory.extendWithTestingInformation(context);
		Constraint contextualConstraint = new CompleteMultiVariableConstraint(constraintTheory);

		List<String> expressionStrings;
		Expression bound;
		Expression expected;
		
		expressionStrings = list("I", "J");
		bound = parse("J");
		expected = parse("if I < J then list(I) else list()");
		runTest(expressionStrings, bound, expected, contextualConstraint, context);	
		
		expressionStrings = list("I", "2", "J");
		bound = parse("3");
		expected = parse("if I < 3 then if J < 3 then list(I, 2, J) else list(I, 2) else if J < 3 then list(2, J) else list(2)");
		runTest(expressionStrings, bound, expected, contextualConstraint, context);	
		
		expressionStrings = list();
		bound = parse("3");
		expected = parse("list()");
		runTest(expressionStrings, bound, expected, contextualConstraint, context);	

		expressionStrings = list("I", "2", "J");
		bound = parse("infinity");
		expected = parse("list(I, 2, J)");
		runTest(expressionStrings, bound, expected, contextualConstraint, context);	

		expressionStrings = list("I", "2", "J");
		bound = parse("-infinity");
		expected = parse("list()");
		runTest(expressionStrings, bound, expected, contextualConstraint, context);	
		
		expressionStrings = list("I", "2", "infinity");
		bound = parse("3");
		expected = parse("if I < 3 then list(I, 2) else list(2)");
		runTest(expressionStrings, bound, expected, contextualConstraint, context);	
	}

	private void runTest(List<String> expressions, Expression bound, Expression expected, Constraint contextualConstraint, Context context) {
		ContextDependentProblemStepSolver<List<Expression>> stepSolver =
				new SelectExpressionsSatisfyingComparisonStepSolver(
						mapIntoArrayList(expressions, Expressions::parse),
						LESS_THAN,
						bound);
		
		ExpressionWrapperStepSolver<List<Expression>> wrapInList
		= new ExpressionWrapperStepSolver<>(stepSolver, selection -> apply("list", selection));

		Expression solution = ContextDependentExpressionProblemSolver.solve(wrapInList, contextualConstraint, context);
		System.out.println("Elements in " + expressions + " which are less than " + bound + ": " + solution);
		assertEquals(expected, solution);
	}
}