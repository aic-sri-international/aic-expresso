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
package com.sri.ai.test.grinder.sgdpll.theory.base;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.solver.ContextDependentExpressionProblemSolver;
import com.sri.ai.grinder.sgdpll.core.solver.EvaluatorStepSolver;
import com.sri.ai.grinder.sgdpll.simplifier.api.TopSimplifier;
import com.sri.ai.grinder.sgdpll.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.propositional.PropositionalConstraintTheory;

@Beta
public class EvaluatorStepSolverTest {

	@Test
	public void test() {
		ConstraintTheory constraintTheory
		= new CompoundConstraintTheory(
				new EqualityConstraintTheory(false, true),
				new InequalityConstraintTheory(false, true),
				new PropositionalConstraintTheory());
		Map<String, Type> variablesAndTypes = new LinkedHashMap<>(constraintTheory.getVariableNamesAndTypesForTesting());
		Type booleanType = variablesAndTypes.get("P");
		variablesAndTypes.put("S", booleanType);
		variablesAndTypes.put("T", booleanType);
		variablesAndTypes.put("U", booleanType);
		constraintTheory.setVariableNamesAndTypesForTesting(variablesAndTypes);
		
		Context context = new DefaultRewritingProcess();
		context = constraintTheory.extendWithTestingInformation(context);
		Constraint contextualConstraint = new CompleteMultiVariableConstraint(constraintTheory);
		TopSimplifier topSimplifier = constraintTheory.getTopSimplifier();
		
		String expressionString;
		Expression expected;
		
		expressionString = "0";
		expected = parse("0");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, context);	
		
		expressionString = "I > J";
		expected = parse("I > J");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, context);	
		
		expressionString = "I > J and I < J";
		expected = parse("false");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, context);	
		
		expressionString = "(if I > J then 1 else 2) + (if I <= J then 30 else 40)";
		expected = parse("if I > J then 41 else 32");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, context);	
		
		expressionString = "(if I > J then 1 else 2) + (if I <= J then 3 else 4)";
		expected = parse("5");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, context);	
		
		expressionString = "(if I > J then if P or Q then 1 else 2 else 5) + (if I <= J then 3 else if not Q then 4 else -3)";
		expected = parse("if I > J then if P then if not Q then 5 else -2 else if Q then -2 else 6 else 8");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, context);	
		
		expressionString = "(if I > J then if P or X = a or Y != b then 1 else 2 else 5) + (if I <= J then 3 else if not (X != a or Y = c and Q) then 4 else -3)";
		expected = parse("if I > J then if P then if X != a then -2 else if Y = c then if Q then -2 else 5 else 5 else if X = a then if Y = c then if Q then -2 else 5 else 5 else if Y != b then -2 else -1 else 8");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, context);	
		
		expressionString = "if P and Q and R then 1 else 0";
		expected = parse("if P then if Q then if R then 1 else 0 else 0 else 0");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, context);	
		
		expressionString = "if P and Q and R and S and T and U then 1 else 0";
		expected = parse("if P then if Q then if R then if S then if T then if U then 1 else 0 else 0 else 0 else 0 else 0 else 0");
		runTest(expressionString, expected, contextualConstraint, topSimplifier, context);	
	}

	private void runTest(String expressionString, Expression expected, Constraint contextualConstraint, TopSimplifier topSimplifier, Context context) {
		Expression expression = parse(expressionString);
		EvaluatorStepSolver stepSolver = new EvaluatorStepSolver(expression, topSimplifier);
		System.out.println("Evaluating " + expression);
		Expression solution = ContextDependentExpressionProblemSolver.solve(stepSolver, contextualConstraint, context);
		System.out.println(expression + " -----> " + solution + "\n");
		assertEquals(expected, solution);
	}
}