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
package com.sri.ai.test.grinder.sgdpll2.theory.base;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.interpreter.SymbolicCommonInterpreter;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.solver.EvaluatorStepSolver;
import com.sri.ai.grinder.sgdpll2.core.solver.QuantifierFreeExpressionSymbolicEvaluatorStepSolver;
import com.sri.ai.grinder.sgdpll2.tester.RandomConditionalExpressionGenerator;
import com.sri.ai.grinder.sgdpll2.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.propositional.PropositionalConstraintTheory;
import com.sri.ai.util.base.NullaryFunction;

@Beta
public class ProfilingOfEvaluatorsTest {

	// @Test
	public void test() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

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

		RewritingProcess process = new DefaultRewritingProcess(null);
		process = constraintTheory.extendWithTestingInformation(process);
		Constraint2 contextualConstraint = new CompleteMultiVariableConstraint(constraintTheory);
		Simplifier topSimplifier = constraintTheory.getTopSimplifier();

		Expression expression;
		Expression actual;

		Random random = new Random(0);
		int depth = 3;
		NullaryFunction<Expression> leafGenerator = () -> makeSymbol(random.nextInt(5));
		expression = new RandomConditionalExpressionGenerator(random, constraintTheory, depth, leafGenerator, process).apply();

		ContextDependentExpressionProblemStepSolver[] evaluators = new ContextDependentExpressionProblemStepSolver[2];
		String[] evaluatorNames = new String[2];
		evaluators[0] = new EvaluatorStepSolver(expression, topSimplifier);
		evaluatorNames[0] = "New evaluator";
		evaluators[1] = new QuantifierFreeExpressionSymbolicEvaluatorStepSolver(expression, new SymbolicCommonInterpreter(constraintTheory));
		evaluatorNames[1] = "Old evaluator";

		long[] totalTimes = new long[]{0, 0};

		int numberOfRuns = 5;

		for (int i = 0; i != numberOfRuns; i++) {
			for (int evaluatorIndex = 0; evaluatorIndex != evaluators.length; evaluatorIndex++) {
				System.out.println("\nSolving " + expression);	
				System.out.println("Using " + evaluatorNames[evaluatorIndex]);	
				long start = System.currentTimeMillis();
				actual = evaluators[evaluatorIndex].solve(contextualConstraint, process);
				long time = System.currentTimeMillis() - start;
				System.out.println("Result: " + actual);
				System.out.println("Time  : " + time + " ms");
				totalTimes[evaluatorIndex] += time;
			}
		}

		for (int evaluatorIndex = 0; evaluatorIndex != evaluators.length; evaluatorIndex++) {
			long average = totalTimes[evaluatorIndex]/numberOfRuns;
			System.out.println("Average time for " + evaluatorNames[evaluatorIndex] + ": " + average);	
		}
	}
}