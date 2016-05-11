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
package com.sri.ai.test.grinder.sgdpll.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.theory.differencearithmetic.SingleVariableDifferenceArithmeticConstraint;
import com.sri.ai.grinder.sgdpll.theory.differencearithmetic.ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver;
import com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic.LinearRealArithmeticConstraintTheory;

@Beta
public class LinearRealArithmeticConstraintTheoryTest {

	@Test
	public void test() {
		ConstraintTheory constraintTheory = new LinearRealArithmeticConstraintTheory(true, true);
		Context context = constraintTheory.makeContextWithTestingInformation();

		Expression variable;
		String constraintString;
		Expression expected;
		
		variable = parse("I");
		constraintString = "true";
		expected = parse("aboveAndUpTo(-1, 4)");
		runTest(variable, constraintString, expected, context);
		
		variable = parse("I");
		constraintString = "false";
		expected = parse("{}");
		runTest(variable, constraintString, expected, context);
		
		variable = parse("I");
		constraintString = "I < 3 and J > I";
		expected = parse("if 3 > J then if 0 < J then aboveAndUpTo(-1, J - 1) else {} else aboveAndUpTo(-1, 2)");
		runTest(variable, constraintString, expected, context);
		
		variable = parse("I");
		constraintString = "I < 3 and J > I and I != 2";
		expected = parse("if 3 > J then if 0 < J then aboveAndUpTo(-1, J - 1) else {} else aboveAndUpTo(-1, 2) - { 2 }");
		runTest(variable, constraintString, expected, context);
		
		variable = parse("I");
		constraintString = "I < 3 and J > I and I != 2 and I != K";
		expected = parse("if 3 > J then if 0 < J then if K + 1 <= J then aboveAndUpTo(-1, J - 1) - { K } else aboveAndUpTo(-1, J - 1) else {} else if K <= 2 then if 2 = K then aboveAndUpTo(-1, 2) - { K } else aboveAndUpTo(-1, 2) - { 2, K } else aboveAndUpTo(-1, 2) - { 2 }");
		runTest(variable, constraintString, expected, context);
	}

	private void runTest(Expression variable, String constraintString, Expression expected, Context context) {
		Constraint constraint
		= new SingleVariableDifferenceArithmeticConstraint(
				variable, true, context.getConstraintTheory());
		constraint = constraint.conjoin(parse(constraintString), context);
		
		ContextDependentExpressionProblemStepSolver stepSolver =
				new ValuesOfSingleVariableDifferenceArithmeticConstraintStepSolver((SingleVariableDifferenceArithmeticConstraint) constraint);
		
		Expression actual = stepSolver.solve(context);

		System.out.println(
				"Variable " + variable + "\nhas possible values:\n" + actual
				+ "\nsatisfying constraint:\n" + constraintString + "\n");
		assertEquals(expected, actual);
	}
}