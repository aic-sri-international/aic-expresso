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
package com.sri.ai.test.grinder.sgdpll2;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.map;
import static org.junit.Assert.assertEquals;

import java.util.Map;
import java.util.Random;

import org.junit.Test;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.interpreter.SymbolicCommonInterpreterWithLiteralConditioning;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.problemtype.Max;
import com.sri.ai.grinder.plaindpll.problemtype.Sum;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.MultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.tester.ConstraintTheoryTester;
import com.sri.ai.grinder.sgdpll2.theory.base.AbstractConstrainTheoryWithBinaryRelations;
import com.sri.ai.grinder.sgdpll2.theory.compound.CompoundConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.equality.EqualityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.inequality.InequalityConstraintTheory;
import com.sri.ai.grinder.sgdpll2.theory.propositional.PropositionalConstraintTheory;

@Beta
public class CompoundConstraintTheoryTest {

	private CompoundConstraintTheory makeCompoundConstraintTheory() {
		return new CompoundConstraintTheory(
				new EqualityConstraintTheory(true),
				// new InequalityConstraintTheory(true), // not efficient enough yet to be included
				new PropositionalConstraintTheory());
	}

	@Test
	public void basicTests() {
		
		ConstraintTheory compound = makeCompoundConstraintTheory();
		
		Expression condition = parse("X = Y and Y = X and P and not Q and P and X = a and X != b");
		
		Constraint2 constraint = new CompleteMultiVariableConstraint(compound);
		RewritingProcess process = compound.extendWithTestingInformation(new DefaultRewritingProcess(null));
		constraint = constraint.conjoin(condition, process);
		Expression expected = parse("not Q and P and (X = Y) and (X = a)");
		assertEquals(expected, constraint);
		
		Simplifier interpreter = new SymbolicCommonInterpreterWithLiteralConditioning(compound);
		Expression input = parse(
				"product({{(on X in SomeType) if X = c then 2 else 3 | X = Y and Y = X and P and not Q and P and X != a and X != b}})");
		process.putGlobalObject(SymbolicCommonInterpreterWithLiteralConditioning.INTERPRETER_CONTEXTUAL_CONSTRAINT, new CompleteMultiVariableConstraint(compound));
		Expression result = interpreter.apply(input, process);
		Expression expectedProduct = parse("if P then if not Q then if Y = c then 2 else if Y != a then if Y != b then 3 else 1 else 1 else 1 else 1");
		assertEquals(expectedProduct, result);
	}
	
	@Test
	public void testSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		ConstraintTheoryTester.testSingleVariableConstraints(
				new Random(),
				makeCompoundConstraintTheory(),
				100 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testMultiVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		ConstraintTheoryTester.testMultiVariableConstraints(
				new Random(),
				makeCompoundConstraintTheory(),
				300 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testCompleteMultiVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		ConstraintTheoryTester.testCompleteMultiVariableConstraints(
				new Random(),
				makeCompoundConstraintTheory(),
				200 /* number of tests */,
				50 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testModelCountingForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();

		ConstraintTheoryTester.testModelCountingForSingleVariableConstraints(
				new Random(),
				makeCompoundConstraintTheory(),
				200 /* number of tests */,
				30 /* number of literals per test */,
				true /* output count */);
	}

	@Test
	public void testSumForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemForSingleVariableConstraints(
				new Random(),
				new Sum(),
				makeCompoundConstraintTheory(),
				10 /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testMaxForSingleVariableConstraints() {
		GrinderUtil.setTraceAndJustificationOffAndTurnOffConcurrency();
		
		ConstraintTheoryTester.testGroupProblemForSingleVariableConstraints(
				new Random(),
				new Max(),
				makeCompoundConstraintTheory(),
				10 /* number of tests */,
				20 /* number of literals per test */,
				3, /* body depth */
				true /* output count */);
	}

	@Test
	public void testCompleteSatisfiabilitySpecialCases() {
		// This test is to make sure that some more tricky cases are indeed tested,
		// even though hopefully the large amount of generated random problems include them.
		
		// These are copied from the equality constraint theory test,
		// so it is really just to check whether things hold up
		// if equality constraint theory is embedded in a compound constraint theory.

		String conjunction;
		Expression expected;
		Map<String, String> variableNamesAndTypeNamesForTesting = // need W besides the other defaults -- somehow not doing this in equality theory alone does not cause a problem, probably because the type for W is never needed when we have only equality theory
				map("X", "SomeType", "Y", "SomeType", "Z", "SomeType", "W", "SomeType");

		
		conjunction = "X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = null;
		runCompleteSatisfiabilityTest(conjunction, expected, variableNamesAndTypeNamesForTesting);
		
		conjunction = "X = Y and X != a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = null;
		runCompleteSatisfiabilityTest(conjunction, expected, variableNamesAndTypeNamesForTesting);
		
		conjunction = "X = a and X != b and X != sometype5 and X != Z and X != W and Z = c and W = d";
		expected = parse("(W = d) and (Z = c) and (X = a) and (X != Z) and (X != W)");
		runCompleteSatisfiabilityTest(conjunction, expected, variableNamesAndTypeNamesForTesting);
	}

	/**
	 * @param conjunction
	 * @param expected
	 */
	private void runCompleteSatisfiabilityTest(String conjunction, Expression expected, Map<String, String> variableNamesAndTypeNamesForTesting) {
		AbstractConstrainTheoryWithBinaryRelations equalityTheory = new EqualityConstraintTheory(true);
		equalityTheory.setVariableNamesAndTypeNamesForTesting(variableNamesAndTypeNamesForTesting);
		ConstraintTheory constraintTheory = new CompoundConstraintTheory(equalityTheory, new PropositionalConstraintTheory());
		MultiVariableConstraint constraint = new CompleteMultiVariableConstraint(constraintTheory);
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		for (Expression literal : And.getConjuncts(parse(conjunction))) {
			constraint = constraint.conjoin(literal, process);
		}
		assertEquals(expected, constraint);
	}
	
}
