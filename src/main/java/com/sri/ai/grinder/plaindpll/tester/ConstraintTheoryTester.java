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
package com.sri.ai.grinder.plaindpll.tester;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.isSatisfiableByBruteForce;
import static com.sri.ai.util.Util.join;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;

/**
 * A class for testing a {@link ConstraintTheory} and its unsatisfiability detection.
 * 
 * @author braz
 *
 */
public class ConstraintTheoryTester {
	
	private static void output(String message) {
		// System.out.println(message); // uncomment out if detailed output is desired.
	}

	/**
	 * Measures the time taken to run a given number of tests with a maximum number of literals,
	 * without brute-force correctness testing.
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param constraintTheory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param outputCount
	 */
	public static long measureTime(Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		
		long start = System.currentTimeMillis();
		test(random, constraintTheory, numberOfTests, maxNumberOfLiterals, outputCount, false /* no correctness */);
		long result = System.currentTimeMillis() - start;
		if (outputCount) {
			System.out.println("Total time: " + result + " ms.");
		}	
		return result;
	}

	/**
	 * Given a constraint theory and a number <code>n</code> of tests,
	 * generates <code>n</code> formulas in the theory
	 * and see if those detected as unsatisfiable by the corresponding solver
	 * are indeed unsatisfiable (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param constraintTheory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param outputCount
	 */
	public static void test(Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		test(random, constraintTheory, numberOfTests, maxNumberOfLiterals, true, outputCount);
	}
	
	private static void test(Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean testCorrectness, boolean outputCount) {
		
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		
		for (int i = 1; i != numberOfTests + 1; i++) {
			Expression variable = makeSymbol(constraintTheory.getTestingVariable());
			SingleVariableConstraint constraint = constraintTheory.makeSingleVariableConstraint(variable);
			Collection<Expression> literals = new LinkedHashSet<>();
			
			output("\n\nStarting new conjunction");	
			for (int j = 0; constraint != null && j != maxNumberOfLiterals; j++) {
				Expression literal = constraintTheory.makeRandomLiteralOn(random, process);
				literals.add(literal);
				output("\nAdded " + literal + " (current conjunction: " + And.make(literals) + ")");	
				constraint = constraint.conjoin(literal, process);
				if (testCorrectness) {
					if (constraint == null) {
						solverSaysItIsUnsatisfiable(literals, constraintTheory, process);
					}
					else if (constraintTheory.singleVariableConstraintIsCompleteWithRespectToItsVariable()) {
						solverSaysItIsSatisfiable(literals, constraint, constraintTheory, process);
					}
					else {
						// if constraint is not null, the conjunction of literals may or may not be satisfiable,
						// because solvers are allowed to be incomplete regarding satisfiability,
						// so in this case we do not test either way.
						output("Solver does not know yet if it is satisfiable or not. Current state is " + constraint.debuggingDescription(process));
					}
				}
			}
			if (outputCount && i % 100 == 0) {
				System.out.println("Tested " + i + " examples for " + constraintTheory.getClass().getSimpleName());
			}	
		}
	}

	/**
	 * @param literals
	 * @param constraint
	 * @param constraintTheory
	 * @param process
	 * @throws Error
	 */
	protected static void solverSaysItIsSatisfiable(Collection<Expression> literals, SingleVariableConstraint constraint, ConstraintTheory constraintTheory, RewritingProcess process) throws Error {
		output("Solver thinks it is satisfiable. Current constraint is " + constraint.debuggingDescription(process));	
		Expression formula = And.make(literals);
		Map<Expression, Expression> satisfyingAssignment = isSatisfiableByBruteForce(formula, constraintTheory, process);
		if (satisfyingAssignment == null) {
			String message = join(literals, " and ") + " is unsatisfiable (by brute-force) but " + 
			constraintTheory.getClass().getSimpleName() + "'s says it is satisfiable. " +
			"Current constraint is " + constraint.debuggingDescription(process);
			output(message);
			throw new Error(message);
		}
		else {
			output("Brute-force satisfiability test agrees that it is satisfiable. Constraint is " + constraint.debuggingDescription(process));	
		}
	}

	/**
	 * @param literals
	 * @param constraintTheory
	 * @param process
	 * @throws Error
	 */
	protected static void solverSaysItIsUnsatisfiable(Collection<Expression> literals, ConstraintTheory constraintTheory, RewritingProcess process) throws Error {
		output("Solver thinks it is unsatisfiable.");	
		Expression formula = And.make(literals);
		Map<Expression, Expression> satisfyingAssignment = isSatisfiableByBruteForce(formula, constraintTheory, process);
		if (satisfyingAssignment != null) {
			String message = join(literals, " and ") + " is satisfiable (by brute-force) but " + 
			constraintTheory.getClass().getSimpleName() + "'s says it is not. " +
			"Satisfying assignment is " + satisfyingAssignment + ".";
			output(message);
			throw new Error(message);
		}
		else {
			output("Brute-force satisfiability test agrees that it is unsatisfiable.");	
		}
	}
}