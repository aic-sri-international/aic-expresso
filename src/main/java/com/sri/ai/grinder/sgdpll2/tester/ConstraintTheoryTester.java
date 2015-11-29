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
package com.sri.ai.grinder.sgdpll2.tester;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.getVariables;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.removeFromSetNonDestructively;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.function.Function;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.interpreter.AbstractInterpreter;
import com.sri.ai.grinder.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.interpreter.SymbolicCommonInterpreterWithLiteralConditioning;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.plaindpll.api.GroupProblemType;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.CompleteMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.DefaultMultiVariableConstraint;
import com.sri.ai.util.base.NullaryFunction;

/**
 * A class for testing a {@link ConstraintTheory} and its unsatisfiability detection.
 * 
 * @author braz
 *
 */
@Beta
public class ConstraintTheoryTester {
	
	private static final int NUMBER_OF_TESTS_TO_INDICATE_ON_CONSOLE = 1;

	private static void output(String message) {
		// System.out.println(message); // uncomment out if detailed output is desired.
	}

	/**
	 * Measures the time taken to run a given number of single-variable constraint tests with a maximum number of literals,
	 * without brute-force correctness testing.
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param constraintTheory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param outputCount
	 */
	public static long measureTimeSingleVariableConstraints(Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		
		long start = System.currentTimeMillis();
		testSingleVariableConstraints(random, constraintTheory, numberOfTests, maxNumberOfLiterals, outputCount, false /* no correctness test */);
		long result = System.currentTimeMillis() - start;
		if (outputCount) {
			System.out.println("Total time: " + result + " ms.");
		}	
		return result;
	}

	/**
	 * Given a constraint theory and a number <code>n</code> of single-variable constraint tests,
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
	public static void testSingleVariableConstraints(Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		testSingleVariableConstraints(random, constraintTheory, numberOfTests, maxNumberOfLiterals, true /* correctness test */, outputCount);
	}
	
	private static void testSingleVariableConstraints(
			Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean testCorrectness, boolean outputCount) {
		
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		
		NullaryFunction<Constraint2> makeConstraint = () -> constraintTheory.makeSingleVariableConstraint(makeSymbol(constraintTheory.pickTestingVariableAtRandom(random)), constraintTheory, process);

		Function<Constraint2, Expression> makeRandomLiteral = c -> constraintTheory.makeRandomLiteralOn(((SingleVariableConstraint)c).getVariable().toString(), random, process);

		boolean isComplete = constraintTheory.singleVariableConstraintIsCompleteWithRespectToItsVariable();

		TestRunner tester = isComplete? ConstraintTheoryTester::testCompleteSatisfiability : ConstraintTheoryTester::testIncompleteSatisfiability;
		
		String problemName = (isComplete? "complete" : "incomplete") + " satisfiability for single-variable constraints";
		
		runAllTests(random, problemName, tester, constraintTheory, makeConstraint, makeRandomLiteral, numberOfTests, maxNumberOfLiterals, testCorrectness, outputCount, process);
	}

	/**
	 * Given a constraint theory and a number <code>n</code> of multi-variable constraint tests,
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
	public static void testMultiVariableConstraints(Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		testMultiVariableConstraints(random, constraintTheory, numberOfTests, maxNumberOfLiterals, true /* correctness test */, outputCount);
	}
	
	private static void testMultiVariableConstraints(
			Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean testCorrectness, boolean outputCount) {
		
		NullaryFunction<Constraint2> makeConstraint = () -> new DefaultMultiVariableConstraint(constraintTheory);

		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		
		Function<Constraint2, Expression> makeRandomLiteral = c -> constraintTheory.makeRandomLiteral(random, process);

		TestRunner tester = ConstraintTheoryTester::testIncompleteSatisfiability; // DefaultMultiVariableConstraint is incomplete
		
		runAllTests(random, "incomplete satisfiability", tester, constraintTheory, makeConstraint, makeRandomLiteral, numberOfTests, maxNumberOfLiterals, testCorrectness, outputCount, process);
	}

	/**
	 * Given a constraint theory and a number <code>n</code> of multi-variable constraint tests,
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
	public static void testCompleteMultiVariableConstraints(Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		testCompleteMultiVariableConstraints(random, constraintTheory, numberOfTests, maxNumberOfLiterals, true /* correctness test */, outputCount);
	}
	
	private static void testCompleteMultiVariableConstraints(
			Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean testCorrectness, boolean outputCount) {
		
		NullaryFunction<Constraint2> makeConstraint = () -> new CompleteMultiVariableConstraint(constraintTheory);

		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		
		Function<Constraint2, Expression> makeRandomLiteral = c -> constraintTheory.makeRandomLiteral(random, process);

		TestRunner tester = ConstraintTheoryTester::testCompleteSatisfiability; // CompleteMultiVariableConstraint is complete
		
		runAllTests(random, "complete satisfiability", tester, constraintTheory, makeConstraint, makeRandomLiteral, numberOfTests, maxNumberOfLiterals, testCorrectness, outputCount, process);
	}

	private static interface TestRunner {
		void runOneTest(Random random, Collection<Expression> literals, Constraint2 constraint, ConstraintTheory constraintTheory, RewritingProcess process) throws Error;		
	}

	/**
	 * Generates a number of tests based on a conjunction each, which is formed by incrementally adding literals to it.
	 * The conjunction is provided to a {@link TestRunner} which runs some test based on it.
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param tester the {@link TestRunner}
	 * @param constraintTheory
	 * @param makeConstraint a thunk generating new constraints equivalent to TRUE
	 * @param makeRandomLiteralGivenConstraint a function generating appropriate new literals (given generated constraint if needed)
	 * @param numberOfTests the number of tests to run
	 * @param maxNumberOfLiterals the maximum number of literals to add to each test conjunction
	 * @param testCorrectness actually tests the result, as opposed to simply running tests (for measuring time, for example)
	 * @param outputCount whether to output the test count
	 * @param process a rewriting process
	 * @throws Error
	 */
	public static void runAllTests(
			Random random,
			String problemName,
			TestRunner tester,
			ConstraintTheory constraintTheory,
			NullaryFunction<Constraint2> makeConstraint,
			Function<Constraint2, Expression> makeRandomLiteralGivenConstraint,
			int numberOfTests,
			int maxNumberOfLiterals,
			boolean testCorrectness,
			boolean outputCount,
			RewritingProcess process) throws Error {
		
		for (int i = 1; i != numberOfTests + 1; i++) {
			Constraint2 constraint = makeConstraint.apply();
			Collection<Expression> literals = new LinkedHashSet<>();
			
			output("\n\nStarting new conjunction");	
			for (int j = 0; constraint != null && j != maxNumberOfLiterals; j++) {
				Expression literal = makeRandomLiteralGivenConstraint.apply(constraint);
				constraint = addLiteralToConstraintAndTest(random, tester, literal, constraint, testCorrectness, literals, constraintTheory, process);
			}
			
			if (outputCount && i % NUMBER_OF_TESTS_TO_INDICATE_ON_CONSOLE == 0) {
				System.out.println("Tested (comparing against brute-force solution) " + i + " examples of " + problemName + " for " + constraintTheory);
			}	
		}
	}

	private static Constraint2 addLiteralToConstraintAndTest(Random random, TestRunner tester, Expression literal, Constraint2 constraint, boolean testCorrectness, Collection<Expression> literals, ConstraintTheory constraintTheory, RewritingProcess process) throws Error {
		output("Constraint is " + constraint);
		output("Adding " + literal + " (literals added so far: " + join(literals, " and ") + ")");
		literals.add(literal);
		Constraint2 newConstraint = constraint.conjoin(literal, process);
		output("New constraint is " + newConstraint);
		if (testCorrectness) {
			tester.runOneTest(random, literals, newConstraint, constraintTheory, process);
		}
		return newConstraint;
	}

	private static void testIncompleteSatisfiability(
			Random random,
			Collection<Expression> literals,
			Constraint2 constraint,
			ConstraintTheory constraintTheory,
			RewritingProcess process)
					throws Error {
		
		if (constraint == null) {
			solverSaysItIsUnsatisfiable(literals, constraintTheory, process);
		}
		else {
			// if constraint is not null, the conjunction of literals may or may not be satisfiable,
			// because solver is incomplete, so in this case we do not check.
			output("SolverUnderAssignment does not know yet if it is satisfiable or not. Current constraint is " + constraint);
		}
	}

	private static void testCompleteSatisfiability(
			Random random,
			Collection<Expression> literals,
			Constraint2 constraint,
			ConstraintTheory constraintTheory,
			RewritingProcess process)
					throws Error {
		
		if (constraint == null) {
			solverSaysItIsUnsatisfiable(literals, constraintTheory, process);
		}
		else {
			solverSaysItIsSatisfiable(literals, constraint, constraintTheory, process);
		}
	}

	/**
	 * @param literals
	 * @param constraint
	 * @param constraintTheory
	 * @param process
	 * @throws Error
	 */
	protected static void solverSaysItIsSatisfiable(Collection<Expression> literals, Constraint2 constraint, ConstraintTheory constraintTheory, RewritingProcess process) throws Error {
		output("SolverUnderAssignment thinks it is satisfiable. Current constraint is " + constraint);	
		Expression literalsConjunction = And.make(literals);
		boolean isUnsatisfiable = ! ConstraintTheoryTester.isSatisfiableByBruteForce(literalsConjunction, constraintTheory, process);;
		if (isUnsatisfiable) {
			String message = join(literals, " and ") + " is unsatisfiable (by brute-force) but " + 
			constraintTheory.getClass().getSimpleName() + " says it is satisfiable. " +
			"Current constraint is " + constraint;
			output(message);
			throw new Error(message);
		}
		else {
			output("Brute-force satisfiability test agrees that it is satisfiable. Constraint is " + constraint);	
		}
	}

	/**
	 * @param literals
	 * @param constraintTheory
	 * @param process
	 * @throws Error
	 */
	protected static void solverSaysItIsUnsatisfiable(Collection<Expression> literals, ConstraintTheory constraintTheory, RewritingProcess process) throws Error {
		output("SolverUnderAssignment thinks it is unsatisfiable.");	
		Expression formula = And.make(literals);
		boolean isSatisfiable = ConstraintTheoryTester.isSatisfiableByBruteForce(formula, constraintTheory, process);
//		Map<Expression, Expression> satisfyingAssignment = getSatisfyingAssignmentByBruteForce(formula, constraintTheory, process);
//		boolean isSatisfiable = satisfyingAssignment != null;
		if (isSatisfiable) {
			String message = join(literals, " and ") + " is satisfiable (by brute-force) but " + 
			constraintTheory.getClass().getSimpleName() + " says it is not. "
//			+ "Satisfying assignment is " + satisfyingAssignment + "."
			;
			output(message);
			throw new Error(message);
		}
		else {
			output("Brute-force satisfiability test agrees that it is unsatisfiable.");	
		}
	}

	/**
	 * Determines whether a formula is satisfiable by adding existential quantifiers for each of its variables
	 * (according to the constraint theory provided) and evaluating it.
	 * @param formula
	 * @param constraintTheory
	 * @param process
	 * @return whether the formula is satisfiable.
	 */
	public static boolean isSatisfiableByBruteForce(Expression formula, ConstraintTheory constraintTheory, RewritingProcess process) {
		Map<String, String> variableNamesAndTypeNamesForTesting = constraintTheory.getVariableNamesAndTypeNamesForTesting();
		Expression quantifiedFormula = formula;
		Collection<Expression> variables = constraintTheory.getVariablesIn(formula, process);
		for (Expression variable : variables) {
			Expression type = parse(variableNamesAndTypeNamesForTesting.get(variable));
			quantifiedFormula = ThereExists.make(IndexExpressions.makeIndexExpression(variable, type), quantifiedFormula);
		}
		process.putGlobalObject(AbstractInterpreter.INTERPRETER_CONTEXTUAL_CONSTRAINT, new CompleteMultiVariableConstraint(constraintTheory));
		Expression evaluation = new BruteForceCommonInterpreter(constraintTheory).apply(quantifiedFormula, process);
		boolean result = evaluation.equals(TRUE);
		return result;
	}

	/**
	 * Given a constraint theory and a number <code>n</code> of single-variable constraint tests,
	 * generates <code>n</code> formulas in the theory
	 * and see if the model counting solver works (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param constraintTheory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param outputCount
	 */
	public static void testModelCountingForSingleVariableConstraints(Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		testModelCountingForSingleVariableConstraints(random, constraintTheory, numberOfTests, maxNumberOfLiterals, true /* correctness test */, outputCount);
	}
	
	private static void testModelCountingForSingleVariableConstraints(
			Random random, ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals, boolean testCorrectness, boolean outputCount) {
		
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		
		Symbol variable = makeSymbol(constraintTheory.pickTestingVariableAtRandom(random));
		
		NullaryFunction<Constraint2> makeConstraint = () -> constraintTheory.makeSingleVariableConstraint(variable, constraintTheory, process);

		Function<Constraint2, Expression> makeRandomLiteral = c -> constraintTheory.makeRandomLiteralOn(((SingleVariableConstraint)c).getVariable().toString(), random, process);

		TestRunner tester = (r, ls, c, cT, p) -> runModelCountingTest(r, variable, ls, c, cT, p);
		
		runAllTests(random, "model counting", tester, constraintTheory, makeConstraint, makeRandomLiteral, numberOfTests, maxNumberOfLiterals, testCorrectness, outputCount, process);
	}

	private static void runModelCountingTest(
			Random random,
			Expression variable,
			Collection<Expression> literals,
			Constraint2 constraint,
			ConstraintTheory constraintTheory,
			RewritingProcess process)
					throws Error {
		
		Expression literalsConjunction = And.make(literals);
		String problemDescription = "model counting of " + literalsConjunction + " for variable " + variable;
		output("Problem: " + problemDescription);

		Simplifier symbolicSolver = (e, p) -> computeModelCountBySolver((SingleVariableConstraint) e, p);
		SingleVariableConstraint singleVariableConstraint = (SingleVariableConstraint) constraint;
		Expression symbolicSolution = symbolicSolver.apply(singleVariableConstraint, process);

		output("Symbolic result: " + symbolicSolution);
		
		if (singleVariableConstraint == null) {
			if (!symbolicSolution.equals(ZERO)) {
				throw new Error("Constraint is contradiction, but symbolic solver does not produce 0, but instead " + symbolicSolution);
			}
		}
		else {
			Expression testingVariable = singleVariableConstraint.getVariable();
			Set<Expression> allVariables = getVariables(singleVariableConstraint, process);
			Collection<Expression> otherVariables = removeFromSetNonDestructively(allVariables, v -> v.equals(testingVariable));
			Function<BruteForceCommonInterpreter, Expression> fromInterpreterWithAssignmentToOtherVariablesToBruteForceSolution =
					interpreterWithAssignmentToOtherVariables
					-> bruteForceModelCounterForVariableGivenInterpreterWithAssignmentToOtherVariables(
							variable, literalsConjunction, interpreterWithAssignmentToOtherVariables, constraintTheory, process);
			testSymbolicVsBruteForceComputationForEachAssignment(constraintTheory, problemDescription, otherVariables, symbolicSolution, fromInterpreterWithAssignmentToOtherVariablesToBruteForceSolution, process);
		}
	}

	private static Expression computeModelCountBySolver(SingleVariableConstraint singleVariableConstraint, RewritingProcess process) {
		Expression symbolicSolution = 
				singleVariableConstraint == null?
						ZERO
						: singleVariableConstraint.modelCount(new CompleteMultiVariableConstraint(singleVariableConstraint.getConstraintTheory()), process);
		return symbolicSolution;
	}

	private static Expression bruteForceModelCounterForVariableGivenInterpreterWithAssignmentToOtherVariables(Expression variable, Expression conjunction, BruteForceCommonInterpreter interpreter, ConstraintTheory constraintTheory, RewritingProcess process) {
		output("Computing model count by brute force of: " + conjunction);
		int modelCount = 0;
		Expression testingVariable = variable;
		AssignmentsIterator testingVariableAssignmentsIterator = new AssignmentsIterator(list(testingVariable), process);
		for (Map<Expression, Expression> testingVariableAssignment : in(testingVariableAssignmentsIterator)) {
			AbstractInterpreter completeInterpreter = interpreter.extendWith(testingVariableAssignment, process);
			process.putGlobalObject(AbstractInterpreter.INTERPRETER_CONTEXTUAL_CONSTRAINT, new CompleteMultiVariableConstraint(constraintTheory));
			Expression value = completeInterpreter.apply(conjunction, process);
			if (value.equals(TRUE)) {
				modelCount++;
			}
			//			output("For " + completeInterpreter.getAssignment() + ",");
			//			output("value is " + value);
			//			output("Model count is " + modelCount + "\n");
		}
		return makeSymbol(modelCount);
	}

	/**
	 * Given a list of problem types, a constraint theory and a number <code>n</code> of single-variable constraint tests,
	 * generates <code>n</code> problems with given body depth (number of levels of if then else expressions)
	 * and checks if {@link SymbolicCommonInterpreterWithLiteralConditioning} works (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param problemTypes
	 * @param constraintTheory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param bodyDepth
	 * @param outputCount
	 */
	public static void testGroupProblemForSingleVariableConstraints(
			Random random,
			GroupProblemType problemType,
			ConstraintTheory constraintTheory,
			int numberOfTests,
			int maxNumberOfLiterals,
			int bodyDepth,
			boolean outputCount) {
		
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		
		NullaryFunction<Constraint2> makeConstraint = () -> constraintTheory.makeSingleVariableConstraint(makeSymbol(constraintTheory.pickTestingVariableAtRandom(random)), constraintTheory, process);
		
		Function<Constraint2, Expression> makeRandomLiteral = c -> constraintTheory.makeRandomLiteralOn(((SingleVariableConstraint)c).getVariable().toString(), random, process);
		
		TestRunner tester = (r, ls, c, cT, p) -> testGroupProblem(r, c, problemType, cT, ls, bodyDepth, p);
		
		String problemName = "quantification of " + problemType.getClass().getSimpleName();
		
		runAllTests(random, problemName, tester, constraintTheory, makeConstraint, makeRandomLiteral, numberOfTests, maxNumberOfLiterals, true /* correctness test */, outputCount, process);
	}
	
	private static void testGroupProblem(
			Random random,
			Constraint2 constraint,
			GroupProblemType problemType,
			ConstraintTheory constraintTheory,
			Collection<Expression> literals,
			int bodyDepth,
			RewritingProcess process)
					throws Error {
		
		SingleVariableConstraint singleVariableConstraint = (SingleVariableConstraint) constraint;
		
		NullaryFunction<Expression> leafGenerator = () -> makeSymbol(random.nextInt(10) - 5);
		Expression body = new RandomConditionalExpressionGenerator(random, constraintTheory, bodyDepth, leafGenerator, process).apply();
		
		testGroupProblem(problemType, singleVariableConstraint, body, process);
	}

	private static void testGroupProblem(
			GroupProblemType problemType,
			SingleVariableConstraint singleVariableConstraint,
			Expression body,
			RewritingProcess process) throws Error {

		if (singleVariableConstraint != null) { // TODO: this would be much more elegant if we did not represent contradictions by null
			Expression index = singleVariableConstraint.getVariable();
			Expression indexType = GrinderUtil.getType(index, process);
			Expression problem = problemType.makeProblemExpression(index, indexType, singleVariableConstraint, body);
			
			String problemDescription = problem.toString();
			output(problemDescription);
			
			ConstraintTheory constraintTheory = singleVariableConstraint.getConstraintTheory();
			SymbolicCommonInterpreterWithLiteralConditioning symbolicInterpreter = new SymbolicCommonInterpreterWithLiteralConditioning(constraintTheory);
			CompleteMultiVariableConstraint trueContextualConstraint = new CompleteMultiVariableConstraint(constraintTheory);
			process.putGlobalObject(SymbolicCommonInterpreterWithLiteralConditioning.INTERPRETER_CONTEXTUAL_CONSTRAINT, trueContextualConstraint);
			Expression symbolicSolution = symbolicInterpreter.apply(problem, process);
			output("Symbolic solution: " + symbolicSolution);

			Collection<Expression> otherVariables = getFreeVariableMinusIndex(singleVariableConstraint, body, process);
			Function<BruteForceCommonInterpreter, Expression> bruteForceSolutionGivenInterpreterWithAssignmentToOtherVariables = i -> i.apply(problem, process);
			testSymbolicVsBruteForceComputationForEachAssignment(
					constraintTheory, problemDescription, otherVariables, symbolicSolution, bruteForceSolutionGivenInterpreterWithAssignmentToOtherVariables, process);
			// A more elegant approach would be to create a "for all free variables : symbolic = problem" expression
			// and solve it by brute force instead of using testSymbolicVsBruteForceComputation
			// which replicates the brute force interpreter to some extent.
			// The reason we do not do this is simply due to the fact that the brute force interpreter would return "false"
			// in case of failure, without indicating which assignment failed, which is very useful for debugging.
			// If interpreters, and in fact the whole framework, provided proofs of its calculations,
			// then we could simply use the more elegant approach.
		}
	}

	/**
	 * @param constraint
	 * @param body
	 * @param process
	 * @return
	 */
	private static Collection<Expression> getFreeVariableMinusIndex(SingleVariableConstraint constraint, Expression body, RewritingProcess process) {
		Expression testingVariable = constraint.getVariable();
		Set<Expression> allVariables = getVariables(constraint, process);
		allVariables.addAll(getVariables(body, process));
		Collection<Expression> freeVariables = removeFromSetNonDestructively(allVariables, v -> v.equals(testingVariable));
		return freeVariables;
	}

	/**
	 * Compares, for each assignment to given free variables, if value of a symbolic solution
	 * is the same as the solution by a brute force solver, given a {@link BruteForceCommonInterpreter} with that same assignment.
	 * @param problemDescription
	 * @param freeVariables
	 * @param symbolicSolution
	 * @param fromInterpreterWithAssignmentToBruteForceSolution
	 * @param process
	 * @throws Error
	 */
	private static void testSymbolicVsBruteForceComputationForEachAssignment(
			ConstraintTheory constraintTheory,
			String problemDescription,
			Collection<Expression> freeVariables,
			Expression symbolicSolution,
			Function<BruteForceCommonInterpreter, Expression> fromInterpreterWithAssignmentToBruteForceSolution,
			RewritingProcess process) throws Error {
		
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(freeVariables, process);
		for (Map<Expression, Expression> assignment : in(assignmentsIterator)) {
			testSymbolicVsBruteForceComputationForAssignment(assignment, constraintTheory, problemDescription, symbolicSolution, fromInterpreterWithAssignmentToBruteForceSolution, process);
		}
	}

	private static void testSymbolicVsBruteForceComputationForAssignment(Map<Expression, Expression> assignment, ConstraintTheory constraintTheory, String problemDescription, Expression symbolicSolution, Function<BruteForceCommonInterpreter, Expression> fromInterpreterWithAssignmentToBruteForceSolution, RewritingProcess process) throws Error {
		BruteForceCommonInterpreter interpreter = new BruteForceCommonInterpreter(constraintTheory, assignment);
		Expression bruteForceResultUnderAssignment = fromInterpreterWithAssignmentToBruteForceSolution.apply(interpreter);
		Expression symbolicResultUnderAssignment = interpreter.apply(symbolicSolution, process);
		output("Under free variables assignment " + assignment);
		output("Symbolic    result becomes " + symbolicResultUnderAssignment);
		output("Brute force result becomes " + bruteForceResultUnderAssignment + "\n");
		if ( ! symbolicResultUnderAssignment.equals(bruteForceResultUnderAssignment)) {
			throw new Error(
					"Failure in testing of " + problemDescription + "\n"
							+ "Symbolic solution: " + symbolicSolution + "\n"
							+ "Under assignment to free variables: " + assignment + "\n"
							+ "Value of symbolic solution      : " + symbolicResultUnderAssignment + "\n"
							+ "Value of brute force computation: " + bruteForceResultUnderAssignment + "\n"
					);
		}
	}
}