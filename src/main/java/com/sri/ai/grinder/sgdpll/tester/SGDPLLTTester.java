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
package com.sri.ai.grinder.sgdpll.tester;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.getVariableReferences;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.pickKElementsWithoutReplacement;
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
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.Theory;
import com.sri.ai.grinder.sgdpll.api.GroupProblemType;
import com.sri.ai.grinder.sgdpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.constraint.CompleteMultiVariableContext;
import com.sri.ai.grinder.sgdpll.core.constraint.DefaultMultiVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.solver.Evaluator;
import com.sri.ai.grinder.sgdpll.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.sgdpll.interpreter.SymbolicCommonInterpreterWithLiteralConditioning;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.util.base.NullaryFunction;

/**
 * A class for testing SGDPLL(T) main components.
 * 
 * @author braz
 *
 */
@Beta
public class SGDPLLTTester {
	
	private static final int NUMBER_OF_TESTS_TO_INDICATE_ON_CONSOLE = 1;

	private static void output(String message) {
		// System.out.println(message); // uncomment out if detailed output is desired.
	}

	/**
	 * Measures the time taken to run a given number of single-variable constraint tests with a maximum number of literals,
	 * without brute-force correctness testing.
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param theory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param outputCount
	 */
	public static long measureTimeSingleVariableConstraints(Random random, Theory theory, long numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		
		long start = System.currentTimeMillis();
		testSingleVariableConstraints(random, false, theory, numberOfTests, maxNumberOfLiterals, false /* no correctness test */);
		long result = System.currentTimeMillis() - start;
		if (outputCount) {
			System.out.println("Total time: " + result + " ms.");
		}	
		return result;
	}

	/**
	 * Given a theory and a number <code>n</code> of single-variable constraint tests,
	 * generates <code>n</code> formulas in the theory
	 * and see if those detected as unsatisfiable by the corresponding solver
	 * are indeed unsatisfiable (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param theory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param outputCount
	 */
	public static void testSingleVariableConstraints(
			Random random, boolean testAgainstBruteForce, Theory theory, long numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		
		Context context = theory.makeContextWithTestingInformation();
		
		NullaryFunction<Constraint> makeInitialConstraint = () -> theory.makeSingleVariableConstraint(makeSymbol(theory.pickTestingVariableAtRandom(random)), theory, context);

		Function<Constraint, Expression> makeRandomLiteral = c -> theory.makeRandomLiteralOn(((SingleVariableConstraint)c).getVariable().toString(), random, context);

		boolean isComplete = theory.singleVariableConstraintIsCompleteWithRespectToItsVariable();

		TestRunner tester = isComplete? SGDPLLTTester::testCompleteSatisfiability : SGDPLLTTester::testIncompleteSatisfiability;
		
		String problemName = (isComplete? "complete" : "incomplete") + " satisfiability for single-variable constraints";
		
		runTesterGivenConjunctionsOfLiterals(random, problemName, tester, numberOfTests, maxNumberOfLiterals, testAgainstBruteForce, theory, makeInitialConstraint, makeRandomLiteral, outputCount, context);
	}

	/**
	 * Given a theory and a number <code>n</code> of multi-variable constraint tests,
	 * generates <code>n</code> formulas in the theory
	 * and see if those detected as unsatisfiable by the corresponding solver
	 * are indeed unsatisfiable (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param theory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param outputCount
	 */
	public static void testMultiVariableConstraints(
			Random random, boolean testAgainstBruteForce, Theory theory, long numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		
		NullaryFunction<Constraint> makeInitialConstraint = () -> new DefaultMultiVariableConstraint(theory);

		Context context = theory.makeContextWithTestingInformation();
		
		Function<Constraint, Expression> makeRandomLiteral = c -> theory.makeRandomLiteral(random, context);

		TestRunner tester = SGDPLLTTester::testIncompleteSatisfiability; // DefaultMultiVariableConstraint is incomplete
		
		runTesterGivenConjunctionsOfLiterals(random, "incomplete satisfiability", tester, numberOfTests, maxNumberOfLiterals, testAgainstBruteForce, theory, makeInitialConstraint, makeRandomLiteral, outputCount, context);
	}

	/**
	 * Given a theory and a number <code>n</code> of multi-variable constraint tests,
	 * generates <code>n</code> formulas in the theory
	 * and see if those detected as unsatisfiable by the corresponding solver
	 * are indeed unsatisfiable (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param theory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param outputCount
	 */
	public static void testCompleteMultiVariableConstraints(
			Random random, boolean testAgainstBruteForce, Theory theory, long numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		
		Context context = theory.makeContextWithTestingInformation();
		
		NullaryFunction<Constraint> makeInitialConstraint = () -> new CompleteMultiVariableContext(theory, context);

		Function<Constraint, Expression> makeRandomLiteral = c -> theory.makeRandomLiteral(random, context);

		TestRunner tester = SGDPLLTTester::testCompleteSatisfiability; // CompleteMultiVariableContext is complete
		
		runTesterGivenConjunctionsOfLiterals(random, "complete satisfiability", tester, numberOfTests, maxNumberOfLiterals, testAgainstBruteForce, theory, makeInitialConstraint, makeRandomLiteral, outputCount, context);
	}

	private static interface TestRunner {
		void runOneTest(Random random, Collection<Expression> literals, Constraint constraint, boolean testAgainstBruteForce, Theory theory, Context context) throws Error;		
	}

	/**
	 * Generates a number of tests based on a conjunction each, which is formed by incrementally adding literals to it.
	 * The conjunction is provided to a {@link TestRunner} which runs some test based on it.
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param tester the {@link TestRunner}
	 * @param numberOfTests the number of tests to run
	 * @param maxNumberOfLiterals the maximum number of literals to add to each test conjunction
	 * @param theory
	 * @param makeInitialConstraint a thunk generating new constraints equivalent to TRUE
	 * @param makeRandomLiteralGivenConstraint a function generating appropriate new literals (given generated constraint if needed)
	 * @param outputCount whether to output the test count
	 * @param context a context
	 * @throws Error
	 */
	public static void runTesterGivenConjunctionsOfLiterals(
			Random random,
			String problemName,
			TestRunner tester,
			long numberOfTests,
			int maxNumberOfLiterals,
			boolean testAgainstBruteForce,
			Theory theory,
			NullaryFunction<Constraint> makeInitialConstraint,
			Function<Constraint, Expression> makeRandomLiteralGivenConstraint,
			boolean outputCount,
			Context context) throws Error {
		
		for (int i = 1; i != numberOfTests + 1; i++) {
			Constraint constraint = makeInitialConstraint.apply();
			Collection<Expression> literals = new LinkedHashSet<>();
			
			output("\n\nStarting new conjunction");	
			for (int j = 0; !constraint.isContradiction() && j != maxNumberOfLiterals; j++) {
				Expression literal = makeRandomLiteralGivenConstraint.apply(constraint);
				constraint = addLiteralToConstraintAndTest(random, tester, literal, constraint, literals, testAgainstBruteForce, theory, context);
			}
			
			if (outputCount && i % NUMBER_OF_TESTS_TO_INDICATE_ON_CONSOLE == 0) {
				if (testAgainstBruteForce) {
				System.out.println("Tested (comparing against brute-force solution) " + i + " examples of " + problemName + " for " + theory);
				}
				else {
					System.out.println("Computed (without comparing against brute-force solution) " + i + " examples of " + problemName + " for " + theory);
				}
			}	
		}
	}

	private static Constraint addLiteralToConstraintAndTest(Random random, TestRunner tester, Expression literal, Constraint constraint, Collection<Expression> literals, boolean testAgainstBruteForce, Theory theory, Context context) throws Error {
		output("Constraint is " + constraint);
		output("Adding " + literal + " (literals added so far: " + join(literals, " and ") + ")");
		literals.add(literal);
		Constraint newConstraint = constraint.conjoin(literal, context);
		output("New constraint is " + newConstraint);
		tester.runOneTest(random, literals, newConstraint, testAgainstBruteForce, theory, context);
		return newConstraint;
	}

	private static void testIncompleteSatisfiability(
			Random random,
			Collection<Expression> literals,
			Constraint constraint,
			boolean testAgainstBruteForce,
			Theory theory,
			Context context)
					throws Error {
		
		if (constraint.isContradiction()) {
			solverSaysItIsUnsatisfiable(literals, testAgainstBruteForce, theory, context);
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
			Constraint constraint,
			boolean testAgainstBruteForce,
			Theory theory,
			Context context)
					throws Error {
		
		if (constraint.isContradiction()) {
			solverSaysItIsUnsatisfiable(literals, testAgainstBruteForce, theory, context);
		}
		else {
			solverSaysItIsSatisfiable(literals, constraint, testAgainstBruteForce, theory, context);
		}
	}

	/**
	 * @param literals
	 * @param constraint
	 * @param theory
	 * @param context
	 * @throws Error
	 */
	protected static void solverSaysItIsSatisfiable(Collection<Expression> literals, Constraint constraint, boolean testAgainstBruteForce, Theory theory, Context context) throws Error {
		if (testAgainstBruteForce) {
			output("SolverUnderAssignment thinks it is satisfiable. Current constraint is " + constraint);	
			Expression literalsConjunction = And.make(literals);
			boolean isUnsatisfiable = ! SGDPLLTTester.isSatisfiableByBruteForce(literalsConjunction, theory, context);;
			if (isUnsatisfiable) {
				String message = join(literals, " and ") + " is unsatisfiable (by brute-force) but " + 
						theory.getClass().getSimpleName() + " says it is satisfiable. " +
						"Current constraint is " + constraint;
				output(message);
				throw new Error(message);
			}
			else {
				output("Brute-force satisfiability test agrees that it is satisfiable. Constraint is " + constraint);	
			}
		}
		else {
			output("Skipping test againt brute-force.");
		}
	}

	/**
	 * @param literals
	 * @param theory
	 * @param context
	 * @throws Error
	 */
	protected static void solverSaysItIsUnsatisfiable(Collection<Expression> literals, boolean testAgainstBruteForce, Theory theory, Context context) throws Error {
		if (testAgainstBruteForce) {
			output("SolverUnderAssignment thinks it is unsatisfiable.");	
			Expression formula = And.make(literals);
			boolean isSatisfiable = SGDPLLTTester.isSatisfiableByBruteForce(formula, theory, context);
			if (isSatisfiable) {
				String message = join(literals, " and ") + " is satisfiable (by brute-force) but " + 
						theory.getClass().getSimpleName() + " says it is not. "
						//			+ "Satisfying assignment is " + satisfyingAssignment + "."
						;
				output(message);
				throw new Error(message);
			}
			else {
				output("Brute-force satisfiability test agrees that it is unsatisfiable.");	
			}
		}
		else {
			output("Skipping test againt brute-force.");
		}
	}

	/**
	 * Determines whether a formula is satisfiable by adding existential quantifiers for each of its variables
	 * (according to the theory provided) and evaluating it.
	 * @param formula
	 * @param theory
	 * @param context
	 * @return whether the formula is satisfiable.
	 */
	public static boolean isSatisfiableByBruteForce(Expression formula, Theory theory, Context context) {
		Map<String, Type> variableNamesAndTypesForTesting = theory.getVariableNamesAndTypesForTesting();
		Expression quantifiedFormula = formula;
		Collection<Expression> variables = theory.getVariablesIn(formula, context);
		for (Expression variable : variables) {
			Expression typeNameExpression = parse(variableNamesAndTypesForTesting.get(variable).toString());
			quantifiedFormula = ThereExists.make(IndexExpressions.makeIndexExpression(variable, typeNameExpression), quantifiedFormula);
		}
		Expression evaluation = new BruteForceCommonInterpreter().apply(quantifiedFormula, context);
		boolean result = evaluation.equals(TRUE);
		return result;
	}

	/**
	 * Given a theory and a number <code>n</code> of single-variable constraint tests,
	 * generates <code>n</code> formulas in the theory
	 * and see if the model counting solver works (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param theory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param outputCount
	 */
	public static void testModelCountingForSingleVariableConstraints(
			Random random, boolean testAgainstBruteForce, Theory theory, long numberOfTests, int maxNumberOfLiterals, boolean outputCount) {
		
		Context context = theory.makeContextWithTestingInformation();
		
		Symbol variable = makeSymbol(theory.pickTestingVariableAtRandom(random));
		
		NullaryFunction<Constraint> makeInitialConstraint = () -> theory.makeSingleVariableConstraint(variable, theory, context);

		Function<Constraint, Expression> makeRandomLiteral = c -> theory.makeRandomLiteralOn(((SingleVariableConstraint)c).getVariable().toString(), random, context);

		TestRunner tester = (r, ls, c, tB, cT, p) -> runModelCountingTestForSingleVariableConstraint(r, variable, ls, c, tB, cT, p);
		
		runTesterGivenConjunctionsOfLiterals(random, "model counting", tester, numberOfTests, maxNumberOfLiterals, testAgainstBruteForce, theory, makeInitialConstraint, makeRandomLiteral, outputCount, context);
	}

	private static void runModelCountingTestForSingleVariableConstraint(
			Random random,
			Expression variable,
			Collection<Expression> literals,
			Constraint constraint,
			boolean testAgainstBruteForce,
			Theory theory,
			Context context) {
		
		Expression literalsConjunction = And.make(literals);
		String problemDescription = "model counting of " + literalsConjunction + " for variable " + variable;
		output("Problem: " + problemDescription);

		Simplifier symbolicSolver = (e, p) -> computeModelCountBySolver((SingleVariableConstraint) e, p);
		SingleVariableConstraint singleVariableConstraint = (SingleVariableConstraint) constraint;
		Expression symbolicSolution = symbolicSolver.apply(singleVariableConstraint, context);

		output("Symbolic result: " + symbolicSolution);
		
		if (testAgainstBruteForce) {
			if (singleVariableConstraint.isContradiction()) {
				if (!symbolicSolution.equals(ZERO)) {
					throw new Error("Constraint is contradiction, but symbolic solver does not produce 0, but instead " + symbolicSolution);
				}
			}
			else {
				Expression testingVariable = singleVariableConstraint.getVariable();
				Set<Expression> allVariables = getVariableReferences(singleVariableConstraint, context);
				Collection<Expression> otherVariables = removeFromSetNonDestructively(allVariables, v -> v.equals(testingVariable));
				Function<BruteForceCommonInterpreter, Expression> fromInterpreterWithAssignmentToOtherVariablesToBruteForceSolution =
						interpreterWithAssignmentToOtherVariables
						-> bruteForceModelCounterForVariableGivenInterpreterWithAssignmentToOtherVariables(
								variable, literalsConjunction, interpreterWithAssignmentToOtherVariables, theory, context);
						testSymbolicVsBruteForceComputationForEachAssignment(theory, problemDescription, otherVariables, symbolicSolution, fromInterpreterWithAssignmentToOtherVariablesToBruteForceSolution, context);
			}
		}
		else {
			output("Skipping test againt brute-force.");
		}
	}

	private static Expression computeModelCountBySolver(SingleVariableConstraint singleVariableConstraint, Context context) {
		Expression symbolicSolution = 
				singleVariableConstraint.isContradiction()?
						ZERO
						: singleVariableConstraint.modelCount(context);
		return symbolicSolution;
	}

	private static Expression bruteForceModelCounterForVariableGivenInterpreterWithAssignmentToOtherVariables(Expression variable, Expression conjunction, BruteForceCommonInterpreter interpreter, Theory theory, Context context) {
		output("Computing model count by brute force of: " + conjunction);
		int modelCount = 0;
		Expression testingVariable = variable;
		AssignmentsIterator testingVariableAssignmentsIterator = new AssignmentsIterator(list(testingVariable), context);
		for (Map<Expression, Expression> testingVariableAssignment : in(testingVariableAssignmentsIterator)) {
			Simplifier completeInterpreter = interpreter.extendWith(testingVariableAssignment, context);
			Expression value = completeInterpreter.apply(conjunction, context);
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
	 * Given a list of problem types, a theory and a number <code>n</code> of single-variable constraint tests,
	 * generates <code>n</code> problems with given body depth (number of levels of if then else expressions)
	 * and checks if {@link SymbolicCommonInterpreterWithLiteralConditioning} works (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param problemTypes
	 * @param theory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param bodyDepth
	 * @param outputCount
	 */
	public static void testGroupProblemSolvingForSingleVariableConstraints(
			Random random,
			boolean testAgainstBruteForce,
			GroupProblemType problemType,
			Theory theory,
			long numberOfTests,
			int maxNumberOfLiterals,
			int bodyDepth,
			boolean outputCount) {
		
		String problemName = "quantification of " + problemType.getClass().getSimpleName() + " with single index";
		TestRunner tester = (r, ls, c, tB, cT, p) -> runGroupProblemSolvingTestForSingleVariableConstraint(r, c, problemType, tB, cT, ls, bodyDepth, p);
		runGroupProblemSolvingTest(random, problemName, tester, testAgainstBruteForce, problemType, theory, numberOfTests, maxNumberOfLiterals, outputCount);
	}

	private static void runGroupProblemSolvingTestForSingleVariableConstraint(
			Random random,
			Constraint constraint,
			GroupProblemType problemType,
			boolean testAgainstBruteForce,
			Theory theory,
			Collection<Expression> literals,
			int bodyDepth,
			Context context) {
		
		SingleVariableConstraint singleVariableConstraint = (SingleVariableConstraint) constraint;
		Expression index = singleVariableConstraint.getVariable();
		runGroupProblemSolvingTest(random, list(index), constraint, problemType, testAgainstBruteForce, theory, bodyDepth, context);
	}

	/**
	 * Given a list of problem types, a theory and a number <code>n</code> of tests,
	 * generates <code>n</code> problems with given number of indices and body depth (number of levels of if then else expressions)
	 * and checks if {@link SymbolicCommonInterpreterWithLiteralConditioning} works (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * @param random
	 * @param numberOfIndices
	 * @param problemTypes
	 * @param theory
	 * @param numberOfTests
	 * @param maxNumberOfLiterals
	 * @param bodyDepth
	 * @param outputCount
	 */
	public static void testGroupProblemSolvingForMultipleIndices(
			Random random,
			int numberOfIndices,
			boolean testAgainstBruteForce,
			GroupProblemType problemType,
			Theory theory,
			long numberOfTests,
			int maxNumberOfLiterals,
			int bodyDepth,
			boolean outputCount) {
		
		String problemName = "quantification of " + problemType.getClass().getSimpleName() + " with " + numberOfIndices + " indices";
		TestRunner tester = (r, ls, c, tB, cT, p) -> runGroupProblemSolvingTestForMultipleIndices(r, numberOfIndices, c, problemType, tB, cT, ls, bodyDepth, p);
		runGroupProblemSolvingTest(random, problemName, tester, testAgainstBruteForce, problemType, theory, numberOfTests, maxNumberOfLiterals, outputCount);
	}

	private static void runGroupProblemSolvingTestForMultipleIndices(
			Random random,
			int numberOfIndices,
			Constraint constraint,
			GroupProblemType problemType,
			boolean testAgainstBruteForce,
			Theory theory,
			Collection<Expression> literals,
			int bodyDepth, Context context) {
		
		Collection<Expression> indices = 
				pickKElementsWithoutReplacement(
						theory.getVariablesForTesting(),
						numberOfIndices,
						random);
		runGroupProblemSolvingTest(random, indices, constraint, problemType, testAgainstBruteForce, theory, bodyDepth, context);
	}

	private static void runGroupProblemSolvingTest(Random random, String problemName, TestRunner tester, boolean testAgainstBruteForce, GroupProblemType problemType, Theory theory, long numberOfTests, int maxNumberOfLiterals, boolean outputCount) throws Error {
		Context context = theory.makeContextWithTestingInformation();
		
		NullaryFunction<Constraint> makeInitialConstraint = () -> theory.makeSingleVariableConstraint(makeSymbol(theory.pickTestingVariableAtRandom(random)), theory, context);
		
		Function<Constraint, Expression> makeRandomLiteral = c -> theory.makeRandomLiteralOn(((SingleVariableConstraint)c).getVariable().toString(), random, context);
		
		runTesterGivenConjunctionsOfLiterals(random, problemName, tester, numberOfTests, maxNumberOfLiterals, testAgainstBruteForce, theory, makeInitialConstraint, makeRandomLiteral, outputCount, context);
	}

	private static void runGroupProblemSolvingTest(Random random, Collection<Expression> indices, Constraint constraint, GroupProblemType problemType, boolean testAgainstBruteForce, Theory theory, int bodyDepth, Context context) throws Error {
		
		Expression body = makeBody(random, problemType, theory, bodyDepth, context);
		Expression problem = makeProblem(indices, constraint, body, problemType, context);
		
		runGroupProblemSolvingTestOnProblem(problem, indices, constraint, body, testAgainstBruteForce, theory, context);
	}

	/**
	 * @param problem
	 * @param indices
	 * @param constraint
	 * @param body
	 * @param testAgainstBruteForce
	 * @param theory
	 * @param context
	 * @throws Error
	 */
	public static void runGroupProblemSolvingTestOnProblem(
			Expression problem, 
			Collection<Expression> indices, 
			Constraint constraint, 
			Expression body, 
			boolean testAgainstBruteForce, 
			Theory theory, 
			Context context) {
		
		Collection<Expression> freeVariables = getFreeVariableMinusIndices(indices, constraint, body, context);
		
		String problemDescription = problem.toString();
		output(problemDescription);
		
		Simplifier symbolicInterpreter =
				new Evaluator(theory);
		
		long start = System.currentTimeMillis();
		Expression symbolicSolution = symbolicInterpreter.apply(problem, context);
		long time = System.currentTimeMillis() - start;
		
		output("Symbolic solution: " + symbolicSolution);
		output("Computed in " + time + " ms");
		
		if (testAgainstBruteForce) {
			Function<BruteForceCommonInterpreter, Expression> 
			bruteForceSolutionGivenInterpreterWithAssignmentToOtherVariables = i -> i.apply(problem, context);
			testSymbolicVsBruteForceComputationForEachAssignment(
					theory, 
					problemDescription, 
					freeVariables, 
					symbolicSolution, 
					bruteForceSolutionGivenInterpreterWithAssignmentToOtherVariables, 
					context);
			// A more elegant approach would be to create a "for all free variables : symbolic = problem" expression
			// and solve it by brute force instead of using testSymbolicVsBruteForceComputation
			// which replicates the brute force interpreter to some extent.
			// The reason we do not do this is simply due to the fact that the brute force interpreter would return "false"
			// in case of failure, without indicating which assignment failed, which is very useful for debugging.
			// If interpreters, and in fact the whole framework, provided proofs of its calculations,
			// then we could simply use the more elegant approach.
		}
		else {
			output("Skipping test againt brute-force.");
		}
	}

	private static Expression makeProblem(Collection<Expression> indices, Constraint constraint, Expression body, GroupProblemType problemType, Context context) {
		Expression problem = body;
		boolean firstIndex = true;
		for (Expression index : indices) {
			Expression indexType = GrinderUtil.getType(index, context);
			Expression constraintOnThisIndex = firstIndex? constraint : TRUE;
			problem = problemType.makeProblemExpression(index, indexType, constraintOnThisIndex, problem);
			firstIndex = false;
		}
		return problem;
	}

	private static Expression makeBody(Random random, GroupProblemType problemType, Theory theory, int bodyDepth, Context context) {
		NullaryFunction<Expression> leafGenerator = () -> problemType.makeRandomConstant(random);
		Expression body = new RandomConditionalExpressionGenerator(random, theory, bodyDepth, leafGenerator, context).apply();
		return body;
	}

	private static Collection<Expression> getFreeVariableMinusIndices(Collection<Expression> indices, Constraint constraint, Expression body, Context context) {
		Set<Expression> allVariables = getVariableReferences(constraint, context);
		allVariables.addAll(getVariableReferences(body, context));
		Collection<Expression> freeVariablesMinusIndex = removeFromSetNonDestructively(allVariables, v -> indices.contains(v));
		return freeVariablesMinusIndex;
	}

	/**
	 * Compares, for each assignment to given free variables, if value of a symbolic solution
	 * is the same as the solution by a brute force solver, given a {@link BruteForceCommonInterpreter} with that same assignment.
	 * @param problemDescription
	 * @param freeVariables
	 * @param symbolicSolution
	 * @param fromInterpreterWithAssignmentToBruteForceSolution
	 * @param context
	 * @throws Error
	 */
	private static void testSymbolicVsBruteForceComputationForEachAssignment(
			Theory theory,
			String problemDescription,
			Collection<Expression> freeVariables,
			Expression symbolicSolution,
			Function<BruteForceCommonInterpreter, Expression> fromInterpreterWithAssignmentToBruteForceSolution,
			Context context) throws Error {
		
		AssignmentsIterator assignmentsIterator = new AssignmentsIterator(freeVariables, context);
		for (Map<Expression, Expression> assignment : in(assignmentsIterator)) {
			testSymbolicVsBruteForceComputationForAssignment(assignment, theory, problemDescription, symbolicSolution, fromInterpreterWithAssignmentToBruteForceSolution, context);
		}
	}

	private static void testSymbolicVsBruteForceComputationForAssignment(Map<Expression, Expression> assignment, Theory theory, String problemDescription, Expression symbolicSolution, Function<BruteForceCommonInterpreter, Expression> fromInterpreterWithAssignmentToBruteForceSolution, Context context) throws Error {
		BruteForceCommonInterpreter interpreter = new BruteForceCommonInterpreter(assignment);
		Expression bruteForceResultUnderAssignment = fromInterpreterWithAssignmentToBruteForceSolution.apply(interpreter);
		Expression symbolicResultUnderAssignment = interpreter.apply(symbolicSolution, context);
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