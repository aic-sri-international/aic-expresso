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
		//System.out.println(message);
	}

	/**
	 * Given a constraint theory and a number <code>n</code> of tests,
	 * generates <code>n</code> formulas in the theory
	 * and see if those detected as unsatisfiable by the corresponding solver
	 * are indeed unsatisfiable (checked by brute force).
	 * Throws an {@link Error} with the failure description if a test fails.
	 * 
	 * @param constraintTheory
	 */
	public static void test(ConstraintTheory constraintTheory, int numberOfTests, int maxNumberOfLiterals) {
		
		RewritingProcess process = constraintTheory.extendWithTestingInformation(new DefaultRewritingProcess(null));
		constraintTheory.setRandomGenerator(new Random(0));
		
		for (int i = 0; i != numberOfTests; i++) {
			//if (i % 100 == 0) { System.out.println(i + " "); }	
			Expression variable = makeSymbol(constraintTheory.getTestingVariable());
			SingleVariableConstraint constraint = constraintTheory.makeSingleVariableConstraint(variable);
			Collection<Expression> literals = new LinkedHashSet<>();
			
			output("\nStarting new conjunction");	
			for (int j = 0; constraint != null && j != maxNumberOfLiterals; j++) {
				Expression literal = constraintTheory.makeRandomLiteralOn(process);
				literals.add(literal);
				output("Added " + literal + ", current conjunction: " + And.make(literals));	
				constraint = constraint.conjoin(literal, process);
				if (constraint == null) {
					solverSaysItIsUnsatisfiable(literals, constraintTheory, process);
				}
				else if (constraintTheory.singleVariableConstraintIsComplete()) {
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
	}

	/**
	 * @param literals
	 * @param constraint
	 * @param constraintTheory
	 * @param process
	 * @throws Error
	 */
	protected static void solverSaysItIsSatisfiable(Collection<Expression> literals, SingleVariableConstraint constraint, ConstraintTheory constraintTheory, RewritingProcess process) throws Error {
		output("Solver thinks it is satisfiable");	
		Expression formula = And.make(literals);
		Map<Expression, Expression> satisfyingAssignment = isSatisfiableByBruteForce(formula, constraintTheory, process);
		if (satisfyingAssignment == null) {
			throw new Error(
			join(literals, " and ") + " is unsatisfiable (by brute-force) but " + 
			constraintTheory.getClass().getSimpleName() + "'s says it is satisfiable. " +
			"Current constraint is " + constraint.debuggingDescription(process));
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
		output("Solver thinks it is unsatisfiable");	
		Expression formula = And.make(literals);
		Map<Expression, Expression> satisfyingAssignment = isSatisfiableByBruteForce(formula, constraintTheory, process);
		if (satisfyingAssignment != null) {
			throw new Error(
			join(literals, " and ") + " is satisfiable (by brute-force) but " + 
			constraintTheory.getClass().getSimpleName() + "'s says it is not. " +
			"Satisfying assignment is " + satisfyingAssignment + ".");
		}
		else {
			output("Brute-force satisfiability test agrees that it is unsatisfiable.");	
		}
	}
}