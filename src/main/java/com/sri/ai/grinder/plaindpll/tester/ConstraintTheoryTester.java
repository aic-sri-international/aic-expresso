package com.sri.ai.grinder.plaindpll.tester;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.isSatisfiableByBruteForce;
import static com.sri.ai.util.Util.join;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.plaindpll.theory.EqualityConstraintTheory;
import com.sri.ai.grinder.plaindpll.theory.term.SymbolTermTheory;

/**
 * A class for testing a {@link ConstraintTheory} and its unsatisfiability detection.
 * 
 * @author braz
 *
 */
public class ConstraintTheoryTester {
	
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
		
		for (int i = 0; i != numberOfTests; i++) {
			Expression variable = makeSymbol(constraintTheory.getTestingVariable());
			SingleVariableConstraint constraint = constraintTheory.makeSingleVariableConstraint(variable);
			Collection<Expression> literals = new LinkedHashSet<>();
			
			System.out.println("\nStarting new conjunction");	
			for (int j = 0; constraint != null && j != maxNumberOfLiterals; j++) {
				Expression literal = constraintTheory.makeRandomLiteralOn();
				literals.add(literal);
				System.out.println("Added " + literal + ", current conjunction: " + And.make(literals));	
				constraint = constraint.conjoin(literal, process);
				if (constraint == null) {
					System.out.println("Solver thinks it is unsatisfiable");	
					Expression formula = And.make(literals);
					Map<Expression, Expression> satisfyingAssignment = isSatisfiableByBruteForce(formula, constraintTheory, process);
					if (satisfyingAssignment != null) {
						throwFailure(constraintTheory, literals, satisfyingAssignment);
					}
					else {
						System.out.println("Brute-force satisfiability test agrees that it is unsatisfiable.");	
					}
				}
				else {
					// if constraint is not null, the conjunction of literals may or may not be satisfiable,
					// because solvers are allowed to be incomplete regarding satisfiability,
					// so in this case we do not test either way.
					System.out.println("Solver does not know yet if it is satisfiable or not");	
				}
			}			
		}
	}

	private static void throwFailure(ConstraintTheory constraintTheory, Collection<Expression> literals, Map<Expression, Expression> satisfyingAssignment) throws Error {
		throw new Error(
				join(literals, " and ") + " is satisfiable but " + 
				constraintTheory.getClass().getSimpleName() + "'s says it is not. " +
				"Satisfying assignment is " + satisfyingAssignment);
	}
	
	public static void main(String[] args) {
		test(new EqualityConstraintTheory(new SymbolTermTheory()), 10, 10);
	}
}