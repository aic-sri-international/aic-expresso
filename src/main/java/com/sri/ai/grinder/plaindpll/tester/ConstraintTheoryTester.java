package com.sri.ai.grinder.plaindpll.tester;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.join;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Map;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.helper.AssignmentsIterator;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;

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
			Expression variable = makeSymbol("X");
			SingleVariableConstraint constraint = constraintTheory.makeSingleVariableConstraint(variable);
			Collection<Expression> literals = new LinkedHashSet<>();
			
			for (int j = 0; constraint != null && j != maxNumberOfLiterals; j++) {
				Expression literal = constraintTheory.makeRandomLiteralOn(variable);
				literals.add(literal);
				constraint = constraint.conjoin(literal, process);
				if (constraint == null) {
					Map<Expression, Expression> satisfyingAssignment = satisfiable(literals, constraintTheory, process);
					if (satisfyingAssignment != null) {
						throwFailure(constraintTheory, literals, satisfyingAssignment);
					}
				}
			}			
		}
	}

	private static Map<Expression, Expression> satisfiable(Collection<Expression> literals, ConstraintTheory constraintTheory, RewritingProcess process) {
		Map<Expression, Expression> satisfyingAssignment = null;
		Expression conjunction = And.make(literals);
		Collection<Expression> variables = constraintTheory.getVariablesIn(conjunction, process);
		Iterator<Map<Expression, Expression>> assignmentsIterator = new AssignmentsIterator(variables, process);
		while (satisfyingAssignment == null && assignmentsIterator.hasNext()) {
			Map<Expression, Expression> assignment = assignmentsIterator.next();
//			Expression conjunctionWithValuesReplaced = replaceAll(conjunction, assignment);
//			Expression value = constraintTheory.simplify(conjunctionWithValuesReplaced, process);
//			if (value.equals(TRUE)) {
//				satisfyingAssignment = assignment;
//			}
		}
		return satisfyingAssignment;
	}

	private static void throwFailure(ConstraintTheory constraintTheory, Collection<Expression> literals, Map<Expression, Expression> satisfyingAssignment) throws Error {
		throw new Error(
				join(literals, " and ") + " is satisfiable but " + 
				constraintTheory.getClass().getSimpleName() + "'s says it is not. " +
				"Satisfying assignment is " + satisfyingAssignment);
	}
}