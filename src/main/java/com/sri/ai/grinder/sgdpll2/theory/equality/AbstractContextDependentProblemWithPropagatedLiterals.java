package com.sri.ai.grinder.sgdpll2.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.iterator;

import java.util.Iterator;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblem;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;

public abstract class AbstractContextDependentProblemWithPropagatedLiterals implements ContextDependentProblem {

	/**
	 * An iterable over the propagated literals from this problem, that is, the necessary
	 * conditions necessary so that the solution for this problem is TRUE.
	 * A propagated literal can be considered a splitter which, if false, renders the whole problem false.
	 * Because they are required, propagated literals are checked first.
	 * @return the propagated literals from this problem.
	 */
	protected abstract Iterable<Expression> propagatedLiterals();
	
	/**
	 * An iterable over a propagated CNF from this problem, that is, the necessary
	 * clauses necessary so that the solution for this problem is TRUE.
	 * Note that we did not have to define {@link #propagatedLiterals()},
	 * and could have instead required propagated literals to be provided as unit clauses in this CNF.
	 * Its definition is for convenience only, since many extensions will only
	 * need to define propagated literals and return an empty propagated CNF.
	 * @return the propagated CNF from this problem.
	 */
	abstract protected Iterable<Iterable<Expression>> getPropagatedCNF(RewritingProcess process);

	/**
	 * The solution to be provided if all propagated literals and splitter DNF are satisfied..
	 */
	protected abstract Expression solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied();

	/**
	 * The solution to be provided if any of the propagated literals is not satisfied by the contextual constraint.
	 * @return The solution to be provided if any of the propagated literals is not satisfied by the contextual constraint.
	 */
	protected abstract Expression solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied();

	@Override
	public SolutionStep step(Constraint contextualConstraint, RewritingProcess process) {
	
		Iterator<Iterable<Expression>> propagatedLiteralsCNF = FunctionIterator.make(propagatedLiterals(), l -> in(iterator(l)));
		
		Iterable<Iterable<Expression>> propagatedCNF =
				in(NestedIterator.<Iterable<Expression>>make(propagatedLiteralsCNF, getPropagatedCNF(process).iterator()));
		
		for (Iterable<Expression> clause : propagatedCNF) {
			boolean clauseIsSatisfied = false;
			for (Expression literal : clause) {
				Expression valueOfLiteralGivenContextualConstraint = getValueOfLiteralGivenContextualConstraint(literal, contextualConstraint, process);
				if (valueOfLiteralGivenContextualConstraint == null) {
					return new ItDependsOn(literal); // necessary but undefined // OPTIMIZATION: instead of returning this, we could look whether some clause is already unsatisfied
				}
				else if (valueOfLiteralGivenContextualConstraint.equals(TRUE)) {
					// clause is true; record literal and move on to next clause
					contextualConstraint = contextualConstraint.conjoin(literal, process);
					clauseIsSatisfied = true;
					break;
				}
				else {
					// literal is false, record that and move on
					Expression literalNegation = contextualConstraint.getConstraintTheory().getLiteralNegation(literal);
					contextualConstraint = contextualConstraint.conjoin(literalNegation, process);
				}
			}
			
			if (!clauseIsSatisfied) {
				// clause is false, so the whole CNF is false
				return new Solution(solutionIfPropagatedLiteralsAndSplittersCNFAreNotSatisfied());
			}
			// else move on to next clause
		}

		SolutionStep result = new Solution(solutionIfPropagatedLiteralsAndSplittersCNFAreSatisfied());
		return result; 
	}

	private Expression getValueOfLiteralGivenContextualConstraint(Expression literal, Constraint contextualConstraint, RewritingProcess process) {
		if (contextualConstraint.contradictoryWith(literal, process)) {
			return FALSE;
		}
		else if (contextualConstraint.implies(literal, process)) {
			return TRUE;
		}
		else {
			return null;
		}
	}
}