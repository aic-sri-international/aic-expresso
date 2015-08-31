package com.sri.ai.grinder.sgdpll2.theory.equality;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblem;

public abstract class AbstractContextDependentProblemWithPropagatedLiterals implements ContextDependentProblem {

	/**
	 * An iterable over the propagated literals from this problem, that is, the necessary
	 * conditions necessary so that the solution for this problem is TRUE.
	 * A propagated literal can be considered a splitter which, if false, renders the whole problem false.
	 * Because they are required, propagated literals are checked first.
	 * @return
	 */
	protected abstract Iterable<Expression> propagatedLiterals();
	
	/**
	 * The solution to be provided if any of the propagated literals is not satisfied by the contextual constraint.
	 * @return The solution to be provided if any of the propagated literals is not satisfied by the contextual constraint.
	 */
	protected abstract Expression solutionIfPropagatedLiteralsAreNotSatisfied();

	/**
	 * The step solution once it's been determined that the propagated literals are consistent with the contextual constraint.
	 * @return The step solution once it's been determined that the propagated literals are consistent with the contextual constraint.
	 */
	protected abstract SolutionStep stepGivenPropagatedLiteralsAreSatisfied(Constraint contextualConstraint, RewritingProcess process);

	@Override
	public SolutionStep step(Constraint contextualConstraint, RewritingProcess process) {
	
		for (Expression propagatedLiteral : propagatedLiterals()) {
			if (contextualConstraint.contradictoryWith(propagatedLiteral, process)) {
				return new Solution(solutionIfPropagatedLiteralsAreNotSatisfied());
			}
			else if (!contextualConstraint.implies(propagatedLiteral, process)) {
				return new ItDependsOn(propagatedLiteral);
			}
		}
	
		// the contextual constraint guarantees all propagated literals are satisfied, so there is a satisfying value for variable for the given context.
		SolutionStep result = stepGivenPropagatedLiteralsAreSatisfied(contextualConstraint, process);
		return result; 
	}
}