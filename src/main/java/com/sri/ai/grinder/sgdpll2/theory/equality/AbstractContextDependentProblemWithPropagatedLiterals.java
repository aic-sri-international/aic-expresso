package com.sri.ai.grinder.sgdpll2.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.list;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblem;
import com.sri.ai.util.Util;

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
	 * The solution to be provided if all propagated literals and splitter DNF are satisfied..
	 */
	protected abstract Expression solutionIfPropagatedLiteralsAndSplittersDNFAreSatisfied();

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
	
		Expression splitter = getFirstNecessaryButUndefinedSplitter(contextualConstraint, process);
		if (splitter != null) {
			return new ItDependsOn(splitter);
		}
		
		SolutionStep result = new Solution(solutionIfPropagatedLiteralsAndSplittersDNFAreSatisfied());
		return result; 
	}

	private Expression getFirstNecessaryButUndefinedSplitter(Constraint contextualConstraint, RewritingProcess process) {
		for (Iterable<Expression> disjunct : Util.<Iterable<Expression>>list()) {
			for (Expression conjunct : disjunct) {
				Expression valueGivenContextualConstraint = getValueGivenContextualConstraint(conjunct, contextualConstraint, process);
				if (valueGivenContextualConstraint == null) {
					return conjunct;
				}
				else if (valueGivenContextualConstraint.equals(FALSE)) {
					break; // disjunct is false, move on to next one
				}
				else {
					// conjunct already satisfied, move on to next conjunct
				}
			}
		}
		return null;
	}

	private Expression getValueGivenContextualConstraint(Expression literal, Constraint contextualConstraint, RewritingProcess process) {
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
	
//	private SolutionStep getStepFromSplittersDNF() {
//		
//	}
}