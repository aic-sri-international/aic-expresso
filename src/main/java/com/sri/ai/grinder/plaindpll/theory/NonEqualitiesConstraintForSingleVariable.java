package com.sri.ai.grinder.plaindpll.theory;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.ConjunctiveConstraint;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory.EqualityConstraint;
import com.sri.ai.util.base.Pair;

/** Defined for the benefit of {@link EqualityConstraint} outside of it because the latter is a non-static class. */	
public interface NonEqualitiesConstraintForSingleVariable extends ConjunctiveConstraint {

	/**
	 * A copy "constructor".
	 * The reason this is not a real constructor is because the signature would be easily confused with the constructor from variable 
	 * since {@link NonEqualitiesConstraintForSingleVariable} implements {@link Expression}.
	 * @param another
	 * @param parentEqualityConstraint
	 * @return
	 */
	NonEqualitiesConstraintForSingleVariable copy(EqualityConstraint parentEqualityConstraint);
	
	/**
	 * Adds a constraint to this object.
	 * @param functor
	 * @param term
	 * @param process
	 */
	void addNonEqualityConstraintDestructively(String functor, Expression term, RewritingProcess process) throws Contradiction;

	/**
	 * Returns a pair of the variable and non-equality constraints map after updating representatives in their
	 * representation, or <code>null</code> if there are no changes.
	 * @param process
	 * @return
	 * @throws Contradiction
	 */
	Pair<Expression, NonEqualitiesForSingleTerm> updatedTermAndNonEqualitiesPair(RewritingProcess process) throws Contradiction;

	/**
	 * Returns splitters on free variables required to hold for this constraint to hold.
	 * @return
	 */
	List<Expression> getSplittersToBeSatisfied();
	
	/**
	 * Returns splitters on free variables the negations of which are required to hold for this constraint to hold.
	 * @return
	 */
	List<Expression> getSplittersToBeNotSatisfied();
}