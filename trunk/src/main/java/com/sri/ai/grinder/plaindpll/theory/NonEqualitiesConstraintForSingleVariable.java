package com.sri.ai.grinder.plaindpll.theory;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.ConjunctiveConstraint;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.util.base.Pair;

/** 
 * Represents a conjunction of literals of binary constraint operators between a variable and
 * a set of terms such that distinct terms.
 * This interface is defined for constraints used inside an EqualityConstraint
 * in addition to equalities themselves.
 * TODO: Eventually it should be merged with Constraint so that EqualityConstraint
 * can contain any internal Constraint.
 */	
public interface NonEqualitiesConstraintForSingleVariable extends ConjunctiveConstraint {

	/**
	 * Adds a constraint to this object.
	 * @param functor
	 * @param term
	 * @param process
	 */
	void addNonEqualityConstraintDestructively(String functor, Expression term, RewritingProcess process) throws Contradiction;
	// TODO: should be replaced by incorporate

	/**
	 * Returns a pair of the variable and non-equality constraints map after updating representatives in their
	 * representation, or <code>null</code> if there are no changes.
	 * @param process
	 * @return
	 * @throws Contradiction
	 */
	Pair<Expression, NonEqualitiesForSingleTerm> updatedTermAndNonEqualitiesPair(RewritingProcess process) throws Contradiction;
	// TODO: should be replaced by simplifyGiven
	
	/**
	 * Returns splitters on free variables required to hold for this constraint to hold.
	 * @return
	 */
	List<Expression> getSplittersToBeSatisfied();
	// TODO: should be replaced by pickSplitter
	
	/**
	 * Returns splitters on free variables the negations of which are required to hold for this constraint to hold.
	 * @return
	 */
	List<Expression> getSplittersToBeNotSatisfied();
	// TODO: should be replaced by pickSplitter
}