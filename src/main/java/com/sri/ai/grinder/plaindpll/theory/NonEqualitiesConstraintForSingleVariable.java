package com.sri.ai.grinder.plaindpll.theory;

import java.util.List;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.core.Contradiction;
import com.sri.ai.util.base.Pair;

/** 
 * Represents a conjunction of literals of binary constraint operators between a variable and
 * a set of terms such that distinct terms.
 * This interface is defined for constraints used inside an EqualityTheoryConstraint
 * in addition to equalities themselves.
 * This is a pretty-specific, performance-based interface meant to be used inside TODO: COMPLETE WHEN WRITTEN.
 */	
public interface NonEqualitiesConstraintForSingleVariable extends Constraint {

	NonEqualitiesConstraintForSingleVariable cloneWithNewNonEqualitiesConstraint(NonEqualitiesConstraint nonEqualitiesConstraint);
	
	/**
	 * A more efficient replacement for {@link #directlyImpliesLiteral(Expression, RewritingProcess)} for disequality literals.
	 * @param term1
	 * @param term
	 * @param process
	 * @return
	 */
	boolean directlyImpliesDisequalityOfVariableAnd(Expression term, RewritingProcess process);

	/**
	 * Gets the {@link NonEqualitiesConstraint} containing this constraint.
	 * @param nonEqualitiesConstraint
	 * @return
	 */
	NonEqualitiesConstraint getNonEqualitiesConstraint(NonEqualitiesConstraint nonEqualitiesConstraint);
	
	/**
	 * Returns a pair of the variable and non-equality constraints map after updating representatives
	 * (according to parent constraint) in their
	 * representation, or <code>null</code> if there are no changes.
	 * The reason this method is needed in this interface and not in Constraint in general is that,
	 * by representing only the constraints on a single variable,
	 * their literals may be modified into equivalent ones that cannot be kept within them, but need
	 * to be moved to some other constraint.
	 * @param process
	 * @return
	 * @throws Contradiction
	 */
	Pair<Expression, NonEqualitiesForSingleTerm> updatedTermAndNonEqualitiesPair(
			Function<Expression, Expression> getRepresentative, RewritingProcess process) throws Contradiction;
	
	/**
	 * Assumes the main variable is a free variable, and returns splitters on it required to hold for this constraint to hold.
	 * @return
	 */
	List<Expression> getSplittersToBeSatisfied();
	
	/**
	 * Assumes the main variable is a free variable,
	 * and returns splitters it the negations of which are required to hold for this constraint to hold.
	 * @return
	 */
	List<Expression> getSplittersToBeNotSatisfied();
}