package com.sri.ai.grinder.plaindpll.theory;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;

/** 
 * Represents a conjunction of literals of binary constraint operators between a variable and
 * a set of terms such that distinct terms.
 * This interface is defined for constraints used inside an EqualityTheoryConstraint
 * in addition to equalities themselves.
 * This is a pretty-specific, performance-based interface meant to be used inside TODO: COMPLETE WHEN WRITTEN.
 */	
public interface NonEqualitiesConstraintForSingleVariable extends Constraint {

	NonEqualitiesConstraintForSingleVariable cloneWithNewNonEqualitiesConstraint(NonEqualitiesConstraint nonEqualitiesConstraint);

	/** Returns the single variable against which all other terms are constrained. */
	Expression getVariable();
	
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