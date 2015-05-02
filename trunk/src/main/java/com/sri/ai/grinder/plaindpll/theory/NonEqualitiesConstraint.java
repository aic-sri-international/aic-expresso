package com.sri.ai.grinder.plaindpll.theory;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;

/** 
 * A constraint representing a conjunction of non-equality constraints, defined for specializing methods for efficiency purposes. 
 */	
public interface NonEqualitiesConstraint extends Constraint {
	
	@Override
	EqualityConstraintTheory getTheory();
	
	TermTheory getTermTheory();
	
	/**
	 * A more efficient specialization of {@link #directlyImpliesLiteral(Expression, RewritingProcess)} for disequality literals.
	 * @param term1
	 * @param term2
	 * @param process
	 * @return
	 */
	boolean directlyImpliesDisequality(Expression term1, Expression term2, RewritingProcess process);

	/**
	 * A more efficient specialization of {@link #directlyImpliesLiteral(Expression, RewritingProcess)} for non-trivial disequality literals.
	 * @param term1
	 * @param term2
	 * @param process
	 * @return
	 */
	boolean directlyImpliesNonTrivialDisequality(Expression term1, Expression term2, RewritingProcess process);

	/**
	 * A more efficient implementation of {@link #incorporateDisequalityDestructively(Expression, Expression, RewritingProcess)
	 * for disequalities.
	 * @param term1
	 * @param term2
	 * @param process
	 */
	void incorporateDisequalityDestructively(Expression term1, Expression term2, RewritingProcess process);

	/**
	 * Destructively remove the non-equality constraints based on given variable V for when it is the later in choosing order
	 * (constraints between V and others coming after it in choosing order are kept).
	 * @param variable
	 * @return
	 */
	NonEqualitiesConstraintForSingleVariable removeNonEqualitiesForGivenVariableDestructively(Expression variable);
}