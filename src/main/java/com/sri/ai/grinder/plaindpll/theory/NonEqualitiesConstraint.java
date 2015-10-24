package com.sri.ai.grinder.plaindpll.theory;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint1;
import com.sri.ai.grinder.plaindpll.api.TermTheory;

/** 
 * A constraint representing a conjunction of non-equality constraints, defined for specializing methods for efficiency purposes. 
 */	
public interface NonEqualitiesConstraint extends Constraint1 {
	
	@Override
	EqualityConstraintTheory getConstraintTheory();
	
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
	
	/**
	 * Given a function mapping each term either to itself or to another term meant to represent it
	 * (determined, most likely, by a system of equalities somewhere),
	 * apply it to the present constraint, possibly destructively if that means much better performance.
	 * Terms with distinct representatives should not appear in the resulting constraint.
	 * @param getRepresentative
	 * @param process
	 */
	public void updateRepresentativesDestructively(Function<Expression, Expression> getRepresentative, RewritingProcess process);
}