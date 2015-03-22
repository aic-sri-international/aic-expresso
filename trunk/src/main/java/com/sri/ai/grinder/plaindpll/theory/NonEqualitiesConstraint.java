package com.sri.ai.grinder.plaindpll.theory;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;

/** 
 * A constraint representing non-equality constraints, defined for specializing methods for efficiency purposes. 
 */	
public interface NonEqualitiesConstraint extends Constraint {
	
	@Override
	EqualityTheory getTheory();
	
	TermTheory getTermTheory();
	
	/**
	 * A more efficient replacement for {@link #directlyImpliesLiteral(Expression, RewritingProcess)} for disequality literals.
	 * @param term1
	 * @param term2
	 * @param process
	 * @return
	 */
	boolean directlyImpliesDisequality(Expression term1, Expression term2, RewritingProcess process);
}