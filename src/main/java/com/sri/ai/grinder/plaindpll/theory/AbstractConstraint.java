package com.sri.ai.grinder.plaindpll.theory;

import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.plaindpll.api.Constraint;

/** An class implementing basic {@link Constraint} functionality. */	
@SuppressWarnings("serial")
public abstract class AbstractConstraint extends AbstractExpressionWrapper implements Constraint {
	
	protected Constraint parentConstraint;
	
	/**
	 * A constraint under which this constraint is embedded,
	 * and the consequences of which should be utilized as much as possible by this constraint;
	 * it should be able to reply to queries from this constraint.
	 */
	protected Constraint getParentConstraint() {
		return parentConstraint;
	}
}