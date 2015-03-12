package com.sri.ai.grinder.plaindpll.theory;

import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.plaindpll.api.Constraint;

/** An class implementing basic {@link Constraint} functionality. */	
@SuppressWarnings("serial")
public abstract class AbstractConstraint extends AbstractExpressionWrapper implements Constraint {
	
	private Constraint parentConstraint;
	
	@Override
	public Constraint getParentConstraint() {
		return parentConstraint;
	}
}