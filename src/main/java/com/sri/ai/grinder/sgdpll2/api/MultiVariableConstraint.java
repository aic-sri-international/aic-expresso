package com.sri.ai.grinder.sgdpll2.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * An {@link Constraint} keeping consistency of multiple variables.
 * 
 * @author braz
 *
 */
public interface MultiVariableConstraint extends Constraint {
	
	@Override
	MultiVariableConstraint conjoin(Expression literal, RewritingProcess process);
}