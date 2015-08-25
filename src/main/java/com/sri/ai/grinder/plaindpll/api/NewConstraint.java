package com.sri.ai.grinder.plaindpll.api;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.tester.ConstraintTheoryTester;

public interface NewConstraint {

	/**
	 * Returns an {@link ConstraintTheoryTester} representing the conjunction of this constraint and a given literal,
	 * or null if they are contradictory.
	 * @param literal the literal
	 * @param process the rewriting process
	 * @return the application result or <code>null</code> if contradiction.
	 */
	public abstract NewConstraint conjoin(Expression literal, RewritingProcess process);

}