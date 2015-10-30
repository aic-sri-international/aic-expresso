package com.sri.ai.grinder.api;

import java.util.Collection;

import com.sri.ai.expresso.api.Expression;

/**
 * Interface to classes able to eliminate quantification (for a fixed group) over given indices, constraint and body.
 * 
 * @author braz
 *
 */
public interface QuantifierEliminator {

	Expression solve(Collection<Expression> indices, Constraint constraint, Expression body, RewritingProcess process);
	
	Expression solve(Expression input, Collection<Expression> indices, RewritingProcess process);

	void interrupt();

	boolean getDebug();

	void setDebug(boolean newValue);
}