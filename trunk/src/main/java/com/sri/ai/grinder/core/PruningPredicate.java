package com.sri.ai.grinder.core;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;

public interface PruningPredicate {
	public boolean apply(Expression expression, RewritingProcess process);
}
