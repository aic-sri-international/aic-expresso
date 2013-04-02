package com.sri.ai.grinder.core;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;

public interface PruningPredicate {
	public boolean apply(Expression expression, Function<Expression, Expression> replacementFunction, RewritingProcess process);
}
