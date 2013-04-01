package com.sri.ai.grinder.core;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;

public interface PruningPredicateMaker {
	public PruningPredicate apply(Expression expression, PruningPredicate pruningPredicate, ExpressionAndContext subExpressionAndContext);
}
