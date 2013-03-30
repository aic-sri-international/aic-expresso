package com.sri.ai.grinder.core;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.grinder.api.RewritingProcess;

public interface ReplacementFunctionMaker {

	public Function<Expression, Expression>
	apply(
			Expression expression, Function<Expression, Expression> replacementFunction, ExpressionAndContext subExpressionAndContext, RewritingProcess process);

}
