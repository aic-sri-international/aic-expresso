package com.sri.ai.grinder.api;

import com.sri.ai.expresso.api.Expression;

public interface ExpressionEvaluator {
	
	Expression eval(Expression expression, Context context);

}
