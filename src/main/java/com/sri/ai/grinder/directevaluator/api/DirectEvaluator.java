package com.sri.ai.grinder.directevaluator.api;

import java.io.InvalidClassException;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.NormalizedExpression;
import com.sri.ai.grinder.api.Context;

public interface DirectEvaluator {
	
	NormalizedExpression evaluate(Expression expression, Context context) throws InvalidClassException ;

}
