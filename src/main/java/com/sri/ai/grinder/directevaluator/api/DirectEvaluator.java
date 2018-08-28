package com.sri.ai.grinder.directevaluator.api;

import java.io.InvalidClassException;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.NormalizedExpression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.rewriter.api.Simplifier;

public interface DirectEvaluator extends Simplifier {
	
	NormalizedExpression evaluate(Expression expression, Context context) throws InvalidClassException ;
	
	default Expression applySimplifier(Expression expression, Context context) {
		Expression result = null;
		try {
			result = evaluate(expression, context);
		} catch (InvalidClassException e) {
			e.printStackTrace();
		}
		return result;
	}

}
