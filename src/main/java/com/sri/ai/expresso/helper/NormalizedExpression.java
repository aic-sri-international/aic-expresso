package com.sri.ai.expresso.helper;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;

/**
 * Extension of {@link AbstractExpressionWrapper} that also acts as a marker indicating
 * that the wrapped expression has already been simplified/evaluated.
 * <p>
 * By checking for this wrapping when simplifying/evaluating, we can avoid duplicating efforts.
 * 
 * @author Bobak (with guidance from Rodrigo)
 *
 */
public class NormalizedExpression extends AbstractExpressionWrapper {

	//TODO:  Currently, any AbstractExpressionWrapper derived class will be wrapped twice (may want to adjust down the road)
	
	private static final long serialVersionUID = 1L;
	
	private Context context;
	
	protected NormalizedExpression(Expression expression, Context context) {
		super(expression);
		this.context = context;
	}

	@Override
	protected Expression computeInnerExpression() {
		Expression innerExpression = super.getInnerExpression();
		return innerExpression;
	}
	
	public static NormalizedExpression normalize(Expression expression, Context context) {
		Expression evaluatedExpression = context.evaluate(expression);
		NormalizedExpression result = wrapAsNormalized(evaluatedExpression, context);
		return result;
	}
	
	
	// ALLOWS FOR PROGRAMMER ERROR //
	
	public static NormalizedExpression wrapAsNormalized(Expression expression, Context context) {
		NormalizedExpression normalizedExpression;
		if(expression instanceof NormalizedExpression && ((NormalizedExpression) expression).getContext() == context) {
			normalizedExpression = (NormalizedExpression) expression;
		}
		else {
			normalizedExpression = new NormalizedExpression(expression, context);
		}
		return normalizedExpression;
	}
	
	public Context getContext() {
		return context;
	}

}
