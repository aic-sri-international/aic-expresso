package com.sri.ai.grinder.helper;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Registry;

/**
 * A {@link Function<Expression, Expression>} version of looking up an expression's type given a registry.
 */
public class GetType implements Function<Expression, Expression> {

	private Registry registry;

	public GetType(Registry registry) {
		super();
		this.registry = registry;
	}

	@Override
	public Expression apply(Expression input) {
		Expression result = GrinderUtil.getTypeExpressionOfExpression(input, registry);
		return result;
	}
}