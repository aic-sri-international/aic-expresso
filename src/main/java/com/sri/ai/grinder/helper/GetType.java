package com.sri.ai.grinder.helper;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.GlobalRegistry;

/**
 * A {@link Function<Expression, Expression>} version of looking up an expression's type given a context.
 */
public class GetType implements Function<Expression, Expression> {

	private GlobalRegistry context;

	public GetType(GlobalRegistry context) {
		super();
		this.context = context;
	}

	@Override
	public Expression apply(Expression input) {
		Expression result = GrinderUtil.getType(input, context);
		return result;
	}
}