package com.sri.ai.grinder.helper;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * A {@link Function<Expression, Expression>} version of looking up an expression's type given a rewriting process.
 */
public class GetType implements Function<Expression, Expression> {

	private RewritingProcess process;

	public GetType(RewritingProcess process) {
		super();
		this.process = process;
	}

	@Override
	public Expression apply(Expression input) {
		Expression result = GrinderUtil.getType(input, process);
		return result;
	}
}