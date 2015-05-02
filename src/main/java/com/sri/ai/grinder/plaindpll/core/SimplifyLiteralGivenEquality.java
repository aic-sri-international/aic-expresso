package com.sri.ai.grinder.plaindpll.core;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;

public class SimplifyLiteralGivenEquality implements Function<Expression, Expression> {
	
	private Expression variable;
	private Expression otherTerm;
	
	public SimplifyLiteralGivenEquality(Expression variable, Expression otherTerm) {
		super();
		this.variable = variable;
		this.otherTerm = otherTerm;
	}

	@Override
	public Expression apply(Expression input) {
		Expression result;
		if (Equality.isEquality(input)) {
			result = Equality.simplifyGivenEquality(input, variable, otherTerm);
		}
		else if (Disequality.isDisequality(input)) {
			result = Disequality.simplifyGivenEquality(input, variable, otherTerm);
		}
		else {
			result = input;
		}
		return result;
	}
}