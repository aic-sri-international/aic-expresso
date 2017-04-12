package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

public class ExtensionalSetEqualEmptySetSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		if (Equality.isEquality(expression) && expression.numberOfArguments() == 2) {
			Expression extensionalSet = null;
			Expression emptySet       = null;
			for (Expression arg : expression.getArguments()) {
				if (emptySet == null && Sets.isEmptySet(arg)) {
					emptySet = arg;
				}
				else if (extensionalSet == null && Sets.isExtensionalSet(arg)) {
					extensionalSet = arg;
				}
			}
			if (extensionalSet != null && emptySet != null) {
				if (extensionalSet.numberOfArguments() == 0) {
					result = Expressions.TRUE;
				}
				else {
					result = Expressions.FALSE;
				}
			}
		}
		
		return result;
	}
}
