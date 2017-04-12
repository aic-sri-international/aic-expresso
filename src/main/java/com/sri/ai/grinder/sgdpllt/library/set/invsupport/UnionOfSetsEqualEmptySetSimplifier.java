package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * <pre>
 * S<sub>1</sub> &cup; S<sub>2</sub> = &empty;
 * &rarr;
 * S<sub>1</sub> = &empty; and S<sub>2</sub> = &empty;
 * </pre>
 * 
 * @author oreilly
 *
 */
public class UnionOfSetsEqualEmptySetSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		if (Equality.isEquality(expression) && expression.numberOfArguments() == 2) {
			Expression union    = null;
			Expression emptySet = null;
			for (Expression arg: expression.getArguments()) {
				if (Expressions.hasFunctor(arg, FunctorConstants.UNION)) {
					union = arg;
				}
				else if (Sets.isEmptySet(arg)) {
					emptySet = arg;
				}
			}
			if (union != null && emptySet != null) {
				List<Expression> conjuncts = new ArrayList<>();
				for (Expression unionArg : union.getArguments()) {
					Expression equality = Equality.make(unionArg, emptySet);
					conjuncts.add(equality);
				}
				result = And.make(conjuncts);
			}
		}
		return result;
	}
}
