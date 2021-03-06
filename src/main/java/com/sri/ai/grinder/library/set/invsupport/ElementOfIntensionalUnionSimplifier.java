package com.sri.ai.grinder.library.set.invsupport;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.rewriter.api.Simplifier;

/**
 * <pre>
 * t &isin; &cup;<sub>i &isin; D:C</sub>&Phi;
 * &rarr;
 * &exist; i &isin; D : (C &and; t &isin; &Phi;)
 * </pre>
 * @author oreilly
 *
 */
public class ElementOfIntensionalUnionSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		if (Expressions.hasFunctor(expression, FunctorConstants.IN) && expression.numberOfArguments() == 2) {
			Expression t   = expression.get(0);
			Expression set = expression.get(1);
			if (Sets.isIntensionalUnion(set)) {
				IntensionalSet intensionalSet = (IntensionalSet) set.get(0);
				Expression tElementOfPhi      = Expressions.apply(FunctorConstants.IN, t, intensionalSet.getHead());
				Expression existsBody         = And.make(intensionalSet.getCondition(), tElementOfPhi);
				result = ThereExists.make(intensionalSet.getIndexExpressions(), existsBody);
			}
		}
		
		return result;
	}
}
