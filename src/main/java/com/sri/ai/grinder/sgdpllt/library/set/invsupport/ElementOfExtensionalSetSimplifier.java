package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.Or;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * <pre>
 * t &isin; {t<sub>1</sub>,&hellip;,t<sub>n</sub>}
 * &rarr;
 * (t = t<sub>1</sub>) &or; t &isin; {t<sub>2</sub>,&hellip;,t<sub>n</sub>}
 * </pre>
 * 
 * @author oreilly
 *
 */
public class ElementOfExtensionalSetSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (Expressions.hasFunctor(expression, FunctorConstants.IN) && expression.numberOfArguments() == 2) {
			Expression t   = expression.get(0);
			Expression set = expression.get(1);
			if (Sets.isExtensionalSet(set)) {
				// if empty set
				if (set.numberOfArguments() == 0) {
					result = Expressions.FALSE;
				}
				else if (set.numberOfArguments() == 1) {
					// (t = t<sub>1</sub>)
					result = Equality.make(t, set.get(0));
				}
				else { // > 1 element to test
					// (t = t<sub>1</sub>)
					Expression tEqualsT1         = Equality.make(t, set.get(0));
					// {t<sub>2</sub>,&hellip;,t<sub>n</sub>}
					Expression tailSet           = ExtensionalSets.makeOfSameTypeAs(set, set.getArguments().subList(1, set.numberOfArguments()));
					// t &isin; {t<sub>2</sub>,&hellip;,t<sub>n</sub>}
					Expression tElementOfTailSet = Expressions.apply(FunctorConstants.IN, t, tailSet);
					// (t = t<sub>1</sub>) &or; t &isin; {t<sub>2</sub>,&hellip;,t<sub>n</sub>}
					result = Or.make(tEqualsT1, tElementOfTailSet);
				}
			}
		}
		
		return result;
	}
}
