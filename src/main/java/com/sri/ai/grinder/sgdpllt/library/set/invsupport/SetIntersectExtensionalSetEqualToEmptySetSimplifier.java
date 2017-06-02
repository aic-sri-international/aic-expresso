package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.And;
import com.sri.ai.grinder.sgdpllt.library.boole.Not;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSets;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * <pre>
 * S &cap; {t<sub>1</sub>,&hellip;,t<sub>n</sub>} = &empty;
 * &rarr;
 * (t<sub>1</sub> &notin; S) &and; (S &cap; {t<sub>2</sub>,&hellip;,t<sub>n</sub>} = &empty;) 
 * </pre>
 * 
 * @author oreilly
 *
 */
public class SetIntersectExtensionalSetEqualToEmptySetSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (Equality.isEquality(expression) && expression.numberOfArguments() == 2) {
			Expression intersection = null;
			Expression emptySet     = null;
			for (Expression arg: expression.getArguments()) {
				if (Expressions.hasFunctor(arg, FunctorConstants.INTERSECTION)) {
					intersection = arg;
				}
				else if (Sets.isEmptySet(arg)) {
					emptySet = arg;
				}
			}
			if (intersection != null && intersection.numberOfArguments() == 2 && emptySet != null) {
				Expression extensionalSet = null;
				Expression otherExtensionalOrIntensionalUnionSet = null;
				for (Expression arg : intersection.getArguments()) {
					if (extensionalSet == null && Sets.isExtensionalSet(arg)) {
						extensionalSet = arg;
					}
					else if (Sets.isExtensionalSet(arg) || Sets.isIntensionalUnion(arg)) {
						otherExtensionalOrIntensionalUnionSet = arg;
					}
				}
				// S &cap; {t<sub>1</sub>,&hellip;,t<sub>n</sub>} = &empty;
				if (extensionalSet != null && otherExtensionalOrIntensionalUnionSet != null) {					
					if (extensionalSet.numberOfArguments() == 0) {
						result = Expressions.FALSE;
					}
					else {
						Expression t1 = extensionalSet.get(0);
						Expression S  = otherExtensionalOrIntensionalUnionSet;
						List<Expression> conjuncts = new ArrayList<>();

						// (t<sub>1</sub> &notin; S)
						Expression t1ElementOfS    = Expressions.apply(FunctorConstants.IN, t1, S);
						Expression notT1ElementOfS = Not.make(t1ElementOfS);
						
						conjuncts.add(notT1ElementOfS);
					
						//  (S = &empty;)
						if (extensionalSet.numberOfArguments() > 1) {											
							// (S &cap; {t<sub>2</sub>,&hellip;,t<sub>n</sub>} = &empty;)
							Expression extensionalSetReducedByT1 = ExtensionalSets.makeOfSameTypeAs(extensionalSet, extensionalSet.getArguments().subList(1, extensionalSet.numberOfArguments()));
	
							Expression intersectionReducedByT1 = Expressions.apply(FunctorConstants.INTERSECTION, otherExtensionalOrIntensionalUnionSet, extensionalSetReducedByT1);

							Expression intersectionReducedByT1EqualEmptySet = Equality.make(intersectionReducedByT1, emptySet);
							// (t<sub>1</sub> &notin; S) &and; (S &cap; {t<sub>2</sub>,&hellip;,t<sub>n</sub>} = &empty;)
							conjuncts.add(intersectionReducedByT1EqualEmptySet);
						}
												
						result = And.make(conjuncts);
					}
				}
			}
		}

		return result;
	}
}
