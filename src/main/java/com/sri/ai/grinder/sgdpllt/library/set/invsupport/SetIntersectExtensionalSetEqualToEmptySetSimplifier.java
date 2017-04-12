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
import com.sri.ai.grinder.sgdpllt.library.set.extensional.ExtensionalSet;
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
			if (intersection != null && emptySet != null) {
				Expression       extensionalSet = null;
				List<Expression> otherSetArgs   = new ArrayList<>();
				for (Expression arg : intersection.getArguments()) {
					if (extensionalSet == null && Sets.isExtensionalSet(arg)) {
						extensionalSet = arg;
					}
					else {
						otherSetArgs.add(arg);
					}
				}
				// S &cap; {t<sub>1</sub>,&hellip;,t<sub>n</sub>} = &empty;
				if (extensionalSet != null && otherSetArgs.size() > 0) {					
					if (extensionalSet.numberOfArguments() == 0) {
						result = Expressions.FALSE;
					}
					else {
						Expression t1 = extensionalSet.get(0);											
						List<Expression> conjuncts = new ArrayList<>();
						for (Expression S : otherSetArgs) {
							// (t<sub>1</sub> &notin; S)
							Expression t1ElementOfS    = Expressions.apply(FunctorConstants.IN, t1, S);
							Expression notT1ElementOfS = Not.make(t1ElementOfS);
							
							conjuncts.add(notT1ElementOfS);
						}
						
						List<Expression> intersectionReducedByT1Args = new ArrayList<>(otherSetArgs);
						//  (S = &empty;)
						if (extensionalSet.numberOfArguments() > 1) {											
							// (S &cap; {t<sub>2</sub>,&hellip;,t<sub>n</sub>} = &empty;)
							Expression extensionalSetReducedByT1 = ExtensionalSet.makeOfSameTypeAs(extensionalSet, extensionalSet.getArguments().subList(1, extensionalSet.numberOfArguments()));
							intersectionReducedByT1Args.add(extensionalSetReducedByT1);
						}
						Expression intersectionReducedByT1;
						if (intersectionReducedByT1Args.size() > 1) {
							intersectionReducedByT1 = Expressions.apply(FunctorConstants.INTERSECTION, intersectionReducedByT1Args);
						}
						else {
							intersectionReducedByT1 = intersectionReducedByT1Args.get(0);
						}
						Expression intersectionReducedByT1EqualEmptySet = Equality.make(intersectionReducedByT1, emptySet);
						
						// (t<sub>1</sub> &notin; S) &and; (S &cap; {t<sub>2</sub>,&hellip;,t<sub>n</sub>} = &empty;)
						conjuncts.add(intersectionReducedByT1EqualEmptySet);
						
						result = And.make(conjuncts);
					}
				}
			}
		}

		return result;
	}
}
