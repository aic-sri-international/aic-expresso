package com.sri.ai.grinder.library.set.invsupport;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.rewriter.api.Simplifier;

/**
 * <pre>
 * (&cup;<sub>i &isin; D:C</sub>&Phi;) = &empty;
 * &rarr;
 * &forall; i &isin; D : (&not;C &or; &Phi; = &empty;) 
 * </pre>
 * 
 * @author oreilly
 *
 */
public class IntensionalUnionEqualToEmptySetSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (Equality.isEquality(expression) && expression.numberOfArguments() == 2) {
			Expression intensionalUnion = null;
			Expression emptySet         = null;
			for (Expression arg : expression.getArguments()) {
				if (Sets.isIntensionalUnion(arg)) {
					intensionalUnion = arg;
				}
				else if (Sets.isEmptySet(arg)) {
					emptySet = arg;
				}
			}
			// (&cup;<sub>i &isin; D:C</sub>&Phi;) = &empty;
			if (intensionalUnion != null && emptySet != null) {
				IntensionalSet intensionalSet = (IntensionalSet) intensionalUnion.get(0);
				Expression C                      = intensionalSet.getCondition();
				Expression notC                   = Not.make(C);
				Expression Phi                    = intensionalSet.getHead();
				Expression notCorPhi              = Or.make(notC, Phi);
				Expression notCorPhiEqualEmptySet = Equality.make(notCorPhi, emptySet);
				
				// &forall; i &isin; D : (&not;C &or; &Phi; = &empty;)
				result = ForAll.make(intensionalSet.getIndexExpressions(), notCorPhiEqualEmptySet);
			}
		}

		return result;
	}
}
