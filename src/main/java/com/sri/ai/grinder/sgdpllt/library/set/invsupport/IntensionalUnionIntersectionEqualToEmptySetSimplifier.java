package com.sri.ai.grinder.sgdpllt.library.set.invsupport;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAll;
import com.sri.ai.grinder.sgdpllt.library.boole.Implication;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.library.set.Sets;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * <pre>
 * (&cup;<sub>i &isin; D:C</sub>&Phi;) &cap; (&cup;<sub>i' &isin; D':C'</sub>&Phi;') = &empty;
 * &rarr;
 * &forall; i &isin; D : C &rArr; &forall; i' &isin; D' : C' &rArr;  (&Phi; &cap; &Phi;' = &empty;)
 * </pre>
 * 
 * @author oreilly
 *
 */
public class IntensionalUnionIntersectionEqualToEmptySetSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (Equality.isEquality(expression) && expression.numberOfArguments() == 2) {
			Expression intersection = null;
			Expression emptySet     = null;
			for (Expression arg : expression.getArguments()) {
				if (Expressions.hasFunctor(arg, FunctorConstants.INTERSECTION)) {
					intersection = arg;
				}
				else if (Sets.isEmptySet(arg)) {
					emptySet = arg;
				}
			}
			if (intersection != null && emptySet != null) {
				// All the intersection args need to be intensional unions
				// (&cup;<sub>i &isin; D:C</sub>&Phi;) &cap; (&cup;<sub>i' &isin; D':C'</sub>&Phi;') = &empty;
				if (intersection.getArguments().stream().allMatch(Sets::isIntensionalUnion)) {
					List<Expression> forAlls = new ArrayList<>();
					List<Expression> phis    = new ArrayList<>();
					// &forall; i &isin; D : C &rArr; &forall; i' &isin; D' : C' &rArr;  (&Phi; &cap; &Phi;' = &empty;)
					for (Expression intensionalUnion : intersection.getArguments()) {
						IntensionalSet intensionalSet = (IntensionalSet) intensionalUnion.get(0);
						Expression[] forAllAndPhi     = intensionalSetToForAllAndPhi(intensionalSet, forAlls, context);
						Expression forAll             = forAllAndPhi[0];
						Expression phi                = forAllAndPhi[1];
						
						handleNestedForAlls(forAll, forAlls);
						
						phis.add(phi);
					}
					result = Equality.make(Expressions.apply(FunctorConstants.INTERSECTION, phis), emptySet);
					for (int i = forAlls.size()-1; i >= 0; i--) {
						Expression forAll = forAlls.get(i);
						Expression body   = Implication.make(ForAll.getBody(forAll), result);
						result = ForAll.make(ForAll.getIndexExpression(forAll), body);
					}					
				}
			}
		}

		return result;
	}
	
	private static Expression[] intensionalSetToForAllAndPhi(IntensionalSet intensionalSet, List<Expression> forAllsSoFar, Context context) {
		IntensionalSet saIntensionalSet = standardizeApart(intensionalSet, forAllsSoFar, context);
		
		Expression forAll = ForAll.make(saIntensionalSet.getIndexExpressions(), saIntensionalSet.getCondition());
		Expression phi    = saIntensionalSet.getHead();
		
		Expression[] result = new Expression[] {
				forAll, phi
		};
		return result;
	}
	
	private static IntensionalSet standardizeApart(IntensionalSet intensionalSet, List<Expression> forAllsSoFar, Context context) {
		IntensionalSet result = intensionalSet;
		
		if (forAllsSoFar.size() > 0) {
			// May need to standardize apart
			List<Expression> tupleArgs = new ArrayList<>();
			for (Expression forAll : forAllsSoFar) {
				tupleArgs.add(ForAll.getIndexExpression(forAll));
				tupleArgs.add(ForAll.getBody(forAll));
			}
			Expression tupleOfForAlls        = Expressions.makeTuple(tupleArgs);
			List<Expression> originalIndexes = IndexExpressions.getIndices(intensionalSet.getIndexExpressions());
			List<Expression> saIndexes       = new ArrayList<>();
			for (Expression index : originalIndexes) {
				Expression saIndex = Expressions.primedUntilUnique(index, tupleOfForAlls, context);
				saIndexes.add(saIndex);
			}
			if (!originalIndexes.equals(saIndexes)) {
				// We need to update intensional set with standardized apart indices
				IndexExpressionsSet saIndexExpressionsSet = intensionalSet.getIndexExpressions();
				Expression          saHead                = intensionalSet.getHead();
				Expression          saCondition           = intensionalSet.getCondition();
				Context             intensionalSetContext = context.extendWith(saIndexExpressionsSet);
				for (int i = 0; i < originalIndexes.size(); i++) {
					Expression originalIndex = originalIndexes.get(i);
					Expression saIndex       = saIndexes.get(i);
					if (!originalIndex.equals(saIndex)) {
						saIndexExpressionsSet = saIndexExpressionsSet.replaceSymbol(originalIndex, saIndex, context);
						saHead                = saHead.replaceAllOccurrences(originalIndex, saIndex, intensionalSetContext);
						saCondition           = saCondition.replaceAllOccurrences(originalIndex, saIndex, intensionalSetContext);
					}
				}
				result = (IntensionalSet) IntensionalSet.intensionalSetOfSameKindAs(intensionalSet, saIndexExpressionsSet, saHead, saCondition);
			}
		}
		
		return result;
	}
	
	private static void handleNestedForAlls(Expression forAll, List<Expression> forAlls) {		
		Expression body = ForAll.getBody(forAll);		
		if (ForAll.isForAll(body)) {
			forAlls.add(ForAll.make(ForAll.getIndexExpression(forAll), Expressions.TRUE));
			handleNestedForAlls(body, forAlls);
		}
		else {
			forAlls.add(forAll);
		}
	}
}
