package com.sri.ai.grinder.core.solver;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminator;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.rewriter.api.Simplifier;

public class SimplifierForAggregateFunctionOnIntensionalSet implements Simplifier {

	private AssociativeCommutativeGroup group;
	private MultiQuantifierEliminator quantifierEliminator;

	public SimplifierForAggregateFunctionOnIntensionalSet(
			AssociativeCommutativeGroup group,
			MultiQuantifierEliminator quantifierEliminator) {
		
		super();
		this.group = group;
		this.quantifierEliminator = quantifierEliminator;
	}

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		Expression result;
		try {
			Expression firstArgument = expression.get(0);
			if (Sets.isIntensionalMultiSet(firstArgument)) {
				IntensionalSet intensionalSet = (IntensionalSet) firstArgument;
				ExtensionalIndexExpressionsSet indexExpressions = 
						(ExtensionalIndexExpressionsSet) intensionalSet.getIndexExpressions();
				result =
						quantifierEliminator.extendContextAndSolve(
								group,
								indexExpressions,
								intensionalSet.getCondition(),
								intensionalSet.getHead(),
								context);
			}
			else {
				result = expression;
			}
		}
		catch (IllegalArgumentException exception) {
			result = expression;
		}
		return result;
	}
}