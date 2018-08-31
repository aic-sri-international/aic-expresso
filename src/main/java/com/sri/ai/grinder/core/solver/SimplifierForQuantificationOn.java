package com.sri.ai.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.TRUE;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.MultiQuantifierEliminator;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.rewriter.api.Simplifier;

public class SimplifierForQuantificationOn implements Simplifier {
	
	public SimplifierForQuantificationOn(AssociativeCommutativeGroup group, MultiQuantifierEliminator quantifierEliminator) {
		super();
		this.group = group;
		this.quantifierEliminator = quantifierEliminator;
	}

	private AssociativeCommutativeGroup group;
	private MultiQuantifierEliminator quantifierEliminator;

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		Expression result;
		try {
			QuantifiedExpressionWithABody quantifiedExpression = (QuantifiedExpressionWithABody) expression;
			Expression body = quantifiedExpression.getBody();
			ExtensionalIndexExpressionsSet indexExpressions = 
					(ExtensionalIndexExpressionsSet) quantifiedExpression.getIndexExpressions();
			// the set is intensional, but not the set of index expressions!
			result = quantifierEliminator.extendContextAndSolve(group, indexExpressions, TRUE, body, context);
		}
		catch (IllegalArgumentException exception) {
			result = expression;
		}
		return result;
	}

}