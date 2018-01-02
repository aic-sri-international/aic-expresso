package com.sri.ai.grinder.sgdpllt.core.solver;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.MultiIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;

/**
 * A {@link MultiIndexQuantifierEliminator} implementation that
 * eliminates each quantifier using the context's theory.
 * 
 * @author braz
 *
 */
public class DefaultMultiIndexQuantifierEliminator extends AbstractMultiIndexQuantifierEliminator {

	@Override
	public Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression condition, Expression body, Context context) {
		Expression bodyWithCondition = IfThenElse.make(condition, body, group.additiveIdentityElement());
		Expression eliminationResult = eliminateAllQuantifiers(group, indices, bodyWithCondition, context);
		Expression normalizedResult = makeSureResultIsNormalized(eliminationResult, indices, context);
		return normalizedResult;
	}

	private Expression eliminateAllQuantifiers(AssociativeCommutativeGroup group, List<Expression> indices, Expression bodyWithCondition, Context context) {
		Expression currentExpression = bodyWithCondition;
		for (int i = indices.size() - 1; i >= 0; i--) {
			Expression index = indices.get(i);
			Expression indexType = context.getTypeExpressionOfRegisteredSymbol(index);
			TheorySolvedSingleQuantifierEliminationProblem nextProblem = makeProblem(group, index, indexType, currentExpression, context);
			currentExpression = nextProblem.solve(context);
		}
		return currentExpression;
	}

	private Expression makeSureResultIsNormalized(Expression currentExpression, List<Expression> indices, Context context) {
		Expression result;
		boolean currentExpressionIsAlreadyNormalizedBecauseItResultedFromAQuantifierElimination = !indices.isEmpty();
		if (currentExpressionIsAlreadyNormalizedBecauseItResultedFromAQuantifierElimination) {
			result = currentExpression;
		}
		else {
			result = context.getTheory().evaluate(currentExpression, context);
		}
		return result;
	}

	private TheorySolvedSingleQuantifierEliminationProblem makeProblem(
			AssociativeCommutativeGroup group, Expression index, Expression indexType, Expression body, Context context) {

		return new TheorySolvedSingleQuantifierEliminationProblem(group, index, indexType, body, context);
	}
}