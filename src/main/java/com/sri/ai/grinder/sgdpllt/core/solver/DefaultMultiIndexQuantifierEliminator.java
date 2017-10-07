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
		Expression currentNormalizedExpression = context.getTheory().evaluate(bodyWithCondition, context);
		for (int i = indices.size() - 1; i >= 0; i--) {
			TheorySolvedQuantifierEliminationProblem nextProblem = makeProblem(group, indices.get(i), currentNormalizedExpression, context);
			currentNormalizedExpression = nextProblem.solve(context);
		}
		return currentNormalizedExpression;
	}

	private TheorySolvedQuantifierEliminationProblem makeProblem(
			AssociativeCommutativeGroup group, Expression index, Expression body, Context context) {

		return new TheorySolvedQuantifierEliminationProblem(group, index, body, context);
	}
}