package com.sri.ai.grinder.sgdpllt.core.solver;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
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
			currentNormalizedExpression = eliminateNextQuantifier(group, indices.get(i), currentNormalizedExpression, context);
		}
		return currentNormalizedExpression;
	}

	private Expression eliminateNextQuantifier(AssociativeCommutativeGroup group, Expression index, Expression body, Context context) throws Error {

		Theory theory = context.getTheory();
		
		SingleVariableConstraint trueConstraint = theory.makeSingleVariableConstraint(index, theory, context);

		ExpressionLiteralSplitterStepSolver quantifierEliminatorStepSolver = 
				theory.getSingleVariableConstraintQuantifierEliminatorStepSolver(
						group, trueConstraint, body, context);
		
		if (quantifierEliminatorStepSolver != null) {
			body = quantifierEliminatorStepSolver.solve(context);
		}
		else {
			// cannot eliminate this level, so reconstruct original expression up to this index
			throw new Error("Reconstruction of quantifier not yet eliminable not yet implemented.");
			// once implemented, must return original expression.
		}
		return body;
	}
}
