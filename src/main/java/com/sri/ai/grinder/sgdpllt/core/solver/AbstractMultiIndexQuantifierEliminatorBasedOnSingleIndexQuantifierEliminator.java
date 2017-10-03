package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.util.Util.getLast;

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;
import com.sri.ai.util.base.Pair;

public abstract class AbstractMultiIndexQuantifierEliminatorBasedOnSingleIndexQuantifierEliminator extends AbstractMultiIndexQuantifierEliminator {

	public AbstractMultiIndexQuantifierEliminatorBasedOnSingleIndexQuantifierEliminator() {
		super();
	}

	protected abstract ExpressionLiteralSplitterStepSolver getQuantifierEliminatorStepSolver(
			AssociativeCommutativeGroup group, 
			SingleVariableConstraint indexConstraint, 
			Expression body, 
			Context context);


	@Override
	public Expression solve(AssociativeCommutativeGroup group, List<Expression> indices, Expression indicesCondition, Expression body, Context context) {
		Expression currentBody = body;
		
		int numberOfIndices = indices.size();
		
		if (numberOfIndices == 0) {
			currentBody = IfThenElse.make(indicesCondition, currentBody, group.additiveIdentityElement());
			// Normalize final result.
			ExpressionLiteralSplitterStepSolver evaluator = context.getTheory().makeEvaluatorStepSolver(currentBody);
			currentBody = evaluator.solve(context);
		}
		else {
			// Re-use {@link SingleVariableConstraint} if condition is one.
			// TODO: eventually we want the algorithm to work so that splitters may be entire constraints,
			// if they are found. Then this encoding would become superfluous,
			// and the condition could always be safely encoded in the body, since it would then be picked and re-used.
			// This would also re-use body if it happens to be a constraint.
			Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint =
					encodeConditionAsLastIndexConstraintIfPossibleOrInBodyOtherwise(
							group, indices, indicesCondition, body, context);
			currentBody = bodyAndLastIndexConstraint.first;
			SingleVariableConstraint lastIndexConstraint = bodyAndLastIndexConstraint.second;

			if (lastIndexConstraint.isContradiction()) {
				return group.additiveIdentityElement();
			}

			for (int i = numberOfIndices - 1; i >= 0; i--) { // evaluate from inside out; this may change in the future
				currentBody = eliminateNextQuantifier(i, group, indices, numberOfIndices, lastIndexConstraint, currentBody, context);
			}
		}
		
		return currentBody;
	}

	private Expression eliminateNextQuantifier(int i, AssociativeCommutativeGroup group, List<Expression> indices, int numberOfIndices, SingleVariableConstraint lastIndexConstraint, Expression currentBody, Context context) throws Error {
		Expression index = indices.get(i);
		
		SingleVariableConstraint constraintForThisIndex = makeConstraintForThisIndex(i, index, numberOfIndices, lastIndexConstraint, context);
		
		ExpressionLiteralSplitterStepSolver quantifierEliminatorStepSolver = getQuantifierEliminatorStepSolver(group, constraintForThisIndex, currentBody, context);
		
		if (quantifierEliminatorStepSolver != null) {
			currentBody = quantifierEliminatorStepSolver.solve(context);
		}
		else {
			// cannot eliminate this level, so reconstruct original expression up to this index
			throw new Error("Reconstruction of quantifier not yet eliminable not yet implemented.");
			// once implemented, must return original expression.
		}
		return currentBody;
	}

	private SingleVariableConstraint makeConstraintForThisIndex(int i, Expression index, int numberOfIndices, SingleVariableConstraint lastIndexConstraint, Context context) {
		SingleVariableConstraint constraintForThisIndex;
		if (i == numberOfIndices - 1) {
			constraintForThisIndex = lastIndexConstraint;
		}
		else {
			constraintForThisIndex = context.getTheory().makeSingleVariableConstraint(index, context.getTheory(), context);
		}
		return constraintForThisIndex;
	}

	/**
	 * If condition is an instance of {@link SingleVariableConstraint} or is representable as one
	 * (that is, it is a conjunctive clause),
	 * then make it the constraint to be used for last index, and use original given body.
	 * Otherwise, encode the condition as part of the body,
	 * in the form <code>if condition then body else identityElement</code>.
	 * This is particularly useful when the condition is already a constraint of {@link SingleVariableConstraint},
	 * since it re-used to work put on building it in the first place.
	 * @param group
	 * @param quantifierFreeIndicesCondition
	 * @param quantifierFreeBody
	 * @param context
	 * @param indexExpressions
	 * @return
	 */
	private static Pair<Expression, SingleVariableConstraint>
	encodeConditionAsLastIndexConstraintIfPossibleOrInBodyOtherwise(
			AssociativeCommutativeGroup group,
			List<Expression> indices,
			Expression quantifierFreeIndicesCondition,
			Expression quantifierFreeBody,
			Context context) {
		
		Expression body;
		SingleVariableConstraint lastIndexConstraint = null;
		Expression lastIndex = getLast(indices);
		try {
			body = quantifierFreeBody;
			lastIndexConstraint = SingleVariableConstraint.make(context.getTheory(), lastIndex, quantifierFreeIndicesCondition, context);
			return Pair.make(body, lastIndexConstraint);
		} catch (Error e) {
			// did not work out because condition is not SingleVariableConstraint on last index -- proceed to default case below
		}
		
		// did not work out because condition is not SingleVariableConstraint on last index
		body = IfThenElse.make(quantifierFreeIndicesCondition, quantifierFreeBody, group.additiveIdentityElement());
		lastIndexConstraint = context.getTheory().makeSingleVariableConstraint(lastIndex, context.getTheory(), context);
		if (lastIndexConstraint == null) {
			throw new Error("The current theory does not know how to manipulate constraints on " + lastIndex + " (type " + context.getTypeOfRegisteredSymbol(lastIndex) + ").");
		}
		Pair<Expression, SingleVariableConstraint> bodyAndLastIndexConstraint = Pair.make(body, lastIndexConstraint);
		
		return bodyAndLastIndexConstraint;
	}
}