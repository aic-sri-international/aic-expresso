package com.sri.ai.grinder.helper;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.MultiQuantifierEliminator;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;

public abstract class FallbackQuantifierEliminationStepSolver implements ExpressionLiteralSplitterStepSolver {

	protected SingleQuantifierEliminationProblem problem;
	protected ExpressionLiteralSplitterStepSolver base;

	protected abstract MultiQuantifierEliminator makeFallbackMultiIndexQuantifierEliminator(Context context);

	public FallbackQuantifierEliminationStepSolver(SingleQuantifierEliminationProblem problem, ExpressionLiteralSplitterStepSolver base) {
		super();
		this.problem = problem;
		this.base = base;
	}

	@Override
	public Step step(Context context) {
		Step result;
		try {
			result = base.step(context);
		}
		catch (IllegalArgumentException exception) {
			result = useFallbackInstead(context);
		}
		return result;
	}

	private Step useFallbackInstead(Context context) {
		Expression resultExpression = solveWithFallbackAndReturnExpression(context);
		Step result = new Solution(resultExpression);
		return result;
	}

	private Expression solveWithFallbackAndReturnExpression(Context context) {
		MultiQuantifierEliminator fallbackMultiIndexQuantifierEliminator = makeFallbackMultiIndexQuantifierEliminator(context);
		Expression resultExpression = fallbackMultiIndexQuantifierEliminator.solveSingleIndexQuantifierEliminationProblem(problem, context);
		return resultExpression;
	}

	@Override
	public FallbackQuantifierEliminationStepSolver clone() {
		try {
			FallbackQuantifierEliminationStepSolver result;
			result = (FallbackQuantifierEliminationStepSolver) super.clone();
			return result;
		}
		catch (CloneNotSupportedException exception) {
			throw new RuntimeException(exception);
		}
	}
}