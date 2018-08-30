package com.sri.ai.grinder.core.constraint;

import static com.sri.ai.util.Util.check;

import java.io.Serializable;

import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.constraint.MultiVariableContextWithCheckedProperty.ContextDependentProblemStepSolverMaker;

public final class CompleteMultiVariableContextDependentProblemStepSolverMaker implements ContextDependentProblemStepSolverMaker, Serializable {
	private static final long serialVersionUID = 1L;
	
	private Theory theory;
	public CompleteMultiVariableContextDependentProblemStepSolverMaker(Theory theory) {
		this.theory = theory;
	}
	
	@Override
	public ExpressionLiteralSplitterStepSolver apply(SingleVariableConstraint constraint, Context context) {
		ExpressionLiteralSplitterStepSolver result = theory.getSingleVariableConstraintSatisfiabilityStepSolver(constraint, context);
		check(result != null,  () -> "No solver present for solving satisfiability of " + constraint.getVariable());
		return result;
	}
}