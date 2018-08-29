package com.sri.ai.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.FALSE;

import static com.sri.ai.grinder.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.thenBranch;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.elseBranch;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionStepSolver;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.constraint.ContextSplitting;

public class IfThenElseStepSolver implements ExpressionStepSolver {
	
	ExpressionStepSolver conditionStepSolver;
	Expression condition;
	Expression thenBranch;
	Expression elseBranch;
	
	IfThenElseStepSolver(Expression ifThenElseExpression) {
		conditionStepSolver = null;
		condition = condition(ifThenElseExpression);
		thenBranch = thenBranch(ifThenElseExpression);
		elseBranch = elseBranch(ifThenElseExpression);
	}

	public IfThenElseStepSolver(ExpressionStepSolver conditionStepSolver, Expression condition, Expression thenBranch, Expression elseBranch) {
		this.conditionStepSolver = conditionStepSolver;
		this.condition = condition;
		this.thenBranch = thenBranch;
		this.elseBranch = elseBranch;
	}

	@Override
	public ExpressionStepSolver clone() {
		return new IfThenElseStepSolver(conditionStepSolver, condition, thenBranch, elseBranch);
	}

	@Override
	public Step step(Context context) {
		Step result;
		
		if(conditionExpressionHasNotYetBeenSteppedOver()) {
			Theory theory = context.getTheory();
			conditionStepSolver = theory.makeEvaluatorStepSolver(condition);
		}
		Step conditionStep = conditionStepSolver.step(context);
		
		if(conditionStep.itDepends()) { //condition needs to be simplified further
			result = createIfThenElseStepResultToContinueSteppingOverCondition(conditionStep);
		}
		else { //we have reached a leaf in the condition stepping
			result = createStepResultToProcessClauses(conditionStep, context);
		}
		
		return result;
	}
	
	private Step createStepResultToProcessClauses(Step conditionStep, Context context) {
		Step result;
		
		Theory theory = context.getTheory();
		
		if(conditionStep.getValue().equals(TRUE)) {
			ExpressionStepSolver stepSolverForThenBranch = theory.makeEvaluatorStepSolver(thenBranch);
			result = stepSolverForThenBranch.step(context);
		}
		else { //conditionStep.getValue().equals(FASLE)
			ExpressionStepSolver stepSolverForElseBranch = theory.makeEvaluatorStepSolver(elseBranch);
			result = stepSolverForElseBranch.step(context);
		}
		return result;
	}

	private Step createIfThenElseStepResultToContinueSteppingOverCondition(Step conditionStep) {
		Expression           splitter = conditionStep.getSplitter();
		ContextSplitting     splitting = conditionStep.getContextSplittingWhenSplitterIsLiteral();
		IfThenElseStepSolver continuingIfThenElseSolverForWhenConditionLiteralIsTrue = 
									new IfThenElseStepSolver(conditionStep.getStepSolverForWhenSplitterIsTrue(), 
											splitting.getContextAndLiteral(), thenBranch, elseBranch);
		IfThenElseStepSolver continuingIfThenElseSolverForWhenConditionLiteralIsFalse = 
									new IfThenElseStepSolver(conditionStep.getStepSolverForWhenSplitterIsFalse(), 
											splitting.getContextAndLiteralNegation(), thenBranch, elseBranch);
		
		Step result = new ItDependsOn(splitter, splitting,
				continuingIfThenElseSolverForWhenConditionLiteralIsTrue, continuingIfThenElseSolverForWhenConditionLiteralIsFalse);
		
		return result;
	}

	private boolean conditionExpressionHasNotYetBeenSteppedOver() {
		return conditionStepSolver == null;
	}

}
