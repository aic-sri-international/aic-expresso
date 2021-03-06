package com.sri.ai.grinder.library.controlflow;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.thenBranch;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.constraint.ContextSplitting;

public class IfThenElseStepSolver implements ExpressionLiteralSplitterStepSolver {
	
	ExpressionLiteralSplitterStepSolver conditionStepSolver;
	Expression condition;
	Expression thenBranch;
	Expression elseBranch;
	
	public IfThenElseStepSolver(Expression ifThenElseExpression) {
		conditionStepSolver = null;
		condition = condition(ifThenElseExpression);
		thenBranch = thenBranch(ifThenElseExpression);
		elseBranch = elseBranch(ifThenElseExpression);
	}

	private IfThenElseStepSolver(ExpressionLiteralSplitterStepSolver conditionStepSolver, Expression condition, Expression thenBranch, Expression elseBranch) {
		this.conditionStepSolver = conditionStepSolver;
		this.condition = condition;
		this.thenBranch = thenBranch;
		this.elseBranch = elseBranch;
	}

	@Override
	public Step step(Context context) {
		Step result;
		
		Step conditionStep = getConditionStepSolver(context).step(context);
		
		if (conditionStep.itDepends()) {
			result = stepWhenConditionHasNotBeenFullyEvaluatedYet(conditionStep);
		}
		else {
			result = stepWhenConditionHasBeenFullyEvaluated(conditionStep, context);
		}
		
		return result;
	}

	private ExpressionLiteralSplitterStepSolver getConditionStepSolver(Context context) {
		if (conditionStepSolver == null) {
			Theory theory = context.getTheory();
			conditionStepSolver = theory.makeEvaluatorStepSolver(condition);
		}
		return conditionStepSolver;
	}
	
	private Step stepWhenConditionHasNotBeenFullyEvaluatedYet(Step conditionStep) {
		
		Expression           splitter = conditionStep.getSplitter();
		ContextSplitting     splitting = conditionStep.getContextSplittingWhenSplitterIsLiteral();
		
		IfThenElseStepSolver sequelStepSolverIfSplitterIsTrue = 
				makeSequelStepSolver(
						conditionStep.getStepSolverForWhenSplitterIs(true), splitting.getContextAndLiteral());
		
		IfThenElseStepSolver sequelStepSolverIfSplitterIsFalse = 
				makeSequelStepSolver(
						conditionStep.getStepSolverForWhenSplitterIs(false), splitting.getContextAndLiteralNegation());
		
		Step result = 
				new ItDependsOn(
						splitter, 
						splitting,
						sequelStepSolverIfSplitterIsTrue, 
						sequelStepSolverIfSplitterIsFalse);
		
		return result;
	}

	private IfThenElseStepSolver makeSequelStepSolver(ExpressionLiteralSplitterStepSolver conditionSequelStepSolver, Context sequelContext) {
		
		IfThenElseStepSolver sequelStepSolver = 
									new IfThenElseStepSolver(
											conditionSequelStepSolver, 
											sequelContext, 
											thenBranch, 
											elseBranch);
		return sequelStepSolver;
	}

	private Step stepWhenConditionHasBeenFullyEvaluated(Step conditionStep, Context context) {
		Step result;
		
		Theory theory = context.getTheory();
		
		if (conditionStep.getValue().equals(TRUE)) {
			ExpressionLiteralSplitterStepSolver stepSolverForThenBranch = theory.makeEvaluatorStepSolver(thenBranch);
			result = stepSolverForThenBranch.step(context);
		}
		else { // conditionStep.getValue().equals(FALSE)
			ExpressionLiteralSplitterStepSolver stepSolverForElseBranch = theory.makeEvaluatorStepSolver(elseBranch);
			result = stepSolverForElseBranch.step(context);
		}
		return result;
	}

	@Override
	public IfThenElseStepSolver clone() {
		return new IfThenElseStepSolver(conditionStepSolver, condition, thenBranch, elseBranch);
	}

}
