package com.sri.ai.grinder.core.solver;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;

public class AssociativeCommutativeGroupOperationApplicationStepSolver implements ExpressionLiteralSplitterStepSolver {
	
	private final AssociativeCommutativeGroup group;
	
	private  ExpressionLiteralSplitterStepSolver[] operandStepSolvers;
	private int currentOperand;
	private Expression accumulatedResult;

	
	public AssociativeCommutativeGroupOperationApplicationStepSolver( AssociativeCommutativeGroup group, 
																	  ExpressionLiteralSplitterStepSolver... operandStepSolvers ) {
		this.group = group;
		this.operandStepSolvers = operandStepSolvers;
		this.currentOperand = 0;
		this.accumulatedResult = group.additiveIdentityElement();
	}
	
	private AssociativeCommutativeGroupOperationApplicationStepSolver(
			AssociativeCommutativeGroup group,
			ExpressionLiteralSplitterStepSolver[] operandStepSolvers, 
			int currentOperand, 
			Expression accumulatedResult) {
		
		this.group = group;
		this.operandStepSolvers = operandStepSolvers;
		this.currentOperand = currentOperand;
		this.operandStepSolvers = operandStepSolvers;
		this.accumulatedResult = accumulatedResult;
	}

	private AssociativeCommutativeGroupOperationApplicationStepSolver makeCopyWithNewOperandStepSolvers(
			ExpressionLiteralSplitterStepSolver[] newOperandStepSolvers) {
		
		return 
				new AssociativeCommutativeGroupOperationApplicationStepSolver(
						group,
						newOperandStepSolvers,
						currentOperand,
						accumulatedResult);
	}

	private AssociativeCommutativeGroupOperationApplicationStepSolver makeCopyWithNewCurrentOperandAndAccumulatedResult(
			int currentOperand, Expression accumulatedResult) {
		
		return 
				new AssociativeCommutativeGroupOperationApplicationStepSolver(
						group,
						operandStepSolvers,
						currentOperand,
						accumulatedResult);
	}

	@Override
	public AssociativeCommutativeGroupOperationApplicationStepSolver clone() {
		AssociativeCommutativeGroupOperationApplicationStepSolver result = null;
		try {
			result = (AssociativeCommutativeGroupOperationApplicationStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}

	@Override
	public Step step(Context context) {
		Step step;
		if (allOperandsHaveBeenProcessed()) { // all operands have been accumulated
			step = makeSolutionStep();
		}
		else {
			step = takeNextStepProcessingOperands(context);
		}
		return step;
	}

	private boolean allOperandsHaveBeenProcessed() {
		return currentOperand >= operandStepSolvers.length;
	}

	private Solution makeSolutionStep() {
		return new Solution(accumulatedResult);
	}

	private Step takeNextStepProcessingOperands(Context context) {
		Step step;
		Step operandStep = takeStepOnCurrentOperand(context);
		if (isSolution(operandStep)) {
			step = makeStepFromProcessingOperandSolution(operandStep, context);
		}
		else {
			step = makeConditionalStep(operandStep, context);
		}
		return step;
	}

	private Step takeStepOnCurrentOperand(Context context) {
		return operandStepSolvers[currentOperand].step(context);
	}

	private Step makeConditionalStep(Step operandStep, Context context) {
		
		ContextSplitting contextSplitting = operandStep.getContextSplittingWhenSplitterIsLiteral();
		
		ExpressionLiteralSplitterStepSolver sequelStepSolverForWhenSplitterIsTrue = 
				makeSequelStepSolverGivenCurrentOperandSequelStepSolver(operandStep.getStepSolverForWhenSplitterIs(true));
		
		ExpressionLiteralSplitterStepSolver sequelStepSolverForWhenSplitterIsFalse = 
				makeSequelStepSolverGivenCurrentOperandSequelStepSolver(operandStep.getStepSolverForWhenSplitterIs(false));
		
		Step conditionalStep = new ItDependsOn( contextSplitting.getLiteral(),
											    contextSplitting,
											    sequelStepSolverForWhenSplitterIsTrue,
											    sequelStepSolverForWhenSplitterIsFalse);
		return conditionalStep;
	}

	private AssociativeCommutativeGroupOperationApplicationStepSolver makeSequelStepSolverGivenCurrentOperandSequelStepSolver(
				ExpressionLiteralSplitterStepSolver currentOperandSequelStepSolver) {
		
		ExpressionLiteralSplitterStepSolver[] operandSequelStepSolvers = 
				makeOperandSequelStepSolversWithCurrentOperandStepSolverUpdatedTo(currentOperandSequelStepSolver);
		
		AssociativeCommutativeGroupOperationApplicationStepSolver sequelStepSolver =
				makeCopyWithNewOperandStepSolvers(operandSequelStepSolvers);
		
		return sequelStepSolver;
	}

	private ExpressionLiteralSplitterStepSolver[] makeOperandSequelStepSolversWithCurrentOperandStepSolverUpdatedTo(
			ExpressionLiteralSplitterStepSolver currentOperandSequelStepSolver) {
		
		ExpressionLiteralSplitterStepSolver[] operandSequelStepSolvers = operandStepSolvers.clone();		
		
		operandSequelStepSolvers[currentOperand] = currentOperandSequelStepSolver;
		
		return operandSequelStepSolvers;
	}

	private boolean isSolution(Step operandStep) {
		return !operandStep.itDepends();
	}

	private Step makeStepFromProcessingOperandSolution(Step operandStep, Context context) {
		Step step;
		Expression operandValue = operandStep.getValue();
		if (operandSolutionShortCircuitsToAdditiveAbsorbingElement(operandValue)) {
			step = makeAdditiveAbsorbingSolutionStep(operandValue);
		}
		else {
			int newCurrentOperand = currentOperand + 1;
			Expression newAccumulatedResult = addOperandValueToAccumulatedResult(operandValue, context);
			ExpressionLiteralSplitterStepSolver next = makeCopyWithNewCurrentOperandAndAccumulatedResult(newCurrentOperand, newAccumulatedResult);
			step = next.step(context);
		}
		return step;
	}

	private Expression addOperandValueToAccumulatedResult(Expression operandValue, Context context) {
		return group.addAndPossiblySolveItDeprecated(accumulatedResult, operandValue, context);
	}

	private Solution makeAdditiveAbsorbingSolutionStep(Expression additiveAbsorbingValue) {
		return new Solution(additiveAbsorbingValue);
	}

	private boolean operandSolutionShortCircuitsToAdditiveAbsorbingElement(Expression operandValue) {
		return group.isAdditiveAbsorbingElement(operandValue);
	}

}
