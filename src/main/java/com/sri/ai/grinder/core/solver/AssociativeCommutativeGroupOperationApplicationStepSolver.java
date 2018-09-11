package com.sri.ai.grinder.core.solver;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
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
	
	public AssociativeCommutativeGroupOperationApplicationStepSolver(AssociativeCommutativeGroup group,
			ExpressionLiteralSplitterStepSolver[] operandStepSolvers, int currentOperand, Expression accumulatedResult) {
		this.group = group;
		this.operandStepSolvers = operandStepSolvers;
		this.currentOperand = currentOperand;
		this.operandStepSolvers = operandStepSolvers;
		this.accumulatedResult = accumulatedResult;
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
		if (allOperandsHaveBeenProcessed()){ //all operands have been accumulated
			step = constructSolutionStep();
		}
		else {
			step = takeNextStepProcessingOperands(context);
		}
		return step;
	}

	private boolean allOperandsHaveBeenProcessed() {
		return currentOperand >= operandStepSolvers.length;
	}

	private Solution constructSolutionStep() {
		return new Solution(accumulatedResult);
	}

	private Step takeNextStepProcessingOperands(Context context) {
		Step step;
		Step operandStep = takeStepOnCurrentOperand(context);
		if (isSolution(operandStep)) {
			step = createStepFromProcessingOperandSolution(operandStep, context);
		}
		else {
			step = constructAnItDependsOnStep(operandStep, context);
		}
		return step;
	}

	private Step constructAnItDependsOnStep(Step operandStep, Context context) {
		ContextSplitting contextSplitting = operandStep.getContextSplittingWhenSplitterIsLiteral();
		ExpressionLiteralSplitterStepSolver operandSequentialStepSolverForWhenSplitterIsTrue = operandStep.getStepSolverForWhenSplitterIsTrue();
		ExpressionLiteralSplitterStepSolver operandSequentialStepSolverForWhenSplitterIsFalse = operandStep.getStepSolverForWhenSplitterIsFalse();
		
		ExpressionLiteralSplitterStepSolver resultingStepSolverForWhenSplitterIsTrue = 
				createResultingStepSolverBasedOnSplittingCaseOfCurrentOperandStep(operandSequentialStepSolverForWhenSplitterIsTrue);
		ExpressionLiteralSplitterStepSolver resultingStepSolverForWhenSplitterIsFalse = 
				createResultingStepSolverBasedOnSplittingCaseOfCurrentOperandStep(operandSequentialStepSolverForWhenSplitterIsFalse);
		
		Step itDependsOnStep = new ItDependsOn( contextSplitting.getLiteral(),
											    contextSplitting,
											    resultingStepSolverForWhenSplitterIsTrue,
											    resultingStepSolverForWhenSplitterIsFalse);
		return itDependsOnStep;
	}

	private AssociativeCommutativeGroupOperationApplicationStepSolver createResultingStepSolverBasedOnSplittingCaseOfCurrentOperandStep(
				ExpressionLiteralSplitterStepSolver operandSequentialStepSolverBasedOnSplittingCaseOfCurrentOperandStep) {
		
		ExpressionLiteralSplitterStepSolver[] operandStepSolversBasedOnSplittingCaseOfCurrentOperandStep = 
				createOperandStepSolversBasedOnSplittingCaseOfCurrentOperandStep(
						operandSequentialStepSolverBasedOnSplittingCaseOfCurrentOperandStep);
		AssociativeCommutativeGroupOperationApplicationStepSolver resultingStepSolver =
				new AssociativeCommutativeGroupOperationApplicationStepSolver( group,
																			   operandStepSolversBasedOnSplittingCaseOfCurrentOperandStep,
																			   currentOperand,
																			   accumulatedResult);
		return resultingStepSolver;
	}
	
	private ExpressionLiteralSplitterStepSolver[] createOperandStepSolversBasedOnSplittingCaseOfCurrentOperandStep(
			ExpressionLiteralSplitterStepSolver sequentialStepSolverBasedOnSplittingCaseOfCurrentOperandStep) {
		
		ExpressionLiteralSplitterStepSolver[] operandStepSolversBasedOnSequentialStepSolverForCurrentOperandSplittingCase =
				operandStepSolvers.clone();		
		operandStepSolversBasedOnSequentialStepSolverForCurrentOperandSplittingCase[currentOperand] = 
				sequentialStepSolverBasedOnSplittingCaseOfCurrentOperandStep;
		
		return operandStepSolversBasedOnSequentialStepSolverForCurrentOperandSplittingCase;
	}

	private boolean isSolution(Step operandStep) {
		return !operandStep.itDepends();
	}

	private Step createStepFromProcessingOperandSolution(Step operandStep, Context context) {
		Step step;
		Expression operandValue = operandStep.getValue();
		if (operandSolutionShortCircuitsToAdditiveAbsorbativeElement(operandValue)) {
			step = createAdditiveAbsorbativeSolutionStep(operandValue);
		}
		else {
			accumulatedResult = addOperandValueToAccumulatedResult(operandValue, context);
			advanceToNextOperand();
			step = this.step(context); //recursively continue taking steps until all operands are processed or an ItDependsStep is reached
		}
		return step;
	}

	private Expression addOperandValueToAccumulatedResult(Expression operandValue, Context context) {
		return group.addAndPossiblySolveItDeprecated(accumulatedResult, operandValue, context);
	}

	private Solution createAdditiveAbsorbativeSolutionStep(Expression additiveAbsorbativeValue) {
		return new Solution(additiveAbsorbativeValue);
	}

	private boolean operandSolutionShortCircuitsToAdditiveAbsorbativeElement(Expression operandValue) {
		return group.isAdditiveAbsorbingElement(operandValue);
	}

	private void advanceToNextOperand() {
		++currentOperand;
	}

	private Step takeStepOnCurrentOperand(Context context) {
		return operandStepSolvers[currentOperand].step(context);
	}

}
