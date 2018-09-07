/*
 * Copyright (c) 2013, SRI International
 * All rights reserved.
 * Licensed under the The BSD 3-Clause License;
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at:
 * 
 * http://opensource.org/licenses/BSD-3-Clause
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * Neither the name of the aic-expresso nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES 
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package com.sri.ai.grinder.core.solver;

import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.thenBranch;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.println;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explain;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.ExpressionStepSolver;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.api.StepSolver;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.constraint.ConstraintSplitting;
import com.sri.ai.grinder.core.constraint.ContextSplitting;
import com.sri.ai.grinder.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.helper.AssignmentMapsIterator;
import com.sri.ai.grinder.interpreter.Assignment;
import com.sri.ai.grinder.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.rewriter.core.Recursive;

/**
 * An abstract implementation for step solvers for quantified expressions
 * (the quantification being based on an associative commutative group's operation).
 * <p>
 * This is done by applying an evaluator step solver on the body expression,
 * picking literals in it according to the context conjoined with the index constraint,
 * and "intercepting" literals containing the indices and splitting the quantifier
 * based on that, solving the two resulting sub-problems.
 * <p>
 * For example, if we have <code>sum({{ (on X in SomeType) if Y != bob then 2 else 3 | X != john }})</code>
 * under context <code>Z = alice</code>,
 * the evaluator step solver is
 * invoked with context <code>Z = alice and X != john</code>.
 * The solver step will depend on literal <code>Y != bob</code>.
 * <p>
 * If however the quantified expression is
 * <code>sum({{ (on X in SomeType) if X != bob then 2 else 3 | X != john }})</code>,
 * the solver step will not be one depending on a literal, but a definite solution equivalent to
 * <code>sum({{ (on X in SomeType) 2 | X != john and X != bob}}) +
 *       sum({{ (on X in SomeType) 3 | X != john and X = bob}})</code>.
 * <p>
 * Because these two sub-problems have literal-free bodies <code>2</code> and <code>3</code>,
 * they will be solved by the extension's
 * {@link #eliminateQuantifierForLiteralFreeBody(Expression, Context)}
 * (which for sums with constant bodies will be equal to the model count of the index constraint
 * under the context times the constant).
 * <p>
 * Extending classes must define method
 * {@link #eliminateQuantifierForLiteralFreeBody(Expression, Context)
 * to solve the case in which the body is its given literal-free version,
 * for the given context and index constraint.
 * <p>
 * At the time of this writing,
 * {@link Recursive} (on which evaluator step solvers are based)
 * supports only expressions that are composed of
 * function applications or symbols only,
 * so this extension inherits this restriction if that is still in place.
 * <p>
 * Ideally, step solvers should only return literal-free solutions, but
 * this abstract implementation currently may do that.
 * This may (or perhaps should) be changed in the future.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleQuantifierEliminationStepSolver implements SingleQuantifierEliminationStepSolver {

	private SingleQuantifierEliminationProblem problem;
	
	private ExpressionLiteralSplitterStepSolver initialBodyEvaluationStepSolver;
	
	private Context initialContextForBody;
	
	/**
	 * Key for {@link Context} global object indicating whether to compare non-conditional solutions with the result provided by {@link BruteForceCommonInterpreter};
	 * check is done if key is present.
	 */
	public static final String BRUTE_FORCE_CHECKING_OF_NON_CONDITIONAL_PROBLEMS = "Brute force checking of non-conditional problems";

	public AbstractSingleQuantifierEliminationStepSolver(SingleQuantifierEliminationProblem problem) {
		this.problem = problem;
	}

	/**
	 * Abstract method defining a quantified expression with a given index constraint and literal-free body is to be solved.
	 * @param literalFreeBody literal-free body
	 */
	protected abstract Step eliminateQuantifierForLiteralFreeBody(Expression literalFreeBody, Context context);

	private ExpressionLiteralSplitterStepSolver getInitialBodyStepSolver(Theory theory) {
		if (initialBodyEvaluationStepSolver == null) {
			initialBodyEvaluationStepSolver = theory.makeEvaluatorStepSolver(getBody());
		}
		return initialBodyEvaluationStepSolver;
	}

	@Override
	public Step step(Context context) {
		return explanationBlock("Taking step in solving ", problem, " with ", this.getClass().getSimpleName(), code(() -> {

			Step step;

			Context contextForBody = getContextForBody(context);  

			if (contextForBody.isContradiction()) {								//this branch should not be included in the solution since it's context is contradictory
				explain("Context for body is contradictory");
				step = new Solution(getGroup().additiveIdentityElement()); 	//return additive identity solution step so when this solution is combined, it will not have any effect (as if it were not part of the final solution)
			}
			else {
				step = stepOverProblemWithConsistentContextForBody(contextForBody, context);
			}

			bruteForceCheckingOfNonConditionalProblemsIfRequested(step, context);

			return step;

		}), "Step is ", RESULT);
	}

	/**
	 * Returns the context defined for the body of the problem
	 * given the context to step over.
	 * <p>
	 * If initialContextForBody is not already defined, 
	 * then we are taking the very first step.  Thus we must
	 * create an appropriate context by conjoining the context 
	 * to step over with the index constraint of the quantifier.
	 * This context will be returned.
	 * <p>
	 * If initialContextForBody is already defined, then
	 * we are continuing from a previous step and the appropriate
	 * context is already stored in initialContextForBody.
	 * Thus the context in link #initialContextForBody will be
	 * returned.
	 * 
	 * @param context
	 * @return
	 */
	private Context getContextForBody(Context context) {
		return explanationBlock("Making context for body ", code(() -> {

			Context result;
			if (initialContextForBody == null) {
				result = context.conjoin(getIndexConstraint(), context);
			}
			else {
				explain("Continuing with context for body computed in a step solver predecessing this one");
				result = initialContextForBody;
			}
			return result;

		}), "Context for body is ", RESULT);
	}

	private Step stepOverProblemWithConsistentContextForBody(Context contextForBody, Context context) {
		return explanationBlock("Solving problem with consistent context for body", code(() -> {
			
			Step step;
			ExpressionLiteralSplitterStepSolver.Step bodyStep = getBodyStep(contextForBody, context);
			explain("Step for solving body alone is ", bodyStep);
			
			if (bodyStep.itDepends()) {
				step = stepOnConditionalQuantifiedProblem(context, contextForBody, bodyStep);
			}
			else {
				step = solveNonConditionalQuantifiedProblem(bodyStep, context);
			}
			
			return step;
			
		}), "Step is ", RESULT);
	}

	private ExpressionLiteralSplitterStepSolver.Step getBodyStep(Context contextForBody, Context context) {
		return explanationBlock("Determining body step: ", code(() -> {
			ExpressionLiteralSplitterStepSolver bodyStepSolver = getInitialBodyStepSolver(context.getTheory());
			ExpressionLiteralSplitterStepSolver.Step bodyStep = bodyStepSolver.step(contextForBody); 

			// At this point, bodyStep may be a non-conditional solver step
			// that nonetheless contains literals (we will probably prohibit step solvers from returning such "solutions" in the future).
			// If one of these literals is the quantifier index, we *must* detect it.
			// Therefore, we run EvaluatorStepSolver on it to make sure to detect literals before going on.
			//
			// One may ask: if the body is solved using an EvaluatorStepSolver,
			// why is it that running *another* EvaluatorStepSolver on its result will
			// now guarantee that literals are detected?
			// Why do we get the guarantee only when running it a second time?
			// The answer lies in the fact that EvaluatorStepSolver returns solutions with literals
			// because *this* class (which EvaluatorStepSolver uses to eliminate quantifiers)
			// does so. Once all quantifiers are eliminated,
			// EvaluatorStepSolver does no longer return such solutions.
			// The solution to this whole situation is to change *this* class
			// so it does not return solutions with literals any longer.
			// This happens in quantifier splits, when the two sub-solutions
			// are computed with an exhaustive solve,
			// which may return solutions with literals
			// (it is only the step solvers that we want to prevent from doing this,
			// not exhaustive solving).
			// Check (**) in this file to see where this happens
			explain("Body step is ", bodyStep);
			if ( ! bodyStep.itDepends()) {
				explain("Body step is not conditional, but it may still contain literals, so we are going to check now if really there are none");
				ExpressionLiteralSplitterStepSolver evaluatorStepSolver = context.getTheory().makeEvaluatorStepSolver(bodyStep.getValue());
				bodyStep = evaluatorStepSolver.step(context);
				explain("After this check, body step is ", bodyStep);
			}
			return bodyStep;
		}), "Body step is ", RESULT);
	}

	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	////////// METHOD ALTERED FOR TRANSITION TO NEW IMPLEMENTATION ///////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	private Step stepOnConditionalQuantifiedProblem(Context context, Context contextForBody, ExpressionLiteralSplitterStepSolver.Step bodyStep) {
		return explanationBlock("Taking step in solving problem split by splitter from body: ", bodyStep, code(() -> {
			
			Step step;
			if (isSubExpressionOf(getIndex(), bodyStep.getSplitterLiteral())) {
				explain("Splitter contains index, so we are going to split the quantifier");
				step = splitQuantifierOnLiteralContainingIndex(bodyStep, contextForBody, context); //TO BE DEPRECIATED
				//step = stepOverExpressionCombiningSplitQuantifierOnLiteralContainingIndex(bodyStep, contextForBody, context);
			}
			else {
				explain("Splitter does not contains index, so we are going to make a conditional on two new subproblems");
				step = splitOnNonIndexVariable(bodyStep, context);
			}
			return step;
			
		}), "Step is ", RESULT);
	}
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	
	
	//TO BE DEPRECIATED
	protected Step splitQuantifierOnLiteralContainingIndex(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context contextForBody, Context context) {
		// if the splitter contains the index, we must split the quantifier:
		// Quant_x:C Body  --->   (Quant_{x:C and L} Body) op (Quant_{x:C and not L} Body)
		ConstraintSplitting indexConstraintSplitting = computeIndexConstraintSplitting(bodyStep, context);
		Step result = computeSolutionOfSplitQuantifier(bodyStep, indexConstraintSplitting, context);
		return result;
	}
	
	protected Step stepOverExpressionCombiningSplitQuantifierOnLiteralContainingIndex(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context contextForBody, Context context) {
		// if the splitter contains the index, we must split the quantifier:
		// Quant_x:C Body  --->   (Quant_{x:C and L} Body) op (Quant_{x:C and not L} Body)
		ConstraintSplitting indexConstraintSplitting = computeIndexConstraintSplitting(bodyStep, context);
		Step result = convertItDependsBodyStepOnIndexedLiteralToAStepOnTheEquivalentSplitQuantifierProblem(bodyStep, indexConstraintSplitting, context);
		return result;
	}

	private ConstraintSplitting computeIndexConstraintSplitting(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context context) {
		// Here, we need to obtain the new index constraints, for the case in which the splitter literal is true and false,
		// to create the corresponding sub-problems, solve them, and combine them.
		// However, it is important to remember that bodyStep.getContextSplittingWhenSplitterIsLiteral()
		// contains the splitting of contextForBody with the splitter literal,
		// so the information on the new index constraints is already there in some form.
		// TODO: We current don't have a Constraint-generic way to extract it, but expect to do it in the future.
		// For now, we split the index constraint separately
		Expression literal = bodyStep.getSplitterLiteral();
		ConstraintSplitting indexConstraintSplitting = new ConstraintSplitting(literal, getIndexConstraint(), context);
		return indexConstraintSplitting;
	}

	//TO BE DEPRECIATED
	private Step computeSolutionOfSplitQuantifier(
			ExpressionLiteralSplitterStepSolver.Step bodyStep,
			ConstraintSplitting indexConstraintSplitting, 
			Context context) {
		
		Expression solutionValue = computeSolutionValueOfSplitQuantifier(bodyStep, indexConstraintSplitting, context);
		Step result = makeSolution(solutionValue);
		return result;
	}
	

	
	private Step convertItDependsBodyStepOnIndexedLiteralToAStepOnTheEquivalentSplitQuantifierProblem(
			ExpressionLiteralSplitterStepSolver.Step bodyStep,
			ConstraintSplitting indexConstraintSplitting,
			Context context) {
		
		return explanationBlock("Computing step for split quantifier problem", code( () -> {
			
			Constraint indexConstraintAndLiteral = indexConstraintSplitting.getConstraintAndLiteral();
			Constraint indexConstraintAndLiteralNegation = indexConstraintSplitting.getConstraintAndLiteralNegation();

			Step step;
			switch (indexConstraintSplitting.getResult()) {
			case CONSTRAINT_IS_CONTRADICTORY:
				step = null;
				break;
			case LITERAL_IS_UNDEFINED:
				explain("Index literal ", bodyStep.getSplitter(), " can be either true or false under current context, so we will solve two sub-problems");
				step = convertItDependsBodyStepOnIndexedLiteralToAStepFromTheAssociativeOperationOnTheExpressionCombiningTheSplitQuantifier(indexConstraintAndLiteral, indexConstraintAndLiteralNegation, context);
				break;
			case LITERAL_IS_TRUE:
				explain("Index literal ", bodyStep.getSplitter(), " is always true under current context, so we will solve a single sub-problem");
				step = stepOverSubProblemIfSplitterIsTrue(bodyStep, indexConstraintAndLiteral, context);
				break;
			case LITERAL_IS_FALSE:
				explain("Index literal ", bodyStep.getSplitter(), " is always false under current context, so we will solve a single sub-problem");
				step = stepOverSubProblemIfSplitterIsFalse(bodyStep, indexConstraintAndLiteralNegation, context);
				break;
			default: throw new Error("Unrecognized result for " + ConstraintSplitting.class + ": " + indexConstraintSplitting.getResult());
			}

			return step;

		}), "Split quantifier problem result in ", RESULT);
	}
	
	private Step convertItDependsBodyStepOnIndexedLiteralToAStepFromTheAssociativeOperationOnTheExpressionCombiningTheSplitQuantifier(
			Constraint indexConstraintAndLiteral,
			Constraint indexConstraintAndLiteralNegation,
			Context context) {
		
		Theory theory = context.getTheory();
		Expression ExpressionCombiningTheSplitQuantifier = getEquivalentAssociativeOperationExpressionSubProblemDueToSplittingBodyOnUndefinedIndexedLiteral(indexConstraintAndLiteral, indexConstraintAndLiteralNegation);
		ExpressionLiteralSplitterStepSolver associativeOperationStepSolver = theory.makeEvaluatorStepSolver(ExpressionCombiningTheSplitQuantifier);
		Step associativeOperationStep = (Step) associativeOperationStepSolver.step(context);

		return associativeOperationStep;
	}

	//TO BE DEPRECIATED
	private Expression computeSolutionValueOfSplitQuantifier(
			ExpressionLiteralSplitterStepSolver.Step bodyStep,
			ConstraintSplitting indexConstraintSplitting,
			Context context) {
		
		return explanationBlock("Computing solution of split quantifier problem", code( () -> {
			
			Constraint indexConstraintAndLiteral = indexConstraintSplitting.getConstraintAndLiteral();
			Constraint indexConstraintAndLiteralNegation = indexConstraintSplitting.getConstraintAndLiteralNegation();

			Expression solutionValue;
			switch (indexConstraintSplitting.getResult()) {
			case CONSTRAINT_IS_CONTRADICTORY:
				solutionValue = null;
				break;
			case LITERAL_IS_UNDEFINED:
				explain("Index literal ", bodyStep.getSplitter(), " can be either true or false under current context, so we will solve two sub-problems");
				solutionValue = solveSubProblemIfSplitterIsUndefined(bodyStep, indexConstraintAndLiteral, indexConstraintAndLiteralNegation, context);
				break;
			case LITERAL_IS_TRUE:
				explain("Index literal ", bodyStep.getSplitter(), " is always true under current context, so we will solve a single sub-problem");
				solutionValue = solveSubProblemIfSplitterIsTrue(bodyStep, indexConstraintAndLiteral, context);
				break;
			case LITERAL_IS_FALSE:
				explain("Index literal ", bodyStep.getSplitter(), " is always false under current context, so we will solve a single sub-problem");
				solutionValue = solveSubProblemIfSplitterIsFalse(bodyStep, indexConstraintAndLiteralNegation, context);
				break;
			default: throw new Error("Unrecognized result for " + ConstraintSplitting.class + ": " + indexConstraintSplitting.getResult());
			}

			return solutionValue;

		}), "Split quantifier problem result in ", RESULT);
	}

	private Step makeSolution(Expression solutionValue) {
		Step result = solutionValue == null? null : new Solution(solutionValue);
		return result;
	}

	//TO BE DEPRECIATED
	private Expression solveSubProblemIfSplitterIsTrue(
			ExpressionLiteralSplitterStepSolver.Step bodyStep, 
			Constraint indexConstraintAndLiteral, 
			Context context) {
		
		ExpressionStepSolver subProblemStepSolverIfSplitterIsTrue = makeSubProblemStepSolver(true, bodyStep, indexConstraintAndLiteral);
		Expression solutionValue = solveSubProblem(subProblemStepSolverIfSplitterIsTrue, context);
		return solutionValue;
	}
	
	private Step stepOverSubProblemIfSplitterIsTrue(
			ExpressionLiteralSplitterStepSolver.Step bodyStep, 
			Constraint indexConstraintAndLiteral, 
			Context context) {
		
		ExpressionStepSolver subProblemStepSolverIfSplitterIsTrue = makeSubProblemStepSolver(true, bodyStep, indexConstraintAndLiteral);
		Step step = subProblemStepSolverIfSplitterIsTrue.step(context);
		return step;
	}

	//TO BE DEPRECIATED
	private Expression solveSubProblemIfSplitterIsFalse(
			ExpressionLiteralSplitterStepSolver.Step bodyStep,
			Constraint indexConstraintAndLiteralNegation, 
			Context context) {
		
		ExpressionStepSolver subProblemStepSolverIfSplitterIsFalse = makeSubProblemStepSolver(false, bodyStep, indexConstraintAndLiteralNegation);
		Expression solutionValue = solveSubProblem(subProblemStepSolverIfSplitterIsFalse, context);
		return solutionValue;
	}
	
	private Step stepOverSubProblemIfSplitterIsFalse(
			ExpressionLiteralSplitterStepSolver.Step bodyStep, 
			Constraint indexConstraintAndLiteral, 
			Context context) {
		
		ExpressionStepSolver subProblemStepSolverIfSplitterIsFalse = makeSubProblemStepSolver(false, bodyStep, indexConstraintAndLiteral);
		Step step = subProblemStepSolverIfSplitterIsFalse.step(context);
		return step;
	}


	private Expression solveSubProblemIfSplitterIsUndefined(
			ExpressionLiteralSplitterStepSolver.Step bodyStep,
			Constraint indexConstraintAndLiteral, 
			Constraint indexConstraintAndLiteralNegation, 
			Context context) {
		
		// (**) IF DELETING THIS MARKER, DELETE ALL THE REFERENCES TO IT IN THIS FILE
		// This is where this step solver may return a Solution with literals in it:
		// solveSubProblem uses an exhaustive solve.
		ExpressionStepSolver subProblemIfSplitterIsTrue = makeSubProblemStepSolver(true, bodyStep, indexConstraintAndLiteral);
		ExpressionStepSolver subProblemIfSplitterIsFalse = makeSubProblemStepSolver(false, bodyStep, indexConstraintAndLiteralNegation);
		Expression solutionValue = solveSubProblems(subProblemIfSplitterIsTrue, subProblemIfSplitterIsFalse, context);
		
		return solutionValue;
	}
	
	private Expression getEquivalentAssociativeOperationExpressionSubProblemDueToSplittingBodyOnUndefinedIndexedLiteral( Constraint indexConstraintAndLiteral, 
																													     Constraint indexConstraintAndLiteralNegation) {
		SingleVariableConstraint newIndexConstraintForWhenLiteralIsTrue = (SingleVariableConstraint) indexConstraintAndLiteral;
		SingleVariableConstraint newIndexConstraintForWhenLiteralIsFalse = (SingleVariableConstraint) indexConstraintAndLiteralNegation;
		SingleQuantifierEliminationProblem subProblemWhenLiteralIsTrue = getProblem().makeWithNewIndexConstraint(newIndexConstraintForWhenLiteralIsTrue);
		SingleQuantifierEliminationProblem subProblemWhenLiteralIsFalse = getProblem().makeWithNewIndexConstraint(newIndexConstraintForWhenLiteralIsFalse);
		String associativeOperation = getGroup().getFunctionString();
		Expression equivalentExpressionToContinueSteppingOver = Expressions.apply(associativeOperation, subProblemWhenLiteralIsTrue, subProblemWhenLiteralIsFalse);
		
		return equivalentExpressionToContinueSteppingOver;
	
	}

	protected ExpressionStepSolver makeSubProblemStepSolver(boolean splitterValue, ExpressionLiteralSplitterStepSolver.Step bodyStep, Constraint newIndexConstraint) {
		
		SingleVariableConstraint newIndexConstraintAsSingleVariableConstraint = (SingleVariableConstraint) newIndexConstraint;
		AbstractSingleQuantifierEliminationStepSolver result = makeWithNewIndexConstraint(newIndexConstraintAsSingleVariableConstraint);
		result.initialBodyEvaluationStepSolver =
				splitterValue
				? bodyStep.getStepSolverForWhenSplitterIsTrue() 
				: bodyStep.getStepSolverForWhenSplitterIsFalse();
		result.initialContextForBody = 
				splitterValue
				? bodyStep.getContextSplittingWhenSplitterIsLiteral().getConstraintAndLiteral() 
				: bodyStep.getContextSplittingWhenSplitterIsLiteral().getConstraintAndLiteralNegation();
		return result;
	}

	protected Expression solveSubProblems(ExpressionStepSolver subProblem1, ExpressionStepSolver subProblem2, Context context) {
		// (**) IF DELETING THIS MARKER, DELETE ALL THE REFERENCES TO IT IN THIS FILE
		// This is where this step solver may return a Solution with literals in it:
		// solveSubProblem uses an exhaustive solve.
		Expression subSolution1 = solveSubProblem(subProblem1, context);
		Expression subSolution2 = solveSubProblem(subProblem2, context);
		Expression result = combine(subSolution1, subSolution2, context);
		return result;
	}

	protected Expression solveSubProblem(ExpressionStepSolver subProblemStepSolver, Context context) {	
		Expression result = subProblemStepSolver.solve(context);
		// (**) IF DELETING THIS, DELETE ALL OTHER OCCURRENCES IN THIS FILE
		// The above code line is the exhaustive solve mentioned in other occurrences of (**)
		return result;
	}

	protected Expression combine(Expression solution1, Expression solution2, Context context) {
		return explanationBlock("Combining solutions to sub-problems", code(() -> {

			Expression result;
			if (isIfThenElse(solution1)) {
				result = addSolution1AndSolution2GivenSolution1IsConditional(solution1, solution2, context);
			}
			else if (isIfThenElse(solution2)) {
				result = addSolution1AndSolution2GivenSolution2IsConditional(solution1, solution2, context);
			}
			else {
				result = addNonConditionalSolutions(solution1, solution2, context);
			}
			return result;
			
		}), "COmbination is ", RESULT);
	}

	private Expression addSolution1AndSolution2GivenSolution1IsConditional(Expression solution1, Expression solution2, Context context) {
		// (if C1 then A1 else A2) op solution2 ---> if C1 then (A1 op solution2) else (A2 op solution2)
		Expression result;
		ContextSplitting split = new ContextSplitting(condition(solution1), context);
		switch (split.getResult()) {
		case CONSTRAINT_IS_CONTRADICTORY:
			result = null;
			break;
		case LITERAL_IS_UNDEFINED:
			Expression subSolution1 = combine(thenBranch(solution1), solution2, split.getContextAndLiteral());
			Expression subSolution2 = combine(elseBranch(solution1), solution2, split.getContextAndLiteralNegation());
			result = IfThenElse.make(condition(solution1), subSolution1, subSolution2, true);
			break;
		case LITERAL_IS_TRUE:
			result = combine(thenBranch(solution1), solution2, split.getContextAndLiteral());
			break;
		case LITERAL_IS_FALSE:
			result = combine(elseBranch(solution1), solution2, split.getContextAndLiteralNegation());
			break;
		default: throw new Error("Unrecognized result for " + ContextSplitting.class + ": " + split.getResult());
		}
		return result;
	}

	private Expression addSolution1AndSolution2GivenSolution2IsConditional(Expression solution1, Expression solution2, Context context) {
		// solution1 op (if C2 then B1 else B2) ---> if C2 then (solution1 op B2) else (solution1 op B2)
		Expression result;
		ContextSplitting split = new ContextSplitting(condition(solution2), context);
		switch (split.getResult()) {
		case CONSTRAINT_IS_CONTRADICTORY:
			result = null;
			break;
		case LITERAL_IS_UNDEFINED:
			Expression subSolution1 = combine(solution1, thenBranch(solution2), split.getContextAndLiteral());
			Expression subSolution2 = combine(solution1, elseBranch(solution2), split.getContextAndLiteralNegation());
			result = IfThenElse.make(condition(solution2), subSolution1, subSolution2, true);
			break;
		case LITERAL_IS_TRUE:
			result = combine(solution1, thenBranch(solution2), split.getContextAndLiteral());
			break;
		case LITERAL_IS_FALSE:
			result = combine(solution1, elseBranch(solution2), split.getContextAndLiteralNegation());
			break;
		default: throw new Error("Unrecognized result for " + ContextSplitting.class + ": " + split.getResult());
		}
		return result;
	}

	private Expression addNonConditionalSolutions(Expression solution1, Expression solution2, Context context) {
		Expression result = getGroup().addAndPossiblySolveItDeprecated(solution1, solution2, context);
		return result;
	}

	private Step splitOnNonIndexVariable(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context context) {
		return explanationBlock("Splitting on non-index splitter", code(() -> {

			ExpressionStepSolver ifTrue = makeStepSolverIfNonIndexSplitterIsTrue(bodyStep);
			ExpressionStepSolver ifFalse = makeStepSolverIfNonIndexSplitterIsFalse(bodyStep);
			ContextSplitting split = makeOriginalContextSplitting(bodyStep, context);

			Step result = new ItDependsOn(bodyStep.getSplitterLiteral(), split, ifTrue, ifFalse);

			return result;
			
		}), "Step is ", RESULT);
	}

	private ExpressionStepSolver makeStepSolverIfNonIndexSplitterIsTrue(ExpressionLiteralSplitterStepSolver.Step bodyStep) {
		return explanationBlock("Making sequel step solver for when non-index splitter is true", code(() -> {
			
			AbstractSingleQuantifierEliminationStepSolver ifTrue = clone();
			ifTrue.initialBodyEvaluationStepSolver  = bodyStep.getStepSolverForWhenSplitterIsTrue();
			ifTrue.initialContextForBody  = bodyStep.getContextSplittingWhenSplitterIsLiteral().getContextAndLiteral();
			return ifTrue;
			
		}), "Sequel step solver for when non-index splitter is true is ", RESULT);
	}

	private ExpressionStepSolver makeStepSolverIfNonIndexSplitterIsFalse(ExpressionLiteralSplitterStepSolver.Step bodyStep) {
		return explanationBlock("Making sequel step solver for when non-index splitter is false", code(() -> {

			AbstractSingleQuantifierEliminationStepSolver ifFalse = clone();
			ifFalse.initialBodyEvaluationStepSolver = bodyStep.getStepSolverForWhenSplitterIsFalse();
			ifFalse.initialContextForBody = bodyStep.getContextSplittingWhenSplitterIsLiteral().getContextAndLiteralNegation();
			return ifFalse;

		}), "Sequel step solver for when non-index splitter is false is ", RESULT);
	}

	private ContextSplitting makeOriginalContextSplitting(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context context) {
		return explanationBlock("Making splitting on original context (that is, context for the problem and not the body context which includes the index constraint", code(() -> {

			// to compute the result's constraint splitting,
			// we cannot directly re-use bodyStep.getConstraintSplitting() because it was not obtained from
			// the context it is returning to,
			// but from the context conjoined with the index constraint.
			// In order to provide two contexts to work with the sequel step solvers for the quantified expression as a whole,
			// we calculate the splittings here.
			// TODO: In the future, we expect it to be possible to efficiently extract the contextForBody component relative
			// to the original context only, excluding the index.
			ContextSplitting split = new ContextSplitting(bodyStep.getSplitterLiteral(), context);
			return split;

		}), "Splitting on original context is ", RESULT);
	}

	private Step solveNonConditionalQuantifiedProblem(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context context) {
		return explanationBlock("Solving non-conditional problem", code(() -> {

			Expression literalFreeBody = bodyStep.getValue();
			Step result = eliminateQuantifierForLiteralFreeBody(literalFreeBody, context);

			registerGroupIntegration(literalFreeBody, result, context);

			return result;
			
		}), "Solution is ", RESULT);
	}

	private void registerGroupIntegration(Expression literalFreeBody, Step result, Context context) {
		boolean solutionToQuantifiedLiteralFreeBodyIsNotConditionalItself = !result.itDepends(); 
		if (solutionToQuantifiedLiteralFreeBodyIsNotConditionalItself) {
			IntegrationRecording.registerGroupIntegration(problem, literalFreeBody, result, context);
		}
	}

	@Override
	public AbstractSingleQuantifierEliminationStepSolver clone() {
		AbstractSingleQuantifierEliminationStepSolver result = null;
		try {
			result = (AbstractSingleQuantifierEliminationStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}

	@Override
	public SingleQuantifierEliminationProblem getProblem() {
		return problem;
	}

	@Override
	public AssociativeCommutativeGroup getGroup() {
		return problem.getGroup();
	}

	@Override
	public SingleVariableConstraint getIndexConstraint() {
		return (SingleVariableConstraint) problem.getConstraint();
	}

	@Override
	public Expression getIndex() {
		return problem.getIndex();
	}

	@Override
	public Expression getBody() {
		return problem.getBody();
	}

	@Override
	public String toString() {
		return this.getClass().getSimpleName() + " on " + problem;
	}

	protected
	AbstractSingleQuantifierEliminationStepSolver makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint) {
		try {
			SingleQuantifierEliminationProblem newProblem = getProblem().makeWithNewIndexConstraint(newIndexConstraint);
			AbstractSingleQuantifierEliminationStepSolver result = getClass().getConstructor(SingleQuantifierEliminationProblem.class).newInstance(newProblem);
			return result;
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new Error(e);
		} 
	}
	
	///////////////////////////////////// BRUTE-FORCE CHECKING DEBUGGING TOOL

	private void bruteForceCheckingOfNonConditionalProblemsIfRequested(Step step, Context context) {
		if (mustBeBruteForceChecked(step, context)) {
			bruteForceCheckingOfUnconditionalProblem(step, context);
		}
	}

	private boolean mustBeBruteForceChecked(Step step, Context context) {
		boolean result = 
				context.getGlobalObject(BRUTE_FORCE_CHECKING_OF_NON_CONDITIONAL_PROBLEMS) != null 
				&& 
				! step.itDepends();
		return result;
	}

	private void bruteForceCheckingOfUnconditionalProblem(Step step, Context context) {
		Expression problemExpression = makeProblemExpression(context);
		AssignmentMapsIterator assignments = makeAssignmentsIterator(problemExpression, context);
		for (Map<Expression, Expression> assignment : in(assignments)) {
			bruteForceCheckAssignmentIfNeeded(assignment, problemExpression, step, context);
		}
	}

	private Expression makeProblemExpression(Context context) {
		Expression indexType = context.getTypeExpressionOfRegisteredSymbol(getIndex());
		SingleQuantifierEliminationProblem problem = new DefaultSingleQuantifierEliminationProblem(getGroup(), getIndex(), indexType, getIndexConstraint(), getBody());
		Expression problemExpression = problem.toExpression();
		return problemExpression;
	}

	private static AssignmentMapsIterator makeAssignmentsIterator(Expression problemExpression, Context context) {
		Set<Expression> freeVariables = Expressions.freeVariables(problemExpression, context);
		AssignmentMapsIterator assignments = new AssignmentMapsIterator(freeVariables, context);
		return assignments;
	}

	private static void bruteForceCheckAssignmentIfNeeded(Map<Expression, Expression> assignment, Expression problemExpression, Step step, Context context) {
		BruteForceCommonInterpreter bruteForceCommonInterpreter = new BruteForceCommonInterpreter();
		Context contextExtendedByAssignment = Assignment.extendAssignments(assignment, context);
		if (assignmentSatisfiesContext(contextExtendedByAssignment, bruteForceCommonInterpreter, context)) {
			bruteForceCheckAssignment(problemExpression, step, assignment, contextExtendedByAssignment, bruteForceCommonInterpreter, context);
		}
	}

	private static boolean assignmentSatisfiesContext(Context contextExtendedByAssignment,
			BruteForceCommonInterpreter bruteForceCommonInterpreter, Context context) {
		return bruteForceCommonInterpreter.apply(context, contextExtendedByAssignment).equals(Expressions.TRUE);
	}

	private static void bruteForceCheckAssignment(
			Expression problemExpression, 
			Step step,
			Map<Expression, Expression> assignment, 
			Context contextExtendedByAssignment,
			BruteForceCommonInterpreter bruteForceCommonInterpreter, 
			Context context) {
		
		Expression bruteForceResult = bruteForceCommonInterpreter.apply(problemExpression, contextExtendedByAssignment);
		Expression resultGivenAssignment = bruteForceCommonInterpreter.apply(step.getValue(), contextExtendedByAssignment);
		Expression evaluatedProblem = bruteForceCommonInterpreter.apply(problemExpression, contextExtendedByAssignment);
		
		if ( ! bruteForceResult.equals(resultGivenAssignment)) {
			String message = 
					"Disagreement on " + problemExpression + "\nunder " + assignment + ".\n"
							+ "Context: " + context + ".\n"
							+ "Evaluated problem: " + evaluatedProblem + ".\n"
							+ "Brute force says " + bruteForceResult + ", symbolic says " + resultGivenAssignment;
			println(message);
			throw new Error(message);
		}
		else {
			String message = 
					"Agreement on " + problemExpression + "\nunder " + assignment + ".\n"
							+ "Context: " + context + ".\n"
							+ "Evaluated problem: " + evaluatedProblem + ".\n"
							+ "Brute force says " + bruteForceResult + ", symbolic says " + resultGivenAssignment;
			println(message);
		}
	}
}