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
import static com.sri.ai.grinder.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter.toExpressionLiteralSplitterStepSolver;
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

			if (contextForBody.isContradiction()) {
				step = stepWhenContextForBodyIsNotConsistent();
			}
			else {
				step = stepWhenContextForBodyIsConsistent(contextForBody, context);
			}

			bruteForceCheckingOfNonConditionalProblemsIfRequested(step, context);

			return step;

		}), "Step is ", RESULT);
	}

	private Step stepWhenContextForBodyIsNotConsistent() {
		explain("Context for body is contradictory");
		Step step = new Solution(getGroup().additiveIdentityElement()); 	// a summation with a contradictory constraint is a sum of the elements of the empty set, that is, the additive identity element
		return step;
	}

	private Context getContextForBody(Context context) {
		return explanationBlock("Making context for body ", code(() -> {

			////////////// DEBUGGING LINE
			//initialContextForBody = null;
			////////////// DEBUGGING LINE
			// TODO: the above line forces the context-for-body to be re-computed at every step.
			// This is needed because we may be invoking a sequel step solver under a new context,
			// different from the original context used to compute the original context-for-body.
			// This may cause the context-for-body to leave some literals as "undefined" which are
			// actually defined by the context being currently used.
			// This causes the body step to be conditional when in fact it should not.
			//
			// For example, suppose we get a step for
			// sum_I ... if I > 1 and K > 0 then ...
			// under context true
			// We still return a body step conditional on I > 1 and split the quantifier,
			// solving an AssociativeCommutativeGroupOperationApplicationStepSolver of + applied to
			// (sum_I:I > 1 if ... K > 0)  and (sum_I:I <= 1 if ... K > 0)
			// with each summation being represented by a sequel body step with initialContextForBody
			// equal to "I > 1", and "I <= 1" respectively.
			// When the first of these two returns a step, it's conditional on K > 0.
			// Since K is a free variable, that is passed up and eventually the first step solver reaches a solution.
			// Then the second one needs to produce a step, but now the context contains K > 0.
			// HOWEVER, its initialContextForBody *still* is I <= 1, that is, it does not imply K > 0
			// even though the contextForBody should always represent a conjunction of the current context (here, K > 0)
			// and the index constraint.
			// So the body step solver will be conditioned on K > 0, and because it is a free variable, the
			// AssociativeCommutativeGroupOperationApplicationStepSolver tries to return a step conditional on K > 0.
			// This however throws an exception when we try to compute the context splitting for such a step,
			// since it would be based on a splitter literal that is not actually undefined under the current context.
			//
			// So recomputing contextForBody at every step ensures that it always contains the information in the context.
			// However, this is more expensive.
			//
			// It would be nice if we could just create a contextForBody by "concatenating" the index expression
			// (which is a SingleVariableConstraint) to the context to create a ConjoinedContext representing the contextForBody.
			// However, this is not robust to refinements to the original context (in our example, the context going from
			// true to K > 0) because it may be that the index constraint was satisfiable for the original context but not
			// the refined one.
			// For example, if the index constraint in our example were I : I < 0 and I > K, concatenating it with "true"
			// when the context is true works fine, but once the context is "K > 0", we cannot simply concatenate
			// the index constraint to it, because we would obtain a ConjoinedContext that is actually contradictory
			// but not detected as so.
			// To be able to actually detect contradictions, we would have to really conjoin the index constraint to the
			// context, which takes us back to the current, more expensive, solution.
			// 
			// So, in order to concatenate the index constraint to the context in the knowledge that it will not be contradictory,
			// the first job of AbstractSingleQuantifierEliminationStepSolver.step
			// should be to check that the index constraint is satisfiable under all assignments satisfying the context.
			// This can be done by running a satisfiability step solver on it under that context.
			// If this is not the case, a conditional step solver will be returned and split the problem.
			// Eventually, we either detect that the index constraint is always unsatisfiable under the context (a trivial case)
			// or that it is always satisfiable.
			// In the second case we can concatenate without risk of building an undetected contradiction.
			
			Context result = context.conjoin(getIndexConstraint(), context);
			return result;

		}), "Context for body is ", RESULT);
	}

	private Step stepWhenContextForBodyIsConsistent(Context contextForBody, Context context) {
		return explanationBlock("Solving problem with consistent context for body", code(() -> {
			
			Step step;
			ExpressionLiteralSplitterStepSolver.Step bodyStep = getBodyStep(contextForBody);
			explain("Step for solving body alone is ", bodyStep);
			
			if (bodyStep.itDepends()) {
				step = stepOnProblemWithConditionalBody(bodyStep, context);
			}
			else {
				step = stepOnProblemWithUnconditionalBody(bodyStep, context);
			}
			
			return step;
			
		}), "Step is ", RESULT);
	}

	private ExpressionLiteralSplitterStepSolver.Step getBodyStep(Context contextForBody) {
		return explanationBlock("Determining body step: ", code(() -> {
			ExpressionLiteralSplitterStepSolver.Step result;
			
			ExpressionLiteralSplitterStepSolver bodyStepSolver = getInitialBodyStepSolver(contextForBody.getTheory());
			ExpressionLiteralSplitterStepSolver.Step bodyStep = bodyStepSolver.step(contextForBody); 

			// TODO: this method must be simplified when we eliminate all quantifier elimination rewriters that return solutions with literals
			
			// At this point, bodyStep may be a non-conditional step
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
			// EvaluatorStepSolver no longer returns such solutions.
			// The solution to this whole situation is to change *this* class
			// so it does not return solutions with literals any longer.
			// This happens in quantifier splits, when the two sub-solutions
			// are computed with an exhaustive solve,
			// which may return solutions with literals
			// (it is only the step solvers that we want to prevent from doing this, not exhaustive solving).
			// Check (**) in this file to see where this happens
			explain("Body step is ", bodyStep);
			if ( ! bodyStep.itDepends()) {
				explain("Body step is not conditional, but it may still contain literals, so we are going to check now if really there are none");
				ExpressionLiteralSplitterStepSolver evaluatorStepSolver = contextForBody.getTheory().makeEvaluatorStepSolver(bodyStep.getValue());
				result = evaluatorStepSolver.step(contextForBody);
				// myAssert( ! result.itDepends(), () -> "We should not be getting conditional steps here anymore");
				explain("After this check, body step is ", bodyStep);
			}
			else {
				result = bodyStep;
			}
			return result;
		}), "Body step is ", RESULT);
	}

	private Step stepOnProblemWithConditionalBody(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context context) {
		return explanationBlock("Taking step in solving problem split by splitter from body: ", bodyStep, code(() -> {
			
			Step step;
			if (isSubExpressionOf(getIndex(), bodyStep.getSplitterLiteral())) {
				explain("Splitter contains index, so we are going to split the quantifier");
				step = splitOnIndexVariable(bodyStep, context);
			}
			else {
				explain("Splitter does not contains index, so we are going to make a conditional on two new subproblems");
				step = splitOnNonIndexVariable(bodyStep, context);
			}
			return step;
			
		}), "Step is ", RESULT);
	}
	
	protected Step splitOnIndexVariable(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context context) {
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
	
	private Step convertItDependsBodyStepOnIndexedLiteralToAStepOnTheEquivalentSplitQuantifierProblem(
			ExpressionLiteralSplitterStepSolver.Step bodyStep,
			ConstraintSplitting indexConstraintSplitting,
			Context context) {
		
		return explanationBlock("Computing step for split quantifier problem", code( () -> {
			
			Constraint indexConstraintAndLiteral = indexConstraintSplitting.getConstraintAndLiteral();
			Constraint indexConstraintAndLiteralNegation = indexConstraintSplitting.getConstraintAndLiteralNegation();

			Step step;
			switch (indexConstraintSplitting.getResult()) {
			case LITERAL_IS_UNDEFINED:
				step = splitOnUndefinedSplitter(bodyStep, indexConstraintAndLiteral, indexConstraintAndLiteralNegation, context);
				break;
			case LITERAL_IS_TRUE: case LITERAL_IS_FALSE:
				step = splitOnDefinedSplitter(bodyStep, indexConstraintSplitting, indexConstraintAndLiteral, context);
				break;

			default: throw new Error("Invalid result for " + ConstraintSplitting.class + ": " + indexConstraintSplitting.getResult());
			}

			return step;

		}), "Split quantifier problem result in ", RESULT);
	}

	private Step splitOnDefinedSplitter(
			ExpressionLiteralSplitterStepSolver.Step bodyStep,
			ConstraintSplitting indexConstraintSplitting, 
			Constraint indexConstraintAndLiteral, 
			Context context) {
		
		boolean splitterValue = indexConstraintSplitting.getResult() == ConstraintSplitting.Result.LITERAL_IS_TRUE;
		explain("Index literal ", bodyStep.getSplitter(), " is always " + splitterValue + " under current context, so we will solve a single sub-problem");
		Step step = stepOverSubProblemIfSplitterIs(splitterValue, bodyStep, indexConstraintAndLiteral, context);
		return step;
	}
	
	private Step splitOnUndefinedSplitter(
			ExpressionLiteralSplitterStepSolver.Step bodyStep,
			Constraint indexConstraintAndLiteral,
			Constraint indexConstraintAndLiteralNegation,
			Context context) {

		explain("Index literal ", bodyStep.getSplitter(), " can be either true or false under current context, so we will solve two sub-problems");

		ExpressionLiteralSplitterStepSolver subProblemIfSplitterIsTrueStepSolver = 
				toExpressionLiteralSplitterStepSolver(
						makeSubProblemStepSolver(true, bodyStep, indexConstraintAndLiteral));
		
		ExpressionLiteralSplitterStepSolver subProblemIfSplitterIsFalseStepSolver = 
				toExpressionLiteralSplitterStepSolver(
						makeSubProblemStepSolver(false, bodyStep, indexConstraintAndLiteralNegation));
		
		StepSolver groupStepSolver =
				new AssociativeCommutativeGroupOperationApplicationStepSolver(
						getGroup(), subProblemIfSplitterIsTrueStepSolver, subProblemIfSplitterIsFalseStepSolver);
		
		Step associativeOperationStep = (Step) groupStepSolver.step(context);

		return associativeOperationStep;
	}
	
	private Step stepOverSubProblemIfSplitterIs(
			boolean splitterValue, 
			ExpressionLiteralSplitterStepSolver.Step bodyStep, 
			Constraint indexConstraintAndLiteral, 
			Context context) {
		
		ExpressionStepSolver subProblemStepSolverForThisSplitterValue = makeSubProblemStepSolver(splitterValue, bodyStep, indexConstraintAndLiteral);
		Step step = subProblemStepSolverForThisSplitterValue.step(context);
		return step;
	}
	
	protected ExpressionStepSolver makeSubProblemStepSolver(boolean splitterValue, ExpressionLiteralSplitterStepSolver.Step bodyStep, Constraint newIndexConstraint) {
		
		SingleVariableConstraint newIndexConstraintAsSingleVariableConstraint = (SingleVariableConstraint) newIndexConstraint;
		AbstractSingleQuantifierEliminationStepSolver result = makeWithNewIndexConstraint(newIndexConstraintAsSingleVariableConstraint);
		result.initialBodyEvaluationStepSolver = bodyStep.getStepSolverForWhenSplitterIs(splitterValue);
		return result;
	}

	private Step splitOnNonIndexVariable(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context context) {
		return explanationBlock("Splitting on non-index splitter", code(() -> {

			ExpressionStepSolver ifTrue  = makeSequelStepSolverIfNonIndexSplitterIs(true,  bodyStep);
			ExpressionStepSolver ifFalse = makeSequelStepSolverIfNonIndexSplitterIs(false, bodyStep);
			ContextSplitting split = makeOriginalContextSplitting(bodyStep, context);

			Step result = new ItDependsOn(bodyStep.getSplitterLiteral(), split, ifTrue, ifFalse);

			return result;
			
		}), "Step is ", RESULT);
	}

	private ExpressionStepSolver makeSequelStepSolverIfNonIndexSplitterIs(boolean splitterValue, ExpressionLiteralSplitterStepSolver.Step bodyStep) {
		return explanationBlock("Making sequel step solver for when non-index splitter is ", splitterValue, code(() -> {
			
			AbstractSingleQuantifierEliminationStepSolver sequelStepSolver = clone();
			sequelStepSolver.initialBodyEvaluationStepSolver = bodyStep.getStepSolverForWhenSplitterIs(splitterValue);
			return sequelStepSolver;
			
		}), "Sequel step solver for when non-index splitter is ", splitterValue, " is ", RESULT);
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

	private Step stepOnProblemWithUnconditionalBody(ExpressionLiteralSplitterStepSolver.Step bodyStep, Context context) {
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