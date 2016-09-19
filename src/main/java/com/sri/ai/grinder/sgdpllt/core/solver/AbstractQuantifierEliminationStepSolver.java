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
package com.sri.ai.grinder.sgdpllt.core.solver;

import static com.sri.ai.expresso.helper.Expressions.isSubExpressionOf;
import static com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse.thenBranch;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Constraint;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.constraint.ConstraintSplitting;
import com.sri.ai.grinder.sgdpllt.core.constraint.ContextSplitting;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.controlflow.IfThenElse;

/**
 * An abstract implementation for step solvers for quantified expressions
 * (the quantification being based on an associative commutative group's operation).
 * <p>
 * This is done by applying a {@link EvaluatorStepSolver} on the body expression,
 * picking literals in it according to the context conjoined with the index constraint,
 * and "intercepting" literals containing the indices and splitting the quantifier
 * based on that, solving the two resulting sub-problems.
 * <p>
 * For example, if we have <code>sum({{ (on X in SomeType) if Y != bob then 2 else 3 | X != john }})</code>
 * under context <code>Z = alice</code>,
 * {@link EvaluatorStepSolver#step(Context)} is
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
 * {@link #eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(SingleVariableConstraint, Expression, Context)}
 * (which for sums with constant bodies will be equal to the model count of the index constraint
 * under the context times the constant).
 * <p>
 * Extending classes must define method
 * {@link #eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(SingleVariableConstraint, Expression, Context)
 * to solve the case in which the body is its given literal-free version,
 * for the given context and index constraint.
 * <p>
 * At the time of this writing,
 * {@link EvaluatorStepSolver} supports only expressions that are composed of
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
public abstract class AbstractQuantifierEliminationStepSolver implements QuantifierEliminationStepSolver {

	public static boolean useEvaluatorStepSolverIfNotConditioningOnIndexFreeLiteralsFirst = true;
	
	protected AssociativeCommutativeGroup group;
	
	protected SingleVariableConstraint indexConstraint;

	protected Expression body;
	
	private ContextDependentExpressionProblemStepSolver initialBodyEvaluationStepSolver;
	
	private Context initialContextForBody;

	public AbstractQuantifierEliminationStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint indexConstraint, Expression body) {
		this.group = group;
		this.indexConstraint = indexConstraint;
		this.body = body;
	}

	/**
	 * Abstract method defining a quantified expression with a given index constraint and literal-free body is to be solved.
	 * @param indexConstraint the index constraint
	 * @param literalFreeBody literal-free body
	 */
	protected abstract SolverStep eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(
			SingleVariableConstraint indexConstraint,
			Expression literalFreeBody,
			Context context);

	@Override
	public AbstractQuantifierEliminationStepSolver clone() {
		AbstractQuantifierEliminationStepSolver result = null;
		try {
			result = (AbstractQuantifierEliminationStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	/**
	 * Creates a new version of this object with a new index constraint.
	 * @param newIndexConstraint
	 * @return
	 */
	abstract protected AbstractQuantifierEliminationStepSolver makeWithNewIndexConstraint(SingleVariableConstraint newIndexConstraint);
	
	@Override
	public AssociativeCommutativeGroup getGroup() {
		return group;
	}
	
	@Override
	public SingleVariableConstraint getIndexConstraint() {
		return indexConstraint;
	}
	
	/**
	 * Convenience method for <code>indexConstraint.getTheory()</code>.
	 * @return
	 */
	@Override
	public Theory getTheory() {
		return indexConstraint.getTheory();
	}
	
	@Override
	public Expression getIndex() {
		return indexConstraint.getVariable();
	}
	
	@Override
	public Expression getBody() {
		return body;
	}
	
	private ContextDependentExpressionProblemStepSolver getInitialBodyStepSolver(Theory theory) {
		if (initialBodyEvaluationStepSolver == null) {
			initialBodyEvaluationStepSolver
			= new EvaluatorStepSolver(body);
		}
		return initialBodyEvaluationStepSolver;
	}

	private Context getContextForBody(Context context) {
		Context result;
		if (initialContextForBody == null) {
			result = context.conjoin(getIndexConstraint(), context);
		}
		else {
			result = initialContextForBody;
		}
		return result;
	}

	@Override
	public SolverStep step(Context context) {
		
		SolverStep result;

		Context contextForBody = getContextForBody(context);
		
		if (contextForBody.isContradiction()) {
			result = new Solution(group.additiveIdentityElement());
		}
		else {
			ContextDependentExpressionProblemStepSolver bodyStepSolver = getInitialBodyStepSolver(context.getTheory());
			SolverStep bodyStep = bodyStepSolver.step(contextForBody); 
			
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
			if ( ! bodyStep.itDepends()) {
				EvaluatorStepSolver evaluatorStepSolver = new EvaluatorStepSolver(bodyStep.getValue());
				bodyStep = evaluatorStepSolver.step(context);
			}
			
			if (bodyStep.itDepends()) {
				// "intercept" literals containing the index and split the quantifier based on it
				if (isSubExpressionOf(getIndex(), bodyStep.getLiteral())) {
					Expression literalOnIndex = bodyStep.getLiteral();
					result = resultIfLiteralContainsIndex(literalOnIndex, bodyStep, contextForBody, context);
				}
				else { // not on index, just pass the expression on which we depend on, but with appropriate sub-step solvers (this, for now)
					AbstractQuantifierEliminationStepSolver ifTrue = clone();
					AbstractQuantifierEliminationStepSolver ifFalse = clone();
					ifTrue.initialBodyEvaluationStepSolver = bodyStep.getStepSolverForWhenLiteralIsTrue();
					ifFalse.initialBodyEvaluationStepSolver = bodyStep.getStepSolverForWhenLiteralIsFalse();
					ifTrue.initialContextForBody  = bodyStep.getContextSplitting().getContextAndLiteral();
					ifFalse.initialContextForBody = bodyStep.getContextSplitting().getContextAndLiteralNegation();
					
					// to compute the result's constraint splitting,
					// we cannot directly re-use bodyStep.getConstraintSplitting() because it was not obtained from
					// the context it is returning to,
					// but from the context conjoined with the index constraint.
					// In order to provide two contexts to work with the sequel step solvers,
					// we calculate the splittings here.
					// TODO: In the future, we expect it possible to efficiently extract the contextForBody component relative
					// to the original context only, excluding the index.
					ContextSplitting split = new ContextSplitting(bodyStep.getLiteral(), context);
					
					result = new ItDependsOn(bodyStep.getLiteral(), split, ifTrue, ifFalse);
				}
			}
			else { // body is already literal free
				result
				= eliminateQuantifierForLiteralFreeBodyAndSingleVariableConstraint(
						indexConstraint, bodyStep.getValue(), context);
			}
		}

		return result;
	}

	protected SolverStep resultIfLiteralContainsIndex(Expression literal, SolverStep bodyStep, Context contextForBody, Context context) {
		// if the splitter contains the index, we must split the quantifier:
		// Quant_x:C Body  --->   (Quant_{x:C and L} Body) op (Quant_{x:C and not L} Body)
		
		SolverStep result;
		Expression solutionValue;

		// Here, we need to obtain the new index constraints, for the case in which the splitter literal is true and false,
		// to create the corresponding sub-problems, solve them, and combine them.
		// However, it is important to remember that bodyStep.getContextSplitting()
		// contains the splitting of contextForBody with the splitter literal,
		// so the information on the new index constraints is already there in some form.
		// TODO: We current don't have a Constraint-generic way to extract it, but expect to do it in the future.
		// For now, we split the index constraint separately
		ConstraintSplitting indexConstraintSplitting = new ConstraintSplitting(literal, getIndexConstraint(), context);
		Constraint indexConstraintAndLiteral = indexConstraintSplitting.getConstraintAndLiteral();
		Constraint indexConstraintAndLiteralNegation = indexConstraintSplitting.getConstraintAndLiteralNegation();
		
		switch (indexConstraintSplitting.getResult()) {
		case CONSTRAINT_IS_CONTRADICTORY:
			solutionValue = null;
			break;
		case LITERAL_IS_UNDEFINED:
			// (**) IF DELETING THIS MARKER, DELETE ALL THE REFERENCES TO IT IN THIS FILE
			// This is where this step solver may return a Solution with literals in it:
			// solveSubProblem uses an exhaustive solve.
			solutionValue = solveSubProblems(makeSubProblem(true, bodyStep, indexConstraintAndLiteral), makeSubProblem(false, bodyStep, indexConstraintAndLiteralNegation), context);
			break;
		case LITERAL_IS_TRUE:
			solutionValue = solveSubProblem(makeSubProblem(true, bodyStep, indexConstraintAndLiteral), context);
			break;
		case LITERAL_IS_FALSE:
			solutionValue = solveSubProblem(makeSubProblem(false, bodyStep, indexConstraintAndLiteralNegation), context);
			break;
		default: throw new Error("Unrecognized result for " + ConstraintSplitting.class + ": " + indexConstraintSplitting.getResult());
		}

		if (solutionValue == null) {
			result = null;
		}
		else {
			result = new Solution(solutionValue);
		}
		
		return result;
	}
	
	protected Expression solveSubProblems(AbstractQuantifierEliminationStepSolver subProblem1, AbstractQuantifierEliminationStepSolver subProblem2, Context context) {
		// (**) IF DELETING THIS MARKER, ALL THE REFERENCES TO IT IN THIS FILE
		// This is where this step solver may return a Solution with literals in it:
		// solveSubProblem uses an exhaustive solve.
		Expression subSolution1 = solveSubProblem(subProblem1, context);
		Expression subSolution2 = solveSubProblem(subProblem2, context);
		Expression result = combine(subSolution1, subSolution2, context);
		return result;
	}

	protected Expression solveSubProblem(AbstractQuantifierEliminationStepSolver subProblem, Context context) {	
		Expression result = subProblem.solve(context);
		// (**) IF DELETING THIS, DELETE ALL OTHER OCCURRENCES IN THIS FILE
		// The above code line is the exhaustive solve mentioned in other occurrences of (**)
		return result;
	}
	
	protected AbstractQuantifierEliminationStepSolver makeSubProblem(boolean valueForLiteral, SolverStep bodyStep, Constraint newIndexConstraint) {
		SingleVariableConstraint newIndexConstraintAsSingleVariableConstraint = 
				(SingleVariableConstraint) newIndexConstraint;
		AbstractQuantifierEliminationStepSolver result = 
				makeWithNewIndexConstraint(newIndexConstraintAsSingleVariableConstraint);
		result.initialBodyEvaluationStepSolver =
				valueForLiteral
				? bodyStep.getStepSolverForWhenLiteralIsTrue() 
				: bodyStep.getStepSolverForWhenLiteralIsFalse();
		result.initialContextForBody = 
				valueForLiteral
				? bodyStep.getContextSplitting().getConstraintAndLiteral() 
				: bodyStep.getContextSplitting().getConstraintAndLiteralNegation();
		return result;
	}

	protected Expression combine(Expression solution1, Expression solution2, Context context) {
		Expression result;
		if (isIfThenElse(solution1)) {
			// (if C1 then A1 else A2) op solution2 ---> if C1 then (A1 op solution2) else (A2 op solution2)
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
		}
		else if (isIfThenElse(solution2)) {
			// solution1 op (if C2 then B1 else B2) ---> if C2 then (solution1 op B2) else (solution1 op B2)
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
		}
		else {
			result = group.add(solution1, solution2, context);
		}
		return result;
	}

	/**
	 * @param expression
	 * @param simplifier
	 * @return
	 */
	public static ContextDependentExpressionProblemStepSolver makeEvaluator(Expression expression) {
		ContextDependentExpressionProblemStepSolver evaluator;
		evaluator = new EvaluatorStepSolver(expression);
		return evaluator;
	}
}