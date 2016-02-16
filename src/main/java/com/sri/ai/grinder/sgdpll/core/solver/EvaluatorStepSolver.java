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
package com.sri.ai.grinder.sgdpll.core.solver;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.sgdpll.theory.base.ExpressionConditionedOnLiteralSolutionStep.stepDependingOnLiteral;
import static com.sri.ai.util.Util.map;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.simplifier.api.TopSimplifier;
import com.sri.ai.util.base.IdentityWrapper;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * A step solver for symbolically evaluating an expression.
 * <p>
 * Currently supports only expressions that are composed of function applications or symbols.
 *
 * This step solver implements the following pseudo-code:
 * <pre>
 * An evaluator step solver for an expression E
keeps a map Evaluators from identity wrappers
and the index SEIndex of the next sub-expression to be analyzed.
of the sub-expression of E to their own evaluator step solver.
Step solver assumes sub-expressions before SEIndex
fully evaluated.

Its step is:

E <- top-simplify E if not already so

if E is literal
    split on it with sub step solvers true and false
else if SEIndex != E.numberOfArguments
    SE <- E.get(SEIndex)
    SEEvaluator <- Evaluators(SE), or make a new one
    SEStep <- SEEvaluator.step
    if SEStep depends on L with sub step solvers S1 and S2
        return ItDependsOn on literal SE
            with sub-step solvers for true and false literals,
            to evaluate E,
            containing a Evaluators(SE) modified to map
            to SEStep's sub-step solvers for SE being true and false,
            respectively.
    else
        SEvalue <- SEStep.getValue() // for abbreviation only

        if SEvalue == SE
            // no change to SE, so no change to E. Just move on to next sub-expression
            Evaluator' <- clone
            Evaluator'.Evaluators <- stacked map on Evaluators with new entry { SE -> SEEvaluator // so we remember the solution if a future simplification surfaces an already fully evaluated sub-expression, including SE, as E'
            Evaluator'.SEIndex++ // go to next sub-expression
            return Evaluator'.step
        else
            E' <- top-simplify(E[SE/SEvalue])
		    if E' == SE // E has been reduced to SE itself, which we just evaluated
		        return SEStep
            else
                Evaluator' <- Evaluators(E') // E' may be a previously evaluated sub-expression or not; re-use evaluator or make a new one 
                return Evaluator'.step
else        
    return Solution(E) // Remember: step solver assumes sub-expressions before SEIndex fully evaluated and E top-simplified already wrt them
</pre> 
 * @author braz
 *
 */
@Beta
public class EvaluatorStepSolver implements ContextDependentExpressionProblemStepSolver {

	private Expression expression;
	private boolean alreadyTopSimplified;
	private int subExpressionIndex;
	private Map<IdentityWrapper, ContextDependentExpressionProblemStepSolver> evaluators;
	private TopSimplifier topSimplifier;
	
	public EvaluatorStepSolver(Expression expression, TopSimplifier topSimplifier) {
		this(expression, topSimplifier, false /* not known to be top-simplified */);
	}
	
	/**
	 * A constructor informing whether expression is already top-simplified (for efficiency).
	 * @param expression
	 * @param topSimplifier
	 * @param alreadyTopSimplified
	 */
	public EvaluatorStepSolver(Expression expression, TopSimplifier topSimplifier, boolean alreadyTopSimplified) {
		super();
		this.expression = expression;
		this.subExpressionIndex = 0;
		this.evaluators = map();
		this.topSimplifier = topSimplifier;
		this.alreadyTopSimplified = alreadyTopSimplified;
	}

	@Override
	public EvaluatorStepSolver clone() {
		EvaluatorStepSolver result = null;
		try {
			result = (EvaluatorStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}
	
	public Expression getExpression() {
		return expression;
	}

	@Override
	public SolutionStep step(Constraint contextualConstraint, Context process) {
		SolutionStep result = null;
		
		Expression topSimplifiedExpression;
		if (alreadyTopSimplified) {
			topSimplifiedExpression = expression;
		}
		else {
			topSimplifiedExpression = topSimplifier.apply(this.expression, process);
		}
		
		if (expressionIsLiteral(contextualConstraint, process)) {
			result = stepDependingOnLiteral(topSimplifiedExpression, TRUE, FALSE, contextualConstraint, process);
		}
		else if (subExpressionIndex != topSimplifiedExpression.numberOfArguments()) {
			Expression subExpression = topSimplifiedExpression.get(subExpressionIndex);
			ContextDependentExpressionProblemStepSolver subExpressionEvaluator = 
					getEvaluatorFor(subExpression, false /* not known to be top-simplified already */);
			SolutionStep subExpressionStep = subExpressionEvaluator.step(contextualConstraint, process);

			if (subExpressionStep == null) {
				return null;
			}
			if (subExpressionStep.itDepends()) {
				EvaluatorStepSolver ifTrue = clone();
				ifTrue.setEvaluatorFor(subExpression, subExpressionStep.getStepSolverForWhenLiteralIsTrue());
				EvaluatorStepSolver ifFalse = clone();
				ifFalse.setEvaluatorFor(subExpression, subExpressionStep.getStepSolverForWhenLiteralIsFalse());
				result =
						new ItDependsOn(
								subExpressionStep.getLiteral(),
								subExpressionStep.getConstraintSplitting(),
								ifTrue,
								ifFalse);
			}
			else if (subExpressionStep.getValue() == subExpression) {
				// no change, just record the evaluator for this sub-expression and move on to the next one
				ContextDependentExpressionProblemStepSolver nextStepSolver;
				nextStepSolver = clone();
				((EvaluatorStepSolver) nextStepSolver).setEvaluatorFor(subExpression, subExpressionEvaluator);
				((EvaluatorStepSolver) nextStepSolver).subExpressionIndex++;
				result = nextStepSolver.step(contextualConstraint, process);
			}
			else {
				Expression expressionWithSubExpressionReplacedByItsValue
				= topSimplifiedExpression.set(subExpressionIndex, subExpressionStep.getValue());

				Expression topSimplifiedAfterSubExpressionEvaluation
				= topSimplifier.apply(expressionWithSubExpressionReplacedByItsValue, process);

				if (topSimplifiedAfterSubExpressionEvaluation == subExpression) {
					// topSimplified turns out to be the sub-expression itself
					// we already know the result for that: the non-dependent subExpressionStep itself.
					return subExpressionStep;
				}
				else {
					// topSimplified is either a former sub-expression, or new.
					// try to reuse evaluator if available, or a make a new one, and use it
					ContextDependentExpressionProblemStepSolver nextStepSolver
					= getEvaluatorFor(topSimplifiedAfterSubExpressionEvaluation, true /* already top-simplified */);
					result = nextStepSolver.step(contextualConstraint, process);
				}
			}
		}
		else {
			result = new Solution(topSimplifiedExpression);
		}
		
		return result;
	}

	private ContextDependentExpressionProblemStepSolver getEvaluatorFor(Expression expression, boolean alreadyTopSimplified) {
		ContextDependentExpressionProblemStepSolver result	= evaluators.get(expression);
		if (result == null) {
			result = new EvaluatorStepSolver(expression, topSimplifier, alreadyTopSimplified);
		}
		return result;
	}

	private void setEvaluatorFor(Expression expression, ContextDependentExpressionProblemStepSolver stepSolver) {
		// make new map based on old one, without altering it
		evaluators = new StackedHashMap<IdentityWrapper, ContextDependentExpressionProblemStepSolver>(evaluators);
		evaluators.put(new IdentityWrapper(expression), stepSolver);
	}

	private boolean expressionIsLiteral(Constraint contextualConstraint, Context process) {
		boolean result = contextualConstraint.getConstraintTheory().isLiteral(expression, process);
		return result;
	}
	
	@Override
	public String toString() {
		return "Evaluate " + expression + " from " + subExpressionIndex + "-th sub-expression on";
	}
}