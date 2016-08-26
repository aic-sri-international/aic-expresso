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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MAX;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PRODUCT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.sgdpllt.library.set.Sets.isIntensionalMultiSet;
import static com.sri.ai.grinder.sgdpllt.theory.base.ExpressionConditionedOnLiteralSolutionStep.stepDependingOnLiteral;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.collect.StackedHashMap.stackedHashMap;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpllt.api.QuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.group.Conjunction;
import com.sri.ai.grinder.sgdpllt.group.Disjunction;
import com.sri.ai.grinder.sgdpllt.group.Max;
import com.sri.ai.grinder.sgdpllt.group.Product;
import com.sri.ai.grinder.sgdpllt.group.Sum;
import com.sri.ai.grinder.sgdpllt.group.SumProduct;
import com.sri.ai.grinder.sgdpllt.interpreter.SGDPLLT;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.simplifier.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.simplifier.api.TopSimplifier;
import com.sri.ai.grinder.sgdpllt.simplifier.core.Recursive;
import com.sri.ai.grinder.sgdpllt.simplifier.core.TopExhaustive;
import com.sri.ai.util.base.IdentityWrapper;

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

E <- exhaustively top-simplify E if not already so

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
            E' <- exhaustively top-simplify(E[SE/SEvalue])
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
	private boolean alreadyExhaustivelyTopSimplified;
	private int subExpressionIndex;
	private Map<IdentityWrapper, ContextDependentExpressionProblemStepSolver> evaluators;
	
	public EvaluatorStepSolver(Expression expression) {
		super();
		this.expression = expression;
		this.subExpressionIndex = 0;
		this.evaluators = map();
		this.alreadyExhaustivelyTopSimplified = false;
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
	
	private EvaluatorStepSolver cloneWithAnotherExpression(Expression newExpression, boolean alreadyExhaustivelyTopSimplified) {
		EvaluatorStepSolver result = clone();
		result.expression = newExpression;
		result.subExpressionIndex = 0;
		result.alreadyExhaustivelyTopSimplified = alreadyExhaustivelyTopSimplified;
		return result;
	}
	
	public Expression getExpression() {
		return expression;
	}

	@Override
	public SolutionStep step(Context context) {
		SolutionStep result;
		
		TopSimplifier topSimplifier = context.getTheory().getMapBasedTopSimplifier();
		TopSimplifier exhaustiveTopSimplifier = new TopExhaustive(topSimplifier);
		Simplifier totalSimplifier = new Recursive(exhaustiveTopSimplifier);

		Expression exhaustivelyTopSimplifiedExpression;
		if (alreadyExhaustivelyTopSimplified) {
			exhaustivelyTopSimplifiedExpression = expression;
		}
		else {
			exhaustivelyTopSimplifiedExpression = exhaustiveTopSimplifier.apply(expression, context);
		}
		
		
		if (context.isLiteral(exhaustivelyTopSimplifiedExpression)) {
			Expression completelySimplifiedLiteral = totalSimplifier.apply(exhaustivelyTopSimplifiedExpression, context);
			result = stepDependingOnLiteral(completelySimplifiedLiteral, TRUE, FALSE, context);
		}
		// TODO: the next few cases always produce Solution steps, never a ItDependsOn solution step.
		// We should eventually use quantifier eliminator step solvers that may return ItDependsOn solution steps
		else if (fromFunctorToGroup.containsKey(expression.getFunctor())&& isIntensionalMultiSet(expression.get(0)) ) {
			QuantifierEliminator quantifierEliminator;
			if (expression.hasFunctor(SUM)) { // take advantage of factorized bodies, if available
				quantifierEliminator = new SGVET(new SumProduct());
			}
			else {
				AssociativeCommutativeGroup group = fromFunctorToGroup.get(expression.getFunctor());
				quantifierEliminator = new SGDPLLT(group);
			}
			Expression quantifierFreeExpression = quantifierEliminator.solve(expression, context);
			result = new Solution(quantifierFreeExpression);
		}
		else if (expression.hasFunctor(FunctorConstants.CARDINALITY) && isIntensionalMultiSet(expression.get(0)) ) {
			// | {{ (on I) Head | Condition }} | ---> sum ( {{ (on I) 1 | Condition }} )
			IntensionalSet intensionalSet = (IntensionalSet) expression.get(0);
			intensionalSet = (IntensionalSet) intensionalSet.setHead(ONE);
			Expression functionOnSet = apply(SUM, intensionalSet);
			QuantifierEliminator sgvet = new SGVET(new SumProduct());
			Expression quantifierFreeExpression = sgvet.solve(functionOnSet, context);
			result = new Solution(quantifierFreeExpression);
		}
		else if (expression.getSyntacticFormType().equals("For all")) {
			QuantifierEliminator sgdpllt = new SGDPLLT(new Conjunction());
			Expression resultExpression = sgdpllt.solve(expression, context);
			result = new Solution(resultExpression);
		}
		else if (expression.getSyntacticFormType().equals("There exists")) {
			QuantifierEliminator sgdpllt = new SGDPLLT(new Disjunction());
			Expression resultExpression = sgdpllt.solve(expression, context);
			result = new Solution(resultExpression);
		}
		else if (subExpressionIndex != exhaustivelyTopSimplifiedExpression.numberOfArguments()) {
			Expression subExpression = exhaustivelyTopSimplifiedExpression.get(subExpressionIndex);
			ContextDependentExpressionProblemStepSolver subExpressionEvaluator = 
					getEvaluatorFor(subExpression, false /* not known to be exhaustively top-simplified already */);
			SolutionStep subExpressionStep = subExpressionEvaluator.step(context);

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
								subExpressionStep.getContextSplitting(),
								ifTrue,
								ifFalse);
			}
			else if (subExpressionStep.getValue() == subExpression) {
				// no change, just record the evaluator for this sub-expression and move on to the next one
				ContextDependentExpressionProblemStepSolver nextStepSolver;
				nextStepSolver = clone();
				((EvaluatorStepSolver) nextStepSolver).setEvaluatorFor(subExpression, subExpressionEvaluator);
				((EvaluatorStepSolver) nextStepSolver).subExpressionIndex++;
				result = nextStepSolver.step(context);
			}
			else {
				Expression expressionWithSubExpressionReplacedByItsValue
				= exhaustivelyTopSimplifiedExpression.set(subExpressionIndex, subExpressionStep.getValue());

				Expression exhaustivelyTopSimplifiedAfterSubExpressionEvaluation
				= exhaustiveTopSimplifier.apply(expressionWithSubExpressionReplacedByItsValue, context);

				if (exhaustivelyTopSimplifiedAfterSubExpressionEvaluation == subExpression) {
					// topSimplified turns out to be the sub-expression itself
					// we already know the result for that: the non-dependent subExpressionStep itself.
					return subExpressionStep;
				}
				else {
					// topSimplified is either a former sub-expression, or new.
					// try to reuse evaluator if available, or a make a new one, and use it
					ContextDependentExpressionProblemStepSolver nextStepSolver
					= getEvaluatorFor(exhaustivelyTopSimplifiedAfterSubExpressionEvaluation, true /* already top-simplified */);
					result = nextStepSolver.step(context);
				}
			}
		}
		else {
			result = new Solution(exhaustivelyTopSimplifiedExpression);
		}
		
		return result;
	}

	private ContextDependentExpressionProblemStepSolver getEvaluatorFor(Expression anotherExpression, boolean alreadyExhaustivelyTopSimplified) {
		ContextDependentExpressionProblemStepSolver result	= evaluators.get(anotherExpression);
		if (result == null) {
			result = cloneWithAnotherExpression(anotherExpression, alreadyExhaustivelyTopSimplified);
			setEvaluatorFor(anotherExpression, result);
		}
		return result;
	}

	private void setEvaluatorFor(Expression expression, ContextDependentExpressionProblemStepSolver stepSolver) {
		// make new map based on old one, without altering it
		evaluators = stackedHashMap(new IdentityWrapper(expression), stepSolver, evaluators);
	}

	@Override
	public String toString() {
		return "Evaluate " + expression + " from " + subExpressionIndex + "-th sub-expression on";
	}
	
	private static Map<Expression, AssociativeCommutativeGroup> fromFunctorToGroup
	= map(
			makeSymbol(SUM), new Sum(),
			makeSymbol(MAX), new Max(),
			makeSymbol(PRODUCT), new Product()
			);
}