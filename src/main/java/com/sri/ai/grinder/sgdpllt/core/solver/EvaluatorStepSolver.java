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

import static com.sri.ai.expresso.api.IntensionalSet.intensionalMultiSet;
import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MAX;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PRODUCT;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.SUM;
import static com.sri.ai.grinder.sgdpllt.library.set.CountingFormulaEquivalentExpressions.getCondition;
import static com.sri.ai.grinder.sgdpllt.library.set.CountingFormulaEquivalentExpressions.getIndexExpressions;
import static com.sri.ai.grinder.sgdpllt.library.set.CountingFormulaEquivalentExpressions.isCountingFormulaEquivalentExpression;
import static com.sri.ai.grinder.sgdpllt.library.set.Sets.isIntensionalMultiSet;
import static com.sri.ai.grinder.sgdpllt.theory.base.ExpressionConditionedOnLiteralSolutionStep.stepDependingOnLiteral;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.collect.StackedHashMap.stackedHashMap;

import java.util.Collection;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.QuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.group.Conjunction;
import com.sri.ai.grinder.sgdpllt.group.Disjunction;
import com.sri.ai.grinder.sgdpllt.group.Max;
import com.sri.ai.grinder.sgdpllt.group.Product;
import com.sri.ai.grinder.sgdpllt.group.Sum;
import com.sri.ai.grinder.sgdpllt.group.SumProduct;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceCommonInterpreter;
import com.sri.ai.grinder.sgdpllt.interpreter.SGDPLLT;
import com.sri.ai.grinder.sgdpllt.library.boole.ForAll;
import com.sri.ai.grinder.sgdpllt.library.boole.ThereExists;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.RewriterFromStepMaker;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Exhaustive;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Recursive;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IdentityWrapper;

/**
 * A step solver for symbolically evaluating an expression.
 * <p>
 * Currently supports only expressions that are composed of function applications or symbols.
 *
 * This step solver implements the following pseudo-code:
 * <pre>
 * An evaluator step solver for an expression E keeps:
 * - a map Evaluators from identity wrappers
 * of the sub-expression of E to their own evaluator step solver
 * (this allows us to remember sequel step solvers obtained from previous attempts at evaluating each sub-expression).
 * - the index SEIndex of the next sub-expression to be analyzed.
 * All sub-expressions with indices less than SEIndex are considered fully evaluated.

Its step is:

E <- exhaustively top-simplify E if not already so

if E is a literal
    split on it with sequel step solvers true and false
else if SEIndex != E.numberOfArguments // not yet evaluated all sub-expressions
    SE <- E.get(SEIndex)
    SEEvaluator <- Evaluators(SE), or make a new one // this may re-use sequel step solver for SE from previous attempt
    SEStep <- SEEvaluator.step
    if SEStep depends on L with sub step solvers S1 and S2
        return ItDependsOn on literal L
            with two sequel step solvers (for evaluating E) for cases L true and L false.
            The sequel step solver for L true (false) contains a Evaluators(SE) map modified to map SE to
            its own sequel solver, provided by SEStep for when L is true (false).
    else
        SEvalue <- SEStep.getValue() // for abbreviation only

        if SEvalue == SE
            // no change to SE, so no change to E. Just move on to next sub-expression
            Evaluator' <- clone
            Evaluator'.Evaluators <- stacked map on Evaluators with new entry { SE -> SEEvaluator // so we remember the solution if a future simplification surfaces an already fully evaluated sub-expression, including SE, as E'
            Evaluator'.SEIndex++ // go to next sub-expression
            return Evaluator'.step()
        else
            Evaluator' <- Evaluators(E') // E' may be a previously evaluated sub-expression or not; re-use evaluator or make a new one 
            return Evaluator'.step()
else        
    return Solution(E) // Remember: step solver assumes sub-expressions before SEIndex fully evaluated and E top-simplified already wrt them
</pre> 
 * @author braz
 *
 */
@Beta
public class EvaluatorStepSolver implements ExpressionLiteralSplitterStepSolver {

	private Expression expression;
	private int subExpressionIndex;
	private Map<IdentityWrapper, ExpressionLiteralSplitterStepSolver> evaluators;
	
	public EvaluatorStepSolver(Expression expression) {
		super();
		this.expression = expression;
		this.subExpressionIndex = 0;
		this.evaluators = map();
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
	
	private EvaluatorStepSolver cloneWithAnotherExpression(Expression newExpression) {
		EvaluatorStepSolver result = clone();
		result.expression = newExpression;
		result.subExpressionIndex = 0;
		return result;
	}
	
	public Expression getExpression() {
		return expression;
	}

	Collection<? extends Rewriter> rewriters;	
	
	@Override
	public Step step(Context context) {
		
		Rewriter topRewriter = context.getTheory().getTopRewriter();
		Rewriter exhaustiveTopSimplifier = new Exhaustive(topRewriter);
		makeRewriters(new Recursive(exhaustiveTopSimplifier));

		Expression exhaustivelyTopSimplifiedExpression = exhaustiveTopSimplifier.apply(expression, context);

		Step result = new Solution(exhaustivelyTopSimplifiedExpression);
		for (Rewriter rewriter : rewriters) {
			if (!result.itDepends() && result.getValue() == exhaustivelyTopSimplifiedExpression) {
				result = rewriter.step(exhaustivelyTopSimplifiedExpression, context);
			}
		}
		
		if (!result.itDepends() && result.getValue() == exhaustivelyTopSimplifiedExpression) {
			if (exhaustivelyTopSimplifiedExpression.getSyntacticFormType().equals("Function application")
					&& subExpressionIndex != exhaustivelyTopSimplifiedExpression.numberOfArguments()) {
				
				Expression subExpression = exhaustivelyTopSimplifiedExpression.get(subExpressionIndex);
				ExpressionLiteralSplitterStepSolver subExpressionEvaluator = getEvaluatorFor(subExpression);

				Step subExpressionStep = subExpressionEvaluator.step(context);

				if (subExpressionStep.itDepends()) {
					EvaluatorStepSolver ifTrue = clone();
					ifTrue.setEvaluatorFor(subExpression, subExpressionStep.getStepSolverForWhenSplitterIsTrue());
					EvaluatorStepSolver ifFalse = clone();
					ifFalse.setEvaluatorFor(subExpression, subExpressionStep.getStepSolverForWhenSplitterIsFalse());
					result =
							new ItDependsOn(
									subExpressionStep.getSplitterLiteral(),
									subExpressionStep.getContextSplittingWhenSplitterIsLiteral(),
									ifTrue,
									ifFalse);
				}
				else if (subExpressionStep.getValue() == subExpression) {
					// no change, just record the evaluator for this sub-expression
					// (so it does not get evaluated from scratch next time) and move on to the next one
					ExpressionLiteralSplitterStepSolver nextStepSolver;
					nextStepSolver = clone();
					((EvaluatorStepSolver) nextStepSolver).setEvaluatorFor(subExpression, subExpressionEvaluator);
					((EvaluatorStepSolver) nextStepSolver).subExpressionIndex++;
					result = nextStepSolver.step(context);
					// this cloning and copying could be eliminated by making this step method contain a loop.
				}
				else {
					Expression expressionWithSubExpressionReplacedByItsValue
					= exhaustivelyTopSimplifiedExpression.set(subExpressionIndex, subExpressionStep.getValue());

					ExpressionLiteralSplitterStepSolver nextStepSolver
					= getEvaluatorFor(expressionWithSubExpressionReplacedByItsValue);
					result = nextStepSolver.step(context);
				}
			}
			else {
				result = new Solution(exhaustivelyTopSimplifiedExpression);
			}
		}
		
		return result;
	}

	/**
	 * @param recursiveExhaustiveTopSimplifier
	 */
	public void makeRewriters(Rewriter totalSimplifier) {
		rewriters = Util.<RewriterFromStepMaker>list(

				(Expression e, Context c) -> {
					if (c.isLiteral(e)) {
						Expression completelySimplifiedLiteral = totalSimplifier.apply(e, c);
						return stepDependingOnLiteral(completelySimplifiedLiteral, TRUE, FALSE, c);
					}
					else {
						return new Solution(e);
					}
				}
				
				,
				(Expression e, Context c) -> {
					if (fromFunctorToGroup.containsKey(e.getFunctor())&& isIntensionalMultiSet(e.get(0)) ) {
						if (isQuantifierOverFunctions((QuantifiedExpression)e.get(0), c)) {
							// TODO - more optimal approach
							return getQuantificationOverFunctionsSolutionUsingBruteForce(e, c);
						}
						else {
							QuantifierEliminator quantifierEliminator;
							if (e.hasFunctor(SUM)) { // take advantage of factorized bodies, if available
								quantifierEliminator = new SGVET(new SumProduct());
							}
							else {
								AssociativeCommutativeGroup group = fromFunctorToGroup.get(e.getFunctor());
								quantifierEliminator = new SGDPLLT(group);
							}
							Expression quantifierFreeExpression = quantifierEliminator.solve(e, c);
							return new Solution(quantifierFreeExpression);
						}
					}
					else {
						return new Solution(e);
					}
				}

				,
				(Expression e, Context c) -> {
					if (isCountingFormulaEquivalentExpression(e)) {
						// | {{ (on I) Head : Condition }} | ---> sum ( {{ (on I) 1 : Condition }} )
						// or:
						// | I : Condition | --> sum({{ (on I) 1 : Condition }})
						Expression set = intensionalMultiSet(getIndexExpressions(e), ONE, getCondition(e));
						Expression functionOnSet = apply(SUM, set);
						if (isQuantifierOverFunctions((QuantifiedExpression) set, c)) {
							// TODO - more optimal approach
							return getQuantificationOverFunctionsSolutionUsingBruteForce(functionOnSet, c);
						}
						else {
							QuantifierEliminator sgvet = new SGVET(new SumProduct());
							Expression quantifierFreeExpression = sgvet.solve(functionOnSet, c);
							return new Solution(quantifierFreeExpression);
						}
					}
					else {
						return new Solution(e);
					}
				}


				,
				(Expression e, Context c) -> {
					if (e.getSyntacticFormType().equals(ForAll.SYNTACTIC_FORM_TYPE)) {
						if (isQuantifierOverFunctions((QuantifiedExpression) e, c)) {
							// TODO - more optimal approach
							return getQuantificationOverFunctionsSolutionUsingBruteForce(e, c);
						}
						else {
							QuantifierEliminator sgdpllt = new SGDPLLT(new Conjunction());
							Expression resultExpression = sgdpllt.solve(e, c);
							return new Solution(resultExpression);
						}
					}
					else {
						return new Solution(e);
					}
				}

				,
				(Expression e, Context c) -> {
					if (e.getSyntacticFormType().equals(ThereExists.SYNTACTIC_FORM_TYPE)) {
						if (isQuantifierOverFunctions((QuantifiedExpression) e, c)) {
							// TODO - more optimal approach
							return getQuantificationOverFunctionsSolutionUsingBruteForce(e, c);
						}
						else {
							QuantifierEliminator sgdpllt = new SGDPLLT(new Disjunction());
							Expression resultExpression = sgdpllt.solve(e, c);
							return new Solution(resultExpression);
						}
					}
					else {
						return new Solution(e);
					}
				}

				);
	}

	/**
	 * Gets pre-existing evaluator for an expression or sub-expression,
	 * or makes a new one if there is no pre-existing one.
	 * @param anotherExpression
	 * @return
	 */
	private ExpressionLiteralSplitterStepSolver getEvaluatorFor(Expression anotherExpression) {
		ExpressionLiteralSplitterStepSolver result	= evaluators.get(anotherExpression);
		if (result == null) {
			result = cloneWithAnotherExpression(anotherExpression);
			setEvaluatorFor(anotherExpression, result);
		}
		return result;
	}

	private void setEvaluatorFor(Expression expression, ExpressionLiteralSplitterStepSolver stepSolver) {
		// make new map based on old one, without altering it
		evaluators = stackedHashMap(new IdentityWrapper(expression), stepSolver, evaluators);
	}

	@Override
	public String toString() {
		return "Evaluate " + expression + " from " + subExpressionIndex + "-th sub-expression on";
	}
	

	private static boolean isQuantifierOverFunctions(QuantifiedExpression quantifiedExpression, Context context) {
		boolean result = false;
		for (Expression typeExpression : IndexExpressions.getIndexDomains(quantifiedExpression)) {
			if (context.getType(typeExpression) instanceof FunctionType) {
				result = true;
				break;
			}
		}
		return result;
	}
	
	private static Solution getQuantificationOverFunctionsSolutionUsingBruteForce(Expression expression, Context context) {
		BruteForceCommonInterpreter bruteForceCommonInterpreter = new BruteForceCommonInterpreter();
		Expression quantifierFreeExpression = bruteForceCommonInterpreter.apply(expression, context);
		Solution result = new Solution(quantifierFreeExpression);
		return result;
	}
	
	
	private static Map<Expression, AssociativeCommutativeGroup> fromFunctorToGroup
	= map(
			makeSymbol(SUM), new Sum(),
			makeSymbol(MAX), new Max(),
			makeSymbol(PRODUCT), new Product()
			);
}