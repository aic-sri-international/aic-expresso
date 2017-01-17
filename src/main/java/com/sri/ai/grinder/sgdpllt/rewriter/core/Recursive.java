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
package com.sri.ai.grinder.sgdpllt.rewriter.core;

import static com.sri.ai.util.Util.addAllToArrayList;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndSyntacticContext;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;

/**
 * Applies a base rewriter on an expression and then recursively on sub-expressions.
 * This means applying the base rewriter to the given expression,
 * then to apply the recursive rewriter to each of its expressions,
 * with an application of the base rewriter to the resulting expression at every turn.
 * 
 * The idea is that evaluating a sub-expression may lead to short-circuiting of the overall expression
 * without having to evaluate the remaining expressions.
 * 
 * There are many possibly subtleties and possible improvements.
 * For example, we could use declarative knowledge about which arguments may provide short-circuiting.
 * We may even knowledge about which arguments are more likely to do so.
 * Furthermore, this evaluation may be dynamic.
 * Another possibility is to take into account an expected evaluation cost based
 * on the cost of evaluating sub-expressions, plus the possibility of short-circuiting.
 * We currently only have a basic version.
 * 
 * While the idea is to allow recursive application to any type of expression,
 * currently only sub-expressions of function applications (that is, its functor and arguments) and tuples are recursed into
 * (other expressions simply have the base rewriter applied to them).
 * This is due to the fact that sub-expressions in more complex expressions
 * are under a new context. For example, in order to recurse into  the body of <code>for all X in Integer : f(X)</code>,
 * we need to be aware that this evaluation occurs under a context that knows about <code>X</code>.
 * However, {@link Expression} does not currently provide this type of information in a generic fashion.
 * Once {@link Expression} is extended with that, we can extend the application of this type of rewriter
 * to any expression.
 * 
 * @author braz
 *
 */
public class Recursive implements Rewriter {
	
	private Rewriter baseRewriter;
	
	public Recursive(Rewriter baseRewriter) {
		super();
		this.baseRewriter = baseRewriter;
	}

	private final static List<String> syntacticFormTypesToRecurse = list("Function application", "Tuple");
	
	@Override
	public ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
		ExpressionLiteralSplitterStepSolver result;
		Object syntacticFormType = expression.getSyntacticFormType();
		if (syntacticFormTypesToRecurse.contains(syntacticFormType)) {
			result = new RecursiveStepSolver(baseRewriter, expression);
		}
		else {
			// if expression is a Symbol, simply applying the base rewriter is equivalent to "recursively" applying it, since there are no sub-expressions to recurse into.
			// TODO: For all other types of expressions, this is potentially incorrect because there may be sub-expressions but we do not recurse into them.
			// The reason for that is that Expression does not support the generic providing of context-altering constructs such as indices and conditions in quantified expressions.
			result = baseRewriter.makeStepSolver(expression);
		}
		return result;
	}

	/**
	 * Implements a step solver for {@link Recursive} rewriter.
	 * 
	 * It works by keeping an array of sub-expressions and an index to the current sub-expression to be evaluated.
	 * It also keeps track of whether it is time to apply the base rewriter on the top expression, or
	 * the recursive rewriter on the current sub-expression.
	 * 
	 * If the result of the appropriate application produces an {@link ItDependsOn} step,
	 * returns a new {@link ItDepends} with the appropriate sequel solvers.
	 * 
	 * Otherwise, if it applied the base rewriter to the top expression and there was no change,
	 * moves on to the current sub-expression.
	 * 
	 * If it applied the base rewriter to the top expression and there was a change,
	 * resets the sub-expressions and moves on to the first one (or simply returns if there are no sub-expressions anymore).
	 * 
	 * If it applied the recursive rewriter to the current sub-expression and there was no change, moves on to the next one.
	 * If there was a change, moves on to the top one.
	 * 
	 * @author braz
	 *
	 */
	private static class RecursiveStepSolver implements ExpressionLiteralSplitterStepSolver {
		
		private Rewriter baseRewriter;
		private Expression currentExpression;
		private ArrayList<ExpressionAndSyntacticContext> subExpressions;
		private int currentSubExpressionIndex;
		private boolean topExpressionIsNextForUsToTakeAStepOn;
		private ExpressionLiteralSplitterStepSolver initialCurrentStepSolver;
		
		// Invariants:
		// baseRewriter is the base rewriter of the {@link Recursive} rewriter which this step solver implements 
		// currentExpression contains the top expression being evaluated
		// subExpressions contains the sub-expressions of currentExpression in an array
		// currentSubExpressionIndex contains the index of the current sub-expression
		// topExpressionIsNextForUsToTakeAStepOn indicates whether the next step is to be takes on the top expression (or instead on the current sub-expression)
		// currentStepSolver is the step solver to be used on the next expression for us to take a step on

		public RecursiveStepSolver(Rewriter baseRewriter, Expression currentExpression) {
			this(baseRewriter, currentExpression, true);
		}

		public RecursiveStepSolver(Rewriter baseRewriter, Expression currentExpression, boolean topExpressionIsNextForUsToTakeAStepOn) {
			this.baseRewriter = baseRewriter;
			this.topExpressionIsNextForUsToTakeAStepOn = topExpressionIsNextForUsToTakeAStepOn;
			this.currentExpression = currentExpression;
			this.subExpressions = addAllToArrayList(currentExpression.getImmediateSubExpressionsAndContextsIterator());
			this.currentSubExpressionIndex = 0;
			this.initialCurrentStepSolver = null;
		}

		private RecursiveStepSolver makeRecursiveStepSolverForSameExpressionButSkippingTopExpression() {
			RecursiveStepSolver next = clone();
			next.topExpressionIsNextForUsToTakeAStepOn = false;
			next.initialCurrentStepSolver = null;
			return next;
		}

		private RecursiveStepSolver makeRecursiveStepSolverForSameExpressionButTargettingNextSubExpression() {
			RecursiveStepSolver result = clone();
			result.topExpressionIsNextForUsToTakeAStepOn = false;
			result.initialCurrentStepSolver = null;
			result.currentSubExpressionIndex++;
			return result;
		}

		private RecursiveStepSolver makeRecursiveStepSolverForSameTopExpressionButTargettingNextSubExpression(Expression newCurrentExpression) {
			RecursiveStepSolver next = new RecursiveStepSolver(baseRewriter, newCurrentExpression); // using constructor ensure proper resetting of 'subExpressions'
			next.currentSubExpressionIndex = currentSubExpressionIndex + 1; // we know that the top structure did not change, and we want to evaluate from next sub-expression on.
			return next;
		}

		@Override
		public RecursiveStepSolver clone() {
			RecursiveStepSolver result = null;
			try {
				result = (RecursiveStepSolver) super.clone();
			} catch (CloneNotSupportedException e) {
				e.printStackTrace();
			}
			return result;
		}

		public Step step(Context context) {
			Step result;
			
			if ( ! (syntacticFormTypesToRecurse.contains(currentExpression.getSyntacticFormType())
					|| currentExpression.getSyntacticFormType().equals("Symbol"))) {
				// For expressions other than function applications, tuples, and symbols, this step solver behaves like the step solver of its base rewriter.
				// Note that here we assume that topExpressionIsNextForUsToTakeAStepOn must be true, as it would not make sense for it for be false
				// for non-function applications and non-tuples.
				return baseRewriter.step(currentExpression, context);
			}

			if (!topExpressionIsNextForUsToTakeAStepOn && currentSubExpressionIndex >= subExpressions.size()) {
				result = new Solution(currentExpression);
			}
			else {
				Step step;
				ExpressionLiteralSplitterStepSolver currentStepSolver;

				if (initialCurrentStepSolver != null) {
					currentStepSolver = initialCurrentStepSolver;
				}
				else if (topExpressionIsNextForUsToTakeAStepOn) {
					currentStepSolver = baseRewriter.makeStepSolver(currentExpression);
				}
				else {
					Expression subExpression = subExpressions.get(currentSubExpressionIndex).getExpression();
					currentStepSolver = new RecursiveStepSolver(baseRewriter, subExpression);
				}

				step = currentStepSolver.step(context);

				if (step.itDepends()) {
					RecursiveStepSolver ifTrue = clone();
					ifTrue.initialCurrentStepSolver = step.getStepSolverForWhenSplitterIsTrue();
					RecursiveStepSolver ifFalse = clone();
					ifFalse.initialCurrentStepSolver = step.getStepSolverForWhenSplitterIsFalse();
					result = new ItDependsOn(step, ifTrue, ifFalse);
				}
				else if (topExpressionIsNextForUsToTakeAStepOn) {
					if (step.getValue() == currentExpression) { // no change, move on to current sub-expression (or finish if there is none)
						RecursiveStepSolver next = makeRecursiveStepSolverForSameExpressionButSkippingTopExpression();
						result = next.step(context);
					}
					else { // top expression change, start over on new expression, but no need to apply base rewriter to its top again (we just did it).
						RecursiveStepSolver next = 
								new RecursiveStepSolver(
										baseRewriter, 
										step.getValue(),
										false /* do not evaluate top first */);
						result = next.step(context);
					}
				}
				else { // sub-expression has been evaluated
					ExpressionAndSyntacticContext subExpressionJustEvaluated = subExpressions.get(currentSubExpressionIndex);
					if (step.getValue() == subExpressionJustEvaluated.getExpression()) { // no change, move on to next sub-expression or finish
						RecursiveStepSolver next = makeRecursiveStepSolverForSameExpressionButTargettingNextSubExpression();
						result = next.step(context);
					}
					else { // it changed, so plug evaluated version back into current expression, and evaluate that
						Expression newCurrentExpression = subExpressionJustEvaluated.replaceSubExpressionIn(currentExpression, step.getValue());
						RecursiveStepSolver next = makeRecursiveStepSolverForSameTopExpressionButTargettingNextSubExpression(newCurrentExpression);
						result = next.step(context);
					}
				}
			}

			return result;
		}

	}
}