/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.grinder.sgdpllt.theory.base;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.StepSolver;
import com.sri.ai.grinder.sgdpllt.core.constraint.ContextSplitting;
import com.sri.ai.grinder.sgdpllt.library.Equality;
import com.sri.ai.util.Util;

public class UnificationStepSolver implements StepSolver<Boolean> {
	
	private List<Expression> unificationEqualitiesToTest;
	private List<Integer> unknownSolutionIndexesForUnificationEqualities;
	private StepSolver.Step<Boolean> precomputedResult;

	public UnificationStepSolver(Expression expression1, Expression expression2) {
		if (expression1.equals(expression2)) { // Check for simple literal equality up front.
			precomputedResult = new StepSolver.Solution<Boolean>(Boolean.TRUE);
		}
		else if (Expressions.isFunctionApplicationWithArguments(expression1) &&
				 Expressions.isFunctionApplicationWithArguments(expression2) &&
				 expression1.numberOfArguments() == expression2.numberOfArguments() &&
				 expression1.getFunctor().equals(expression2.getFunctor())) {
			unificationEqualitiesToTest = Util.zipWith(
					(matchingArgFrom1, matchingArgFrom2) -> Equality.make(matchingArgFrom1, matchingArgFrom2),
					expression1.getArguments(), expression2.getArguments());
		}
		else if (Expressions.isSymbol(expression1) && Expressions.isSymbol(expression2)) {
			unificationEqualitiesToTest = Arrays.asList(Equality.make(expression1, expression2));
		}
		else {
			precomputedResult = new StepSolver.Solution<Boolean>(Boolean.FALSE);
		}
		
		if (unificationEqualitiesToTest != null) {
			unknownSolutionIndexesForUnificationEqualities = 
					IntStream.range(0, unificationEqualitiesToTest.size())
						.boxed()
						.collect(Collectors.toList());
		}
	}

	@Override
	public UnificationStepSolver clone() {
		try {
			return (UnificationStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}
	
	@Override
	public StepSolver.Step<Boolean> step(Context context) {
		StepSolver.Step<Boolean> result = null;
		
		if (precomputedResult != null) {
			result = precomputedResult;
		}
		else {
			List<Integer> stepUnknownSolutionIndexesForUnificationEqualities = new ArrayList<>(unknownSolutionIndexesForUnificationEqualities);
			List<Integer> stepFoundSolutions = new ArrayList<>();
			
			for (Integer unknownSolutionIndex : stepUnknownSolutionIndexesForUnificationEqualities) {
				Expression equality = unificationEqualitiesToTest.get(unknownSolutionIndex);
				ExpressionLiteralSplitterStepSolver evaluatorStepSolver = context.getTheory().makeEvaluatorStepSolver(equality);
//				EvaluatorStepSolver evaluatorStepSolver = new EvaluatorStepSolver(equality);
				Expression equalityResult = evaluatorStepSolver.solve(context);
				if (equalityResult.equals(TRUE)) {
					stepFoundSolutions.add(unknownSolutionIndex);
				}
				else if (equalityResult.equals(FALSE)) {
					// Can't unify
					result = new StepSolver.Solution<>(Boolean.FALSE);
					break;
				}
				else { 
					// Solution to unification equality still unknown
				}	
			}
			
			if (result == null) {
				stepUnknownSolutionIndexesForUnificationEqualities.removeAll(stepFoundSolutions);
				if (stepUnknownSolutionIndexesForUnificationEqualities.size() == 0) {
					// No more unknown solutions and this means all of them were true if we got to here
					result = new StepSolver.Solution<>(Boolean.TRUE);
				}
				else {
					// We still have unknown equality unifications, so will split on the first unknown 
					// of these equalities
					Integer firstUnknownUnificationEqualityIndex = stepUnknownSolutionIndexesForUnificationEqualities.get(0);
					Expression unknownUnificationEqualityToSplitOn = unificationEqualitiesToTest.get(firstUnknownUnificationEqualityIndex);
					
					StepSolver<Boolean> ifTrue;					
					if (stepUnknownSolutionIndexesForUnificationEqualities.size() == 1) {
						// If there is only 1 unknown unification equality remaining, then on the true branch
						// we know the unification will result in true, so just return that known up front.
						ifTrue = new ConstantStepSolver<>(Boolean.TRUE);
					}
					else {
						ifTrue = this.clone();
						((UnificationStepSolver)ifTrue).unknownSolutionIndexesForUnificationEqualities = new ArrayList<>(stepUnknownSolutionIndexesForUnificationEqualities);
					}					
					StepSolver<Boolean> ifFalse = new ConstantStepSolver<>(Boolean.FALSE);
					
					ContextSplitting contextSplitting = null;
					// If the splitter is a literal then we want to include the context splitting
					// information for the literal.
					if (context.getTheory().isLiteral(unknownUnificationEqualityToSplitOn, context)) {
						contextSplitting = new ContextSplitting(unknownUnificationEqualityToSplitOn, context);
					}
					result = new StepSolver.ItDependsOn<>(unknownUnificationEqualityToSplitOn, 
									contextSplitting, 
									ifTrue, ifFalse);					
				}
			}
		}
		
		return result;
	}
}
