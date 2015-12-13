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
package com.sri.ai.grinder.sgdpll2.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.sgdpll2.api.Constraint2;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.core.constraint.ConstraintSplitting;
import com.sri.ai.util.base.OrderedPairsOfIntegersIterator;
import com.sri.ai.util.base.PairOf;

/**
 * A context-dependent problem step solver deciding whether the number of unique given expression is smaller than a given limit
 *
 * @author braz
 *
 */
@Beta
public class NumberOfDistinctExpressionsIsLessThanStepSolver implements ContextDependentProblemStepSolver {

	private int limit;
	private List<Expression> expressions;
	private OrderedPairsOfIntegersIterator initialIndices;
	// indices ranges over pairs (i, j) of indices of 'expressions'
	// all expressions before the one indexed by i have been checked to be distinct or not from all others in front of it
	// all expressions after the one indexed by i and before the one indexed by j have been checked to be distinct from the one indexed by i
	private int numberOfNonUniqueExpressionsSoFar; // number of expressions before the one indexed by i that are equal to some element after it (that is, they don't count towards the number of distinct disequals
	
	public NumberOfDistinctExpressionsIsLessThanStepSolver(int limit, ArrayList<Expression> expressions) {
		this(limit, expressions, 0, 1, 0);
	}

	public NumberOfDistinctExpressionsIsLessThanStepSolver(int limit, List<Expression> expressions, int i, int j, int numberOfUniqueExpressionsSoFar) {
		super();
		this.limit = limit;
		this.expressions = expressions;
		this.initialIndices = new OrderedPairsOfIntegersIterator(expressions.size(), i, j);
		this.numberOfNonUniqueExpressionsSoFar = numberOfUniqueExpressionsSoFar;
	}

	private NumberOfDistinctExpressionsIsLessThanStepSolver(int limit, List<Expression> expressions, OrderedPairsOfIntegersIterator initialIndices, int numberOfUniqueExpressionsSoFar) {
		super();
		this.limit = limit;
		this.expressions = expressions;
		this.initialIndices = initialIndices;
		this.numberOfNonUniqueExpressionsSoFar = numberOfUniqueExpressionsSoFar;
	}

	@Override
	public ContextDependentProblemStepSolver clone() {
		try {
			return (ContextDependentProblemStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}

	@Override
	public SolutionStep step(Constraint2 contextualConstraint, RewritingProcess process) {

		OrderedPairsOfIntegersIterator indices = initialIndices.clone();
		
		if (indices.hasNext()) {
			PairOf<Integer> pair = indices.next();
			int i = pair.first;
			int j = pair.second;

			if (numberOfUniqueExpressionsSoFar(i) >= limit) {
				return new Solution(FALSE);
			}
			else if (numberOfUniqueExpressionsSoFar(i) + maximumPossibleNumberOfRemainingUniqueExpressions(i) < limit) {
				// we already know the limit will never be reached
				return new Solution(TRUE);
			}

			Expression equality = apply(EQUALITY, expressions.get(i), expressions.get(j));

			ContextDependentProblemStepSolver stepSolverForEquality    = null; // this null is never used, just making compiler happy
			ContextDependentProblemStepSolver stepSolverForDisequality = null; // this null is never used, just making compiler happy

			ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, equality, process);
			if (split.getResult().equals(ConstraintSplitting.Result.CONSTRAINT_IS_CONTRADICTORY)) {
				return null;
			}
			
			boolean equalityHolds    =  split.getResult() == ConstraintSplitting.Result.LITERAL_IS_TRUE;
			boolean disequalityHolds = !equalityHolds && split.getResult() == ConstraintSplitting.Result.LITERAL_IS_FALSE;
			boolean undefined = !equalityHolds && !disequalityHolds;
			boolean needStepSolverForEquality    = equalityHolds    || undefined;
			boolean needStepSolverForDisequality = disequalityHolds || undefined;
			
			if (needStepSolverForEquality) {
				// if indexed disequals turn out to be equal, move to the next i and register one more non-unique element
				OrderedPairsOfIntegersIterator nextInitialIndices = initialIndices.clone(); nextInitialIndices.incrementI(); // note that cloning 'indices' might not work because it may have already just skipped to the next i
				stepSolverForEquality
				= new NumberOfDistinctExpressionsIsLessThanStepSolver(limit, expressions, nextInitialIndices, numberOfNonUniqueExpressionsSoFar + 1);
			}
			
			if (needStepSolverForDisequality) {
				// if they turn out to be disequal, keep moving like we did above (move to the next j)
				OrderedPairsOfIntegersIterator nextInitialIndices = indices;
				stepSolverForDisequality
				= new NumberOfDistinctExpressionsIsLessThanStepSolver(limit, expressions, nextInitialIndices, numberOfNonUniqueExpressionsSoFar);
			}
			
			if (equalityHolds) {
				return stepSolverForEquality.step(split.getConstraintAndLiteral(), process);
			}
			else if (disequalityHolds) {
				return stepSolverForDisequality.step(split.getConstraintAndLiteralNegation(), process);
			}
			else {
				return new ItDependsOn(equality, split, stepSolverForEquality, stepSolverForDisequality);
			}
		}
		// went over all pairs
		int elementsExaminedSoFar = expressions.size();
		return new Solution(makeSymbol(numberOfUniqueExpressionsSoFar(elementsExaminedSoFar) < limit));
	}

	private int maximumPossibleNumberOfRemainingUniqueExpressions(int elementsExaminedSoFar) {
		return expressions.size() - elementsExaminedSoFar;
	}

	private int numberOfUniqueExpressionsSoFar(int elementsExaminedSoFar) {
		return elementsExaminedSoFar - numberOfNonUniqueExpressionsSoFar;
	}
}