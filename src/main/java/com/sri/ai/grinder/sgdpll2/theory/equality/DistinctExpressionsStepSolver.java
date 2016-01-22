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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.getLast;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultExtensionalUniSet;
import com.sri.ai.grinder.sgdpll2.theory.base.AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver;
import com.sri.ai.util.base.OrderedPairsOfIntegersIterator;
import com.sri.ai.util.collect.ImmutableStackedLinkedList;

/**
 * A context-dependent problem step solver deciding the unique values among a set of expressions.
 *
 * @author braz
 *
 */
@Beta
public class DistinctExpressionsStepSolver extends AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver {

	// INSTEAD OF MODIFYING THIS CLASS,
	// make it based on DistinctExpressionsStepSolver.
	
	private List<Expression> uniqueValuesWhenStepSolverWasConstructed;
	
	public DistinctExpressionsStepSolver(ArrayList<Expression> expressions) {
		super(expressions, 0, 1);
		if (expressions.size() < 2) {
			this.uniqueValuesWhenStepSolverWasConstructed = expressions;
		}
		else {
			this.uniqueValuesWhenStepSolverWasConstructed = list();
		}
	}

	private DistinctExpressionsStepSolver(List<Expression> expressions, OrderedPairsOfIntegersIterator initialIndices, List<Expression> uniqueValues) {
		super(expressions, initialIndices);
		this.uniqueValuesWhenStepSolverWasConstructed = uniqueValues;
	}
	
	public List<Expression> getUniqueValuesWhenStepSolverWasConstructed() {
		return uniqueValuesWhenStepSolverWasConstructed;
	}
	
	@Override
	public Solution makeSolutionStepWhenThereAreNoPairs() {
		Solution result = makeSolution(getExpressions());
		return result;
	}

	@Override
	public Solution makeSolutionStepAfterGoingOverAllPairs() {
		// unique expressions after examining all pairs
		// are the same as unique expressions when step solver was constructed
		// plus the last one, because the last expression is always unique.
		List<Expression> uniqueValuesAfterExaminingAllPairs =
				new ImmutableStackedLinkedList<Expression>(
						getLast(getExpressions()), uniqueValuesWhenStepSolverWasConstructed);
		
		Solution result = makeSolution(uniqueValuesAfterExaminingAllPairs);
		
		return result;
	}

	/**
	 * @param distinctValuesList
	 * @return
	 */
	private Solution makeSolution(List<Expression> distinctValuesList) {
		ArrayList<Expression> arrayList = new ArrayList<>(distinctValuesList);
		DefaultExtensionalUniSet set = new DefaultExtensionalUniSet(arrayList);
		Solution result = new Solution(set);
		return result;
	}

	/**
	 * Creates literal for splitting based on ordered pair (i, j).
	 * @return
	 */
	@Override
	public Expression makeLiteral() {
		Expression result = apply(EQUALITY, iThExpression, jThExpression);
		return result;
	}

	/**
	 * @return
	 */
	@Override
	public DistinctExpressionsStepSolver makeSubStepSolverForWhenLiteralIsTrue() {
		DistinctExpressionsStepSolver stepSolverForEquality;
		// if indexed disequals turn out to be equal, move to the next i and register one more non-unique element
		OrderedPairsOfIntegersIterator nextInitialIndices = nextIndices.clone(); nextInitialIndices.makeSureToBeAtRowBeginning(); // note that cloning 'indices' might not work because it may have already just skipped to the next i
		stepSolverForEquality
		= new DistinctExpressionsStepSolver(
				getExpressions(), 
				nextInitialIndices, 
				uniqueValuesWhenStepSolverWasConstructed);
		return stepSolverForEquality;
	}

	/**
	 * @return
	 */
	@Override
	public DistinctExpressionsStepSolver makeSubStepSolverForWhenLiteralIsFalse() {
		DistinctExpressionsStepSolver stepSolverForDisequality;
		// if they turn out to be disequal, keep moving like we did above (move to the next j)
		List<Expression> newUniqueValues;
		if (nextIndices.hadPreviousAndItWasLastOfRow()) { // if row is over, that is, we are done with the current i
			newUniqueValues = new ImmutableStackedLinkedList<>(iThExpression, uniqueValuesWhenStepSolverWasConstructed);
		}
		else {
			newUniqueValues = uniqueValuesWhenStepSolverWasConstructed;
		}
		stepSolverForDisequality
		= new DistinctExpressionsStepSolver(
				getExpressions(), 
				nextIndices, 
				newUniqueValues);
		return stepSolverForDisequality;
	}
}