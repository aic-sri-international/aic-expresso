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
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpll2.theory.base.AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver;
import com.sri.ai.util.base.OrderedPairsOfIntegersIterator;

/**
 * A context-dependent problem step solver deciding the number of unique values among a set of expressions
 *
 * @author braz
 *
 */
@Beta
public class NumberOfDistinctExpressionsStepSolver extends AbstractDecisionOnAllOrderedPairsOfExpressionsStepSolver {

	private int numberOfUniqueExpressionsWhenStepSolverWasConstructed;
	
	public NumberOfDistinctExpressionsStepSolver(ArrayList<Expression> expressions) {
		super(expressions, 0, 1);
		this.numberOfUniqueExpressionsWhenStepSolverWasConstructed = 0;
	}

	private NumberOfDistinctExpressionsStepSolver(List<Expression> expressions, OrderedPairsOfIntegersIterator initialIndices, int numberOfElementsExaminedSoFar, int numberOfUniqueExpressionsSoFar, int numberOfNonUniqueExpressionsSoFar) {
		super(expressions, initialIndices, numberOfElementsExaminedSoFar);
		this.numberOfUniqueExpressionsWhenStepSolverWasConstructed = numberOfUniqueExpressionsSoFar;
	}
	
	public int getNumberOfUniqueExpressionsWhenStepSolverWasConstructed() {
		return numberOfUniqueExpressionsWhenStepSolverWasConstructed;
	}
	
	@Override
	public Solution makeSolutionStepWhenThereAreNoPairs() {
		Solution result = new Solution(makeSymbol(getExpressions().size()));
		return result;
	}

	/**
	 * Provides solution step after going over all ordered pairs.
	 * @return
	 */
	@Override
	public Solution makeSolutionStepAfterGoingOverAllPairs() {
		Solution result = new Solution(makeSymbol(getNumberOfUniqueExpressionsAfterExaminingAllPairs()));
		return result;
	}

	/**
	 * Creates literal for splitting based on ordered pair (i, j).
	 * @param i
	 * @param j
	 * @return
	 */
	@Override
	public Expression makeLiteral(int i, int j) {
		Expression result = apply(EQUALITY, getExpressions().get(i), getExpressions().get(j));
		return result;
	}

	/**
	 * @param indices
	 * @return
	 */
	@Override
	public NumberOfDistinctExpressionsStepSolver makeSubStepSolverForWhenLiteralIsTrue(OrderedPairsOfIntegersIterator indices) {
		NumberOfDistinctExpressionsStepSolver stepSolverForEquality;
		// if indexed disequals turn out to be equal, move to the next i and register one more non-unique element
		OrderedPairsOfIntegersIterator nextInitialIndices = indices.clone(); nextInitialIndices.makeSureToBeAtRowBeginning(); // note that cloning 'indices' might not work because it may have already just skipped to the next i
		stepSolverForEquality
		= new NumberOfDistinctExpressionsStepSolver(getExpressions(), nextInitialIndices, getNumberOfElementsExaminedWhenStepSolverWasConstructed() + 1, numberOfUniqueExpressionsWhenStepSolverWasConstructed, 0);
		return stepSolverForEquality;
	}

	/**
	 * @param indices
	 * @return
	 */
	@Override
	public NumberOfDistinctExpressionsStepSolver makeSubStepSolverForWhenLiteralIsFalse(OrderedPairsOfIntegersIterator indices) {
		NumberOfDistinctExpressionsStepSolver stepSolverForDisequality;
		// if they turn out to be disequal, keep moving like we did above (move to the next j)
		OrderedPairsOfIntegersIterator nextInitialIndices = indices;
		int newNumberOfUniqueExpressionsSoFar;
		int newNumberOfElementsExaminedSoFar;
		if (indices.hadPreviousAndItWasLastOfRow()) { // if row is over, that is, we are done with the current i
			newNumberOfUniqueExpressionsSoFar = numberOfUniqueExpressionsWhenStepSolverWasConstructed + 1; // we compared i-th against all j-th's, it is equal to none and therefore unique
			newNumberOfElementsExaminedSoFar = getNumberOfElementsExaminedWhenStepSolverWasConstructed() + 1;
		}
		else {
			newNumberOfUniqueExpressionsSoFar = numberOfUniqueExpressionsWhenStepSolverWasConstructed; // not done with i-th yet, number of uniques remains the same
			newNumberOfElementsExaminedSoFar = getNumberOfElementsExaminedWhenStepSolverWasConstructed();
		}
		stepSolverForDisequality
		= new NumberOfDistinctExpressionsStepSolver(getExpressions(), nextInitialIndices, newNumberOfElementsExaminedSoFar, newNumberOfUniqueExpressionsSoFar, 0);
		return stepSolverForDisequality;
	}

	private int getNumberOfUniqueExpressionsAfterExaminingAllPairs() {
		// number of unique expressions after examining all pairs
		// is the same as number of unique expressions when step solver was constructed
		// plus one, because the last element is always unique.
		int numberOfUniqueExpressionsAfterExaminingAllPairs = 
				numberOfUniqueExpressionsWhenStepSolverWasConstructed + 1;
		
		return numberOfUniqueExpressionsAfterExaminingAllPairs;
	}
}