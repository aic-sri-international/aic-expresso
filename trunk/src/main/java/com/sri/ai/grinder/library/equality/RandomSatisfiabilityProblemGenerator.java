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
package com.sri.ai.grinder.library.equality;

import java.util.List;
import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;

/**
 * An iterator of random satisfiability expressions, generated according to certain parameters
 * (see {@link #RandomSatisfiabilityProblemGenerator(Random, int, int, int, int, int)} for their description).
 * 
 * @author braz
 *
 */
public class RandomSatisfiabilityProblemGenerator extends AbstractRandomDPLLProblemGenerator {
	
	/**
	 * Creates an iterator over random satisfiability expressions.
	 * This is done by sampling a random formula F from {@link RandomEqualityFormulaGenerator},
	 * picking a random set of supportedIndices I, and returning there exists I : F.
	 * 
	 * @param random a {@link Random} number generator.
	 * @param numberOfVariables the (maximum) number of variables in the formula.
	 * @param numberOfConstants the (maximum) number of constants in the formula.
	 * @param minimumNumberOfIndices minimum number of variables used as supportedIndices (maximum is <i>all</i> variables).
	 * @param depth the depth of the formula (all its sub-expressions with have depth equal to <code>depth - 1</code>).
	 * @param breadth the number of sub-expressions of conjunctions and disjunctions.
	 */
	public RandomSatisfiabilityProblemGenerator(Random random, int numberOfVariables, int numberOfConstants, int minimumNumberOfIndices, int depth, int breadth) {
		super(random, numberOfVariables, numberOfConstants, minimumNumberOfIndices, depth, breadth);
	}

	@Override
	protected Expression makeProblem(Expression formula, List<Expression> indices) {
		Expression problem = new DefaultExistentiallyQuantifiedFormula(new ExtensionalIndexExpressionsSet(indices), formula);
		return problem;
	}

	/** A simple test sampling 10 random satisfiability expressions and printing them to the standard output. */
	public static void main(String[] args) {
		RandomSatisfiabilityProblemGenerator iterator = new RandomSatisfiabilityProblemGenerator(new Random(), 10, 5, 0, 2, 2);
		for (int i = 0; i != 10; i++) {
			System.out.println(iterator.next());	
		}
	}
}
