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
package com.sri.ai.grinder.library.equality.cardinality.direct.core;

import java.util.Deque;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityConfiguration;

/**
 * Default implementation of the {@link CardinalityConfiguration.PickCheapestTopLevelCostFunction} interface.
 * 
 * @author oreilly
 *
 */
@Beta
public class DefaultPickCheapestTopLevelCostFunction implements CardinalityConfiguration.PickCheapestTopLevelCostFunction {

	public DefaultPickCheapestTopLevelCostFunction() {
		
	}
	
	
	//
	// START-CardinalityConfiguration.PickCheapestTopLevelCostFunction
	@Override
	public double cost(Expression expression, Deque<Expression> nestedStructure) {
		double result = 0;
		
		// cost of an expression is its size times 
		// the number of disjunctive operators in it
		
		// i.e. itself and the # of children it has
		int size  = 1 + expression.numberOfArguments();
		// Don't want to multiply by 0.
		int times = 1 + numberOfDisjuncts(expression, nestedStructure);
		
		result = size * times;
		
		return result;
	}
	// END-CardinalityConfiguration.PickCheapestTopLevelCostFunction
	//
	
	//
	// PRIVATE METHODS
	//
	
	private int numberOfDisjuncts(Expression expression, Deque<Expression> nestedStructure) {
		int result = 0;
		if (isDisjunctive(expression)) {
			result++;
		}

		for (Expression arg : expression.getArguments()) {
			// Meet the interfaces contractual obligation
			// Note: this is an optimization that saves
			// traversing the argument list twice (once here
			// and once inside of pick cheapest).
			nestedStructure.addLast(arg);
			if (isDisjunctive(arg)) {
				result++;
			}
		}
		
		return result;
	}
	
	// or, => and <=> are disjunctive operators
	private boolean isDisjunctive(Expression expression) {
		boolean result = false;
		
		if (expression.hasFunctor(FunctorConstants.OR) ||
			expression.hasFunctor(FunctorConstants.IMPLICATION) ||
			expression.hasFunctor(FunctorConstants.EQUIVALENCE)) {
			result = true;
		}
		
		return result;
	}
}
