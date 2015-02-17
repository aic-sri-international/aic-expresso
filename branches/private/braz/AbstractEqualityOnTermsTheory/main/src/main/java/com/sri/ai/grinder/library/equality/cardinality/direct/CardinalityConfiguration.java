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
package com.sri.ai.grinder.library.equality.cardinality.direct;

import java.util.Deque;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;

/**
 * Cardinality configurable parameters.
 * 
 * @author oreilly
 * 
 */
@Beta
public interface CardinalityConfiguration {
	/**
	 * Calculates the heuristic cost associated with the information contained
	 * in the the expression passed in but only taking into account the type of
	 * expression and the number of children it contains and their types but not
	 * their nested structure.
	 * 
	 * @author oreilly
	 * 
	 */
	interface PickCheapestTopLevelCostFunction {
		/**
		 * 
		 * @param expression
		 *            the expression whose type and # of arguments plus their
		 *            types are to be used to associate a cost with.
		 * @param nestedStructure
		 *            each of the arguments of the input expression should be
		 *            added in order via addLast() to the nested structure.
		 * @return the heuristic cost associated with the type of expression
		 *         passed in and the number of arguments and their types it
		 *         contains.
		 */
		double cost(Expression expression, Deque<Expression> nestedStructure);
	}

	/**
	 * 
	 * @return the top level cost function to be used by pick cheapest.
	 */
	PickCheapestTopLevelCostFunction getPickCheapestTopLevelCostFunction();

	/**
	 * 
	 * @param pickCheapestTopLevelCostFunction
	 *            The top level cost function to be used by pick cheapest.
	 */
	void setPickCheapestTopLevelCostFunction(
			PickCheapestTopLevelCostFunction pickCheapestTopLevelCostFunction);
}
