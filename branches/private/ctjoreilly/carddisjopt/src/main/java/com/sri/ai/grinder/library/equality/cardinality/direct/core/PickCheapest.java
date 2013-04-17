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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityConfiguration;
import com.sri.ai.grinder.library.set.tuple.Tuple;

/**
 * Default implementation of pick_cheapest( Candidates ).
 * 
 * @author oreilly
 * 
 */
@Beta
public class PickCheapest {
	
	private CardinalityConfiguration.PickCheapestTopLevelCostFunction topLevelCostFunction = new DefaultPickCheapestTopLevelCostFunction();
	
	public PickCheapest() {
	
	}
	
	public CardinalityConfiguration.PickCheapestTopLevelCostFunction getPickCheapestTopLevelCostFunction() {
		return topLevelCostFunction;
	}
	
	public void setPickCheapestTopLevelCostFunction(CardinalityConfiguration.PickCheapestTopLevelCostFunction pickCheapestTopLevelCostFunction) {
		this.topLevelCostFunction = pickCheapestTopLevelCostFunction;
	}


	/**
	 * <pre>
	 * pick_cheapest( Candidates )
	 * Candidates is a set of pairs (F, i) where F is a formula and i an index
	 * Returns the pair where F has the least heuristic cost as defined by this function.
	 * 
	 * // cost of an expression is its size times the number of disjunctive operators in it
	 * // or, => and <=> are disjunctive operators
	 * // implementation note: when picking the smallest Fi, down-recurse all candidates in parallel
	 * // keeping the lower bound (accumulated cost so far) for each of them. Stop when you have the
	 * // final cost of a candidate and it is cheaper than the lower bounds of all others. This will avoid
	 * // having to recurse down all of them till the end.
	 * </pre>
	 * 
	 * @param candidates
	 *            a set of pairs (F, i) where F is a formula and i an index.
	 * @return the pair where F has the least heuristic cost as defined by this
	 *         function.
	 */
	public Expression pick(Set<Expression> candidates) {
		return pick(candidates.toArray(new Expression[candidates.size()]));
	}
	
	/**
	 * Convenience method for calling pick_cheapest( Candidates ) with a
	 * variable argument list instead of an explicit Set of candidates.
	 * 
	 * @param candidates
	 *            a variable argument list of pairs (F, i) where F is a formula
	 *            and i an index.
	 * @return the pair where F has the least heuristic cost as defined by this
	 *         function.
	 */
	public Expression pick(Expression... candidates) {
		// Assert input arguments
		// and place in a working data structure
		Expression[] candidateList = new Expression[candidates.length];
		int i = 0;
		for (Expression candidate : candidates) {
			if (Tuple.isTuple(candidate) && Tuple.size(candidate) == 2) {
				candidateList[i] = Tuple.get(candidate, 0);
				i++;
			} 
			else {
				throw new IllegalArgumentException(
						"Candidate is not a pair (F, i) where F is a formula and i an index :"
								+ candidate);
			}
		}
		return candidates[pickIndex(candidateList)];
	}

	/**
	 * Convenience routine for picking the cheapest formula in a variable list
	 * of arguments.
	 * 
	 * @param candidates
	 *            a variable argument list of formula expressions.
	 * @return the index (0..n based on the input order of the candidates
	 *         argument).
	 */
	public int pickIndex(Expression... candidates) {
		int lowestIndex = -1;

		// Set up working attributes
		double lowestCost = Double.MAX_VALUE;
		List<ArrayDeque<Expression>> candidatesNestedStructure = new ArrayList<ArrayDeque<Expression>>();
		double[] accumulatedCandidateCosts = new double[candidates.length];
		ArrayDeque<Expression> candidateNestedStructure;
		Expression candidateElement;
		for (Expression candidate : candidates) {
			candidateNestedStructure = new ArrayDeque<Expression>();
			candidateNestedStructure.addFirst(candidate);
			candidatesNestedStructure.add(candidateNestedStructure);
		}

		// implementation note: when picking the smallest Fi, down-recurse all
		// candidates in parallel keeping the lower bound (accumulated cost so 
		// far) for each of them.
		// Stop when you have the final cost of a candidate and it is cheaper 
		// than the lower bounds of all others. This will avoid having to recurse 
		// down all of them till the end.
		boolean foundCheapest = false;
		int i;
		do {
			// Reset the lowest cost for each pass thru
			lowestCost = Double.MAX_VALUE;
			for (i = 0; i < candidates.length; i++) {
				candidateNestedStructure = candidatesNestedStructure.get(i);
				if (candidateNestedStructure.size() > 0) {
					candidateElement = candidateNestedStructure.removeFirst();
					accumulatedCandidateCosts[i] += topLevelCostFunction.cost(
							candidateElement, candidateNestedStructure);
					if (accumulatedCandidateCosts[i] < lowestCost) {
						lowestCost = accumulatedCandidateCosts[i];
						lowestIndex = i;
					}
				}
			}

			// The lowest index has no more elements and all the
			// other candidates have 0 or more nested elements
			// that can only increase their cost, so we have
			// found the lowest cost element.
			if (candidatesNestedStructure.get(lowestIndex).size() == 0) {
				foundCheapest = true;
			}

		} while (!foundCheapest);

		return lowestIndex;
	}

	// END-PickCheapest
	//
}
