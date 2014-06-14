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
package com.sri.ai.grinder.helper;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.SyntacticSubstitute;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * Under developement. 
 */
@Beta
public class SymbolicMap  {

	public static class UnificationToIntensionalSetResult {
		public Expression conditionOnExpressionVariables;
		public Map<Expression, Expression> mapOfUnifiedVariables = new LinkedHashMap<Expression, Expression>();
		public boolean isSuccessful() {
			boolean result = ! conditionOnExpressionVariables.equals(Expressions.FALSE);
			return result;
		}
	}

	/**
	 * Returns result of unifying an expression to the elements of an intensional set,
	 * <b>assuming</b> that both the expression and the intensional set head
	 * are (possible nested) injective expressions,
	 * running a given rewriter at the end
	 * (unification result will only directly indicate whether a unification has been found or not
	 * if this rewriter is a complete simplifier or normalizer).
	 * Variables and constants are considered trivial injective expressions.
	 * For example, assume we want to unify <code>(Y, Z)</code> to the elements in set <code>{(on X) (X, X) | X != a}</code>.
	 * Because tuples are injective expressions, we obtain a result containing the condition
	 * <code>Y != and Z != a</code> and mapping <code>X -> Z</code>
	 * (<code>X -> Y</code> is also correct but implementation will pick the last occurring variable,
	 * although that may change).
	 */
	@SuppressWarnings("unchecked")
	public UnificationToIntensionalSetResult unifyToIntensionalSetAssumingInjectiveExpressions(Expression expression, Expression intensionalSet, String completeNormalizerName, RewritingProcess process) {
		
		List<? extends Object> cacheKey = Util.list(expression, intensionalSet, completeNormalizerName, process.getContextualConstraint());
		UnificationToIntensionalSetResult result = cache.get(cacheKey);
		
		if (result == null) {
			result = new UnificationToIntensionalSetResult();

			// just here for now while we switch algorithms
			result.conditionOnExpressionVariables = Expressions.FALSE; // by default, it fails

			Expression originalIntensionalSet = intensionalSet;
			intensionalSet = StandardizedApartFrom.standardizedApartFrom(originalIntensionalSet, expression, process);

			Expression intensionalSetHead = IntensionalSet.getHead(intensionalSet);

			Pair<List<Expression>, List<Expression>> symbols = GrinderUtil.getListsOfElementsToBeUnifiedInInjectiveExpressions(expression, intensionalSetHead, process);
			if (symbols == null) {
				return result;
			}

			// Prepare data structure for storing condition on variables of looked up value.
			List<Expression> conjunctsOfConditionOnExpressionVariables = new LinkedList<Expression>();

			// Go over random variable arguments pairs, compare them for obvious contradictions (distinct constants having to be equal)
			// and collect mapping from logical variables in message value set to logical variables in previous message
			Iterator<Expression> argumentsFromExpressionIterator         = symbols.first.iterator();
			Iterator<Expression> argumentsFromIntensionalSetHeadIterator = symbols.second.iterator();
			while (argumentsFromIntensionalSetHeadIterator.hasNext()) {
				if ( ! argumentsFromExpressionIterator.hasNext())
					throw new Error("Iterators over arguments have ranges of difference lengths even though injective tokens were equal: " + intensionalSet + " and " + expression);

				Expression argumentFromExpression = argumentsFromIntensionalSetHeadIterator.next();
				Expression argumentFromIntensionalSetHead = argumentsFromExpressionIterator.next();

				if (process.isConstant(argumentFromExpression)) {
					if (process.isConstant(argumentFromIntensionalSetHead)) {
						if ( ! argumentFromExpression.equals(argumentFromIntensionalSetHead)) {
							return result; // two distinct constants, no possible match
						}
						else {
							// no need to do anything, equality becomes tautology
						}
					}
					else {
						// keep track of the fact that expression's logical variable needs to be equal to this constant
						conjunctsOfConditionOnExpressionVariables.add(Equality.make(argumentFromIntensionalSetHead, argumentFromExpression));
					}
				}
				else {
					// keep track of that fact that the logical variable
					// in intensional set head needs to be translated to the corresponding logical variable or constant in expression
					result.mapOfUnifiedVariables.put(argumentFromExpression, argumentFromIntensionalSetHead);
				}
			}

			// Conjoin unification equalities and intensional set condition, translated to logical variables in expression
			Expression intensionalSetCondition = IntensionalSet.getCondition(intensionalSet);
			Expression intensionalSetConditionInExpressionVariables = SyntacticSubstitute.replaceAll(intensionalSetCondition, result.mapOfUnifiedVariables, process);
			conjunctsOfConditionOnExpressionVariables.add(intensionalSetConditionInExpressionVariables);
			Expression unnormalizedCondition = And.make(conjunctsOfConditionOnExpressionVariables);

			result.conditionOnExpressionVariables = process.rewrite(completeNormalizerName, unnormalizedCondition);

			// provide the "external world" with a map from the original indices to the symbols in expression
			result.mapOfUnifiedVariables = getMapFromOriginalIndicesToExpressionSymbols(result.mapOfUnifiedVariables, intensionalSet, originalIntensionalSet);
			
			cache.put((List<Object>) cacheKey, result);
		}
		
		return result;
	}

	private Map<Expression, Expression> getMapFromOriginalIndicesToExpressionSymbols(Map<Expression, Expression> fromStandardizedApartIndicesToExpressionSymbols, Expression standardizedApartIntensionalSet, Expression originalIntensionalSet) {
		List<Expression> standardizedApartIndices = IntensionalSet.getIndices(standardizedApartIntensionalSet);
		List<Expression> originalIndices          = IntensionalSet.getIndices(originalIntensionalSet);
		Map<Expression, Expression> fromOriginalIndicesToStandardizedApartIndices = Util.mapFromListOfKeysAndListOfValues(originalIndices, standardizedApartIndices);
		
		Map<Expression, Expression> result = Util.composeMaps(fromOriginalIndicesToStandardizedApartIndices, fromStandardizedApartIndicesToExpressionSymbols);
		
		return result;
	}

	private ConcurrentHashMap<List<Object>, UnificationToIntensionalSetResult> cache = new ConcurrentHashMap<List<Object>, UnificationToIntensionalSetResult>();
}