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
package com.sri.ai.grinder.sgdpllt.library.set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.CountingFormula;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.library.FunctorConstants;
import com.sri.ai.grinder.sgdpllt.library.set.tuple.Tuple;

/**
 * A collection of utility routines around counting formula equivalent
 * expressions. These are expressions which have the express goal of
 * representing a number of satisfying solutions. Counting formula equivalent
 * expressions are:
 * <ol>
 * <li>Counting formulas: e.g.:
 * 
 * <pre>
 *     | X in 1..10 : X < 5 |
 * </pre>
 * 
 * </li>
 * <li>Cardinality functor applications to intensional multisets, e.g.:
 * 
 * <pre>
 *     | {{ (on X in 1..10) tuple(X) : X < 5 }} |
 *     and
 *     | {{ (on X in 1..10) p(X) : X < 5 }} |
 * </pre>
 * 
 * </li>
 * <li>Cardinality functor applications to intensional unisets whose head is a
 * tuple over the indices, e.g.:
 * 
 * <pre>
 *     | { (on X in 1..10) tuple(X) : X < 5 } |
 * </pre>
 * 
 * </li>
 * </ol>
 * 
 * @author oreilly
 */
@Beta
public class CountingFormulaEquivalentExpressions {
	private static final Expression _countingFormulaExpressionType = Expressions.makeSymbol("Integer");

	/**
	 * @return the type (i.e. Integer) of all counting formula equivalent
	 *         expressions.
	 */
	public static Expression getType() {
		Expression result = _countingFormulaExpressionType;
		return result;
	}

	/**
	 * Tests if a given expression is equivalent to a counting formula. Three
	 * cases exist for this currently:
	 * <ol>
	 * <li>Instances of the CountingFormula interface.</li>
	 * <li>Applications of the cardinality functor to intensional
	 * multisets.</li>
	 * <li>Applications of the cardinality functor to intensional unisets whose
	 * head is a tuple over the indices of the set.</li>
	 * </ol>
	 * 
	 * @param expression
	 *            the expression to be tested.
	 * @return true if the given expression is equivalent semantically to a
	 *         counting formula, false otherwise.
	 */
	public static boolean isCountingFormulaEquivalentExpression(Expression expression) {
		boolean result = false;

		if (expression instanceof CountingFormula) {
			result = true;
		} else if (expression.hasFunctor(FunctorConstants.CARDINALITY)) {
			Expression cardinalityArg = expression.get(0);
			if (Sets.isIntensionalMultiSet(cardinalityArg)) {
				result = true;
			} else if (Sets.isIntensionalUniSet(cardinalityArg)) {
				IntensionalSet intensionalUniSet = (IntensionalSet) cardinalityArg;
				Expression head = intensionalUniSet.getHead();
				if (Tuple.isTuple(head)) {
					// TODO - we should really test if the args of the tuple
					// match the indices but for now we will trust they are the
					// same.
					result = true;
				}
			}
		}

		return result;
	}

	public static Expression getCondition(Expression countingFormulaEquivalentExpression) {
		Expression result = null;
		if (countingFormulaEquivalentExpression instanceof CountingFormula) {
			result = ((CountingFormula) countingFormulaEquivalentExpression).getBody();
		} else if (countingFormulaEquivalentExpression.hasFunctor(FunctorConstants.CARDINALITY)) {
			Expression cardinalityArg = countingFormulaEquivalentExpression.get(0);
			if (cardinalityArg instanceof IntensionalSet) {
				result = ((IntensionalSet) cardinalityArg).getCondition();
			}
		}

		// fast fail
		if (result == null) {
			throw new IllegalArgumentException("Given argument is not a counting formula equivalent expression: "
					+ countingFormulaEquivalentExpression);
		}

		return result;
	}

	public static IndexExpressionsSet getIndexExpressions(Expression countingFormulaEquivalentExpression) {
		IndexExpressionsSet result = null;
		if (countingFormulaEquivalentExpression instanceof CountingFormula) {
			result = ((CountingFormula) countingFormulaEquivalentExpression).getIndexExpressions();
		} else if (countingFormulaEquivalentExpression.hasFunctor(FunctorConstants.CARDINALITY)) {
			Expression cardinalityArg = countingFormulaEquivalentExpression.get(0);
			if (cardinalityArg instanceof IntensionalSet) {
				result = ((IntensionalSet) cardinalityArg).getIndexExpressions();
			}
		}

		// fast fail
		if (result == null) {
			throw new IllegalArgumentException("Given argument is not a counting formula equivalent expression: "
					+ countingFormulaEquivalentExpression);
		}

		return result;
	}
}