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

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.AbstractCardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.util.Util;

/**
 * Default implementation of R_equality_in_conjunction(| x_i = t and Phi |_{x1, ..., xn}).
 * 
 * @author oreilly
 *
 */
@Beta
public class EqualityInConjunction extends AbstractCardinalityRewriter {
	
	public EqualityInConjunction() {
	}
	
	@Override
	public String getName() {
		return R_equality_in_conjunction;
	}
	

	public static boolean isOptimizable(Expression expression, RewritingProcess process) {
		boolean result = false;
		if (getFirstEqualityOnIndexInformation(expression) != null) {
			result = true;
		}
		
		return result;
	}
	
	/**
	 * @see CardinalityRewriter#R_equality_in_conjunction
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		// Assert input arguments, (| F |_x, quantification)
		if (!(Tuple.isTuple(expression) &&
			  Tuple.size(expression) == 2)
			  ) {
			throw new IllegalArgumentException("Invalid input argument expression, expect (| F |_X, quantification):"+expression);
		}
		
		Expression cardinalityOfIndexedFormulaExpression  = Tuple.get(expression, 0);
		Expression quantificationSymbol                   = Tuple.get(expression, 1);
		CardinalityRewriter.Quantification quantification = CardinalityRewriter.Quantification.getQuantificationForSymbol(quantificationSymbol);
		
		EqualityOnIndexInformation equalityOnIndexInformation = getFirstEqualityOnIndexInformation(cardinalityOfIndexedFormulaExpression);
		if (equalityOnIndexInformation == null) {
			throw new IllegalArgumentException("Invalid input argument expression, cannot be optimized:"+cardinalityOfIndexedFormulaExpression);
		} 
		else {
			IndexExpressionsSet indexExpressions = equalityOnIndexInformation.indexExpressions;
			List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) indexExpressions).getList();
			if (equalityOnIndexInformation.valuesEquatedToIndex.size() == 0) {
				Trace.log("if t is the same expression as x_i");
				Trace.log("    return R_card(| Phi |_X, quantification)");
				Expression cardinalityWithPhiIndexedByX =
						CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(equalityOnIndexInformation.phi,
								indexExpressionsList.toArray(new Expression[indexExpressionsList.size()]));
				result = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardinalityWithPhiIndexedByX, quantification));
			} 
			else {
				Expression t = equalityOnIndexInformation.valuesEquatedToIndex.iterator().next();
				// i.e {x \ xi}
				List<Expression> newIndexExpressionsList = Util.removeNonDestructively(indexExpressionsList, new IndexExpressions.HasIndex(equalityOnIndexInformation.index));
				Trace.log("return R_card(| R_normalize(Phi[x_i / t]) |_X\\{xi}, quantification) // Phi={}, x_i={}, t={}", equalityOnIndexInformation.phi, equalityOnIndexInformation.index, t);
				Expression phiXiReplacedWithT           = SemanticSubstitute.replace(equalityOnIndexInformation.phi, equalityOnIndexInformation.index, t, process);

				RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIndexExpressions(newIndexExpressionsList, process);
				Expression simplifiedPhiXiReplacedWithT = subProcess.rewrite(R_normalize, phiXiReplacedWithT);
				
				Expression cardPhiXiReplacedWithTIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(simplifiedPhiXiReplacedWithT, newIndexExpressionsList.toArray(new Expression[newIndexExpressionsList.size()]));
				result = process.rewrite(R_card, CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardPhiXiReplacedWithTIndexedByX, quantification));
			}
		}
		
		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	private static class EqualityOnIndexInformation {
		public Expression index;
		public Set<Expression> valuesEquatedToIndex = new LinkedHashSet<Expression>();
		public Expression phi;
		public IndexExpressionsSet indexExpressions;
	}
	
	private static EqualityOnIndexInformation getFirstEqualityOnIndexInformation(Expression expression) {
		EqualityOnIndexInformation result = null;
		
		if (CardinalityUtil.isCardinalityOfIndexedFormulaExpression(expression)) {
			// | {(on I1,..., In) (x1, ..., xn) | F} |
			Expression intensionalSet = expression.get(0);
			Expression constraint     = ((IntensionalSet) intensionalSet).getCondition();
			IndexExpressionsSet indexExpressions = ((IntensionalSet) intensionalSet).getIndexExpressions();
			if (And.isConjunction(constraint)) {
				// Note: want to use a set for efficiency but keep the order by using a linked hash set.
				int index = 0;
				for (Expression conjunct : constraint.getArguments()) {
					// x_i = t and Phi
					if (conjunct.hasFunctor(FunctorConstants.EQUAL)) {
						result = getEqualityOnIndexInformation(constraint, conjunct, indexExpressions, index);
						if (result != null) {
							break;
						}
					}
					index++;
				}
			} 
			else if (constraint.hasFunctor(FunctorConstants.EQUAL)) {
				// Note: want to use a set for efficiency but keep the order by using a linked hash set.
				result = getEqualityOnIndexInformation(constraint, constraint, indexExpressions, 0);
			}
		}
		
		return result;
	}
	
	private static EqualityOnIndexInformation getEqualityOnIndexInformation(Expression formula, Expression equality, IndexExpressionsSet indexExpressions, int indexIndex) {
		EqualityOnIndexInformation result = null;
		List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) indexExpressions).getList();
		for (Expression equalityArgument : equality.getArguments()) {
			if (Util.findFirst(indexExpressionsList, new IndexExpressions.HasIndex(equalityArgument)) != null) {
				result = new EqualityOnIndexInformation();
				result.index = equalityArgument;
				result.valuesEquatedToIndex.addAll(equality.getArguments());
				List<Expression> phiConjuncts = new ArrayList<Expression>();
				if (formula != equality) { // formula is a conjunction, and equality is one of its conjuncts
					phiConjuncts.addAll(formula.getArguments().subList(0, indexIndex));
					phiConjuncts.addAll(formula.getArguments().subList(indexIndex + 1, formula.getArguments().size()));
				} 
				// Using a Set ensures this case is handled:
				// X = X = Y
				result.valuesEquatedToIndex.remove(equalityArgument);
				
				// Ensure this case is handled:
				// X = Y = Z
				// by returning X = Y and pushing X = Z into Phi to be handled later on
				while (result.valuesEquatedToIndex.size() > 1) {
					Expression anotherTerm = result.valuesEquatedToIndex.iterator().next();
					phiConjuncts.add(Equality.make(result.index, anotherTerm));
					result.valuesEquatedToIndex.remove(anotherTerm);
				}
				
				result.phi = And.make(phiConjuncts);
				result.indexExpressions = indexExpressions;
				break;
			}
		}
		return result;
	}
}
