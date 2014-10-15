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
package com.sri.ai.grinder.library.set.intensional;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.StandardizedApartFrom;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.util.Util;

/**
 * A rewriter reducing an equality between intensional uni-sets into a formula
 * on equalities between their defining expressions, correctly handling
 * equalities with multiple arguments, even if some of them are not intensional
 * uni-sets.
 * 
 * @author braz
 */
@Beta
public class EqualityOfIntensionalUniSets extends AbstractRewriter {
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		List<Expression> firstTwoIntensionalUniSets = new ArrayList<Expression>();
		List<Expression> remainingArguments = new ArrayList<Expression>();
		if (expression.hasFunctor("=") &&
				Util.collectFirstN(
						expression.getArguments().iterator(), 2,
						Sets.IS_INTENSIONAL_UNI_SET,
						firstTwoIntensionalUniSets, remainingArguments)
						!= -1) {
			
			Expression set1 = firstTwoIntensionalUniSets.get(0);
			Expression set2 = firstTwoIntensionalUniSets.get(1);
			Expression standardizedApartSet2 = StandardizedApartFrom.standardizedApartFrom(set2, set1, process);

			List<Expression> conjuncts = getConjunctsForEqualityOfTwoIntensionalUniSetsAssumingStandardizationApart(set1, standardizedApartSet2);
			
			if ( ! remainingArguments.isEmpty()) {
				remainingArguments.add(0, set2); // need to keep the equality between the two sets and the rest (by making one of them equal to the rest)
				Expression equality = Equality.make(remainingArguments.toArray());
				conjuncts.add(equality);
			}
			Expression result = And.make(conjuncts);
			return result;
		}
		return expression;
	}

	private static List<Expression> getConjunctsForEqualityOfTwoIntensionalUniSetsAssumingStandardizationApart(Expression expression1, Expression expression2) {
		ArrayList<Expression> result = new ArrayList<Expression>();
		result.add(getBooleanFormulaEquivalentToSet1BeingASubsetOfOrEqualToSet2AssumingStandardizationApart(expression1, expression2));
		result.add(getBooleanFormulaEquivalentToSet1BeingASubsetOfOrEqualToSet2AssumingStandardizationApart(expression2, expression1));
		return result;
	}
	
	/**
	 * Takes two expressions assumed to represent intensional unisets
	 * (although it will also work if they are multi-sets, but in this case it will interpret them as unisets),
	 * standardizes them apart (see {@link #getBooleanFormulaEquivalentToSet1BeingASubsetOfOrEqualToSet2AssumingStandardizationApart(Expression, Expression)}
	 * for a version that does not do that),
	 * and returns a quantified equality logic boolean formula equivalent to the first set being a subset of, or equal to, the second set.
	 */
	public static Expression getBooleanFormulaEquivalentToSet1BeingASubsetOfOrEqualToSet2(Expression set1, Expression set2, RewritingProcess process) {
		Expression standardizedApartSet2 = StandardizedApartFrom.standardizedApartFrom(set2, set1, process);
		Expression result = getBooleanFormulaEquivalentToSet1BeingASubsetOfOrEqualToSet2AssumingStandardizationApart(set1, standardizedApartSet2);
		return result;
	}

	/**
	 * Takes two expressions assumed to represent intensional unisets
	 * (although it will also work if they are multi-sets, but in this case it will interpret them as unisets),
	 * and assumed to be standardized apart,
	 * and returns a quantified equality logic boolean formula equivalent to the first set being a subset of, or equal to, the second set.
	 */
	public static Expression getBooleanFormulaEquivalentToSet1BeingASubsetOfOrEqualToSet2AssumingStandardizationApart(Expression set1, Expression set2) {
		// { (on I1) Head1 | C1 } is subset of { (on I2) Head2 | C2 }
		// is equivalent to
		// for all I1 : C1 => there exists I2 : C2 and Head1 = Head2
		
		List<Expression> indexExpressions1 = ((IntensionalSet) set1).getIndexExpressions();
		Expression head1 = ((IntensionalSet) set1).getHead();
		Expression condition1 = ((IntensionalSet) set1).getCondition();

		List<Expression> indexExpressions2 = ((IntensionalSet) set2).getIndexExpressions();
		Expression head2 = ((IntensionalSet) set2).getHead();
		Expression condition2 = ((IntensionalSet) set2).getCondition();
		
		Expression equality = Equality.make(head1, head2);
		Expression conjunction = Expressions.apply(FunctorConstants.AND, condition2, equality);
		Expression existentialQuantification = ThereExists.make(indexExpressions2, conjunction);
		Expression implication = Expressions.apply(FunctorConstants.IMPLICATION, condition1, existentialQuantification);
		Expression result = ForAll.make(indexExpressions1, implication);

		return result;
	}
}
