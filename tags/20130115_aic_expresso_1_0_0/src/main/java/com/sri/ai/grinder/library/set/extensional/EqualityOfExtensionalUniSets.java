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
package com.sri.ai.grinder.library.set.extensional;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.util.Util;

/**
 * A rewriter reducing an equality between extensional uni-sets into a formula
 * on equalities between their elements, correctly handling equalities with
 * multiple arguments, even if some of them are not extensional uni-sets.
 * 
 * @author braz
 */
@Beta
public class EqualityOfExtensionalUniSets extends AbstractRewriter {
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		List<Expression> firstTwoExtensionalUniSets = new ArrayList<Expression>();
		List<Expression> remainingArguments = new ArrayList<Expression>();
		if (expression.hasFunctor("=") &&
				Util.collectFirstN(
						expression.getArguments().iterator(), 2,
						Sets.IS_EXTENSIONAL_UNI_SET,
						firstTwoExtensionalUniSets, remainingArguments)
						!= -1) {
			List<Expression> conjuncts =
					getConjunctsForEqualityOfTwoExtensionalUniSets(firstTwoExtensionalUniSets.get(0), firstTwoExtensionalUniSets.get(1));
			
			if ( ! remainingArguments.isEmpty()) {
				remainingArguments.add(0, firstTwoExtensionalUniSets.get(1)); // need to keep the equality between the two sets and the rest (by making one of them equal to the rest)
				Expression equality = Equality.make(remainingArguments.toArray());
				conjuncts.add(equality);
			}
			Expression result = And.make(conjuncts);
			return result;
		}
		return expression;
	}

	private static List<Expression> getConjunctsForEqualityOfTwoExtensionalUniSets(Expression expression1, Expression expression2) {
		ArrayList<Expression> result = new ArrayList<Expression>();
		getConjunctsForAllElementsInSet1ToBeInSet2(expression1, expression2, result);
		getConjunctsForAllElementsInSet1ToBeInSet2(expression2, expression1, result);
		return result;
	}
	
	private static void getConjunctsForAllElementsInSet1ToBeInSet2(Expression expression1, Expression expression2, List<Expression> results) {
		for (int i = 0; i != ExtensionalSet.cardinality(expression1); i++) {
			Expression element1 = ExtensionalSet.getElements(expression1).get(i);
			ArrayList<Expression> disjunctsForElement1ToBeInSet2 = new ArrayList<Expression>();
			for (int j = 0; j != ExtensionalSet.cardinality(expression2); j++) {
				Expression element2 = ExtensionalSet.getElements(expression2).get(j);
				disjunctsForElement1ToBeInSet2.add(Equality.make(element1, element2));
			}
			results.add(Or.make(disjunctsForElement1ToBeInSet2));
		}
	}
}
