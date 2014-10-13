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

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSetInterface;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.AbstractCardinalityRewriter;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.set.tuple.Tuple;

/**
 * Default implementation of R_card_implication(| F |_X, quantification).
 *
 * @author saadati
 */
@Beta
public class CardinalityImplication extends AbstractCardinalityRewriter {
	
	public CardinalityImplication() {
	}
	
	@Override
	public String getName() {
		return R_card_implication;
	}
	
	/**
	 * @see CardinalityRewriter#R_card_implication
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression;
		// Assert input arguments, (| F |_x, quantification)
		if (!(Tuple.isTuple(expression) &&
			  Tuple.size(expression) == 2)
			  ) {
			throw new IllegalArgumentException("Invalid input argument expression, expect (| F |_X, quantification):"+expression);
		}
		Expression cardinalityOfIndexedFormulaExpression = Tuple.get(expression, 0);
		Expression quantificationSymbol                  = Tuple.get(expression, 1);
		// 
		CardinalityUtil.assertIsCardinalityOfIndexedFormulaExpression(cardinalityOfIndexedFormulaExpression);
		// | {(on x1,..., xn)(x1, ..., xn) | F} |
		Expression       intensionalSet          = cardinalityOfIndexedFormulaExpression.get(0);
		Expression       f                       = ((IntensionalSetInterface) intensionalSet).getCondition();
		List<Expression> indexExpressions        = ((IntensionalSetInterface) intensionalSet).getIndexExpressions();
		Expression[]     indexExpressionsAsArray = indexExpressions.toArray(new Expression[indexExpressions.size()]);
		RewritingProcess subProcess = GrinderUtil.extendContextualSymbolsWithIntensionalSetIndices(intensionalSet, process);
		
		CardinalityRewriter.Quantification quantification = CardinalityRewriter.Quantification.getQuantificationForSymbol(quantificationSymbol);
		if (quantification == null) {
			throw new IllegalArgumentException("Invalid quantification symbol: " + quantificationSymbol);
		}
		
		if (f.hasFunctor(Implication.FUNCTOR) ) {
			Trace.log("F is G => H");
			Expression g = f.get(0);
			Expression h = f.get(1);
			
			Trace.log("    G <- R_top_simplify(G)");
			g = subProcess.rewrite(R_top_simplify, g);
			Trace.log("    H <- R_top_simplify(H)");
			h = subProcess.rewrite(R_top_simplify, h);
			
			Trace.log("    return R_card( | R_top_simplify_disjunction(not G or H) |_X, quantification)");
			Expression notGOrH           = CardinalityUtil.makeOr(CardinalityUtil.makeNot(g), h);
			Expression simplifiedNotGOrH = subProcess.rewrite(R_top_simplify_disjunction, notGOrH);
			
			Expression cardNotGOrHIndexedByX = CardinalityUtil.makeCardinalityOfIndexedFormulaExpression(simplifiedNotGOrH, indexExpressionsAsArray);
			result = process.rewrite(R_card,
						CardinalityUtil.argForCardinalityWithQuantifierSpecifiedCall(cardNotGOrHIndexedByX, quantification));
		} 
		else {
			throw new IllegalArgumentException("The input should be of the form G => H");
		}
		
		return result;
	}	
	
}
