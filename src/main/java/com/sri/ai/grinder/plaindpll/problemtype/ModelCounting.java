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
package com.sri.ai.grinder.plaindpll.problemtype;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;

import java.util.Random;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.plaindpll.group.SymbolicPlusGroup;
import com.sri.ai.util.base.Pair;

/**
 * Satisfiability uses the boolean group and does not boolean formulas
 * (applies the group additive operation, disjunction, directly on them).
 * 
 * @author braz
 *
 */
public class ModelCounting extends AbstractGroupProblemTypeWithFunctionApplicationExpression {

	public ModelCounting() {
		super(new SymbolicPlusGroup());
	}
	
	@Override
	public String getFunctorString() {
		return FunctorConstants.CARDINALITY;
	}

	/**
	 * We override this method for model counting because the we must use ONE,
	 * instead of getHead(), to build the body as done by super's implementation.
	 */
	@Override
	public Pair<Expression, IndexExpressionsSet> getExpressionAndIndexExpressionsFromRewriterProblemArgument(Expression expression, RewritingProcess process) {
		CardinalityUtil.assertIsCardinalityOfIndexedFormulaExpression(expression);
		IntensionalSet set = (IntensionalSet) expression.get(0);
		Expression body = IfThenElse.make(set.getCondition(), ONE, ZERO);
		Pair<Expression, IndexExpressionsSet> result = Pair.make(body, set.getIndexExpressions());
		return result;
	}

	@Override
	public Expression makeRandomConstant(Random random) {
		Expression result = makeSymbol(random.nextBoolean());
		return result;
	}
}
