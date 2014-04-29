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
package com.sri.ai.grinder.library;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.HasFunctor;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.function.AbstractRewriterDefiningSymmetricFunction;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;

/**
 * Implements disequality.
 * 
 * @author braz
 */
@Beta
public class Disequality extends AbstractRewriterDefiningSymmetricFunction {

	public  static final Expression FUNCTOR = Expressions.createSymbol(FunctorConstants.INEQUALITY);
	//
	public Disequality() {
		this.setReifiedTests(new HasFunctor(FUNCTOR),
				             new HasNumberOfArguments(2));
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
	
		Expression equals = Equality.equalityResultIfItIsKnown(expression, process);
		if (equals != expression) {
			Symbol equalsResult = (Symbol) equals.getSyntaxTree();
			Boolean booleanObject = (Boolean) equalsResult.getValue();
			boolean booleanValue = booleanObject.booleanValue();
			return Expressions.createSymbol(!booleanValue);
		}
		
		return expression;
	}

	public Expression getFunctor() {
		return FUNCTOR;
	}

	public static Expression conditionForSubExpressionsDisequality(
			Expression expression1, Expression expression2) {
		List<Expression> conditionsForSubExpressionsToBeDistinct = listOfDisequalitiesOfSubExpressions(
				expression1, expression2);
		Expression condition = Or.make(conditionsForSubExpressionsToBeDistinct);
		return condition;
	}

	public static List<Expression> listOfDisequalitiesOfSubExpressions(
			Expression expression1, Expression expression2) {
		List<Expression> conditionsForSubExpressionsToBeDistinct = Util
				.zipWith(MAKE_PAIR_DISEQUALITY, Util.listFrom(expression1
						.getImmediateSubExpressionsIterator()), Util
						.listFrom(expression2
								.getImmediateSubExpressionsIterator()));
		return conditionsForSubExpressionsToBeDistinct;
	}

	/**
	 * Makes a disequality of two expressions, returning Expressions.FALSE if
	 * the expressions are identical.
	 */
	public static Expression make(Object expression1Object,
			Object expression2Object) {
		Expression expression1 = Expressions.wrap(expression1Object);
		Expression expression2 = Expressions.wrap(expression2Object);
		if (expression1.equals(expression2)) {
			return Expressions.FALSE;
		}
		return Expressions.apply(FunctorConstants.INEQUALITY, expression1, expression2);
	}

	public static BinaryFunction<Expression, Expression, Expression> MAKE_PAIR_DISEQUALITY = new BinaryFunction<Expression, Expression, Expression>() {
		@Override
		public Expression apply(Expression e1, Expression e2) {
			return make(e1, e2);
		}
	};

	public static boolean isDisequality(Expression expression) {
		return expression.hasFunctor(FunctorConstants.INEQUALITY);
	}
	
	/**
	 * Returns X != c, if 'expression' is c != X, for X a variable and c a constant, or 'expression' otherwise. 
	 */
	public static Expression normalize(Expression expression, RewritingProcess process) {
		if (process.isConstant(expression.get(0))) {
			if ( ! process.isConstant(expression.get(1))) {
				Expression result = Expressions.makeFunctionApplication(expression.getFunctor(), expression.get(1), expression.get(0));
				return result;
			}
		}
		return expression;
	}
}
