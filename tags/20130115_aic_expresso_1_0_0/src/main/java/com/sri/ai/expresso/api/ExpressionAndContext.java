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
package com.sri.ai.expresso.api;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;

/**
 * An <b>ExpressionAndContext</b> is a pair of an expression and the context
 * information indicating where it fits in another, parent expression it is a
 * sub-expression of. As such, it contains information allowing its rewritings
 * to substitute for it in the expression it is a sub-expression of. For
 * example, a sub-expression may encapsulate an expression that will be
 * rewritten. This rewriting needs to be re-inserted in whatever expression is
 * its parent. The sub-expression, which was provided by this parent expression,
 * contains the information regarding the sub-expression's location in the
 * parent expression. For expressions, this will typically be the path of the
 * symbol subtree. For example, 1 will have path (0) with respect to f(1,2).
 * However, expressions based on more complex syntax trees can provide
 * sub-expressions based on sub-sub-syntax trees as their sub-expressions, as
 * for example D in { f(X) }_(X in D), since '_' and 'in' are the functors of
 * sub-syntax trees of this expression that are not provided as sub-expressions;
 * D is provided directly as such, and its path will be (1,1). Paths to functors
 * use the number -1.
 * 
 * @author braz
 */
@Beta
public interface ExpressionAndContext extends Serializable {
	public static final Function<ExpressionAndContext, Expression> GET_EXPRESSION = new Function<ExpressionAndContext, Expression>() {
		@Override
		public Expression apply(ExpressionAndContext expressionAndContext) {
			return  expressionAndContext.getExpression();
		}
	};

	/**
	 * 
	 * @return an expression that is a sub-expression of a parent expression
	 *         that determines it current context.
	 */
	Expression getExpression();

	/**
	 * 
	 * @return the path to the sub-expression from the parent expression.
	 */
	List<Integer> getPath();

	/**
	 * 
	 * @return a set of quantified variables that the parent expression may
	 *         declare that the sub-expression in this context is bound to.
	 */
	Set<Expression> getQuantifiedVariables();

	/**
	 * The constraining condition that a parent expression imposes on its
	 * sub-expression in this context. For example, the expression:
	 * 
	 * <pre>
	 * if A = B then 1 else 2
	 * 
	 * imposes the condition 'A = B' on the then branch sub-expression '1' 
	 * and 'not(A = B)' on the else branch sub-expression '2'.
	 * </pre>
	 * 
	 * @return if no constraining condition specified should return 'true'
	 *         otherwise an expression representing the condition that the
	 *         parent expression imposes on the sub-expression in this context.
	 */
	Expression getConstrainingCondition();

	/**
	 * Create a new ExpressionAndContext instance based on this but with its
	 * internal expression replaced by the one passed in.
	 * 
	 * @param expression
	 *            the expression to replace the current expression when creating
	 *            a new ExpressionAndContext.
	 * @return a new ExpressionAndContext based on this but with its internal
	 *         expression by the one passed in.
	 */
	ExpressionAndContext setExpression(Expression expression);
}
