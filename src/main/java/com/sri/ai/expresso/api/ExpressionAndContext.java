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

import com.google.common.annotations.Beta;
import com.google.common.base.Function;

/**
 * An <b>ExpressionAndContext</b> encapsulates information about a sub-expression
 * and the context in which it appears.
 * This includes quantified variables introduced at its scope by its parent expression,
 * new conditions imposed by the parent expression on the sub-expression's scope,
 * as well as information that can be used to replace it by a different sub-expression in the parent expression.
 * 
 * For example, in the expression <code>{ (on X) p(X) : X != a }</code>,
 * the ExpressionAndContext object related to <code>p(X)</code> informs that
 * there is a newly scoped variable <code>X</code> for it, as well as a new condition <code>X != a</code>.
 * It also contains information (the path) that allows its replacement in the parent expression
 * (to create, say, <code>{ (on X) anotherExpression(X) : X != a }</code>).
 * This information is the path over the syntax tree of the parent expression needed
 * to reach the sub-expression
 * (in the future, syntax trees will be more hidden and
 * this will be replaced by more encapsulated and high-level sorts of information).
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
	 * Create a new ExpressionAndContext instance based on this but with its
	 * internal expression replaced by the one passed in.
	 * 
	 * @param expression
	 *            the expression to replace the current expression when creating
	 *            a new ExpressionAndContext.
	 * @return a new ExpressionAndContext based on this but with its internal
	 *         expression replaced by the one passed in.
	 */
	ExpressionAndContext setExpression(Expression expression);

	/**
	 * 
	 * @return the path to the sub-expression from the parent expression.
	 */
	List<Integer> getPath();

	/**
	 * 
	 * @return a list of index expressions ({@link IndexExpressions}) that the parent expression may
	 *         declare that the sub-expression in this context is bound to.
	 */
	List<Expression> getIndexExpressions();

	/**
	 * 
	 * @return a list of quantified variables that the parent expression may
	 *         declare that the sub-expression in this context is bound to.
	 */
	List<Expression> getQuantifiedVariables();

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
}
