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
package com.sri.ai.expresso.core;

import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.collect.ImmutableList;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.Util;

/**
 * A default implementation of {@link ExpressionAndContext}.
 * 
 * @author braz
 */
@Beta
public class DefaultExpressionAndContext implements ExpressionAndContext {
	private static final long serialVersionUID = 1L;
	
	private Expression                expression;
	private SubExpressionAddress      address;
	private IndexExpressionsSet       indexExpressions;
	private Expression                constrainingCondition;
	
	private ImmutableList<Expression> cachedIndices;

	public DefaultExpressionAndContext(Expression expression) {
		this(expression, SyntaxTreeBasedSubExpressionAddress.get(Util.<Integer>list()));
	}
	
	public DefaultExpressionAndContext(Expression expression, SubExpressionAddress path) {
		this(expression, path, new ExtensionalIndexExpressionsSet(Collections.emptyList()), Expressions.TRUE);
	}
	
	public DefaultExpressionAndContext(Expression expression, SubExpressionAddress address, IndexExpressionsSet indexExpressions, Expression constrainingCondition) {
		this.expression            = expression;
		this.address               = address;
		this.indexExpressions      = indexExpressions;
		this.constrainingCondition = constrainingCondition;

		this.cachedIndices         = null;
	}
	
	//
	// START-ExpressionAndContext

	@Override
	public Expression getExpression() {
		return expression;
	}
	
	@Override
	public ExpressionAndContext setExpression(Expression expression) {
		DefaultExpressionAndContext result = new DefaultExpressionAndContext(expression, getAddress(), getIndexExpressions(), getConstrainingCondition());
		return result;
	}

	@Override
	public SubExpressionAddress getAddress() {
		return address;
	}

	@Override
	public IndexExpressionsSet getIndexExpressions() {
		return indexExpressions;
	}
	
	@Override
	public List<Expression> getIndices() {
		if (cachedIndices == null) {
			cachedIndices = ImmutableList.<Expression>builder().addAll(IndexExpressions.getIndices(indexExpressions)).build();
		}
		return cachedIndices;
	}
	
	@Override 
	public Expression getConstrainingCondition() {
		return constrainingCondition;
	}

	
	
	// END-ExpressionAndContext
	//

	@Override
	public String toString() {
		return getExpression() + " at " + getAddress() + " with quantified variables " + getIndices() + " and constraining condition " + getConstrainingCondition();
	}
}
