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

import static com.sri.ai.util.Util.castOrThrowError;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * An abstract implementation of quantified expressions with a body expression, such
 * as universal and existential quantified formulas, and lambda expressions
 * (but not, for example, intensionally defined sets, which are quantified but
 * have more parts than just a body expression).
 * 
 * @author braz
 */
@Beta
public abstract class AbstractQuantifiedExpressionWithABody extends AbstractQuantifiedExpression implements QuantifiedExpressionWithABody {

	private static final long serialVersionUID = 1L;

	private Expression body;
	private List<ExpressionAndContext> cachedSubExpressionsAndContext;

	public AbstractQuantifiedExpressionWithABody(List<Expression> indexExpressions, Expression body) {
		super(indexExpressions);
		this.body = body;
		makeImmediateSubExpressionsAndContexts();
	}

	@Override
	public Expression getBody() {
		return body;
	}
	
	@Override
	public abstract QuantifiedExpressionWithABody setBody(Expression newBody);

	protected static class BodyAddress implements SubExpressionAddress {
		@Override
		public Expression replace(Expression expression, Expression newBody) {
			AbstractQuantifiedExpressionWithABody quantifiedExpression = castOrThrowError(AbstractQuantifiedExpressionWithABody.class, expression, "Attempt at replacing body expression of %s which should be an instance of %s but is an instance of %s");
			Expression result = quantifiedExpression.setBody(newBody);
			return result;
		}
	}
	
	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(RewritingProcess process) {
		return cachedSubExpressionsAndContext.iterator();
	}

	private void makeImmediateSubExpressionsAndContexts() {
		cachedSubExpressionsAndContext = new LinkedList<ExpressionAndContext>();
		makeIndexExpressionSubExpressionsAndContext(cachedSubExpressionsAndContext);
		cachedSubExpressionsAndContext.add(new DefaultExpressionAndContext(getBody(), new BodyAddress()));
	}
}
