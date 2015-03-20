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
package com.sri.ai.expresso.helper;

import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractExpression;

/**
 * A implementation of {@link Expression} that redirects methods to an inner {@link Expression}.
 * This is useful when implementing extending an interface of {@link Expression}
 * that applies to all implementations of {@link Expression},
 * without having to extend each of those implementations.
 * <p>
 * While {@link Expression} is an immutable type, extensions of this class are allowed to be
 * mutable, as long as they are no longer changed after having being used as an {@link Expression}
 * for the first time. This may be useful for efficiently setting up objects before
 * releasing them for general use.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractExpressionWrapper extends AbstractExpression {

	private static final long serialVersionUID = 1L;
	
	protected Expression cachedInnerExpression;
	
	protected abstract Expression computeInnerExpression();
	
	private Expression getInnerExpression() {
		if (cachedInnerExpression == null) {
			cachedInnerExpression = computeInnerExpression();
		}
		return cachedInnerExpression;
	}
	
	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator() {
		return getInnerExpression().getImmediateSubExpressionsAndContextsIterator();
	}

	@Override
	public List<Expression> getScopedExpressions(RewritingProcess process) {
		return getInnerExpression().getScopedExpressions(process);
	}

	@Override
	public Object getSyntacticFormType() {
		return getInnerExpression().getSyntacticFormType();
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return getInnerExpression().getSyntaxTree();
	}

	@Override
	public Expression replaceSymbol(Expression symbol, Expression newSymbol, RewritingProcess process) {
		return getInnerExpression().replaceSymbol(symbol, newSymbol, process);
	}

	@Override
	public Expression getFunctor() {
		return getInnerExpression().getFunctor();
	}

	@Override
	public List<Expression> getArguments() {
		return getInnerExpression().getArguments();
	}

	@Override
	public Expression set(int i, Expression newIthArgument) {
		return getInnerExpression().set(i, newIthArgument);
	}

	@Override
	public String makeToString() {
		return getInnerExpression().toString();
	}
	
	/**
	 * Overridden in order to avoid using {@link Expression}'s implementation,
	 * which would use the cached inner expression and not produce
	 * an update String if the instance changed (remember that instances
	 * to this class are allowed to change before being used as an expression 
	 * for the first time).
	 * This implementation re-computes an Expression every time it is invoked.
	 */
	@Override
	public String toString() {
		return computeInnerExpression().toString();
	}
}
