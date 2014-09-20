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
package com.sri.ai.grinder.expression;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.core.DefaultExpressionAndContext;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.ExpressionAndContextDepthFirstIterator;
import com.sri.ai.util.base.NullaryFunction;
import com.sri.ai.util.cache.DefaultCacheMap;

/**
 * A cache for use at rewriting processes with expressions.
 * 
 * @author braz
 */
@Beta
public class ExpressionCache extends DefaultCacheMap<ExpressionCacheKey, Expression> {
	public ExpressionCache(
			long maximumSize,
			NullaryFunction<Iterator<ExpressionCacheKey>> reachableExpressionsIteratorMaker,
			int garbageCollectionPeriod) {
		super(false, maximumSize, reachableExpressionsIteratorMaker, garbageCollectionPeriod);
	}
	
	public ExpressionCacheKey getCacheKeyFor(Expression expression, RewritingProcess process) {
		ExpressionCacheKey result = new ExpressionCacheKey(expression, process.getContextualConstraint());
		return result;
	}
	
	public static Iterator<ExpressionCacheKey> makeIteratorFor(Expression rootExpression, RewritingProcess process) {
		Iterator<ExpressionCacheKey> result = null;
		
		List<ExpressionCacheKey> expressionKeys = new ArrayList<ExpressionCacheKey>();
		//
		ExpressionAndContext rootExpressionAndContext                       = new DefaultExpressionAndContext(rootExpression);
		ExpressionAndContextDepthFirstIterator expressionAndContextIterator = new ExpressionAndContextDepthFirstIterator(rootExpressionAndContext, process);
		
		while (expressionAndContextIterator.hasNext()) {
			ExpressionAndContext expressionAndContext = expressionAndContextIterator.next();
			expressionKeys.add(new ExpressionCacheKey(expressionAndContext.getExpression(), expressionAndContext.getConstrainingCondition()));
		}
		
		result = expressionKeys.iterator();
		
		return result;
	}
	
	/**
	 * Shorthand for <code>put(getCacheKeyFor(keyExpression, process), valueExpression)</code>.
	 */
	public Expression putUnderKeyFor(Expression keyExpression, Expression valueExpression, RewritingProcess process) {
		Expression result = put(getCacheKeyFor(keyExpression, process), valueExpression);
		return result;
	}
	
	/**
	 * Shorthand for <code>put(getCacheKeyFor(keyExpression, process), valueExpression)</code>.
	 */
	public Expression getUnderKeyFor(Expression keyExpression, RewritingProcess process) {
		Expression result = get(getCacheKeyFor(keyExpression, process));
		return result;
	}
	
	/**
	 * Shorthand for <code>containsKey(getCacheKeyFor(expression, process))</code>.
	 */
	public boolean containsKeyFor(Expression expression, RewritingProcess process) {
		boolean result = containsKey(getCacheKeyFor(expression, process));
		return result;
	}
	
	@Override 
	public Expression get(Object key) {
		Expression result = null;
		if (key instanceof ExpressionCacheKey) {
			Expression cachedItem = super.get(key);
			if (cachedItem != null) {
				Expression lookupExpression = ((ExpressionCacheKey)key).getExpression();
				if (cachedItem.equals(lookupExpression)) {
					// If the same as the input expression
					// return the input expression to ensure
					// no change is detected by an external 
					// rewriter's logic.
					result = lookupExpression;
				} 
				else {
					result = cachedItem;
				}
			}
		}
		
		return result;
	}
}
