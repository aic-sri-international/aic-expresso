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
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.collect.ImmutableList;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.PairIterator;

/**
 * A default implementation of {@link ExpressionAndContext}.
 * 
 * @author braz
 */
@Beta
public class DefaultExpressionAndContext implements ExpressionAndContext {
	private static final long serialVersionUID = 1L;
	
	private Expression                expression;
	private List<Integer>             path;
	private ImmutableList<Expression> indexExpressions;
	private ImmutableList<Expression> quantifiedVariables;
	private Expression                constrainingCondition;
	
	public DefaultExpressionAndContext(Expression expression, List<Integer> path) {
		this(expression, path, new LinkedList<Expression>(), Expressions.TRUE);
	}
	
	public DefaultExpressionAndContext(Expression expression, List<Integer> path, List<Expression> indexExpressions) {
		this(expression, path, indexExpressions, Expressions.TRUE);
	}
	
	public DefaultExpressionAndContext(Expression expression, List<Integer> path, List<Expression> indexExpressions, Expression constrainingCondition) {
		this.expression            = expression;
		this.path                  = path;
		this.indexExpressions      = ImmutableList.<Expression>builder().addAll(indexExpressions).build();
		this.quantifiedVariables   = null;
		this.constrainingCondition = constrainingCondition;
	}
	
	//
	// START-ExpressionAndContext

	@Override
	public Expression getExpression() {
		return expression;
	}
	
	@Override
	public ExpressionAndContext setExpression(Expression expression) {
		DefaultExpressionAndContext result = new DefaultExpressionAndContext(expression, getPath(), getIndexExpressions());
		return result;
	}

	@Override
	public List<Integer> getPath() {
		return path;
	}
	
	@Override
	public List<Expression> getIndexExpressions() {
		return indexExpressions;
	}
	
	@Override
	public List<Expression> getQuantifiedVariables() {
		if (quantifiedVariables == null) {
			quantifiedVariables = ImmutableList.<Expression>builder().addAll(IndexExpressions.getIndices(indexExpressions)).build();
		}
		return quantifiedVariables;
	}
	
	@Override 
	public Expression getConstrainingCondition() {
		return constrainingCondition;
	}

	
	
	// END-ExpressionAndContext
	//

	public String toString() {
		return getExpression() + " at " + getPath() + " with quantified variables " + getQuantifiedVariables() + " and constraining condition " + getConstrainingCondition();
	}
	
	public static FunctionIterator<Pair<Expression, List<Integer>>, ExpressionAndContext> makeExpressionAndContextIteratorFromPairs(
			PairIterator<Expression, List<Integer>> expressionAndPathPairsIterator) {
		List<Expression> emptyList = Collections.emptyList();
		return new FunctionIterator<Pair<Expression, List<Integer>>, ExpressionAndContext>(
				new MakerFromExpressionAndPathPair(emptyList),
				expressionAndPathPairsIterator);
	}

	/**
	 * A function mapping lists of two elements, an expression and a path,
	 * to the corresponding knowledge-based sub-expression.
	 */
	public static class MakerFromExpressionAndPathList implements Function<List<Object>, ExpressionAndContext> {
		private List<Expression>       indexExpressions;
		
		public MakerFromExpressionAndPathList(List<Expression> indexExpressions) {
			this.indexExpressions    = indexExpressions;
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public ExpressionAndContext apply(List<Object> expressionAndPath) {
			Expression expression = (Expression) expressionAndPath.get(0);
			List<Integer> path    = (List<Integer>) expressionAndPath.get(1);
			DefaultExpressionAndContext result = new DefaultExpressionAndContext(expression, path, indexExpressions);
			return result;
		}
	}
	
	/**
	 * A function mapping pairs of expression and path
	 * to the corresponding knowledge-based sub-expression.
	 */
	public static class MakerFromExpressionAndPathPair implements Function<Pair<Expression, List<Integer>>, ExpressionAndContext> {
		private List<Expression>       indexExpressions;
		
		public MakerFromExpressionAndPathPair(List<Expression> indexExpressions) {
			this.indexExpressions    = indexExpressions;
		}
		
		@Override
		public ExpressionAndContext apply(Pair<Expression, List<Integer>> expressionAndPath) {
			Expression    expression = expressionAndPath.first;
			List<Integer> path       = expressionAndPath.second;
			DefaultExpressionAndContext result = new DefaultExpressionAndContext(expression, path, indexExpressions);
			return result;
		}
	}

	/**
	 * A function based on a base path that successfully returns ExpressionAndContext objects
	 * based on the function's argument (which must be an expression) and a path formed by
	 * the base path with an incrementing integer at the end.
	 * For example, if the base path is [1,2], the successive paths will be [1,2,0], [1,2,1], and so on,
	 * for each successive call to the function.
	 */
	public static class MakerFromExpressionAndSuccessivePathsFormedFromABasePath implements Function<Expression, ExpressionAndContext> {
		private List<Integer> basePath;
		private int i = 0;
		
		public MakerFromExpressionAndSuccessivePathsFormedFromABasePath(List<Integer> basePath) {
			this.basePath = basePath;
		}
	
		@Override
		public ExpressionAndContext apply(Expression expression) {
			List<Integer> path = new LinkedList<Integer>(basePath);
			path.add(i++);
			ExpressionAndContext result = new DefaultExpressionAndContext(expression, path);
			return result;
		}
	}
}
