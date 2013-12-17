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
package com.sri.ai.grinder.library.boole;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultExpressionAndContext;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.ScopedVariables;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;

/**
 * Provides the basic mechanism for providing sub-expressions and scoped
 * variables for quantified expressions with one index. It relies on two
 * abstract methods, {@link #getRootTreeString()} and
 * {@link #getSyntacticFormType()}, to provide the details for a particular
 * quantifier. It does assume the index is the first subtree of the expression's
 * SyntaxTree. There can be only one index per quantifier expression.
 * Eventually, we should just make it uniform with IntensionalSet functionality.
 * 
 * @author braz
 */
@Beta
public abstract class QuantifierSubExpressionAndScopedVariableProvider
extends AbstractRewriter
implements ExpressionKnowledgeModule.Provider, ScopedVariables.Provider, NoOpRewriter
{
	private static final List<Integer> _pathZero = Collections.unmodifiableList(Arrays.asList(new Integer(0)));
	private static final List<Integer> _pathOne  = Collections.unmodifiableList(Arrays.asList(new Integer(1)));


	public QuantifierSubExpressionAndScopedVariableProvider() {
		super();
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(Expression expression, final RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			
			// get the body of the expression
			ExpressionAndContext bodyExpressionAndContext =
				new DefaultExpressionAndContext(
						expression.getSyntaxTree().getSubTree(1),
						_pathOne,
						getIndexExpressions(expression));
			
			// get arguments of index, for example the X and Y in there exists p(X,Y) : ...
			List<Integer> indexBasePath = _pathZero; // path to get to index; this is the base for paths to arguments of the index, which are also sub-expressions.
			SyntaxTree indexExpression = expression.getSyntaxTree().getSubTree(0); // remember that the index expression is not a sub-expression, only a sub-syntax tree
			Expression index = indexExpression.hasFunctor(FunctorConstants.IN)? indexExpression.get(0) : indexExpression;
			Iterator<ExpressionAndContext> indexArgumentsAndContextsIterator =
				new FunctionIterator<Expression, ExpressionAndContext>(
						index.getArguments(),
						new DefaultExpressionAndContext.
						MakerFromExpressionAndSuccessivePathsFormedFromABasePath(indexBasePath));
			
			return new NestedIterator<ExpressionAndContext>(bodyExpressionAndContext, indexArgumentsAndContextsIterator);
			
		}
		return null;
	}

	private boolean knowledgeApplies(Expression expression) {
		return
		expression != null && expression.getSyntaxTree().getRootTree() != null &&
		expression.getSyntaxTree().getRootTree().equals(getRootTreeString());
	}

	@Override
	public Object getSyntacticFormType(Expression expression, RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			return getSyntacticFormType();
		}
		return null;
	}

	protected abstract String getRootTreeString();
	
	protected abstract String getSyntacticFormType();
	
	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
		ExpressionKnowledgeModule knowledgeBasedExpressionModule =
			(ExpressionKnowledgeModule) process.findModule(ExpressionKnowledgeModule.class);
		if (knowledgeBasedExpressionModule != null) {
			knowledgeBasedExpressionModule.register(this);
		}
		
		ScopedVariables scopedVariables =
			(ScopedVariables) process.findModule(ScopedVariables.class);
		if (scopedVariables != null) {
			scopedVariables.register(this);
		}
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: Is a NoOpRewriter
		return expression; // will be removed eventually, not a real rewriter, just a module.
	}

	public static List<Expression> getIndexExpressions(Expression expression) {
		return Expressions.ensureListFromKleeneList(expression.getSyntaxTree().getSubTree(0)); // does need to be sub tree
	}

	@Override
	public Expression getScopedVariablesAsExpression(Expression expression, RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			List<Expression> indexExpressions = getIndexExpressions(expression);
			List<Expression> quantifiedVariables = new LinkedList<Expression>(IndexExpressions.getIndexToDomainMapWithDefaultTypeOfIndex(indexExpressions).keySet());
			Expression result = Expressions.apply("list", quantifiedVariables);
			return result;
		}
		return null;
	}
}
