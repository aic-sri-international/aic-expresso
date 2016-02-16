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

import static com.sri.ai.expresso.helper.SyntaxTrees.makeCompoundSyntaxTree;
import static com.sri.ai.util.Util.castOrThrowError;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.mapIntoList;
import static com.sri.ai.util.Util.replaceElementsNonDestructively;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndSyntacticContext;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.QuantifiedExpressionWithABody;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;

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
	private SyntaxTree cachedSyntaxTree;
	private List<ExpressionAndSyntacticContext> cachedSubExpressionsAndContext;

	public AbstractQuantifiedExpressionWithABody(IndexExpressionsSet indexExpressions, Expression body) {
		super(indexExpressions);
		this.body = body;
		cachedSubExpressionsAndContext = makeImmediateSubExpressionsAndContexts();
		cachedSyntaxTree = makeSyntaxTree();
	}

	public abstract AbstractQuantifiedExpressionWithABody make(IndexExpressionsSet indexExpressions, Expression body);

	public abstract String getSyntaxTreeLabel();

	@Override
	public Expression getBody() {
		return body;
	}
	
	protected static class BodyAddress implements SubExpressionAddress {
		@Override
		public Expression replace(Expression expression, Expression newBody) {
			AbstractQuantifiedExpressionWithABody quantifiedExpression = castOrThrowError(AbstractQuantifiedExpressionWithABody.class, expression, "Attempt at replacing body expression of %s which should be an instance of %s but is an instance of %s");
			Expression result = quantifiedExpression.setBody(newBody);
			return result;
		}

		@Override
		public Expression getSubExpressionOf(Expression expression) {
			QuantifiedExpressionWithABody quantifiedExpressionWithABody = castOrThrowError(QuantifiedExpressionWithABody.class, expression, "Attempt at replacing body expression of %s which should be an instance of %s but is an instance of %s");
			Expression result = quantifiedExpressionWithABody.getBody();
			return result;
		}
	}
	
	@Override
	public Iterator<ExpressionAndSyntacticContext> getImmediateSubExpressionsAndContextsIterator() {
		return cachedSubExpressionsAndContext.iterator();
	}

	protected List<ExpressionAndSyntacticContext> makeImmediateSubExpressionsAndContexts() {
		cachedSubExpressionsAndContext = new LinkedList<ExpressionAndSyntacticContext>();
		makeIndexExpressionSubExpressionsAndContext(cachedSubExpressionsAndContext);
		cachedSubExpressionsAndContext.add(new DefaultExpressionAndSyntacticContext(getBody(), new BodyAddress(), getIndexExpressions(), Expressions.TRUE));
		return cachedSubExpressionsAndContext;
	}
	
	@Override
	public SyntaxTree getSyntaxTree() {
		return cachedSyntaxTree;
	}
	
	private SyntaxTree makeSyntaxTree() {
		IndexExpressionsSet indexExpressions = getIndexExpressions();
		List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) indexExpressions).getList();
		List<SyntaxTree> indexExpressionsSyntaxTrees = mapIntoArrayList(indexExpressionsList, Expression::getSyntaxTree);
		SyntaxTree parameterList = SyntaxTrees.makeKleeneListIfNeeded(indexExpressionsSyntaxTrees);
		SyntaxTree result = makeCompoundSyntaxTree(getSyntaxTreeLabel(), parameterList, getBody().getSyntaxTree());
		return result;
	}

	@Override
	public AbstractQuantifiedExpression setIndexExpressions(IndexExpressionsSet newIndexExpressions) {
		AbstractQuantifiedExpression result;
		if (newIndexExpressions != getIndexExpressions()) {
			result = make(newIndexExpressions, getBody());
		}
		else {
			result = this;
		}
		return result;
	}

	@Override
	public AbstractQuantifiedExpressionWithABody setBody(Expression newBody) {
		AbstractQuantifiedExpressionWithABody result;
		if (newBody != getBody()) {
			result = make(getIndexExpressions(), newBody);
		}
		else {
			result = this;
		}
		return result;
	}

	@Override
	public Expression replaceSymbol(Expression symbol, Expression newSymbol, Context context) {
		AbstractQuantifiedExpression result = this;
		
		Function<Expression, Expression> renameSymbol = e -> IndexExpressions.renameSymbol(e, symbol, newSymbol, context);
		List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) getIndexExpressions()).getList();
		IndexExpressionsSet newIndexExpressions = new ExtensionalIndexExpressionsSet(replaceElementsNonDestructively(indexExpressionsList, renameSymbol));
		
		Expression newBody = getBody().replaceSymbol(symbol, newSymbol, context);
		
		result = replaceIfNeeded(newIndexExpressions, newBody);

		return result;
	}

	private AbstractQuantifiedExpression replaceIfNeeded(IndexExpressionsSet newIndexExpression, Expression newBody) {
		AbstractQuantifiedExpression result = this;
		if (newIndexExpression != getIndexExpressions() || newBody != getBody()) {
			result = make(newIndexExpression, newBody);
		}
		return result;
	}

	@Override
	public Expression clone() {
		Expression result = make(getIndexExpressions(), getBody());
		return result;
	}

	@Override
	public String makeToString() {
		IndexExpressionsSet indexExpressions = getIndexExpressions();
		List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) indexExpressions).getList();
		List<String> indexExpressionsStrings = mapIntoList(indexExpressionsList, Expression::toString);
		String parametersRepresentation = join(", ", indexExpressionsStrings);
		if ( ! parametersRepresentation.isEmpty()) {
			parametersRepresentation = " " + parametersRepresentation;
		}
		String result = getHeadString() + parametersRepresentation + " : " + getBody();
		return result;
	}

	/**
	 * Provides the <i>head string</i>, which
	 * is used in the string representation of this type of expression:<br>
	 * <code>[head string] [index expressions separated by comma] : [body]</code>.
	 * @return the head string
	 */
	protected abstract String getHeadString();
}
