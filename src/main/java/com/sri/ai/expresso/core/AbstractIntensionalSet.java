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
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.replaceElementsNonDestructively;

import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.IntensionalSetInterface;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;

/**
 * A default implementation of a {@link IntensionalSetInterface}.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractIntensionalSet extends AbstractQuantifiedExpression implements IntensionalSetInterface {

	private static final long serialVersionUID = 1L;
	
	private Expression head;
	private Expression condition;
	
	public AbstractIntensionalSet(List<Expression> indexExpressions, Expression head, Expression condition) {
		super(indexExpressions);
		this.head      = head;
		this.condition = condition;
		cachedSubExpressionsAndContext = makeImmediateSubExpressionsAndContexts();
		cachedSyntaxTree = makeSyntaxTree();
	}

	private SyntaxTree makeSyntaxTree() {
		List<SyntaxTree> indexExpressionsSyntaxTrees = mapIntoArrayList(getIndexExpressions(), Expression::getSyntaxTree);
		SyntaxTree parameterList       = SyntaxTrees.makeKleeneListIfNeeded(indexExpressionsSyntaxTrees);
		SyntaxTree scopingSyntaxTree   = new DefaultCompoundSyntaxTree(IntensionalSet.SCOPED_VARIABLES_LABEL, parameterList);
		SyntaxTree headSyntaxTree      = getHead().getSyntaxTree();
		SyntaxTree conditionSyntaxTree =
				(getCondition() == null || getCondition().equals("true"))?
						null
						: SyntaxTrees.makeCompoundSyntaxTree(IntensionalSet.CONDITION_LABEL, condition.getSyntaxTree());
		cachedSyntaxTree = makeCompoundSyntaxTree(getSyntaxTreeLabel(), scopingSyntaxTree, headSyntaxTree, conditionSyntaxTree);
		return cachedSyntaxTree;
	}

	private List<ExpressionAndContext> makeImmediateSubExpressionsAndContexts() {
		cachedSubExpressionsAndContext = new LinkedList<ExpressionAndContext>();
		makeIndexExpressionSubExpressionsAndContext(cachedSubExpressionsAndContext);
		cachedSubExpressionsAndContext.add(new DefaultExpressionAndContext(getHead(), new HeadAddress(), getIndexExpressions(), getCondition()));
		if ( ! getCondition().equals(Expressions.TRUE)) {
			cachedSubExpressionsAndContext.add(new DefaultExpressionAndContext(getCondition(), new ConditionAddress(), getIndexExpressions(), Expressions.TRUE));
		}
		return cachedSubExpressionsAndContext;
	}

	@Override
	public Object getSyntacticFormType() {
		return "Intensional set";
	}

	@Override
	public Expression renameSymbol(Expression symbol, Expression newSymbol, RewritingProcess process) {
		IntensionalSetInterface result = this;
		
		Function<Expression, Expression> renameSymbol = e -> IndexExpressions.renameSymbol(e, symbol, newSymbol, process);
		List<Expression> newIndexExpressions = replaceElementsNonDestructively(getIndexExpressions(), renameSymbol);
		
		Expression newHead      = getHead().renameSymbol(symbol, newSymbol, process);
		Expression newCondition = getCondition().renameSymbol(symbol, newSymbol, process);
		
		result = replaceIfNeeded(newIndexExpressions, newHead, newCondition);

		return result;
	}

	@Override
	public Expression getHead() {
		return head;
	}

	@Override
	public Expression getCondition() {
		return condition;
	}
	
	@Override
	public Expression setHead(Expression newHead) {
		IntensionalSetInterface result = this;
		if (newHead != getHead()) {
			result = make(getIndexExpressions(), newHead, getCondition());
		}
		return result;
	}

	@Override
	public Expression setCondition(Expression newCondition) {
		IntensionalSetInterface result = this;
		if (newCondition != getCondition()) {
			result = make(getIndexExpressions(), getHead(), newCondition);
		}
		return result;
	}

	@Override
	public QuantifiedExpression setIndexExpressions(List<Expression> newIndexExpressions) {
		IntensionalSetInterface result = this;
		if (newIndexExpressions != getIndexExpressions()) {
			result = make(newIndexExpressions, getHead(), getCondition());
		}
		return result;
	}

	public IntensionalSetInterface replaceIfNeeded(List<Expression> newIndexExpressions, Expression newHead, Expression newCondition) {
		IntensionalSetInterface result = this;
		if (newIndexExpressions != getIndexExpressions() || newHead != getHead() || newCondition != getCondition()) {
			result = make(newIndexExpressions, newHead, newCondition);
		}
		return result;
	}
	
	@Override
	public Expression clone() {
		IntensionalSetInterface result = make(getIndexExpressions(), getHead(), getCondition());
		return result;
	}
	
	protected static class HeadAddress implements SubExpressionAddress {
		@Override
		public Expression replace(Expression expression, Expression newHead) {
			IntensionalSetInterface intensionalUniSet = castOrThrowError(IntensionalSetInterface.class, expression, "Attempt at replacing head expression of %s which should be an instance of %s but is an instance of %s");
			Expression result = intensionalUniSet.setHead(newHead);
			return result;
		}

		@Override
		public Expression getSubExpressionOf(Expression expression) {
			IntensionalSetInterface intensionalUniSet = castOrThrowError(IntensionalSetInterface.class, expression, "Attempt at replacing head expression of %s which should be an instance of %s but is an instance of %s");
			Expression result = intensionalUniSet.getHead();
			return result;
		}
	}
	
	protected static class ConditionAddress implements SubExpressionAddress {
		@Override
		public Expression replace(Expression expression, Expression newCondition) {
			IntensionalSetInterface intensionalSet = castOrThrowError(IntensionalSetInterface.class, expression, "Attempt at obtaining head expression of %s which should be an instance of %s but is an instance of %s");
			Expression result = intensionalSet.setCondition(newCondition);
			return result;
		}

		@Override
		public Expression getSubExpressionOf(Expression expression) {
			IntensionalSetInterface intensionalUniSet = castOrThrowError(IntensionalSetInterface.class, expression, "Attempt at obtaining condition expression of %s which should be an instance of %s but is an instance of %s");
			Expression result = intensionalUniSet.getCondition();
			return result;
		}
	}

	abstract public String getSyntaxTreeLabel();

	@Override
	abstract public boolean isUniSet();

	@Override
	abstract public boolean isMultiSet();

	abstract public AbstractIntensionalSet make(List<Expression> indexExpressions, Expression head, Expression condition);
}
