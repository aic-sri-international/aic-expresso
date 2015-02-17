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

import static com.sri.ai.util.Util.arrayList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.BracketedExpression;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractNonQuantifiedExpression;
import com.sri.ai.grinder.helper.FunctionSignature;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.util.Util;

/**
 * A default implementation of a {@link BracketedExpression}.
 * 
 * @author braz
 */
@Beta
public class DefaultBracketedExpression extends AbstractNonQuantifiedExpression implements BracketedExpression {

	private static final long serialVersionUID = 1L;
	
	private Expression innerExpression;
	private Collection<FunctionSignature> randomPredicates;
	private SyntaxTree cachedSyntaxTree;
	private List<ExpressionAndContext> cachedExpressionsAndContexts;

	/**
	 * TODO: Temporary field until syntax-tree-based expressions are removed
	 */
	public static Collection<FunctionSignature> defaultPredicateSignatures = null;


	public DefaultBracketedExpression(Expression innerExpression, Collection<FunctionSignature> randomPredicates) {
		super();
		this.innerExpression  = innerExpression;
		this.randomPredicates = randomPredicates;
		this.cachedSyntaxTree = makeSyntaxTree();
		this.cachedExpressionsAndContexts = makeExpressionsAndContexts();
	}

	@Override
	public Expression getInnerExpression() {
		return innerExpression;
	}

	@Override
	public Collection<FunctionSignature> getRandomPredicates() {
		return randomPredicates;
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator() {
		return cachedExpressionsAndContexts.iterator();
	}

	@Override
	public Object getSyntacticFormType() {
		return "Bracketed expression";
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return cachedSyntaxTree;
	}

	protected SyntaxTree makeSyntaxTree() {
		SyntaxTree result = new DefaultCompoundSyntaxTree("[ . ]", innerExpression.getSyntaxTree());
		return result;
	}

	@Override
	public Expression replaceSymbol(Expression symbol, Expression newSymbol, RewritingProcess process) {
		Expression result = this;
		Expression newInnerExpression = getInnerExpression().replaceSymbol(symbol, newSymbol, process);
		if (newInnerExpression != getInnerExpression()) {
			result = new DefaultBracketedExpression(newInnerExpression, getRandomPredicates());
		}
		return result;
	}

	@Override
	public Expression clone() {
		return new DefaultBracketedExpression(getInnerExpression(), getRandomPredicates());
	}
	
	private List<ExpressionAndContext> makeExpressionsAndContexts() {
		ArrayList<ExpressionAndContext> result = new ArrayList<ExpressionAndContext>();
		ArrayList<ExpressionAndContext> initialPath = arrayList();
		makeExpressionAndContexts(innerExpression, initialPath, result);
		return result;
	}

	private void makeExpressionAndContexts(Expression innerExpression, ArrayList<ExpressionAndContext> path, List<ExpressionAndContext> collectedExpressionsAndContexts) {
		if (isRandomPredicateApplication(innerExpression)) {
			for (int i = 0; i != innerExpression.numberOfArguments(); i++) {
				Expression subExpressionOfBracketedExpression = innerExpression.get(i);
				BracketedExpressionSubExpressionAddress address = new BracketedExpressionSubExpressionAddress(path, i);
				ExpressionAndContext expressionAndContext       = new DefaultExpressionAndContext(subExpressionOfBracketedExpression, address);
				collectedExpressionsAndContexts.add(expressionAndContext);
			}
		}
		else {
			Iterator<ExpressionAndContext> expressionAndContextIterator = innerExpression.getImmediateSubExpressionsAndContextsIterator();
			while (expressionAndContextIterator.hasNext()) {
				ExpressionAndContext expressionAndContext = expressionAndContextIterator.next();
				Expression newInnerExpression = expressionAndContext.getExpression();
				ArrayList<ExpressionAndContext> newPath = new ArrayList<ExpressionAndContext>(path);
				newPath.add(expressionAndContext); // TODO: could use List implementation that re-uses a list instead of copying it, since the resulting list will be modified
				makeExpressionAndContexts(newInnerExpression, newPath, collectedExpressionsAndContexts);
			}
		}
	}

	/**
	 * @param innerExpression
	 * @return whether {@link innerExpression} is a random predicate application
	 */
	protected boolean isRandomPredicateApplication(Expression innerExpression) {
		boolean result =
				innerExpression.getSyntacticFormType().equals("Function application")
				&&
				! FormulaUtil.functorIsAnEqualityLogicalConnectiveIncludingConditionals(innerExpression);
//		boolean result =
//				innerExpression.getSyntacticFormType().equals("Function application")
//				&&
//				randomPredicates.contains(new FunctionSignature(innerExpression.getFunctor(), innerExpression.numberOfArguments()));
		return result;
	}

	/**
	 * The address of a sub-expression of a bracketed expression
	 * that has been reached after descending through a path of {@link ExpressionAndContext} objects
	 * to reach the an argument of a random function application with the given index.
	 * 
	 * @author braz
	 *
	 */
	private static class BracketedExpressionSubExpressionAddress implements SubExpressionAddress {

		private ArrayList<ExpressionAndContext> path;
		private int argumentIndex;
		
		public BracketedExpressionSubExpressionAddress(ArrayList<ExpressionAndContext> path, int argumentIndex) {
			this.path = path;
			this.argumentIndex = argumentIndex;
		}

		@Override
		public Expression replace(Expression expression, Expression newFinalSubExpression) {
			BracketedExpression bracketedExpression = Util.castOrThrowError(BracketedExpression.class, expression, "Expecting " + BracketedExpression.class + " instance but got instance of " + expression.getClass());
			Expression innerExpression = bracketedExpression.getInnerExpression();
			Expression newInnerExpression = replaceFromIthPathPosition(innerExpression, 0, newFinalSubExpression);
			Expression result = new DefaultBracketedExpression(newInnerExpression, bracketedExpression.getRandomPredicates());
			return result;
		}

		private Expression replaceFromIthPathPosition(Expression currentExpression, int i, Expression newFinalSubExpression) {
			Expression result;
			if (i == path.size()) { // no path to follow, just replace argument
				result = currentExpression.set(argumentIndex, newFinalSubExpression);
			}
			else { // need to take the next step down the path
				ExpressionAndContext nextOriginalExpressionAndContext = path.get(i);
				SubExpressionAddress subExpressionAddress = nextOriginalExpressionAndContext.getAddress();
				Expression subExpression = subExpressionAddress.getSubExpressionOf(currentExpression);
				Expression newSubExpression = replaceFromIthPathPosition(subExpression, i + 1, newFinalSubExpression);
				result = subExpressionAddress.replace(currentExpression, newSubExpression);
			}
			return result;
		}

		@Override
		public Expression getSubExpressionOf(Expression expression) {
			BracketedExpression bracketedExpression = Util.castOrThrowError(BracketedExpression.class, expression, "Expecting " + BracketedExpression.class + " instance but got instance of " + expression.getClass());
			Expression innerExpression = bracketedExpression.getInnerExpression();
			Expression result = getSubExpressionFromIthPathPosition(innerExpression, 0);
			return result;
		}

		private Expression getSubExpressionFromIthPathPosition(Expression currentExpression, int i) {
			Expression result;
			if (i == path.size()) { // no path to follow, just get argument
				result = currentExpression.get(argumentIndex);
			}
			else { // need to take the next step down the path
				ExpressionAndContext nextOriginalExpressionAndContext = path.get(i);
				Expression nextExpression = nextOriginalExpressionAndContext.getAddress().getSubExpressionOf(currentExpression);
				result = getSubExpressionFromIthPathPosition(nextExpression, i + 1);
			}
			return result;
		}
	}
	
	@Override
	public String makeToString() {
		String result = "[ " + innerExpression + " ]";
		return result;
	}
}
