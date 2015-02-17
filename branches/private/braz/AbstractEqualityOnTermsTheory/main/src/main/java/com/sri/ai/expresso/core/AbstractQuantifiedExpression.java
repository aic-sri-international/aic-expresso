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

import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.replaceArgument;
import static com.sri.ai.grinder.library.indexexpression.IndexExpressions.replaceOrAddType;
import static com.sri.ai.util.Util.castOrThrowError;
import static com.sri.ai.util.Util.replaceElementNonDestructively;

import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractExpression;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;

/**
 * An abstract implementation of a {@link QuantifiedExpression}.
 * 
 * @author braz
 */
@SuppressWarnings("serial")
@Beta
public abstract class AbstractQuantifiedExpression extends AbstractExpression implements QuantifiedExpression {

	private   IndexExpressionsSet                    indexExpressions;
	protected List<Expression>           cachedScopedExpressions;
	protected SyntaxTree                 cachedSyntaxTree;
	protected List<ExpressionAndContext> cachedSubExpressionsAndContext;
	
	public AbstractQuantifiedExpression(IndexExpressionsSet indexExpressions) {
		this.indexExpressions = indexExpressions;
		cachedScopedExpressions = makeScopedExpressions();
	}
	
	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator() {
		return cachedSubExpressionsAndContext.iterator();
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return cachedSyntaxTree;
	}

	@Override
	public List<Expression> getScopedExpressions(RewritingProcess process) {
		return cachedScopedExpressions;
	}

	protected List<Expression> makeScopedExpressions() {
		List<Expression> result = IndexExpressions.getIndices(getIndexExpressions());
		return result;
	}

	@Override
	public IndexExpressionsSet getIndexExpressions() {
		return indexExpressions;
	}

	@Override
	public abstract QuantifiedExpression setIndexExpressions(IndexExpressionsSet newIndexExpressions);

	/**
	 * A convenience method (for extensions' benefit) generating the {@link ExpressionAndContext} objects with respect to the index expressions.
	 * Extensions of {@link AbstractQuantifiedExpression} will still need to generate the {@link ExpressionAndContext} objects
	 * referring to other sub-expressions.
	 */
	protected List<ExpressionAndContext> makeIndexExpressionSubExpressionsAndContext(List<ExpressionAndContext> result) {
		
		List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet)getIndexExpressions()).getList();
		for (int indexExpressionIndex = 0; indexExpressionIndex != indexExpressionsList.size(); indexExpressionIndex++) {
			Expression indexExpression = indexExpressionsList.get(indexExpressionIndex);
			Expression index = IndexExpressions.getIndex(indexExpression);
			if (index.getSyntacticFormType().equals("Function application")) {
				for (int argumentIndex = 0; argumentIndex != index.numberOfArguments(); argumentIndex++) {
					ExpressionAndContext expressionAndContext = makeAddressForIndexArgument(indexExpressionIndex, index, argumentIndex);
					result.add(expressionAndContext);
				}
			}
			Expression type = IndexExpressions.getType(indexExpression);
			if (indexExpression.hasFunctor(FunctorConstants.IN)) {
				ExpressionAndContext expressionAndContext = makeAddressForIndexType(indexExpressionIndex, type);
				result.add(expressionAndContext);
			}
		}
		
		return result;
	}

	private ExpressionAndContext makeAddressForIndexType(int indexExpressionIndex, Expression type) {
		SubExpressionAddress address = new IndexExpressionTypeSubExpressionAddress(indexExpressionIndex);
		ExpressionAndContext expressionAndContext = new DefaultExpressionAndContext(type, address);
		return expressionAndContext;
	}

	private ExpressionAndContext makeAddressForIndexArgument(int indexExpressionIndex, Expression index, int argumentIndex) {
		SubExpressionAddress address = new IndexExpressionArgumentSubExpressionAddress(indexExpressionIndex, argumentIndex);
		ExpressionAndContext expressionAndContext = new DefaultExpressionAndContext(index.get(argumentIndex), address);
		return expressionAndContext;
	}

	/**
	 * Abstract class responsible for keeping the address of an index expression's sub-expression S,
	 * and allowing new sub-expressions S' to replace it.
	 * It records the index expression's index I and relies
	 * on its abstract method {@link #replaceAddressedSubExpressionInIndexExpressionByNewSubExpression(Expression, Expression)}
	 * to replace S by S' in the I-th index expression.
	 * @author braz
	 *
	 */
	protected static abstract class IndexExpressionSubExpressionAddress implements SubExpressionAddress {
		
		protected int indexExpressionIndex;
		
		public IndexExpressionSubExpressionAddress(int indexExpressionIndex) {
			super();
			this.indexExpressionIndex = indexExpressionIndex;
		}

		@Override
		public Expression replace(Expression expression, Expression newSubExpression) {
			AbstractQuantifiedExpression quantifiedExpression = castOrThrowError(AbstractQuantifiedExpression.class, expression, "Attempt to use " + IndexExpressionTypeSubExpressionAddress.class.getSimpleName() + " to replace sub-expression " + newSubExpression + " in %s, but the latter should have been an instance of %s but is instead an instance of %s");
			assert quantifiedExpression.getIndexExpressions() instanceof ExtensionalIndexExpressionsSet : quantifiedExpression.getClass().getSimpleName() + ".replaceIndexExpression assumes extensional set of index expressions but " + quantifiedExpression + " has a non-extensional index expression set";

			List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) quantifiedExpression.getIndexExpressions()).getList();
			List<Expression> newIndexExpressionsList =
					replaceElementNonDestructively(indexExpressionsList, indexExpressionIndex,
							ie -> replaceAddressedSubExpressionInIndexExpressionByNewSubExpression(ie, newSubExpression));

			Expression result;
			if (newIndexExpressionsList != indexExpressionsList) {
				IndexExpressionsSet newIndexExpressions = new ExtensionalIndexExpressionsSet(newIndexExpressionsList);
				result = quantifiedExpression.setIndexExpressions(newIndexExpressions);
			}
			else {
				result = quantifiedExpression;
			}
			
			return result;
		}

		/**
		 * Implementations must override this method to specify how a new sub-expression is to replace the one addressed by this {@link IndexExpressionSubExpressionAddress}.
		 * As usual, they must return the same instance if the sub-expressions are the same.
		 * @param indexExpression
		 * @param newSubExpression
		 * @return
		 */
		abstract protected Expression replaceAddressedSubExpressionInIndexExpressionByNewSubExpression(Expression indexExpression, Expression newSubExpression);
		
		@Override
		abstract public Expression getSubExpressionOf(Expression expression);
	}

	/**
	 * A sub-expression address referring to an argument of a function-application index expression.
	 * <p>
	 * For example, <code>new IndexExpressionArgumentSubExpressionAddress(1, 2)</code>
	 * would refer to sub-expression <code>g(c, d)</code>
	 * in
	 * <p>
	 * <code>there exists X in Things, f(a, b, g(c, d)) in boolean, Z : ... </code>
	 * <p>
	 * because, in the 2nd (that is, with index 1) index above (<code>f(a, b, g(c, d))</code>),
	 * the 3rd argument (that is, with index 2) is <code>g(c, d)</code>.
	 * <p>
	 * This class is left abstract because the implementation of
	 * {@link SubExpressionAddress#replace(Expression, Expression)}
	 * will depend on the particular implementation of {@link AbstractQuantifiedExpression}.
	 * 
	 * @author braz
	 *
	 */
	protected static class IndexExpressionArgumentSubExpressionAddress extends IndexExpressionSubExpressionAddress {
		
		protected int argumentIndex;
		
		public IndexExpressionArgumentSubExpressionAddress(int indexExpressionIndex, int argumentIndex) {
			super(indexExpressionIndex);
			this.argumentIndex = argumentIndex;
		}

		@Override
		public Expression getSubExpressionOf(Expression expression) {
			QuantifiedExpression quantifiedExpression = castOrThrowError(QuantifiedExpression.class, expression, "Attempt at obtaining index expression argument of %s which should be an instance of %s but is an instance of %s");
			Expression indexExpression = ((ExtensionalIndexExpressionsSet) quantifiedExpression.getIndexExpressions()).getList().get(indexExpressionIndex);
			Expression index = IndexExpressions.getIndex(indexExpression);
			Expression result = index.get(argumentIndex);
			return result;
		}

		@Override
		protected Expression replaceAddressedSubExpressionInIndexExpressionByNewSubExpression(Expression indexExpression, Expression newSubExpression) {
			Expression result = replaceArgument(indexExpression, argumentIndex, newSubExpression);
			return result;
		}
	}

	/**
	 * Similar to {@link #IndexExpressionArgumentSubExpressionAddress},
	 * but referring to the type of an index expression.
	 * <p>
	 * For example, <code>new IndexExpressionTypeSubExpressionAddress(1)</code>
	 * would refer to sub-expression <code>boolean</code>
	 * in
	 * <p>
	 * <code>there exists X in Things, f(a, b, g(c, d)) in boolean, Z : ... </code>
	 * <p>
	 * because, in the 2nd (that is, with index 1) index expression above (<code>f(a, b, g(c, d)) in boolean</code>),
	 * the type is <code>boolean</code>.
	 * <p>
	 * This class is left abstract because the implementation of
	 * {@link SubExpressionAddress#replace(Expression, Expression)}
	 * will depend on the particular implementation of {@link AbstractQuantifiedExpression}.
	 *
	 * @author braz
	 *
	 */
	protected static class IndexExpressionTypeSubExpressionAddress extends IndexExpressionSubExpressionAddress {
		public IndexExpressionTypeSubExpressionAddress(int indexExpressionIndex) {
			super(indexExpressionIndex);
		}

		@Override
		public Expression getSubExpressionOf(Expression expression) {
			QuantifiedExpression quantifiedExpression = castOrThrowError(QuantifiedExpression.class, expression, "Attempt at obtaining index expression argument of %s which should be an instance of %s but is an instance of %s");
			List<Expression> indexExpressionsList = ((ExtensionalIndexExpressionsSet) quantifiedExpression.getIndexExpressions()).getList();
			if (indexExpressionIndex >= indexExpressionsList.size()) {
				throw new Error("Attempt to obtain " + indexExpressionIndex + "-th index expression of " + expression + " but it does not have one.");
			}
			Expression indexExpression = indexExpressionsList.get(indexExpressionIndex);
			Expression result;
			if (indexExpression.hasFunctor(FunctorConstants.IN)){ 
				result = IndexExpressions.getType(indexExpression);
			}
			else {
				throw new Error("Attempt to obtain type for " + indexExpressionIndex + "-th index expression of " + expression + " but it does not have a type.");
			}
			return result;
		}

		@Override
		protected Expression replaceAddressedSubExpressionInIndexExpressionByNewSubExpression(Expression indexExpression, Expression newSubExpression) {
			Expression result = replaceOrAddType(indexExpression, newSubExpression);
			return result;
		}
	}
}
