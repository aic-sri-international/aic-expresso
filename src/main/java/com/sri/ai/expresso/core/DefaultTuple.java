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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.api.TupleInterface;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractNonQuantifiedExpression;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.util.Util;

/**
 * A default implementation of a {@link TupleInterface}.
 * 
 * @author braz
 */
@Beta
public class DefaultTuple extends AbstractNonQuantifiedExpression implements TupleInterface {

	private static final long serialVersionUID = 1L;
	
	private ArrayList<Expression>      arguments;
	private SyntaxTree                 syntaxTree;
	private List<ExpressionAndContext> expressionAndContexts;
	
	public DefaultTuple(ArrayList<Expression> arguments) {
		super();
		this.arguments = arguments;
		
		this.syntaxTree = makeSyntaxTree();
		
		expressionAndContexts = new LinkedList<ExpressionAndContext>();
		int i = 0;
		for (Expression argument : arguments) {
			expressionAndContexts.add(new DefaultExpressionAndContext(argument, new IndexAddress(i++)));
		}
	}

	private DefaultCompoundSyntaxTree makeSyntaxTree() {
		DefaultCompoundSyntaxTree result;
		if (arguments.size() == 1) {
			result = new DefaultCompoundSyntaxTree("tuple", arguments.get(0));
		}
		else {
			ArrayList<SyntaxTree> argumentsSyntaxTrees = Util.mapIntoArrayList(arguments, e -> e == null? null : e.getSyntaxTree());
			SyntaxTree kleeneList = SyntaxTrees.makeKleeneListIfNeeded(argumentsSyntaxTrees);
			result = new DefaultCompoundSyntaxTree(Tuple.TUPLE_LABEL, kleeneList);
		}
		return result;
	}

	@Override
	public List<Expression> getArguments() {
		return arguments;
	}

	@Override
	public int numberOfArguments() {
		return arguments.size();
	}

	@Override
	public Expression get(int index) {
		return arguments.get(index);
	}

	@Override
	public Expression set(int i, Expression newIthArgument) {
		TupleInterface result;
		
		if (get(i) == newIthArgument) {
			result = this;
		}
		else {
			ArrayList<Expression> newArguments = new ArrayList<Expression>(arguments);
			newArguments.set(i, newIthArgument);
			result = new DefaultTuple(newArguments);
		}
		
		return result;
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(RewritingProcess process) {
		return expressionAndContexts.iterator();
	}

	@Override
	public Object getSyntacticFormType() {
		return "Tuple";
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return syntaxTree;
	}

	@Override
	public Expression renameSymbol(Expression symbol, Expression newSymbol, RewritingProcess process) {
		// TODO: incorrect! Must replace quantified symbols in sub-expressions too, this won't do it.
		Expression result = replaceAllOccurrences(symbol, newSymbol, process);
		return result;
	}

	@Override
	public Expression clone() {
		return new DefaultTuple((ArrayList<Expression>) getArguments());
	}
	
	private static class IndexAddress implements SubExpressionAddress {

		private int index;
		
		public IndexAddress(int index) {
			super();
			this.index = index;
		}

		@Override
		public Expression replace(Expression expression, Expression newSubExpression) {
			assert expression instanceof DefaultTuple : DefaultTuple.class.getSimpleName() + ".IndexAddress applied to expression " + expression + " of class " + expression.getClass();
			Expression result = expression.set(this.index, newSubExpression);
			return result;
		}

		@Override
		public Expression getSubExpressionOf(Expression expression) {
			TupleInterface tuple = castOrThrowError(TupleInterface.class, expression, "Attempt at obtaining " + index + "-th component expression of %s which should be an instance of %s but is an instance of %s");
			Expression result = tuple.get(index);
			return result;
		}
	}

	@Override
	public String makeToString() {
		String result;
		if (numberOfArguments() == 1) {
			result = "tuple(" + get(0) + ")";
		}
		else {
			result = "(" + Util.join(", ", getArguments()) + ")";
		}
		return result;
	}
}
