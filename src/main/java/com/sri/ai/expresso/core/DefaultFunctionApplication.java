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

import static com.sri.ai.util.Util.mapIntoArray;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.api.SubExpressionAddress;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractExpression;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.Not;

/**
 * A default implementation of a {@link FunctionApplication}..
 * 
 * @author braz
 */
@Beta
public class DefaultFunctionApplication extends AbstractExpression implements FunctionApplication {

	private static final long serialVersionUID = 1L;
	
	private Expression                 functor;
	private ArrayList<Expression>      arguments;
	private SyntaxTree                 syntaxTree;
	private List<ExpressionAndContext> expressionAndContexts;
	
	public DefaultFunctionApplication(Expression functor, ArrayList<Expression> arguments) {
		super();
		this.functor   = functor;
		this.arguments = arguments;
		
		this.syntaxTree = new DefaultCompoundSyntaxTree(functor.getSyntaxTree(), mapIntoArray(arguments, e -> e == null? null : e.getSyntaxTree()));
		
		expressionAndContexts = new LinkedList<ExpressionAndContext>();
		expressionAndContexts.add(new DefaultExpressionAndContext(functor, new IndexAddress(-1)));
		int i = 0;
		for (Expression argument : arguments) {
			Expression conditioningConstraint = getConditioningConstraint(argument, i);
			expressionAndContexts.add(new DefaultExpressionAndContext(argument, new IndexAddress(i++), Expressions.EMPTY_LIST, conditioningConstraint));
		}
	}

	/**
	 * A method determining the conditioning constraint of an argument.
	 * For now, it only returns something different from {@link Expressions#TRUE} when the functor is {@link FunctorConstants#IF_THEN_ELSE}.
	 * New functions will require changing this code.
	 * TODO: make it extensible by end user code, without the need to change the method directly.
	 * @param argument the argument for which to determine the conditioning constraint
	 * @param argumentIndex the index of the argument
	 * @return the conditioning constraint
	 */
	private Expression getConditioningConstraint(Expression argument, int argumentIndex) {
		Expression conditioningConstraint;
		if (functor.equals(FunctorConstants.IF_THEN_ELSE)) { // hard-coded for now
			conditioningConstraint = argumentIndex == 1? arguments.get(0) : argumentIndex == 2? Not.make(arguments.get(0)) : Expressions.TRUE;
		}
		else {
			conditioningConstraint = Expressions.TRUE;
		}
		return conditioningConstraint;
	}

	@Override
	public Expression getFunctor() {
		return functor;
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
		if (index == -1) {
			return functor;
		}
		return arguments.get(index);
	}

	@Override
	public Expression set(int i, Expression newIthArgument) {
		FunctionApplication result;
		
		if (get(i) == newIthArgument) {
			result = this;
		}
		else {
			if (i == -1) {
				result = new DefaultFunctionApplication(newIthArgument, arguments);
			}
			else {
				ArrayList<Expression> newArguments = new ArrayList<Expression>(arguments);
				newArguments.set(i, newIthArgument);
				result = new DefaultFunctionApplication(functor, newArguments);
			}
		}
		
		return result;
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(RewritingProcess process) {
		return expressionAndContexts.iterator();
	}

	@Override
	public Object getSyntacticFormType() {
		return "Function application";
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return syntaxTree;
	}

	@Override
	public Expression renameSymbol(Expression symbol, Expression newSymbol, RewritingProcess process) {
		Expression result = replaceAllOccurrences(symbol, newSymbol, process);
		return result;
	}

	@Override
	public Expression getFunctorOrSymbol() {
		return getFunctor();
	}

	@Override
	public Expression clone() {
		return new DefaultFunctionApplication(getFunctor(), (ArrayList<Expression>) getArguments());
	}
	
	private static class IndexAddress implements SubExpressionAddress {

		private int index;
		
		public IndexAddress(int index) {
			super();
			this.index = index;
		}

		@Override
		public Expression replace(Expression expression, Expression newSubExpression) {
			assert expression instanceof DefaultFunctionApplication : "DefaultFunctionApplication.IndexAddress applied to expression " + expression + " of class " + expression.getClass();
			Expression result = expression.set(this.index, newSubExpression);
			return result;
		}
	}

	private int hashCode = -1;
	@Override
	public int hashCode() {
		if (hashCode == -1) {
			SyntaxTree rootTree = getSyntaxTree().getRootTree();
			int rootHashCode = rootTree.hashCode();
			List<SyntaxTree> immediateSubTrees = getSyntaxTree().getImmediateSubTrees();
			int subTreesHashCode = immediateSubTrees.hashCode();
			hashCode = rootHashCode + subTreesHashCode;
		}
		
		return hashCode;
	}

	@Override
	public boolean equals(Object anotherObject) {
		
		if (this == anotherObject) {
			return true;
		}
		
		boolean result = false;
		
		boolean anotherObjectiIsExpressionDefinedOnCompoundSyntaxTree =
				anotherObject instanceof Expression &&
				((Expression) anotherObject).getSyntaxTree() instanceof CompoundSyntaxTree;
		
		if (anotherObjectiIsExpressionDefinedOnCompoundSyntaxTree) {
			
			Expression anotherCompoundSyntaxTree = (Expression) anotherObject;
			
			if (this.hashCode() == anotherCompoundSyntaxTree.hashCode()) {
				
				SyntaxTree       thisRootTree    = this.getSyntaxTree().getRootTree();
				SyntaxTree       anotherRootTree = anotherCompoundSyntaxTree.getSyntaxTree().getRootTree();
				
				List<SyntaxTree> thisSubTrees    = this.getSyntaxTree().getImmediateSubTrees();
				List<SyntaxTree> anotherSubTrees = anotherCompoundSyntaxTree.getSyntaxTree().getImmediateSubTrees();

				result = thisRootTree.equals(anotherRootTree) && thisSubTrees.equals(anotherSubTrees);
			}
		}
		return result;
	}
}
