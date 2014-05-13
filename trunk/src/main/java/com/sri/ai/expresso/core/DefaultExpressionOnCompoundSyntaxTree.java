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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.core.AbstractExpression;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.math.Rational;

/**
 * An implementation of Expression based on {@link CompoundSyntaxTree}s.
 * 
 * @author braz
 */
@Beta
public class DefaultExpressionOnCompoundSyntaxTree extends AbstractExpression {
	private static final long serialVersionUID = 1L;
	//
	private int hashCode = -1; // lazy init and re-use the calculated hashCode.
	
	public DefaultExpressionOnCompoundSyntaxTree(SyntaxTree syntaxTree) {
		this.syntaxTree = syntaxTree;
	}
	
	/**
	 * Constructs an expression based on a syntax tree with given label and sub-trees (or their Expressions),
	 * copying them for internal use.
	 * Objects that are not Expressions are wrapped as DefaultSymbols, unless
	 * a single argument is given and it is a Collection;
	 * in this case, it is considered to be a Collection
	 * and a copy of the collection is used as the sub-trees.
	 * There is also a guarantee that if Expressions are provided,
	 * the corresponding sub-expressions will be the same object instances.
	 */
	public DefaultExpressionOnCompoundSyntaxTree(Object label, Object ... subTrees) {

		if (label instanceof AbstractExpression) { // when we are sure SyntaxTrees don't extend Expression, we can replace this test by Expression, which is more clear
			List<Integer> path = Util.list(-1);
			originalExpressionsByPath.put(path, (Expression) label);
		}

		if (subTrees.length == 1 && subTrees[0] instanceof Collection) {
			subTrees = ((Collection) subTrees[0]).toArray();
		}
		for (int i = 0; i != subTrees.length; i++) {
			Object subTreeObject = subTrees[i];
			if (subTreeObject instanceof AbstractExpression) { // when we are sure SyntaxTrees don't extend Expression, we can replace this test by Expression, which is more clear
				List<Integer> path = Util.list(i);
				originalExpressionsByPath.put(path, (Expression) subTreeObject);
			}
		}

		syntaxTree = SyntaxTrees.makeCompoundSyntaxTree(label, subTrees);
	}

	@Override
	public Object getValue() {
		return null;
	}

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

	public boolean equals(Object anotherObject) {
		boolean result = false;
		
		if (anotherObject instanceof Expression &&
				((Expression) anotherObject).getSyntaxTree() instanceof CompoundSyntaxTree) {
			Expression anotherCompoundSyntaxTree = (Expression) anotherObject;
			if (this.hashCode() == anotherCompoundSyntaxTree.hashCode()) {
				List<SyntaxTree> anotherSubTrees = anotherCompoundSyntaxTree.getSyntaxTree().getImmediateSubTrees();
				result =
						this.getSyntaxTree().getRootTree().equals(anotherCompoundSyntaxTree.getSyntaxTree().getRootTree())
						&& this.getSyntaxTree().getImmediateSubTrees().equals(anotherSubTrees);
			}
		}
		return result;
	}

	public String defaultToString() {
		String rootTreeString = getSyntaxTree().getRootTree().defaultToString();
		if ( ! (getSyntaxTree().getRootTree() instanceof Symbol)) {
			rootTreeString = "(" + rootTreeString + ")";
		}
		Iterator defaultToStringOfSubTrees =
			new FunctionIterator<SyntaxTree, String>(new DefaultToString(), getSyntaxTree().getImmediateSubTrees());
		return rootTreeString + "(" + Util.join(", ", defaultToStringOfSubTrees) + ")";
	}
	
	private static class DefaultToString implements Function<SyntaxTree, String> {
		@Override
		public String apply(SyntaxTree syntaxTree) {
			if (syntaxTree == null) {
				return "null";
			}
			return syntaxTree.defaultToString();
		}
	}

	public Expression clone() {
		return Expressions.makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(getSyntaxTree().getRootTree(), getSyntaxTree().getImmediateSubTrees());
		// it is best to use the field 'arguments' instead of method 'getArguments'
		// because we can share argument lists among function applications, since they are never modified.
		// The method 'getArguments' would unnecessarily create an unmodifiable list object.
	}

	@Override
	public int intValue() {
		throw new Error("Expression.intValue() not defined for CompoundSyntaxTree " + this);
	}

	@Override
	public int intValueExact() throws ArithmeticException {
		throw new Error("Expression.intValueExact() not defined for CompoundSyntaxTree " + this);
	}

	@Override
	public double doubleValue() {
		throw new Error("Expression.doubleValue() not defined for CompoundSyntaxTree " + this);
	}

	@Override
	public Rational rationalValue() {
		throw new Error("Expression.rationalValue() not defined for CompoundSyntaxTree " + this);
	}
}
