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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryProcedure;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;
import com.sri.ai.util.math.Rational;

/**
 * A default implementation of {@link CompoundSyntaxTree}. It is based on
 * keeping a functor Expression and a list of argument SyntaxTrees.
 * 
 * @author braz
 */
@Beta
public class DefaultCompoundSyntaxTree extends AbstractSyntaxTree implements CompoundSyntaxTree, Expression {
	private static final long serialVersionUID = 1L;
	//
	private int hashCode = -1; // lazy init and re-use the calculated hashCode.
	
	/**
	 * Constructs an expression based on a syntax tree with given label and sub-trees,
	 * copying them for internal use.
	 * Objects that are not Expressions are wrapped as DefaultSymbols, unless
	 * a single argument is given and it is a List;
	 * in this case, it is considered to be a List<Expression>
	 * and a copy is used the function application arguments
	 * (doing pretty much the same thing as {@link Expressions#makeExpressionBasedOnSyntaxTreeWithLabelAndSubTrees(Object, List)}, but in a slightly slower manner).
	 */
	@SuppressWarnings("unchecked")
	public DefaultCompoundSyntaxTree(Object label, Object ... args) {
//		this.syntaxTree = new DefaultCompoundSyntaxTree2(functor, args);
		
		this.valueOrRootSyntaxTree = SyntaxTrees.wrap(label);
		if (args.length == 1 && args[0] instanceof List) {
			// Note: We can have nulls, therefore cannot use ImmutableList directly.
			this.subTrees = Collections.unmodifiableList((List<SyntaxTree>) args[0]); // makes a copy since this constructor does not assume ownership.
		}
		else {
			// Note: We can have nulls, therefore cannot use ImmutableList directly.
			this.subTrees = Collections.unmodifiableList(SyntaxTrees.wrap(args));
		}
		syntaxTree = this;
	}

	@Override
	public Object getValue() {
		return null;
	}

	@Override
	public SyntaxTree getRootTree() {
		return (SyntaxTree) valueOrRootSyntaxTree;
	}

	@Override
	public Object getLabel() {
		return valueOrRootSyntaxTree;
	}

	@Override
	public Iterator<SyntaxTree> getImmediateSubTreesIncludingRootOneIterator() {
		return new NestedIterator<SyntaxTree>(Util.list(getRootTree(), getImmediateSubTreesIterator()));
	}

	@Override
	public int numberOfImmediateSubTreesIncludingRootOneIterator() {
		return 1 + numberOfImmediateSubTrees();
	}

	@Override
	public SyntaxTree setImmediateSubTree(int i, Object newIthSubTreeObject) {
		SyntaxTree newRootTree = getRootTree();
		SyntaxTree newIthSubTree = SyntaxTrees.wrap(newIthSubTreeObject);
		List<SyntaxTree> newSubTrees = subTrees;
		// it is important to use field {@link #subTrees} above instead of method {@link #getSubTrees()}
		// because we want to be able to reuse {@link #subTrees} in case the root tree is being set.
		// If we use {@link #getSubTrees()}, we get an unmodifiable list object instead of the original arguments list.
		if (i == -1) {
			newRootTree = newIthSubTree;
		}
		else {
			newSubTrees = storeSubTreeReplacement(newSubTrees, i, getSubTree(i), newIthSubTree);
		}	
		SyntaxTree result = makeReplacementIfAny(newRootTree, newSubTrees);
		return result;
	}

	public Expression orderNormalized = null;
	public static final boolean useOrderNormalization = false;
	
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
		boolean result;
		
		if (anotherObject instanceof Expression &&
				((Expression) anotherObject).getSyntaxTree() instanceof CompoundSyntaxTree) {
			Expression normalizedAnotherCompoundSyntaxTree = (Expression) anotherObject;
			if (this.hashCode() == normalizedAnotherCompoundSyntaxTree.hashCode()) {
				List<SyntaxTree> anotherSubTrees = normalizedAnotherCompoundSyntaxTree.getSyntaxTree().getImmediateSubTrees();
				result = this.getSyntaxTree().getRootTree().equals(normalizedAnotherCompoundSyntaxTree.getSyntaxTree().getRootTree()) && this.getSyntaxTree().getImmediateSubTrees().equals(anotherSubTrees);
			}
			else {
				result = false;
			}
		}
		else {
			result = false;
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

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTrees(replacementFunction, true /* only the first one */, prunePredicate, listener, false);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTrees(replacementFunction, false /* not only the first one */, prunePredicate, listener, false);
	}

	@Override
	public SyntaxTree replaceSubTrees(
			Function<SyntaxTree, SyntaxTree> replacementFunction, boolean onlyTheFirstOne,
			Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener, boolean ignoreTopExpression) {
		
		if (prunePredicate != null && prunePredicate.apply(this)) {
			return this;
		}
		
		if ( ! ignoreTopExpression) {
			SyntaxTree topReplacement = replacementFunction.apply(this);
			if (topReplacement != this && topReplacement != null) {
				if (listener != null) {
					listener.apply(this, topReplacement);
				}
				return topReplacement;
			}
		}
		
		SyntaxTree rootTreeReplacement = getRootTree().replaceSubTrees(replacementFunction, onlyTheFirstOne, prunePredicate, listener, false);
		List<SyntaxTree> argumentListReplacement = subTrees;
		if (rootTreeReplacement == null) {
			rootTreeReplacement = getRootTree();
		}
	
		if (!onlyTheFirstOne || rootTreeReplacement == getRootTree()) {
			for (int i = 0; i != subTrees.size(); i++) {
				SyntaxTree argument = subTrees.get(i);
				SyntaxTree argumentReplacement = argument == null? null :
					argument.replaceSubTrees(replacementFunction, onlyTheFirstOne, prunePredicate, listener, false);
				argumentListReplacement =
					storeSubTreeReplacement(argumentListReplacement, i, argument, argumentReplacement);
				if (onlyTheFirstOne && argumentListReplacement != subTrees) {
					break;
				}
			}
		}
		
		SyntaxTree result = makeReplacementIfAny(rootTreeReplacement, argumentListReplacement);
		
		if (listener != null) {
			listener.apply(this, result);
		}
		
		return result;
	}

	@Override
	public SyntaxTree replaceSubTrees(Function<SyntaxTree, SyntaxTree> replacementFunction) {
		Object rootTreeReplacement = replacementFunction.apply(getRootTree());
		if (rootTreeReplacement == null) {
			rootTreeReplacement = getRootTree();
		}
	
		List<SyntaxTree> subTreeListReplacement = subTrees;
		for (int i = 0; i != subTrees.size(); i++) {
			SyntaxTree subTree = subTrees.get(i);
			SyntaxTree subTreeReplacement = null;
			if (subTree != null) {
				subTreeReplacement = replacementFunction.apply(subTree);
			}
			subTreeListReplacement =
				storeSubTreeReplacement(subTreeListReplacement, i, subTree, subTreeReplacement);
		}
		
		SyntaxTree result = makeReplacementIfAny(rootTreeReplacement, subTreeListReplacement);
		
		return result;
	}

	private List<SyntaxTree> storeSubTreeReplacement(
			List<SyntaxTree> subTreesReplacement, int i, SyntaxTree subTree,
			SyntaxTree subTreeReplacement) {
		if (subTreeReplacement != null && subTreeReplacement != subTree)	{
			if (subTreesReplacement == subTrees) {
				subTreesReplacement = new LinkedList<SyntaxTree>(subTrees);
			}
			subTreesReplacement.set(i, subTreeReplacement);
		}
		return subTreesReplacement;
	}

	/**
	 * The following needs to be private because it relies on whether the given subTreesReplacement
	 * is the same object as subTrees, which is private (getImmediateSubTrees() provides an unmodifiable version of it).
	 */
	private SyntaxTree makeReplacementIfAny(Object rootTreeReplacement, List<SyntaxTree> subTreesReplacement) {
		if (rootTreeReplacement != getRootTree() || subTreesReplacement != subTrees) {
			return SyntaxTrees.make(rootTreeReplacement, Collections.unmodifiableList(new ArrayList<SyntaxTree>(subTreesReplacement)));
		}
		return this;
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
