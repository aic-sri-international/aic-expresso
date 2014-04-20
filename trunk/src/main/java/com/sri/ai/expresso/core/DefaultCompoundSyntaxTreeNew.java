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
import com.sri.ai.expresso.api.CompoundSyntaxTreeNew;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxTreeNew;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryProcedure;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;

/**
 * A default implementation of {@link CompoundSyntaxTreeNew}. It is based on
 * keeping a functor Expression and a list of argument SyntaxTrees.
 * 
 * @author braz
 */
@Beta
public class DefaultCompoundSyntaxTreeNew extends AbstractSyntaxTreeNew implements CompoundSyntaxTreeNew {
	private static final long serialVersionUID = 1L;
	//
	private int hashCode = -1; // lazy init and re-use the calculated hashCode.
	
	private DefaultCompoundSyntaxTreeNew() {
	}

	/**
	 * Constructs a function application with given root tree or label and sub-trees, copying them for internal use.
	 * Objects that are not syntax trees are wrapped as Symbol syntax trees, unless
	 * a single argument is given and it is a List;
	 * in this case, it is considered to be a List<SyntaxTreeNew>
	 * and a copy is used for the immediate sub-trees.
	 * (doing pretty much the same thing as {@link #make(Object, List)}, but in a slightly slower manner).
	 */
	@SuppressWarnings("unchecked")
	public DefaultCompoundSyntaxTreeNew(Object functor, Object ... subTrees) {
		this.valueOrRootSyntaxTree = wrap(functor);
		if (subTrees.length == 1 && subTrees[0] instanceof List) {
			// Note: We can have nulls, therefore cannot use ImmutableList directly.
			this.subTrees = Collections.unmodifiableList(new ArrayList<SyntaxTreeNew>((List<SyntaxTreeNew>)subTrees[0])); // makes a copy since this constructor does not assume ownership.
		}
		else {
			// Note: We can have nulls, therefore cannot use ImmutableList directly.
			this.subTrees = Collections.unmodifiableList(wrap(subTrees));
		}
	}

	public Object getValue() {
		return null;
	}

	@Override
	public SyntaxTreeNew getRootTree() {
		return (SyntaxTreeNew) valueOrRootSyntaxTree;
	}

	@Override
	public Object getLabel() {
		return valueOrRootSyntaxTree;
	}

	/**
	 * Constructs a compound syntax tree with given root tree (or the value of Symbol root tree (label) if not a SyntaxTree)
	 * and sub trees, keeping the sub trees ownership.
	 * This is a more efficient constructor than {@link #DefaultCompoundSyntaxTree(Object, Object...)}
	 * and offers the possibility of using an already existing list.
	 */
	public static DefaultCompoundSyntaxTreeNew make(Object rootTreeOrLabel, List<? extends SyntaxTreeNew> subTrees) {
		DefaultCompoundSyntaxTreeNew result = new DefaultCompoundSyntaxTreeNew();
		result.valueOrRootSyntaxTree = wrap(rootTreeOrLabel);
		// Note: We can have nulls, therefore cannot use ImmutableList directly.
		result.subTrees = Collections.unmodifiableList(new ArrayList<SyntaxTreeNew>(subTrees));
		return result;
	}

	@Override
	public Iterator<SyntaxTreeNew> getImmediateSubTreesIncludingRootOneIterator() {
		return new NestedIterator<SyntaxTreeNew>(Util.list(getRootTree(), getImmediateSubTreesIterator()));
	}

	@Override
	public int numberOfImmediateSubTreesIncludingRootOneIterator() {
		return 1 + numberOfImmediateSubTrees();
	}

	@Override
	public SyntaxTreeNew setImmediateSubTree(int i, Object newIthSubTree) {
		SyntaxTreeNew newRootTree = getRootTree();
		List<SyntaxTreeNew> newSubTrees = subTrees;
		// it is important to use field {@link #subTrees} above instead of method {@link #getSubTrees()}
		// because we want to be able to reuse {@link #subTrees} in case the root tree is being set.
		// If we use {@link #getSubTrees()}, we get an unmodifiable list object instead of the original arguments list.
		if (i == -1) {
			newRootTree = wrap(newIthSubTree);
		}
		else {
			newSubTrees = storeSubTreeReplacement(newSubTrees, i, getSubTree(i), newIthSubTree);
		}	
		SyntaxTreeNew result = makeReplacementIfAny(newRootTree, newSubTrees);
		return result;
	}

	public int hashCode() {
		if (hashCode == -1) {
			SyntaxTreeNew rootTree = getRootTree();
			int rootHashCode = rootTree.hashCode();
			List<SyntaxTreeNew> immediateSubTrees = getImmediateSubTrees();
			int subTreesHashCode = immediateSubTrees.hashCode();
			hashCode = rootHashCode + subTreesHashCode;
		}
		
		return hashCode;
	}

	public boolean equals(Object anotherObject) {
		boolean result;
		
		if (anotherObject instanceof CompoundSyntaxTreeNew) {
			CompoundSyntaxTreeNew another = (CompoundSyntaxTreeNew) anotherObject;
			if (this.hashCode() == another.hashCode()) {
				List<SyntaxTreeNew> anotherSubTrees = another.getImmediateSubTrees();
				result = this.getRootTree().equals(another.getRootTree()) && this.getImmediateSubTrees().equals(anotherSubTrees);
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
		String rootTreeString = getRootTree().defaultToString();
		if ( ! (getRootTree() instanceof Symbol)) {
			rootTreeString = "(" + rootTreeString + ")";
		}
		Iterator defaultToStringOfSubTrees =
			new FunctionIterator<SyntaxTreeNew, String>(new DefaultToString(), getImmediateSubTrees());
		return rootTreeString + "(" + Util.join(", ", defaultToStringOfSubTrees) + ")";
	}
	
	private static class DefaultToString implements Function<SyntaxTreeNew, String> {
		@Override
		public String apply(SyntaxTreeNew syntaxTree) {
			if (syntaxTree == null) {
				return "null";
			}
			return syntaxTree.defaultToString();
		}
	}

	@Override
	public SyntaxTreeNew replaceSubTreesFirstOccurrence(Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction, Predicate<SyntaxTreeNew> prunePredicate, BinaryProcedure<SyntaxTreeNew, SyntaxTreeNew> listener) {
		return replaceSubTrees(replacementFunction, true /* only the first one */, prunePredicate, listener, false);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesAllOccurrences(Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction, Predicate<SyntaxTreeNew> prunePredicate, BinaryProcedure<SyntaxTreeNew, SyntaxTreeNew> listener) {
		return replaceSubTrees(replacementFunction, false /* not only the first one */, prunePredicate, listener, false);
	}

	@Override
	public SyntaxTreeNew replaceSubTrees(
			Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction, boolean onlyTheFirstOne,
			Predicate<SyntaxTreeNew> prunePredicate, BinaryProcedure<SyntaxTreeNew, SyntaxTreeNew> listener, boolean ignoreTopExpression) {
		
		if (prunePredicate != null && prunePredicate.apply(this)) {
			return this;
		}
		
		if ( ! ignoreTopExpression) {
			SyntaxTreeNew topReplacement = replacementFunction.apply(this);
			if (topReplacement != this && topReplacement != null) {
				if (listener != null) {
					listener.apply(this, topReplacement);
				}
				return topReplacement;
			}
		}
		
		SyntaxTreeNew rootTreeReplacement = getRootTree().replaceSubTrees(replacementFunction, onlyTheFirstOne, prunePredicate, listener, false);
		List<SyntaxTreeNew> argumentListReplacement = subTrees;
		if (rootTreeReplacement == null) {
			rootTreeReplacement = getRootTree();
		}
	
		if (!onlyTheFirstOne || rootTreeReplacement == getRootTree()) {
			for (int i = 0; i != subTrees.size(); i++) {
				SyntaxTreeNew argument = subTrees.get(i);
				SyntaxTreeNew argumentReplacement = argument == null? null :
					argument.replaceSubTrees(replacementFunction, onlyTheFirstOne, prunePredicate, listener, false);
				argumentListReplacement =
					storeSubTreeReplacement(argumentListReplacement, i, argument, argumentReplacement);
				if (onlyTheFirstOne && argumentListReplacement != subTrees) {
					break;
				}
			}
		}
		
		SyntaxTreeNew result = makeReplacementIfAny(rootTreeReplacement, argumentListReplacement);
		
		if (listener != null) {
			listener.apply(this, result);
		}
		
		return result;
	}

	@Override
	public SyntaxTreeNew replaceSubTrees(Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction) {
		Object rootTreeReplacement = replacementFunction.apply(getRootTree());
		if (rootTreeReplacement == null) {
			rootTreeReplacement = getRootTree();
		}
	
		List<SyntaxTreeNew> subTreeListReplacement = subTrees;
		for (int i = 0; i != subTrees.size(); i++) {
			SyntaxTreeNew subTree = subTrees.get(i);
			SyntaxTreeNew subTreeReplacement = null;
			if (subTree != null) {
				subTreeReplacement = replacementFunction.apply(subTree);
			}
			subTreeListReplacement =
				storeSubTreeReplacement(subTreeListReplacement, i, subTree, subTreeReplacement);
		}
		
		SyntaxTreeNew result = makeReplacementIfAny(rootTreeReplacement, subTreeListReplacement);
		
		return result;
	}

	private List<SyntaxTreeNew> storeSubTreeReplacement(
			List<SyntaxTreeNew> subTreesReplacement, int i, SyntaxTreeNew subTree,
			Object subTreeReplacement) {
		if (subTreeReplacement != null && subTreeReplacement != subTree)	{
			if (subTreesReplacement == subTrees) {
				subTreesReplacement = new LinkedList<SyntaxTreeNew>(subTrees);
			}
			subTreesReplacement.set(i, wrap(subTreeReplacement));
		}
		return subTreesReplacement;
	}

	/**
	 * The following needs to be private because it relies on whether the given argumentsReplacement
	 * is the same object as arguments, which is private (getArguments() provides an unmodifiable version of it.
	 */
	private SyntaxTreeNew makeReplacementIfAny(Object rootTreeReplacement, List<SyntaxTreeNew> subTreesReplacement) {
		if (rootTreeReplacement != getRootTree() || subTreesReplacement != subTrees) {
			return make(wrap(rootTreeReplacement), subTreesReplacement);
		}
		return this;
	}
	
	public SyntaxTreeNew clone() {
		return DefaultCompoundSyntaxTreeNew.make(getRootTree(), subTrees);
		// it is best to use the field 'arguments' instead of method 'getArguments'
		// because we can share argument lists among function applications, since they are never modified.
		// The method 'getArguments' would unnecessarily create an unmodifiable list object.
	}
}
