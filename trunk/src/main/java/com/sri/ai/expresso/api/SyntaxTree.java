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
package com.sri.ai.expresso.api;

import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.util.base.BinaryProcedure;

/**
 * A Syntax tree (also known as syntactic tree).
 * 
 * A syntax tree is either a {@link Symbol}, or a {@link CompoundSyntaxTree}.
 * It has the following properties:
 * <ul>
 * <li>value: a non-null Object in the case of Symbols and null in the case of CompoundSyntaxTree.
 * <li>root syntax tree: another syntax tree in the case of CompoundSyntaxTree, and null for Symbols.
 * <li>label: the root syntax tree in the case of CompoundSyntaxTree, and the value in the case of a Symbol.
 * <li>immediate sub-trees: an empty list for Symbol, and a list of syntax trees for CompoundSyntaxTree. 
 * </ul>
 * <p>
 * The interface provides methods for accessing these properties and for setting them.
 * SyntaxTree objects must be immutable, so setting a property
 * returns a distinct syntax tree resulting from the modification.
 * 
 * @author braz
 *
 */
@Beta
public interface SyntaxTree extends Comparable {

	Object getValue();
	
	SyntaxTree getRootTree();
	
	Object getLabel();

	/** 
	 * Returns the list of subtrees.
	 */
	List<SyntaxTree> getImmediateSubTrees();

	/**
	 * Returns the number of subtrees in this tree.
	 */
	int numberOfImmediateSubTrees();

	/**
	 * Returns the subtree corresponding to a given key (so far, only integers are supported),
	 * with -1 representing the label.
	 */
	SyntaxTree getSubTree(Object fieldKey);
	
	/** 
	 * An iterator-based (and therefore possibly lazy) version of {@link #getImmediateSubTrees()}.
	 */
	Iterator<SyntaxTree> getImmediateSubTreesIterator();

	/**
	 * Returns an iterator for subtrees of this tree, including the one stored in the root (which comes first).
	 */
	Iterator<SyntaxTree> getImmediateSubTreesIncludingRootOneIterator();

	/**
	 * Returns the number of subtrees, including the tree stored in the root.
	 */
	int numberOfImmediateSubTreesIncludingRootOneIterator();

	/**
	 * Returns a tree equal to this one, but for replacing the i-th subtree by the given one.
	 */
	SyntaxTree setImmediateSubTree(int i, Object newIthSubTree);

	/**
	 * Replaces the first (in depth-first order) subtree or root tree <code>s</code> by
	 * <code>replacementFunction.evaluate(s)</code> if it is non-null and a different instance from <code>s</code>.
	 * Does not operate on the subtree replacement itself.
	 * Returns <code>this</code> if there are no replacements.
	 */
	SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction);

	/**
	 * Replaces each subtree or root tree <code>s</code> by <code>replacementFunction.evaluate(s)</code>
	 * if it is non-null and a different instance.
	 * Does not operate on replacements themselves.
	 * Returns <code>this</code> if there are no replacements.
	 */
	SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction);

	/**
	 * Replaces the first (in depth-first order) subtrees or root tree equal to <code>replaced</code> by <code>replacement</code>
	 * (does not examine <code>replacement</code> itself).
	 * Returns <code>this</code> if there are no replacements.
	 */
	SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement);

	/**
	 * Replaces all subtrees or root tree equal to <code>replaced</code> by <code>replacement</code>
	 * (does not examine <code>replacement</code> itself).
	 * Returns <code>this</code> if there are no replacements.
	 */
	SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement);

	/**
	 * Just like {@link #replaceSubTreesFirstOccurrence(Function)},
	 * but with a pruning predicate.
	 * A pruning predicate ignores subtrees or root tree for which it evaluates as true.
	 */
	SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate);

	/**
	 * Just like {@link #replaceSubTreesAllOccurrences(Function)},
	 * but with a pruning predicate.
	 * A pruning predicate ignores subtrees or root tree for which it evaluates as true.
	 */
	SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate);

	/**
	 * Just like {@link #replaceSubTreesFirstOccurrence(SyntaxTree, SyntaxTree)},
	 * but with a pruning predicate.
	 * A pruning predicate ignores subtrees or root tree for which it evaluates as true.
	 */
	SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate);

	/**
	 * Just like {@link #replaceSubTreesAllOccurrences(SyntaxTree, SyntaxTree)},
	 * but with a pruning predicate.
	 * A pruning predicate ignores subtrees or root tree for which it evaluates as true.
	 */
	SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate);

	/**
	 * Replaces the first (in depth-first order) subtree or root tree <code>s</code> by
	 * <code>replacementFunction.evaluate(s)</code> if it is non-null and a different instance from <code>s</code>.
	 * Does not operate on the replacement itself.
	 * Returns <code>this</code> if there are no replacements.
	 * Calls a listener on both given and returned trees.
	 */
	SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction, BinaryProcedure<SyntaxTree, SyntaxTree> listener);

	/**
	 * Replaces each subtree or root tree <code>s</code> by <code>replacementFunction.evaluate(s)</code>
	 * if it is non-null and a different instance.
	 * Does not operate on replacements themselves.
	 * Returns <code>this</code> if there are no replacements.
	 * Calls a listener on both given and returned trees.
	 */
	SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction, BinaryProcedure<SyntaxTree, SyntaxTree> listener);

	/**
	 * Replaces the first (in depth-first order) subtrees or root tree equal to <code>replaced</code> by <code>replacement</code>
	 * (does not examine <code>replacement</code> itself).
	 * Returns <code>this</code> if there are no replacements.
	 * Calls a listener on both given and returned trees.
	 */
	SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement, BinaryProcedure<SyntaxTree, SyntaxTree> listener);

	/**
	 * Replaces all subtrees or root tree equal to <code>replaced</code> by <code>replacement</code>
	 * (does not examine <code>replacement</code> itself).
	 * Returns <code>this</code> if there are no replacements.
	 * Calls a listener on both given and returned trees.
	 */
	SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement, BinaryProcedure<SyntaxTree, SyntaxTree> listener);

	/**
	 * Just like {@link #replaceSubTreesFirstOccurrence(Function)},
	 * but with a pruning predicate.
	 * A pruning predicate ignores subtrees or root tree for which it evaluates as true.
	 * Calls a listener on both given and returned trees.
	 */
	SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener);

	/**
	 * Just like {@link #replaceSubTreesAllOccurrences(Function)},
	 * but with a pruning predicate.
	 * A pruning predicate ignores subtrees or root tree for which it evaluates as true.
	 * Calls a listener on both given and returned trees.
	 */
	SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener);

	/**
	 * Just like {@link #replaceSubTreesFirstOccurrence(SyntaxTree, SyntaxTree)},
	 * but with a pruning predicate.
	 * A pruning predicate ignores subtrees or root tree for which it evaluates as true.
	 * Calls a listener on both given and returned trees.
	 */
	SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener);

	/**
	 * Just like {@link #replaceSubTreesAllOccurrences(SyntaxTree, SyntaxTree)},
	 * but with a pruning predicate.
	 * A pruning predicate ignores subtrees or root tree for which it evaluates as true.
	 * Calls a listener on both given and returned trees.
	 */
	SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener);

	/**
	 * Returns new tree with subtrees or root tree replaced according to given replacement function,
	 * or original tree if no subtree or root tree is replaced.
	 */
	SyntaxTree replaceSubTrees(Function<SyntaxTree, SyntaxTree> replacementFunction);
	
	/**
	 * Just like {@link #replaceSubTreesAllOccurrences(Function)},
	 * but with a flag determining if only the first occurrence should be replaced,
	 * a pruning predicate, and a listener and.
	 * A pruning predicate ignores subtrees or root tree for which it evaluates as true.
	 * Calls a listener on both given and returned trees.
	 * If <code>ignoreRootTree</code> is true, replacement function is not invoked on
	 * root tree.
	 */
	SyntaxTree replaceSubTrees(Function<SyntaxTree, SyntaxTree> replacementFunction, boolean onlyTheFirstOne, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener, boolean ignoreRootTree);

	/** A default <code>toString()</code> method not using any user-defined writing object. */
	String toStringWithoutCaching();
}
