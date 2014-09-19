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

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryProcedure;
import com.sri.ai.util.base.ReplaceByIfEqualTo;

/**
 * A basic, default implementation of some SyntaxTree and helper methods.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractSyntaxTree implements SyntaxTree {
	
	public static final Function<Object, SyntaxTree> wrapper = new Function<Object, SyntaxTree>() {
		@Override
		public SyntaxTree apply(Object object) {
			return SyntaxTrees.wrap(object);
		}
	};

	/**
	 * An arbitrary Java object associated with tree node.
	 * How it is used is up to users; a typical use is representing the name or value of a symbol, in the case of leaf nodes,
	 * and a type of expression in the case of non-leaf nodes.
	 */
	protected Object valueOrRootSyntaxTree;
	// Note: Should only be assigned immutable lists.
	protected List<SyntaxTree> subTrees = Collections.emptyList();
	//
	//
	private String cachedToString = null;

	public static Map<SyntaxTree, SyntaxTree> wrapAsMap(Object... pairs) {
		return Util.map(Expressions.wrap(pairs).toArray());
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), null, null);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), null, null);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), prunePredicate);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), prunePredicate);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction) {
		return replaceSubTreesFirstOccurrence(replacementFunction, null, null);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction) {
		return replaceSubTreesAllOccurrences(replacementFunction, null, null);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate) {
		return replaceSubTreesFirstOccurrence(replacementFunction, prunePredicate, null);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction, Predicate<SyntaxTree> prunePredicate) {
		return replaceSubTreesAllOccurrences(replacementFunction, prunePredicate, null);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(Function<SyntaxTree, SyntaxTree> replacementFunction, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesFirstOccurrence(replacementFunction, null, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(Function<SyntaxTree, SyntaxTree> replacementFunction, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesAllOccurrences(replacementFunction, null, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), null, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), null, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesFirstOccurrence(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), prunePredicate, listener);
	}

	@Override
	public SyntaxTree replaceSubTreesAllOccurrences(SyntaxTree replaced, SyntaxTree replacement, Predicate<SyntaxTree> prunePredicate, BinaryProcedure<SyntaxTree, SyntaxTree> listener) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTree>(replacement, replaced), prunePredicate, listener);
	}

	/** Makes a shallow copy of this tree. */
	@Override
	abstract public Object clone() throws CloneNotSupportedException;
	
	@Override
	public int numberOfImmediateSubTrees() {
		return subTrees.size();
	}

	@Override
	public SyntaxTree getSubTree(Object fieldKey) {
		if (fieldKey.equals("functor")) {
			return getRootTree();
		}
		else if (fieldKey instanceof Number) {
			int index = ((Number)fieldKey).intValue();
			if (index == -1) {
				return (SyntaxTree) valueOrRootSyntaxTree;
			}
			if (index < subTrees.size()) {
				return subTrees.get(index);
			}
			else {
				return null;
			}
		}
		return null;
	}

	@Override
	public List<SyntaxTree> getImmediateSubTrees() {
		return subTrees;
	}

	@Override
	public Iterator<SyntaxTree> getImmediateSubTreesIterator() {
		return getImmediateSubTrees().iterator();
	}

	@Override
	public String toString() {
		if (cachedToString == null) {
			cachedToString = toStringWithoutCaching();
		}
		return cachedToString;
	}
}
