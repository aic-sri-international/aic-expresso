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
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.base.Predicate;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.api.SyntaxTreeNew;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryProcedure;
import com.sri.ai.util.base.ReplaceByIfEqualTo;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A basic, default implementation of some SyntaxTreeNew and helper methods.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractSyntaxTreeNew implements SyntaxTreeNew {
	private static final long serialVersionUID = 1L;
	
	/** Gets an object and returns it if it is a syntax tree, or a {@link DefaultSymbol} containing it as value. */
	public static SyntaxTreeNew wrap(Object object) {
		if (object == null || object instanceof SyntaxTree) {
			return (SyntaxTreeNew) object;
		}
		return DefaultSymbolNew.createSymbol(object);
	}

	/** The array version of {@link #wrap(Object)}. */
	public static List<SyntaxTreeNew> wrap(Object[] array) {
		LinkedList<SyntaxTreeNew> result = new LinkedList<SyntaxTreeNew>();
		for (int i = 0; i!= array.length; i++) {
			SyntaxTreeNew wrap = wrap(array[i]);
			result.add(wrap);
		}
		return result;
	}
	
	/** A version of {@link #wrap(Object)} getting an iterator and returning a list. */
	public static List<SyntaxTreeNew> wrap(Iterator<Object> iterator) {
		List<SyntaxTreeNew> result = Util.listFrom(new FunctionIterator<Object, SyntaxTreeNew>(iterator, WRAPPER));
		return result;
	}

	/** A {@link Function} version of {@link #wrap(Object)}. */
	public static final Function<Object, SyntaxTreeNew> WRAPPER = new Function<Object, SyntaxTreeNew>() {
		@Override
		public SyntaxTreeNew apply(Object object) {
			return wrap(object);
		}
	};
	
	public static final Function<Object, SyntaxTreeNew> wrapper = new Function<Object, SyntaxTreeNew>() {
		@Override
		public SyntaxTreeNew apply(Object object) {
			return wrap(object);
		}
	};

	/**
	 * An arbitrary Java object associated with tree node.
	 * How it is used is up to users; a typical use is representing the name or value of a symbol, in the case of leaf nodes,
	 * and a type of expression in the case of non-leaf nodes.
	 */
	protected Object valueOrRootSyntaxTree;
	// Note: Should only be assigned immutable lists.
	protected List<SyntaxTreeNew> subTrees = Collections.emptyList();
	//
	private static Cache<Thread, Function<SyntaxTreeNew, String>> threadToString = newThreadToStringCache();
	//
	private String                             cachedToString                      = null;

	public static Map<SyntaxTreeNew, SyntaxTreeNew> wrapAsMap(Object... pairs) {
		return Util.map(Expressions.wrap(pairs).toArray());
	}

	@Override
	public SyntaxTreeNew replaceSubTreesFirstOccurrence(SyntaxTreeNew replaced, SyntaxTreeNew replacement) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTreeNew>(replacement, replaced), null, null);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesAllOccurrences(SyntaxTreeNew replaced, SyntaxTreeNew replacement) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTreeNew>(replacement, replaced), null, null);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesFirstOccurrence(SyntaxTreeNew replaced, SyntaxTreeNew replacement, Predicate<SyntaxTreeNew> prunePredicate) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTreeNew>(replacement, replaced), prunePredicate);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesAllOccurrences(SyntaxTreeNew replaced, SyntaxTreeNew replacement, Predicate<SyntaxTreeNew> prunePredicate) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTreeNew>(replacement, replaced), prunePredicate);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesFirstOccurrence(Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction) {
		return replaceSubTreesFirstOccurrence(replacementFunction, null, null);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesAllOccurrences(Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction) {
		return replaceSubTreesAllOccurrences(replacementFunction, null, null);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesFirstOccurrence(Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction, Predicate<SyntaxTreeNew> prunePredicate) {
		return replaceSubTreesFirstOccurrence(replacementFunction, prunePredicate, null);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesAllOccurrences(Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction, Predicate<SyntaxTreeNew> prunePredicate) {
		return replaceSubTreesAllOccurrences(replacementFunction, prunePredicate, null);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesFirstOccurrence(Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction, BinaryProcedure<SyntaxTreeNew, SyntaxTreeNew> listener) {
		return replaceSubTreesFirstOccurrence(replacementFunction, null, listener);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesAllOccurrences(Function<SyntaxTreeNew, SyntaxTreeNew> replacementFunction, BinaryProcedure<SyntaxTreeNew, SyntaxTreeNew> listener) {
		return replaceSubTreesAllOccurrences(replacementFunction, null, listener);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesFirstOccurrence(SyntaxTreeNew replaced, SyntaxTreeNew replacement, BinaryProcedure<SyntaxTreeNew, SyntaxTreeNew> listener) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTreeNew>(replacement, replaced), null, listener);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesAllOccurrences(SyntaxTreeNew replaced, SyntaxTreeNew replacement, BinaryProcedure<SyntaxTreeNew, SyntaxTreeNew> listener) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTreeNew>(replacement, replaced), null, listener);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesFirstOccurrence(SyntaxTreeNew replaced, SyntaxTreeNew replacement, Predicate<SyntaxTreeNew> prunePredicate, BinaryProcedure<SyntaxTreeNew, SyntaxTreeNew> listener) {
		return replaceSubTreesFirstOccurrence(new ReplaceByIfEqualTo<SyntaxTreeNew>(replacement, replaced), prunePredicate, listener);
	}

	@Override
	public SyntaxTreeNew replaceSubTreesAllOccurrences(SyntaxTreeNew replaced, SyntaxTreeNew replacement, Predicate<SyntaxTreeNew> prunePredicate, BinaryProcedure<SyntaxTreeNew, SyntaxTreeNew> listener) {
		return replaceSubTreesAllOccurrences(new ReplaceByIfEqualTo<SyntaxTreeNew>(replacement, replaced), prunePredicate, listener);
	}

	abstract public SyntaxTreeNew clone();
	
	@Override
	public int numberOfImmediateSubTrees() {
		return subTrees.size();
	}

	@Override
	public SyntaxTreeNew getSubTree(Object fieldKey) {
		if (fieldKey.equals("functor")) {
			return getRootTree();
		}
		else if (fieldKey instanceof Number) {
			int index = ((Number)fieldKey).intValue();
			if (index == -1) {
				return (SyntaxTreeNew) valueOrRootSyntaxTree;
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
	public List<SyntaxTreeNew> getImmediateSubTrees() {
		return subTrees;
	}

	@Override
	public Iterator<SyntaxTreeNew> getImmediateSubTreesIterator() {
		return getImmediateSubTrees().iterator();
	}

	///////////////////////// FUNCTION APPLICATION METHODS //////////////////////

	
	@Override
	public String toString() {
		if (cachedToString == null) {
			Function<SyntaxTreeNew, String> toString = getToString();
			if (toString != null) {
				cachedToString = toString.apply(this);
			}
			else {
				cachedToString = defaultToString();
			}
		}
		return cachedToString;
	}
	
	private static Cache<Thread, Function<SyntaxTreeNew, String>> newThreadToStringCache() {
		Cache<Thread, Function<SyntaxTreeNew, String>> result = CacheBuilder.newBuilder()
				.expireAfterAccess(ExpressoConfiguration.getSyntaxToStringThreadCacheTimeoutInSeconds(), TimeUnit.SECONDS)
				// Don't hold onto old threads unnecessarily
				.weakKeys()
				.build();
		
		return result;
	}
	
	/**
	 * 
	 * @return the default configured to string unary function for the current
	 *         thread. The instance to use is determined by the configuration
	 *         settings for
	 *         {@link ExpressoConfiguration#KEY_DEFAULT_SYNTAX_TO_STRING_UNARY_FUNCTION_CLASS}
	 *         .
	 */
	private static Function<SyntaxTreeNew, String> getToString() {
		Function<SyntaxTreeNew, String> result = threadToString.getIfPresent(Thread.currentThread());
		if (result == null) {
			// Initialize with the defaultToString() caller, as other methods can
			// rely on Grammar instances that can cause recursive calls, this
			// prevents such recursion from occurring.
			threadToString.put(Thread.currentThread(), new SyntaxTreeNewToStringFunction());
			
			result = ExpressoConfiguration.newConfiguredInstance(ExpressoConfiguration.getDefaultSyntaxToStringUnaryFunctionClass());

			// Now assign the configured object.
			threadToString.put(Thread.currentThread(), result);
		}
		
		return result;
	}
}
