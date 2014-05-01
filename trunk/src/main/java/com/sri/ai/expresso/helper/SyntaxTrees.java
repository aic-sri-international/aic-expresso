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
package com.sri.ai.expresso.helper;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.collect.Lists;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree2;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.core.DefaultSymbol2;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A class of helper methods for {@link SyntaxTree}s.
 * 
 * @author braz
 */
@Beta
public class SyntaxTrees {
	/**
	 * If argument is a "kleene list" application, returns a {@link List} of its arguments;
	 * otherwise, returns a {@link List} containing this argument.
	 */
	public
	static List<SyntaxTree> ensureListFromKleeneList(SyntaxTree listOrSingleElementOfList) {
		boolean isListName = listOrSingleElementOfList != null && listOrSingleElementOfList.getLabel().equals("kleene list");
		return (isListName ? listOrSingleElementOfList.getImmediateSubTrees() : Lists.newArrayList(listOrSingleElementOfList));
	}

	/** Gets an object and returns it if it is an expression, or a {@link DefaultSymbol} containing it as value. */
	public static SyntaxTree wrap(Object object) {
		if (object == null || object instanceof SyntaxTree) {
			return (SyntaxTree) object;
		}
		return DefaultSymbol2.createSymbol(object);
	}

	/** The array version of {@link #wrap(Object)}. */
	public static List<SyntaxTree> wrap(Object[] array) {
		LinkedList<SyntaxTree> result = new LinkedList<SyntaxTree>();
		for (int i = 0; i!= array.length; i++) {
			SyntaxTree wrap = wrap(array[i]);
			result.add(wrap);
		}
		return result;
	}
	
	/** A version of {@link #wrap(Object)} getting an iterator and returning a list. */
	public static List<SyntaxTree> wrap(Iterator<Object> iterator) {
		List<SyntaxTree> result = Util.listFrom(new FunctionIterator<Object, SyntaxTree>(iterator, WRAPPER));
		return result;
	}

	/** A {@link Function} version of {@link #wrap(Object)}. */
	public static final Function<Object, SyntaxTree> WRAPPER = new Function<Object, SyntaxTree>() {
		@Override
		public SyntaxTree apply(Object object) {
			return wrap(object);
		}
	};

	public static SyntaxTree make(Object rootTreeReplacement, Object... subTrees) {
		SyntaxTree result = new DefaultCompoundSyntaxTree2(rootTreeReplacement, subTrees);
		return result;
	}

	/**
	 * Returns a "kleen list"-labeled syntax tree if given list is not singleton,
	 * and the single element itself otherwise.
	 * Wraps objects into expressions.
	 * This is a inverse operation of {@link #ensureListFromKleeneList(SyntaxTree)}.
	 */
	public static <T> SyntaxTree makeKleeneListIfNeeded(Collection<T> objects) {
		if (objects.size() == 1 ) {
			return SyntaxTrees.wrap(Util.getFirstOrNull(objects));
		}
		return SyntaxTrees.make("kleene list", objects);
	}
	
}
