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
import com.sri.ai.brewer.api.Parser;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.SyntaxLeaf;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.expresso.core.DefaultSyntaxLeaf;
import com.sri.ai.expresso.core.ExpressionOnSyntaxLeaf;
import com.sri.ai.grinder.parser.antlr.AntlrGrinderParserWrapper;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A class of helper methods for {@link SyntaxTree}s.
 * 
 * @author braz
 */
@Beta
public class SyntaxTrees {
	/** Gets an object and returns it if it is an syntax tree, or a {@link SyntaxLeaf} containing it as value. */
	public static SyntaxTree wrap(Object object) {
		if (object == null || object instanceof SyntaxTree) {
			return (SyntaxTree) object;
		}
		return makeSymbol(object);
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

	public static CompoundSyntaxTree makeCompoundSyntaxTree(Object label, Object... subTrees) {
		CompoundSyntaxTree result = new DefaultCompoundSyntaxTree(label, subTrees);
		return result;
	}
	
	public static SyntaxLeaf makeSymbol(Object value) {
		SyntaxLeaf result = DefaultSyntaxLeaf.createSymbol(value);
		return result;
	}

	/**
	 * If argument is a "kleene list" application, returns a {@link List} of its arguments;
	 * otherwise, returns a {@link List} containing this argument.
	 */
	public
	static List<SyntaxTree> ensureListFromKleeneList(SyntaxTree listOrSingleElementOfList) {
		boolean isListName = listOrSingleElementOfList != null && listOrSingleElementOfList.getLabel().equals("kleene list");
		return (isListName ? listOrSingleElementOfList.getImmediateSubTrees() : Lists.newArrayList(listOrSingleElementOfList));
	}

	/**
	 * Returns a "kleene list"-labeled syntax tree if given list is not singleton,
	 * and the single element itself otherwise.
	 * Wraps objects into syntax trees.
	 * This is a inverse operation of {@link #ensureListFromKleeneList(SyntaxTree)}.
	 */
	public static <T> SyntaxTree makeKleeneListIfNeeded(Collection<T> objects) {
		if (objects.size() == 1 ) {
			return SyntaxTrees.wrap(Util.getFirst(objects));
		}
		CompoundSyntaxTree result = SyntaxTrees.makeCompoundSyntaxTree("kleene list", objects);
		return result;
	}

	public static String makeStringValuedSymbolParseSafe(String string) {
		return DefaultSyntaxLeaf.makeStringValuedSymbolParseSafe(string);
	}

	static private Parser parser = new AntlrGrinderParserWrapper();
	
	/**
	 * Parse a string into a syntax tree by parsing it as an expression using {@link AntlrGrinderParserWrapper}
	 * returning its syntax tree.
	 */
	public static SyntaxTree parse(String string) {
		Expression expression = parser.parse(string);
		SyntaxTree result = expression.getSyntaxTree();
		return result;
	}
	
	public static void flushGlobalSymbolTable() {
		ExpressionOnSyntaxLeaf.flushGlobalSymbolTable();
	}

	/**
	 * Set the number of integer places a number is to have before it is
	 * displayed in scientific notation.
	 * 
	 * @param numIntegerPlaces
	 * @return the value previously used before being set here.
	 */
	public static int setDisplayScientificGreaterNIntegerPlaces(int numIntegerPlaces) {
		int oldValue = DefaultSyntaxLeaf._displayScientificGreaterNIntegerPlaces;
		
		DefaultSyntaxLeaf._displayScientificGreaterNIntegerPlaces = numIntegerPlaces;
				
		return oldValue;
	}

	/**
	 * Set the number of decimal places a number is to have before it is
	 * displayed in scientific notation.
	 * 
	 * @param numDecimalPlaces
	 * @return the value previously used before being set here.
	 */
	public static int setDisplayScientificAfterNDecimalPlaces(int numDecimalPlaces) {
		int oldValue = DefaultSyntaxLeaf._displayScientificAfterNDecimalPlaces;
		
		DefaultSyntaxLeaf._displayScientificAfterNDecimalPlaces = numDecimalPlaces;
				
		return oldValue;
	}

	/**
	 * Set the numeric display precision for numeric valued symbols.
	 * 
	 * @param precision
	 *        the decimal display precision.
	 *        
	 * @return the old numeric display precision;
	 */
	public static int setNumericDisplayPrecision(int precision) {
		int oldPrecision = DefaultSyntaxLeaf._displayNumericPrecision;
		
		DefaultSyntaxLeaf._displayNumericPrecision = precision;
		
		return oldPrecision;
	}
	

	public static int setNumericDisplayPrecision(Integer value) {
		return SyntaxTrees.setNumericDisplayPrecision(value.intValue());
	}

	/**
	 * Given a syntax tree, a path, and a sub-syntax tree,
	 * returns the syntax tree with its path-sub-syntax tree replaced by the given sub-syntax tree.
	 * The path is a list of indices indicating a path in the syntax tree.
	 * If there are no indices to be followed (path is empty), the sub-syntax tree itself is returned.
	 * The method assumes the path describes an existing path-sub-syntax tree.
	 */
	public static SyntaxTree replaceAtPath(SyntaxTree syntaxTree, List<Integer> path, SyntaxTree subTree) {
		return replaceAtPath(syntaxTree, path, 0, subTree);
	}

	/**
	 * Given an syntax tree, a path, a position i in the path, and a sub-syntax tree,
	 * returns the syntax tree with its path-i-sub-syntax tree replaced by the given sub-syntax tree.
	 * The path is a list of indices indicating a path in the syntax tree.
	 * The path-i-sub-syntax tree is the syntax tree obtained by following the path from the position i on.
	 * If there are no indices to be followed (i is equal to the path's length), the sub-syntax tree is returned.
	 * The method assumes the path describes an existing path-i-sub-syntax tree.
	 */
	private static SyntaxTree replaceAtPath(SyntaxTree syntaxTree, List<Integer> path, int i, SyntaxTree subTree) {
		if (i != path.size()) {
			int index = path.get(i);
			SyntaxTree subTreeAtI = replaceAtPath(syntaxTree.getSubTree(index), path, i + 1, subTree);
			 // does need to be sub tree
			SyntaxTree result = syntaxTree.setImmediateSubTree(index, subTreeAtI);
			return result;
		}
		return subTree;
	}
}
