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

import java.lang.reflect.InvocationTargetException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.DepthFirstIterator;

/**
 * An extension to {@link SubSyntaxTreeAndPathDepthFirstIterator} that "marks"
 * the immediate sub-expressions of expression-path pairs satisfying a given
 * predicate. Pairs are marked as they are iterated over, and a pair is checked
 * to be marked or not through the method {@link #isMarked(Pair)}.
 * 
 * @author braz
 */
@Beta
public class SubSyntaxTreeAndPathMarkingThoseWhoseParentsSatisfyAGivenPredicateIterator extends SubSyntaxTreeAndPathDepthFirstIterator {

	private Predicate<Pair<Expression, List<Integer>>> parentCondition;
	private List<Pair<Expression, List<Integer>>> marked;
	
	public SubSyntaxTreeAndPathMarkingThoseWhoseParentsSatisfyAGivenPredicateIterator(Expression expression, Predicate<Pair<Expression, List<Integer>>> parentCondition) {
		super(expression);
		this.parentCondition = parentCondition;
		this.marked = new LinkedList<Pair<Expression, List<Integer>>>();
	}
	
	public SubSyntaxTreeAndPathMarkingThoseWhoseParentsSatisfyAGivenPredicateIterator(
			Pair<Expression, List<Integer>> expressionAndPath,
			Predicate<Pair<Expression, List<Integer>>> parentCondition,
			List<Pair<Expression, List<Integer>>> marked) {
		super(expressionAndPath);
		this.parentCondition = parentCondition;
		this.marked = marked;
	}
	
	@Override
	public DepthFirstIterator<Pair<Expression, List<Integer>>> newInstance(Pair<Expression, List<Integer>> object) {
		throw new UnsupportedOperationException("newInstance() not supported.");
	}
	
	/**
	 * Makes a new SubExpressionAndPathMarkingThoseWhoseParentsSatisfyAGivenPredicateIterator for the given child object.
	 * This is necessary because the constructors of this class take the <code>parentCondition</code> parameter as well as
	 * the object, so the default implementation does not work.
	 */
	protected DepthFirstIterator<Pair<Expression, List<Integer>>>  makeDepthFirstIteratorOfCurrentExtendingClass(Pair<Expression, List<Integer>> object)
	throws InstantiationException, IllegalAccessException,
	InvocationTargetException {
		return new SubSyntaxTreeAndPathMarkingThoseWhoseParentsSatisfyAGivenPredicateIterator(object, parentCondition, marked);
	}

	@Override
	public Iterator<Pair<Expression, List<Integer>>> getChildrenIterator(Pair<Expression, List<Integer>> expressionAndPathObject) {
		Pair<Expression, List<Integer>> expressionAndPath = expressionAndPathObject;
		if (parentCondition.apply(expressionAndPath)) {
			List<Pair<Expression, List<Integer>>> childrenExpressionAndPath = 
				Util.listFrom(super.getChildrenIterator(expressionAndPathObject));
			marked.addAll(childrenExpressionAndPath);
			return childrenExpressionAndPath.iterator();
		}
		return super.getChildrenIterator(expressionAndPathObject);
	}
	
	public boolean isMarked(Pair<Expression, List<Integer>> expressionAndPath) {
		return marked.contains(expressionAndPath);
	}
	
	public static class IsMarked implements Predicate<Pair<Expression, List<Integer>>> {
		private final SubSyntaxTreeAndPathMarkingThoseWhoseParentsSatisfyAGivenPredicateIterator iterator;
	
		private IsMarked(
				SubSyntaxTreeAndPathMarkingThoseWhoseParentsSatisfyAGivenPredicateIterator subExpressionsAndPathsIterator) {
			this.iterator = subExpressionsAndPathsIterator;
		}
	
		public boolean apply(Pair<Expression, List<Integer>> expressionAndPath) {
			return iterator.isMarked(expressionAndPath);
		}
	}

	public Predicate<Pair<Expression, List<Integer>>> getIsMarkedPredicate() {
		return new IsMarked(this);
	}
}
