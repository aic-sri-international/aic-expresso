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
package com.sri.ai.grinder.sgdpllt.rewriter.core;

import static com.sri.ai.grinder.sgdpllt.rewriter.core.FirstOf.flattenListOfRewritersWithRespectToFirstOfToANewList;
import static com.sri.ai.util.Util.applyFunctionToValuesOfMap;
import static com.sri.ai.util.Util.combineMapsIntoNewMapGroupingValuesUnderTheSameKey;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.join;
import static com.sri.ai.util.Util.myAssert;
import static com.sri.ai.util.collect.FunctionIterator.functionIterator;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.base.ConstantExpressionStepSolver;

/**
 * A rewriter that stores a map from keys in some type to base {@link Rewriter}s
 * and a key maker function to {@link Expression}s to the key type.
 * When rewriting an expression, it computes a key for it and
 * acts as the key's corresponding base rewriter.
 * If the computed key has no corresponding base rewriter, the same expression instance is returned.
 * 
 * {@link Switch} rewriters can be merged.
 * If there are two {@link Switch} rewriters <code>S1</code> and <code>S2</code>
 * using the same key maker function (instance-wise, naturally,
 * since it would be too hard to decide that two distinct instance of function objects represent
 * the same key maker), they can be merged into a new {@link Switch} rewriter <code>S</code>.
 * This merged rewriter <code>S</code> maps each key value <code>K</code>
 * into a {@link FirstOf} rewriter composed of the rewriters under the same key value <code>K</code>
 * in <code>S1</code> and <code>S2</code>, in this order
 * (or just the original base rewriter if <code>K</code> is used in only one of <code>S1</code> and <code>S2</code>.
 * 
 * @author braz
 *
 */
public class Switch<T> implements TopRewriter {

	private Function<Expression, T> keyMaker;
	private Map<T, ? extends Rewriter> fromKeyValueToRewriter;

	/**
	 * A standard key maker mapping expressions to their functors's string value
	 * (unfortunately, this is different from their toString() output -- for example, the functor of 'if p then 1 else 0' has string value "if . then . else ." but toString returns "'if . then . else .'").
	 * Using standard key makers minimizes search cost in merged {@link Switch} rewriters
	 * because merging uses instance identity of key makers (see {@link #mergeSwitchesWithTheSameKeyMakerIntoASingleOne(List)}.
	 */
	public final static Function<Expression, String> FUNCTOR = 
			e -> e.getFunctor() == null || !(e.getFunctor().getValue() instanceof String)? "" : (String) e.getFunctor().getValue();
	
	/**
	 * A standard key maker mapping expressions to the syntactic form types.
	 * Using standard key makers minimizes search cost in merged {@link Switch} rewriters
	 * because merging uses instance identity of key makers (see {@link #mergeSwitchesWithTheSameKeyMakerIntoASingleOne(List)}.
	 */
	public final static Function<Expression, Object> SYNTACTIC_FORM_TYPE = e -> e.getSyntacticFormType();
	
	public Switch(Function<Expression, T> keyMaker, Map<T, ? extends Rewriter> fromKeyToRewriter) {
		super();
		this.keyMaker = keyMaker;
		this.fromKeyValueToRewriter = fromKeyToRewriter;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver makeStepSolver(Expression expression) {
		ExpressionLiteralSplitterStepSolver result;
		T key = keyMaker.apply(expression);
		Rewriter baseRewriter = fromKeyValueToRewriter.get(key);
		if (baseRewriter != null) {
			result = baseRewriter.makeStepSolver(expression);
		}
		else {
			result = new ConstantExpressionStepSolver(expression);
		}
		return result;
	}
	
	public Function<Expression, T> getKeyMaker() {
		return keyMaker;
	}

	public Map<T, ? extends Rewriter> getMapFromKeyToRewriter() {
		return fromKeyValueToRewriter;
	}

	@Override
	public String toString() {
		return "Switch rewriter with key maker " + keyMaker + " and map " + fromKeyValueToRewriter;
	}
	
	@Override
	public boolean equals(Object another) {
		boolean result =
				another instanceof Switch
				&& ((Switch) another).keyMaker == keyMaker
				&& ((Switch) another).fromKeyValueToRewriter.equals(fromKeyValueToRewriter);
		return result;
	}
	
	@Override
	public int hashCode() {
		return keyMaker.hashCode() + fromKeyValueToRewriter.hashCode();
	}
	
	public static <T> Switch<T> mergeSwitchesWithTheSameKeyMakerIntoASingleOne(List<Rewriter> switches) {
		
		checkMergingConditions(switches);
		
		Function<Expression, T> keyMaker = getSharedKeyMaker(switches);
		
		Map<T, Rewriter> fromKeyValueToRewriter = makeMapFromKeyValueToSingleRewriter(switches);
		
		Switch<T> result = new Switch<T>(keyMaker, fromKeyValueToRewriter);
		
		return result;
	}

	private static <T> Map<T, Rewriter> makeMapFromKeyValueToSingleRewriter(List<Rewriter> switches) {
		
		Map<T, LinkedList<Rewriter>> fromKeyValueToRewritersUnderSameKeyValue = groupRewritersUnderSwitchesByKeyValue(switches);

		Map<T, List<Rewriter>> fromKeyValueToFlattenedRewritersUnderSameKeyValue = flattenEachKeyValueGroup(fromKeyValueToRewritersUnderSameKeyValue);

		Map<T, Rewriter> fromKeyValueToRewriter = combineEachFlattenedKeyValueGroupIntoASingleRewriter(fromKeyValueToFlattenedRewritersUnderSameKeyValue);
		
		return fromKeyValueToRewriter;
	}

	@SuppressWarnings("unchecked")
	private static <T> void checkMergingConditions(List<Rewriter> switches) throws Error {
		
		myAssert(switches.size() != 0, () -> "Only a non-empty set of switch rewriters can be merged.");
		
		Function<Expression, T> keyMaker = getSharedKeyMaker(switches);
		
		boolean allSwitchesHaveTheSameKeyMaker = forAll(switches, s -> ((Switch<T>)s).keyMaker == keyMaker);
		
		myAssert(allSwitchesHaveTheSameKeyMaker, () ->
                 "Set of switches to be merged must all have the same instance of key maker, but " + join(switches) + " does not"); 
	}

	@SuppressWarnings("unchecked")
	private static <T> Function<Expression, T> getSharedKeyMaker(List<Rewriter> switches) throws Error {
		Switch<T> first = (Switch<T>) getFirst(switches);
		Function<Expression, T> keyMaker = first.keyMaker;
		return keyMaker;
	}

	@SuppressWarnings("unchecked")
	private static <T> Map<T, LinkedList<Rewriter>> groupRewritersUnderSwitchesByKeyValue(List<Rewriter> switches) {
		Map<T, LinkedList<Rewriter>> fromKeyValueToRewritersUnderSameKeyValue = 
				combineMapsIntoNewMapGroupingValuesUnderTheSameKey(
						functionIterator(switches, s -> ((Switch<T>)s).fromKeyValueToRewriter));
		return fromKeyValueToRewritersUnderSameKeyValue;
	}

	private static <T> Map<T, List<Rewriter>> flattenEachKeyValueGroup(Map<T, LinkedList<Rewriter>> fromKeyValueToRewritersUnderSameKeyValue) {
		Function<List<Rewriter>, List<Rewriter>> makeFlatListOfSwitches = l -> flattenListOfRewritersWithRespectToFirstOfToANewList(l);
		Map<T, List<Rewriter>> fromKeyValueToFlattenedRewritersUnderSameKeyValue = 
				applyFunctionToValuesOfMap(fromKeyValueToRewritersUnderSameKeyValue, makeFlatListOfSwitches);
		return fromKeyValueToFlattenedRewritersUnderSameKeyValue;
	}

	private static <T> Map<T, Rewriter> combineEachFlattenedKeyValueGroupIntoASingleRewriter(Map<T, List<Rewriter>> fromKeyValueToFlattenedRewritersUnderSameKeyValue) {
		Map<T, Rewriter> fromKeyToRewriter = applyFunctionToValuesOfMap(fromKeyValueToFlattenedRewritersUnderSameKeyValue, makeFirstOfRewriterFromListOfRewriters);
		return fromKeyToRewriter;
	}

	private static final Function<List<Rewriter>, Rewriter> makeFirstOfRewriterFromListOfRewriters = list -> list.size() == 1? list.get(0) : new FirstOf(list);
}