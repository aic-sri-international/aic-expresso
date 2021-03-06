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
package com.sri.ai.grinder.rewriter.api;

import static com.sri.ai.grinder.rewriter.core.CombiningTopRewriter.combineAlreadyMergedSwitches;
import static com.sri.ai.grinder.rewriter.core.Switch.mergeSwitchesWithTheSameKeyMakerIntoASingleOne;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.rewriter.core.CombiningTopRewriter;
import com.sri.ai.grinder.rewriter.core.FirstOf;
import com.sri.ai.grinder.rewriter.core.Switch;
import com.sri.ai.util.Util;


/**
 * An indicator interface for rewriters that are either a {@link Switch} rewriter,
 * or a {@link CombiningTopRewriter} (an extension of {@link FirstOf}) rewriter with base top rewriters,
 * or a basic rewriter that typically will not recurse.
 * 
 * The name "top rewriter" comes from the fact that these only apply once to the "top expression" of an expression;
 * that is, they do no recurse, and do not apply multiple times, automatically
 * (although their inner user-defined code may do that).
 * They are meant as "atomic" rewriters that may be provided to recursive and exhaustive rewriters.
 * 
 * @author braz
 *
 */
@FunctionalInterface
public interface TopRewriter extends Rewriter {

	/**
	 * Varargs version of {@link #makeTopRewriterFromTopRewritersThatAreEitherFirstOfOrSwitches(List)}.
	 * @param topRewriters
	 * @param <T> the type of keys in the switch rewriters.
	 * @return
	 */
	public static <T> TopRewriter merge(TopRewriter... topRewriters) {
		return makeTopRewriterFromTopRewritersThatAreEitherFirstOfOrSwitches(Arrays.asList(topRewriters));
	}

	/**
	 * Takes a list of {@link TopRewriter}s,
	 * then determines a list of {@link Switch<T>} rewriters,
	 * each of them the merge of all {@link Switch<T>} rewriters with the same key maker
	 * (from {@link Switch.merge(List<Rewriter>)}.
	 * If this list contains more than one {@link Switch<T>} rewriter,
	 * returns a {@link CombiningTopRewriter} rewriter with them as base rewriters.
	 * If this list contains only one {@link Switch<T>} rewriter, returns it.
	 * @param topRewritersThatAreEitherFirstOfOrSwitches
	 * @param <T> the type of keys in the switch rewriters.
	 * @return
	 */
	public static <T> TopRewriter makeTopRewriterFromTopRewritersThatAreEitherFirstOfOrSwitches(List<? extends TopRewriter> topRewritersThatAreEitherFirstOfOrSwitches) {
		
		List<Switch<T>> mergedSwitches = makeMergedSwitchesFromTopRewritersThatAreEitherFirstOfOrSwitches(topRewritersThatAreEitherFirstOfOrSwitches);
		
		TopRewriter result = makeTopRewriterFromAlreadyMergedSwitches(mergedSwitches);
		
		return result;
	}

	public static <T> TopRewriter makeTopRewriterFromAlreadyMergedSwitches(List<Switch<T>> mergedSwitches) {
		TopRewriter result;
		if (mergedSwitches.size() == 1) {
			result = mergedSwitches.get(0);
		}
		else {
			result = combineAlreadyMergedSwitches("Various simplifiers", mergedSwitches);
		}
		return result;
	}

	public static <T> List<Switch<T>> makeMergedSwitchesFromTopRewritersThatAreEitherFirstOfOrSwitches(
			TopRewriter... topRewritersThatAreEitherFirstOfOrSwitches) throws Error {
		
		return makeMergedSwitchesFromTopRewritersThatAreEitherFirstOfOrSwitches(Arrays.asList(topRewritersThatAreEitherFirstOfOrSwitches));
	}

	public static <T> List<Switch<T>> makeMergedSwitchesFromTopRewritersThatAreEitherFirstOfOrSwitches(
			List<? extends TopRewriter> topRewritersThatAreEitherFirstOfOrSwitches) throws Error {
		
		List<Rewriter> switchRewriters = FirstOf.flattenListOfRewritersWithRespectToFirstOfToANewList(topRewritersThatAreEitherFirstOfOrSwitches);
	
		Map<Function<Expression, T>, List<Rewriter>> fromKeyMakerToSwitches = gatherListsOfSwitchesSharingKeyMaker(switchRewriters);
	
		List<Switch<T>> mergedSwitchRewriters = list();
		for (Map.Entry<Function<Expression, T>, List<Rewriter>> entry : fromKeyMakerToSwitches.entrySet()) {
			List<Rewriter> listOfSwitchRewritersWithTheSameKeyMaker = entry.getValue();
			Switch<T> mergedSwitchRewriterForListAtThisEntry = mergeSwitchesWithTheSameKeyMakerIntoASingleOne(listOfSwitchRewritersWithTheSameKeyMaker);
			mergedSwitchRewriters.add(mergedSwitchRewriterForListAtThisEntry);
			// we don't need to use entry.getKey() because that's the keyMaker, which is also present in the values ({@link Switch} rewriters).
			// The entry.getKey()s are only needed to make sure the rewriters being merged share the same keyMaker.
		}
		return mergedSwitchRewriters;
	}

	public static <T> Map<Function<Expression, T>, List<Rewriter>> gatherListsOfSwitchesSharingKeyMaker(List<Rewriter> switches) throws Error {
		Map<Function<Expression, T>, List<Rewriter>> fromKeyMakerToSwitches = map();
		
		for (Rewriter switchAsRewriter : switches) {
			@SuppressWarnings("unchecked")
			Switch<T> switchObject = (Switch<T>) switchAsRewriter;
			Util.addToCollectionValuePossiblyCreatingIt(
					fromKeyMakerToSwitches,
					switchObject.getKeyMaker(),
					switchObject,
					LinkedList.class);
		}
		return fromKeyMakerToSwitches;
	}
	
}