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
package com.sri.ai.grinder.sgdpll.simplifier.core;

import static com.sri.ai.util.Util.addToCollectionValuePossiblyCreatingIt;
import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.map;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpll.simplifier.api.MapBasedSimplifier;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;

/**
 * A {@link MapBasedSimplifier} receiving elementary simplifiers on their own or from other {@link MapBasedSimplifier}s,
 * with a serialization collision policy, that is, if a new simplifier <code>s2</code> applies to the
 * same function application or syntactic form as a pre-existing simplifier <code>s1</code>,
 * then a new simplifier <code>s3(e) = if s1(e) != e then s1(e) else s2(e)</code> is created.
 * That is to say, the new simplifier only acts if the old one has nothing to say about it,
 * thus complementing it.
 * Therefore, this class is useful for adding functionality to a simplifier,
 * as opposed to overriding functionality (as done by {@link OverridingMergedMapBasedTopSimplifier}). 
 * 
 * @see OverridingMergedMapBasedTopSimplifier
 * 
 * @author braz
 *
 */
@Beta
public class SeriallyMergedMapBasedTopSimplifier extends DefaultMapBasedTopSimplifier {
	
	/**
	 * Creates a simplifier from the function and syntactic form simplifiers of given simplifiers,
	 * with the later ones getting serially merged to the earlier ones.
	 * @param simplifiers
	 */
	public SeriallyMergedMapBasedTopSimplifier(MapBasedSimplifier... simplifiers) {
		super(
				merge ( Merge.mapsOfFunctionApplicationSimplifiersIterator(simplifiers) ),
				merge ( Merge.mapsOfSyntacticFormTypeSimplifiersIterator(simplifiers) ));
	}

	/**
	 * Adds function and syntactic form simplifiers to those of given simplifiers,
	 * with the additional ones getting serially merged with the ones in the {@link MapBasedSimplifier}s.
	 * @param additionalFunctionApplicationSimplifiers
	 * @param additionalSyntacticFormTypeSimplifiers
	 * @param simplifiers
	 */
	public SeriallyMergedMapBasedTopSimplifier(
			Map<String, Simplifier> additionalFunctionApplicationSimplifiers,
			Map<String, Simplifier> additionalSyntacticFormTypeSimplifiers,
			MapBasedSimplifier... simplifiers) {
		super(
				merge ( Merge.mapsOfFunctionApplicationSimplifiersIterator(additionalFunctionApplicationSimplifiers, simplifiers) ),
				merge ( Merge.mapsOfSyntacticFormTypeSimplifiersIterator(additionalSyntacticFormTypeSimplifiers, simplifiers) ));
	}
	
	private static Map<String, Simplifier> merge(Iterator<Map<String, Simplifier>> simplifiersIterator) {
		/** The consolidated map, mapping each key (functor string or syntactic form name) to the simplifier resulting from merging all simplifiers mapped to the same key. */
		LinkedHashMap<String, Simplifier> result = map();
		
		/** Record of all simplifiers merged so far for each key, to avoid duplication. */
		LinkedHashMap<String, Collection<Simplifier>> simplifiersAlreadyPresentForGivenKey = map();

		// OPTIMIZATION: in principle, we could extend the Simplifier interface with a 'merge' method,
		// and each merged simplifier would already know of its internal simplifiers and check for redundancies itself.
		// This would more efficient because we would not have to iterate over all basic simplifiers every time,
		// but only over the top-level ones (the merged ones).
		// However, this would complicate the interface, so I am not sure it is worth it,
		// especially considering that simplifiers are typically merged before all the heavy processing starts, and typically only once.
		
		for (Map<String, Simplifier> simplifiersMap : in(simplifiersIterator)) {
			for (Map.Entry<String, Simplifier> entry : simplifiersMap.entrySet()) {
				String key = entry.getKey();
				Simplifier moreRecentSimplifier = entry.getValue();
				
				if (simplifierNotYetMergedForThisKey(key, moreRecentSimplifier, simplifiersAlreadyPresentForGivenKey)) {

					Simplifier previousSimplifier = result.get(key);

					Simplifier finalSimplifier;
					if (previousSimplifier != null) {
						finalSimplifier = serialize(moreRecentSimplifier, previousSimplifier);
					}
					else {
						finalSimplifier = moreRecentSimplifier;
					}

					registerSimplifierAsAlreadyMergedForThisKey(key, moreRecentSimplifier, simplifiersAlreadyPresentForGivenKey);

					result.put(key, finalSimplifier);
				}
			}
		}
		return result;
	}

	private static boolean simplifierNotYetMergedForThisKey(String key, Simplifier simplifier, LinkedHashMap<String, Collection<Simplifier>> simplifiersAlreadyPresentForGivenKey) {
		boolean result = 
				! simplifiersAlreadyPresentForGivenKey.containsKey(key)
				|| 
				! simplifiersAlreadyPresentForGivenKey.get(key).contains(simplifier);
		return result;
	}

	private static void registerSimplifierAsAlreadyMergedForThisKey(String key, Simplifier simplifier, LinkedHashMap<String, Collection<Simplifier>> simplifiersAlreadyPresentForGivenKey) {
		addToCollectionValuePossiblyCreatingIt(simplifiersAlreadyPresentForGivenKey, key, simplifier, LinkedHashSet.class);
	}

	private static Simplifier serialize(Simplifier moreRecentSimplifier, Simplifier previousSimplifier) {
		return (e, p) -> {
			Expression result = previousSimplifier.apply(e, p);
			if (result == e) {
				result = moreRecentSimplifier.apply(e, p);
			}
			return result;
		};
	}
}