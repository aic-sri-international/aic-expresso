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
package com.sri.ai.grinder.core.simplifier;

import static com.sri.ai.util.Util.in;
import static com.sri.ai.util.Util.map;

import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.api.Simplifier;

/**
 * A {@link MapBasedSimplifier} receiving elementary simplifiers on their own or from other {@link MapBasedSimplifier}s,
 * with a serialization collision policy, that is, if a new simplifier <code>s2</code> applies to the
 * same function application or syntactic form as a pre-existing simplifier <code>s1</code>,
 * then a new simplifier <code>s3(e) = if s1(e) != e then s1(e) else s2(e)</code> is created.
 * 
 * @see OverridingMergedMapBasedSimplifier
 * 
 * @author braz
 *
 */
@Beta
public class SeriallyMergedMapBasedSimplifier extends DefaultMapBasedSimplifier {
	
	/**
	 * Creates a simplifiers from the function and syntactic form simplifiers of given simplifiers,
	 * with the additional ones overriding the ones in the {@link MapBasedSimplifier}s.
	 * @param additionalFunctionApplicationSimplifiers
	 * @param additionalSyntacticFormTypeSimplifiers
	 * @param simplifiers
	 */
	public SeriallyMergedMapBasedSimplifier(MapBasedSimplifier... simplifiers) {
		super(
				merge ( Merge.functionApplicationSimplifiersIterator(simplifiers) ),
				merge ( Merge.syntacticFormTypeSimplifiersIterator(simplifiers) ));
				
	}

	/**
	 * Adds function and syntactic form simplifiers to those of given simplifiers,
	 * with the additional ones overriding the ones in the {@link MapBasedSimplifier}s.
	 * to create an effect of overriding.
	 * @param additionalFunctionApplicationSimplifiers
	 * @param additionalSyntacticFormTypeSimplifiers
	 * @param simplifiers
	 */
	public SeriallyMergedMapBasedSimplifier(
			Map<String, Simplifier> additionalFunctionApplicationSimplifiers,
			Map<String, Simplifier> additionalSyntacticFormTypeSimplifiers,
			MapBasedSimplifier... simplifiers) {
		super(
				merge ( Merge.functionApplicationSimplifiersIterator(additionalFunctionApplicationSimplifiers, simplifiers) ),
				merge ( Merge.syntacticFormTypeSimplifiersIterator(additionalSyntacticFormTypeSimplifiers, simplifiers) ));
				
	}
	
	private static Map<String, Simplifier> merge(Iterator<Map<String, Simplifier>> simplifiersIterator) {
		LinkedHashMap<String, Simplifier> result = map();
		for (Map<String, Simplifier> simplifiersMap : in(simplifiersIterator)) {
			for (Map.Entry<String, Simplifier> entry : simplifiersMap.entrySet()) {
				String key = entry.getKey();
				Simplifier moreRecentSimplifier = entry.getValue();
				Simplifier previousSimplifier = result.get(key);
				
				Simplifier finalSimplifier;
				if (previousSimplifier != null) {
					finalSimplifier = serialize(moreRecentSimplifier, previousSimplifier);
				}
				else {
					finalSimplifier = moreRecentSimplifier;
				}
				
				result.put(key, finalSimplifier);
			}
		}
		return result;
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