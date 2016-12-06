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

import static com.sri.ai.util.Util.getFirst;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.util.Util;


/**
 * A utility class for merging {@link Switch} rewriters, possibly in {@link FirstOf} rewriters.
 * 
 * @author braz
 *
 */
public class FirstOfSwitchMerge {
	
	/**
	 * Varargs version of {@link #merge(List)}.
	 * @param rewriters
	 * @param <T> the type of keys in the switch rewriters.
	 * @return
	 */
	public static <T> Rewriter merge(Rewriter... rewriters) {
		return merge(Arrays.asList(rewriters));
	}
	
	/**
	 * Takes a list of rewriters, each of which can be either a {@link Switch<T>>} rewriter,
	 * or a {@link FirstOf} rewriter with {@link Switch<T>>} base rewriters,
	 * It then determines a list of {@link Switch<T>} rewriters,
	 * each of them the merge of all {@link Switch<T>} rewriters with the same key maker
	 * (from {@link Switch.merge(List<Rewriter>)}.
	 * If this list contains more than one {@link Switch<T>} rewriter,
	 * returns a {@link FirstOf} rewriter with them as base rewriters.
	 * If this list contains only one {@link Switch<T>} rewriter, returns it.
	 * @param rewriters
	 * @param <T> the type of keys in the switch rewriters.
	 * @return
	 */
	public static <T> Rewriter merge(List<Rewriter> rewriters) {
		
		List<Rewriter> flattenedRewriters = FirstOf.flatten(rewriters);

		Map<Function<Expression, T>, List<Switch<T>>> switchRewritersSeparatedByKeyMaker = map();
		for (Rewriter switchRewriterAsRewriter : flattenedRewriters) {
			if ( ! (switchRewriterAsRewriter instanceof Switch)) {
				throw new Error(FirstOfSwitchMerge.class + " merge must be applied only to lists of rewriters that, once flattened with regard to " + FirstOf.class + ", are composed only of " + Switch.class + " rewriters. This is requires because the final product must be a " + Switch.class + " rewriter, and it needs each basic rewriter to be associated with a key value, and only other " + Switch.class + "rewriters provide those.");
			}
			else {
				@SuppressWarnings("unchecked")
				Switch<T> switchRewriter = (Switch<T>) switchRewriterAsRewriter;
				Util.addToCollectionValuePossiblyCreatingIt(
						switchRewritersSeparatedByKeyMaker,
						switchRewriter.getKeyMaker(),
						switchRewriter,
						LinkedList.class);
			}
		}

		List<Rewriter> mergedRewriters = list();
		for (Map.Entry<Function<Expression, T>, List<Switch<T>>> entry : switchRewritersSeparatedByKeyMaker.entrySet()) {
			Rewriter mergedForEntry = Switch.merge(entry.getValue());
			mergedRewriters.add(mergedForEntry);
			// we don't need to use entry.getKey() because that's the keyMaker, which is also present in the values ({@link Switch} rewriters).
			// The entry.getKey()s are only needed to make sure the rewriters being merged share the same keyMaker.
		}
		
		Rewriter result;
		if (mergedRewriters.size() == 1) {
			result = getFirst(mergedRewriters);
		}
		else {
			result = new FirstOf(mergedRewriters);
		}
		
		return result;
	}

}