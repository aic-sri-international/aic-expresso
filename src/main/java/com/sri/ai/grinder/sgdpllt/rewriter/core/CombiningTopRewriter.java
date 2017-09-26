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

import static com.sri.ai.util.Util.join;

import java.util.List;

import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;


/**
 * A {@link TopRewriter} extension of {@link FirstOf}
 * that consolidates a list of {@link TopRewriter}s
 * by merging {@link Switch} rewriters based on the same key maker,
 * and flattening {@link TopRewriters}.
 * 
 * @author braz
 *
 */
public class CombiningTopRewriter extends FirstOf implements TopRewriter {
	
	/**
	 * Creates a {@link FirstOf} rewriter containing {@link Switch} rewriters
	 * merging all {@link Switch} rewriters embedded in input top rewriters.
	 * @param topRewriters
	 */
	public CombiningTopRewriter(String name, TopRewriter... topRewriters) {
		super(name, TopRewriter.makeMergedSwitchesFromTopRewritersThatAreEitherFirstOfOrSwitches(topRewriters));
	}
	
	public CombiningTopRewriter(String name, List<? extends TopRewriter> topRewriters) {
		super(name, TopRewriter.makeMergedSwitchesFromTopRewritersThatAreEitherFirstOfOrSwitches(topRewriters));
	}
	
	/* constructor that is told not to merge switches; boolean parameter must always be true and serves differentiation only;
	 * hence the constructor being private and available through static method following it.
	 */
	private <T> CombiningTopRewriter(String name, List<? extends Switch<T>> alreadyMergedSwitches, boolean doNotMerge) {
		super(name, alreadyMergedSwitches);
		if ( ! doNotMerge) {
			throw new Error("Use other " + CombiningTopRewriter.class + " if you do want to merge top rewriters.");
		}
	}
	
	public static <T> CombiningTopRewriter combineAlreadyMergedSwitches(String name, List<? extends Switch<T>> alreadyMergedSwitches) {
		return new CombiningTopRewriter(name, alreadyMergedSwitches, true);
	}

	public CombiningTopRewriter(TopRewriter... topRewriters) {
		this("CombiningTopRewriter on " + join(topRewriters), topRewriters);
	}
	
	public CombiningTopRewriter(List<? extends TopRewriter> topRewriters) {
		this("CombiningTopRewriter on " + join(topRewriters), topRewriters);
	}
	
	/* constructor that is told not to merge switches; boolean parameter must always be true and serves differentiation only;
	 * hence the constructor being private and available through static method following it.
	 */
	private <T> CombiningTopRewriter(List<? extends Switch<T>> alreadyMergedTopRewriters, boolean doNotMerge) {
		this("CombiningTopRewriter on " + join(alreadyMergedTopRewriters), alreadyMergedTopRewriters, doNotMerge);
	}

	public static <T> CombiningTopRewriter combineAlreadyMergedSwitches(List<? extends Switch<T>> alreadyMergedSwitches) {
		return new CombiningTopRewriter(alreadyMergedSwitches, true);
	}
}