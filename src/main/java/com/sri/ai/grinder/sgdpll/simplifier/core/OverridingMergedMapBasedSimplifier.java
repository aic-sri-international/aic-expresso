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

import static com.sri.ai.util.Util.union;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;

/**
 * A basic {@link DefaultMapBasedTopSimplifier} receiving its elementary simplifiers from other {@link MapBasedSimplifier}s,
 * with an overriding collision policy, that is, simplifiers for a function or syntactic form override
 * those already present for the same element. 
 * 
 * @see SeriallyMergedMapBasedTopSimplifier
 * 
 * @author braz
 *
 */
@Beta
public class OverridingMergedMapBasedSimplifier extends DefaultMapBasedTopSimplifier {
	
	/**
	 * Creates a simplifiers from the function and syntactic form simplifiers of given simplifiers,
	 * with the additional ones overriding the ones in the {@link MapBasedSimplifier}s.
	 * @param additionalFunctionApplicationSimplifiers
	 * @param additionalSyntacticFormTypeSimplifiers
	 * @param simplifiers
	 */
	public OverridingMergedMapBasedSimplifier(MapBasedSimplifier... simplifiers) {
		super(
				union ( Merge.functionApplicationSimplifiersIterator(simplifiers) ),
				union ( Merge.syntacticFormTypeSimplifiersIterator(simplifiers) ));
				
	}

	/**
	 * Adds function and syntactic form simplifiers to those of given simplifiers,
	 * with the additional ones overriding the ones in the {@link MapBasedSimplifier}s.
	 * to create an effect of overriding.
	 * @param additionalFunctionApplicationSimplifiers
	 * @param additionalSyntacticFormTypeSimplifiers
	 * @param simplifiers
	 */
	public OverridingMergedMapBasedSimplifier(
			Map<String, Simplifier> additionalFunctionApplicationSimplifiers,
			Map<String, Simplifier> additionalSyntacticFormTypeSimplifiers,
			MapBasedSimplifier... simplifiers) {
		super(
				union ( Merge.functionApplicationSimplifiersIterator(additionalFunctionApplicationSimplifiers, simplifiers) ),
				union ( Merge.syntacticFormTypeSimplifiersIterator(additionalSyntacticFormTypeSimplifiers, simplifiers) ));
				
	}
}