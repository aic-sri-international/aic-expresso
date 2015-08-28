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
package com.sri.ai.grinder.core;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.grinder.api.MapBasedSimplifier;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A basic {@link MapBasedSimplifier} receiving its elementary simplifiers from other {@link MapBasedSimplifier}s.
 * 
 * @author braz
 *
 */
@Beta
public class MergingMapBasedSimplifier extends DefaultMapBasedSimplifier {
	
	public MergingMapBasedSimplifier(MapBasedSimplifier... simplifiers) {
		super(Util.union(functionApplicationSimplifiersIterator(simplifiers)), Util.union(syntacticFormTypeSimplifiers(simplifiers)));
				
	}

	private static
	Iterator<Map<String, Simplifier>>
	functionApplicationSimplifiersIterator(MapBasedSimplifier... simplifiers) {
		return FunctionIterator.make(simplifiersList(simplifiers), fromSimplifierToFunctionApplicationSimplifiers());
	}

	private static
	Iterator<Map<String, Simplifier>>
	syntacticFormTypeSimplifiers(MapBasedSimplifier... simplifiers) {
		return FunctionIterator.make(simplifiersList(simplifiers), fromSimplifierToSyntacticFormTypeSimplifiers());
	}

	private static
	Function<MapBasedSimplifier, Map<String, Simplifier>>
	fromSimplifierToFunctionApplicationSimplifiers() {
		return s -> s.getFunctionApplicationSimplifiers();
	}

	private static
	Function<MapBasedSimplifier, Map<String, Simplifier>>
	fromSimplifierToSyntacticFormTypeSimplifiers() {
		return s -> s.getSyntacticFormTypeSimplifiers();
	}

	private static List<MapBasedSimplifier> simplifiersList(MapBasedSimplifier... simplifiers) {
		return Arrays.asList(simplifiers);
	}
}