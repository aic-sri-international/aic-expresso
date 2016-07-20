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

import static com.sri.ai.util.Util.list;
import static java.util.Arrays.asList;

import java.util.Iterator;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.grinder.sgdpll.simplifier.api.MapBasedSimplifier;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.collect.NestedIterator;

/**
 * A collection of utility methods for merging maps of elementary simplifiers.
 * 
 * @author braz
 *
 */
@Beta
public class Merge  {

	/**
	 * Iterates over maps of function application simplifiers, <b>going over the vararg list of {@link MapBasedSimplifier}s first</b>
	 * to create an effect of overriding.
	 * @param functionApplicationSimplifiers
	 * @param simplifiers
	 * @return
	 */
	public static
	Iterator<Map<String, Simplifier>>
	mapsOfFunctionApplicationSimplifiersIterator(Map<String, Simplifier> functionApplicationSimplifiers, MapBasedSimplifier... simplifiers) {
		return new NestedIterator<>(mapsOfFunctionApplicationSimplifiersIterator(simplifiers), list(functionApplicationSimplifiers));
	}

	/**
	 * Iterates over syntactic form type simplifiers, <b>going over the vararg list of {@link MapBasedSimplifier}s first</b>
	 * to create an effect of overriding.
	 * @param syntacticFormTypeSimplifiers
	 * @param simplifiers
	 * @return
	 */
	public static
	Iterator<Map<String, Simplifier>>
	mapsOfSyntacticFormTypeSimplifiersIterator(Map<String, Simplifier> syntacticFormTypeSimplifiers, MapBasedSimplifier... simplifiers) {
		return new NestedIterator<>(mapsOfSyntacticFormTypeSimplifiersIterator(simplifiers), list(syntacticFormTypeSimplifiers));
	}

	public static
	Iterator<Map<String, Simplifier>>
	mapsOfFunctionApplicationSimplifiersIterator(MapBasedSimplifier... simplifiers) {
		return FunctionIterator.make(asList(simplifiers), fromSimplifierToMapOfFunctionApplicationSimplifiers());
	}

	public static
	Iterator<Map<String, Simplifier>>
	mapsOfSyntacticFormTypeSimplifiersIterator(MapBasedSimplifier... simplifiers) {
		return FunctionIterator.make(asList(simplifiers), fromSimplifierToMapOfSyntacticFormTypeSimplifiers());
	}

	public static
	Function<MapBasedSimplifier, Map<String, Simplifier>>
	fromSimplifierToMapOfFunctionApplicationSimplifiers() {
		return s -> s.getFunctionApplicationSimplifiers();
	}

	private static
	Function<MapBasedSimplifier, Map<String, Simplifier>>
	fromSimplifierToMapOfSyntacticFormTypeSimplifiers() {
		return s -> s.getSyntacticFormTypeSimplifiers();
	}
	
}