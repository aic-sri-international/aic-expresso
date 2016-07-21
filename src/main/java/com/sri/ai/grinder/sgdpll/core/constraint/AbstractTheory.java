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
package com.sri.ai.grinder.sgdpll.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.camelCaseToSpacedString;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpll.api.Theory;
import com.sri.ai.grinder.sgdpll.simplifier.api.MapBasedSimplifier;
import com.sri.ai.grinder.sgdpll.simplifier.api.MapBasedTopSimplifier;
import com.sri.ai.grinder.sgdpll.simplifier.api.Simplifier;
import com.sri.ai.grinder.sgdpll.simplifier.core.SeriallyMergedMapBasedTopSimplifier;

@Beta
/** 
 * Basic implementation of some methods of {@link Theory}.
 */
abstract public class AbstractTheory implements Theory {

	protected Simplifier simplifier;
	protected MapBasedTopSimplifier topSimplifier;
	
	/**
	 * Initializes types for testing to be the collection of a single type,
	 * a {@link Categorical} {@link Type} named <code>SomeType</code>
	 * with domain size 5 and known constants <code>a, b, c, d</code>,
	 * variables for testing to <code>X, Y, Z</code> of type <code>SomeType</code>,
	 * of which <code>X</code> is the main testing variable on which testing literals are generated.
	 * @param simplifier a source of elementary simplifiers for this theory
	 */
	public AbstractTheory(MapBasedSimplifier simplifier) {
		super();
		Categorical someType = getDefaultTestingType();
		setVariableNamesAndTypesForTesting(map("X", someType, "Y", someType, "Z", someType));
		setSimplifierFromElementarySimplifiersIn(simplifier);
	}

	/**
	 * Sets the theory's simplifier based on elementary simplifiers
	 * present in given {@link MapBasedSimplifier}.
	 * @param simplifier
	 */
	protected void setSimplifierFromElementarySimplifiersIn(MapBasedSimplifier simplifier) {
		this.simplifier = simplifier;
//		this.cachedTotalSimplifier = new RecursiveExhaustiveMapBasedSimplifier(topSimplifier);
		this.topSimplifier = new SeriallyMergedMapBasedTopSimplifier(simplifier);
	}

	private static Categorical someType;
	
	/**
	 * Returns the type used for the default testing variables.
	 * @return
	 */
	static public Categorical getDefaultTestingType() {
		if (someType == null) {
			ArrayList<Expression> knownConstants = mapIntoArrayList(list("a", "b", "c", "d"), s -> makeSymbol(s));
			someType = new Categorical("SomeType", 5, knownConstants);
		}
		return someType;
	}
	
	private Map<String, Type> variableNamesAndTypesForTesting;
	private Collection<Type>  cachedTypesForTesting;
	private ArrayList<String> cachedVariableNamesForTesting;
	
	@Override
	public Expression simplify(Expression expression, Context context) {
		Expression result = simplifier.apply(expression, context);
		return result;
	}

	@Override
	public Collection<Type> getNativeTypes() {
		return list();
	}
	
	@Override
	public MapBasedTopSimplifier getTopSimplifier() {
		return topSimplifier;
	}
	
	@Override
	public Collection<Type> getTypesForTesting() {
		if (cachedTypesForTesting == null) {
			cachedTypesForTesting = new LinkedHashSet<Type>(variableNamesAndTypesForTesting.values());
			cachedTypesForTesting.addAll(getNativeTypes());
		}
		return Collections.unmodifiableCollection(cachedTypesForTesting);
	}

	@Override
	public void setVariableNamesAndTypesForTesting(Map<String, Type> variableNamesAndTypesForTesting) {
		this.variableNamesAndTypesForTesting = variableNamesAndTypesForTesting;
		this.cachedTypesForTesting = null; // force recomputation if needed.
		this.cachedVariableNamesForTesting = null; // force recomputation if needed.
	}
	
	@Override
	public Map<String, Type> getVariableNamesAndTypesForTesting() {
		return Collections.unmodifiableMap(variableNamesAndTypesForTesting);
	}

	@Override
	public List<String> getVariableNamesForTesting() {
		if (cachedVariableNamesForTesting == null) {
			if (variableNamesAndTypesForTesting == null) {
				cachedVariableNamesForTesting = null;
			}
			else {
				cachedVariableNamesForTesting = new ArrayList<String>(variableNamesAndTypesForTesting.keySet());
			}
		}
		return Collections.unmodifiableList(cachedVariableNamesForTesting);
	}
	
	@Override
	public Context extendWithTestingInformation(Context context) {
		// we only need to provide the variables types, and not the known constant types, because the latter will be extracted from the already registered types.
		Map<String, String> mapFromSymbolNamesToTypeNames = new LinkedHashMap<String, String>();
		for (Map.Entry<String, Type> symbolAndType : getVariableNamesAndTypesForTesting().entrySet()) {
			mapFromSymbolNamesToTypeNames.put(symbolAndType.getKey(), symbolAndType.getValue().toString());
		}
		
		Context result = GrinderUtil.extendProcessWith(mapFromSymbolNamesToTypeNames, getTypesForTesting(), context);
		return result;
	}
	
	@Override
	public
	String toString() {
		return camelCaseToSpacedString(getClass().getSimpleName());
	}
}