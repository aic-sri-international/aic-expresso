/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.grinder.sgdpllt.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;

@Beta
abstract public class AbstractTheoryTestingSupport implements TheoryTestingSupport {
	private static Categorical _someType;
	//
	private Theory theory;
	//
	private Random random;
	//
	private Map<String, Type> variableNamesAndTypesForTesting;
	//
	private List<String> cachedVariableNamesForTesting;
	private Collection<Type> cachedTypesForTesting;
	
	public AbstractTheoryTestingSupport(Theory theory, Random random) {
		this.theory = theory;
		this.random = random;
	}
	
	@Override
	public Theory getTheory() {
		return theory;
	}
	
	@Override
	public Random getRandom() {
		return random;
	}
	
	public void setRandom(Random random) {
		this.random = random;
	}
	
	@Override
	public void setVariableNamesAndTypesForTesting(Map<String, Type> variableNamesAndTypesForTesting) {
		this.variableNamesAndTypesForTesting = Collections.unmodifiableMap(variableNamesAndTypesForTesting);
	     this.cachedTypesForTesting = null; // force re-computation if needed.
	     this.cachedVariableNamesForTesting = null; // force re-computation if needed.
	}

	@Override
	public Map<String, Type> getVariableNamesAndTypesForTesting() {
		return variableNamesAndTypesForTesting;
	}
	
	@Override
	public List<String> getVariableNamesForTesting() {
		if (cachedVariableNamesForTesting == null) {
			cachedVariableNamesForTesting = Collections.unmodifiableList(new ArrayList<String>(getVariableNamesAndTypesForTesting().keySet()));
		}
		return cachedVariableNamesForTesting;
	}
	
	@Override
	public Collection<Type> getTypesForTesting() {
		if (cachedTypesForTesting == null) {
			Set<Type> topTypesForTesting = new LinkedHashSet<>(getVariableNamesAndTypesForTesting().values());
			topTypesForTesting.addAll(getTheory().getNativeTypes());
			// Ensure we include embedded types (e.g. in a function or tuple type) as well
			Set<Type> embeddedTypes = new LinkedHashSet<>();
			for (Type topType : topTypesForTesting) {
				embeddedTypes.addAll(topType.getEmbeddedTypes());
			}
			topTypesForTesting.addAll(embeddedTypes);
			cachedTypesForTesting = Collections.unmodifiableCollection(topTypesForTesting);
		}
		return cachedTypesForTesting;
	}
	
	@Override
	public Context extendWithTestingInformation(Context context) {
		// we only need to provide the variables types, and not the known constant types, because the latter will be extracted from the already registered types.
		Map<String, String> mapFromSymbolNamesToTypeNames = new LinkedHashMap<String, String>();
		for (Map.Entry<String, Type> symbolAndType : getVariableNamesAndTypesForTesting().entrySet()) {
			mapFromSymbolNamesToTypeNames.put(symbolAndType.getKey(), symbolAndType.getValue().toString());
		}
		for (Map.Entry<String, Type> symbolAndType : getExtendedVariableNamesAndTypesForTesting().entrySet()) {
			mapFromSymbolNamesToTypeNames.put(symbolAndType.getKey(), symbolAndType.getValue().toString());
		}
		
		Context result = (Context) GrinderUtil.extendRegistryWith(mapFromSymbolNamesToTypeNames, getTypesForTesting(), context);
		return result;
	}
	
	/**
	 * Override this method if additional variable names and types should be included.
	 * 
	 * @return an extended set of variable names and types for testing.
	 */
	public Map<String, Type> getExtendedVariableNamesAndTypesForTesting() {
		return Collections.emptyMap();
	}
	
	/**
	 * Returns the type used for the default testing variables.
	 * @return
	 */
	static public Categorical getDefaultTestingType() {
		if (_someType == null) {
			ArrayList<Expression> knownConstants = mapIntoArrayList(list("a", "b", "c", "d"), s -> makeSymbol(s));
			_someType = new Categorical("SomeType", 5, knownConstants);
		}
		return _someType;
	}
	
	@Override
	public String toString() {
		return theory.toString();
	}
}