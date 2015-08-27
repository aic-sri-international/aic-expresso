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
package com.sri.ai.grinder.sgdpll2.core;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.core.AbstractMapsBasedSimplifier;
import com.sri.ai.grinder.plaindpll.util.DPLLUtil;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;

@Beta
/** 
 * Basic implementation of some methods of {@link ConstraintTheory}.
 */
abstract public class AbstractConstraintTheory extends AbstractMapsBasedSimplifier implements ConstraintTheory {

	/**
	 * Initializes types for testing to be the collection of a single type,
	 * a {@link Categorical} {@link Type} named <code>SomeType</code>
	 * with domain size 5 and known constants <code>a, b, c, d</code>,
	 * variables for testing to <code>X, Y, Z</code> of type <code>SomeType</code>,
	 * of which <code>X</code> is the main testing variable on which testing literals are generated.
	 */
	public AbstractConstraintTheory() {
		super();
		ArrayList<Expression> knownConstants = mapIntoArrayList(list("a", "b", "c", "d"), s -> makeSymbol(s));
		setTypesForTesting(list(new Categorical("SomeType", 5, knownConstants)));
		setVariableNamesAndTypeNamesForTesting(map("X", "SomeType", "Y", "SomeType", "Z", "SomeType"));
		setTestingVariable("X");
	}
	
	private Collection<Type> typesForTesting = null;
	
	private String testingVariable = null;
	
	private Map<String, String> variableNamesAndTypeNamesForTesting;
	
	@Override
	public Collection<Type> getTypesForTesting() {
		if (typesForTesting == null) {
			return null;
		}
		return Collections.unmodifiableCollection(typesForTesting);
	}

	@Override
	public void setTypesForTesting(Collection<Type> newTypesForTesting) {
		typesForTesting = newTypesForTesting;
	}
	
	@Override
	public void setVariableNamesAndTypeNamesForTesting(Map<String, String> variableNamesForTesting) {
		this.variableNamesAndTypeNamesForTesting = variableNamesForTesting;
	}
	
	@Override
	public Map<String, String> getVariableNamesAndTypeNamesForTesting() {
		return Collections.unmodifiableMap(variableNamesAndTypeNamesForTesting);
	}

	@Override
	public RewritingProcess extendWithTestingInformation(RewritingProcess process) {
		RewritingProcess result = process.put(getTypesForTesting());
		Map<String, String> mapFromTypeNameToSizeString = map();
		for (Type type : typesForTesting) {
			mapFromTypeNameToSizeString.put(type.getName(), Integer.toString(type.size()));
		}
		result = DPLLUtil.extendProcessWith(variableNamesAndTypeNamesForTesting, mapFromTypeNameToSizeString, result);
		return result;
	}

	@Override
	public String getTestingVariable() {
		return testingVariable;
	}

	@Override
	public void setTestingVariable(String newTestingVariable) {
		this.testingVariable = newTestingVariable;
	}
}