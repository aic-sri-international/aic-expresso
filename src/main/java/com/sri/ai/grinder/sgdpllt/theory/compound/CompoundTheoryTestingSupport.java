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
package com.sri.ai.grinder.sgdpllt.theory.compound;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.expresso.helper.Expressions.primedUntilUnique;
import static com.sri.ai.util.Util.check;
import static com.sri.ai.util.Util.map;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;

@Beta
public class CompoundTheoryTestingSupport extends AbstractTheoryTestingSupport {
	private Map<Theory, TheoryTestingSupport> theoryToTestingSupport;
	
	public CompoundTheoryTestingSupport(CompoundTheory theory, Random random) {
		super(theory, random);
		aggregateTestingInformation();
	}
	
	public CompoundTheoryTestingSupport(Random random, TheoryTestingSupport... subTheoryTestingSupports) {
		super(newCompoundTheory(subTheoryTestingSupports), random);
		theoryToTestingSupport = new LinkedHashMap<>();
		for (TheoryTestingSupport subTheoryTestingSupport : subTheoryTestingSupports) {
			theoryToTestingSupport.put(subTheoryTestingSupport.getTheory(), subTheoryTestingSupport);
		}
		aggregateTestingInformation();
	}
	
	@Override
	public CompoundTheory getTheory() {
		return (CompoundTheory) super.getTheory();
	}
	
	@Override
	public Map<String, Type> getExtendedVariableNamesAndTypesForTesting() {
		Map<String, Type> result = new LinkedHashMap<>();
		for (TheoryTestingSupport subTheoryTestingSupport : theoryToTestingSupport.values()) {
			result.putAll(((AbstractTheoryTestingSupport)subTheoryTestingSupport).getExtendedVariableNamesAndTypesForTesting());
		}
		return result;
	}
	
	/**
	 * This is overridden so that given variables and types for testing are distributed to their
	 * respective theories according to {@link #isSuitableFor(Expression, Type)}.
	 */
	@Override
	public void setVariableNamesAndTypesForTesting(Map<String, Type> variableNamesAndTypesForTesting) {
		// First ensure the compound set of variables names and type information is setup correctly
		super.setVariableNamesAndTypesForTesting(variableNamesAndTypesForTesting);
		
		// Then update the sub-theories so that they share appropriate subsets of this information
		Map<Theory, Map<String, Type>> mapForSubTheory = map();
		
		for (Theory subTheory : getTheory().getSubTheories()) {
			mapForSubTheory.put(subTheory, map());
		}

		for (Map.Entry<String, Type> variableNameAndType : getVariableNamesAndTypesForTesting().entrySet()) {
			String variableName = variableNameAndType.getKey();
			Expression variable = Expressions.parse(variableName);
			Type type = variableNameAndType.getValue();
			for (Theory subTheory : getTheory().getSubTheories()) {
				if (subTheory.isSuitableFor(variable, type) || (type instanceof FunctionType && subTheory.isSuitableFor(variable, ((FunctionType)type).getCodomain()))) {
					mapForSubTheory.get(subTheory).put(variableName, type);
				}
			}
		}

		for (Map.Entry<Theory, TheoryTestingSupport> entry : getTheoryToTestingSupport().entrySet()) {
			Map<String, Type> forThisSubTheory = mapForSubTheory.get(entry.getKey());
			entry.getValue().setVariableNamesAndTypesForTesting(forThisSubTheory);
		}
	}
	
	@Override
	public Expression makeRandomAtomOn(String variable, Context context) {
		TheoryTestingSupport theoryTestingSupport = getTheoryTestingSupport(variable);
		Expression result = theoryTestingSupport.makeRandomAtomOn(variable, context);
		return result;
	}
	
	//
	//	
	private void aggregateTestingInformation() throws Error {
		Map<String, Type> variableNamesAndTypesForTesting = new LinkedHashMap<>();
		for (TheoryTestingSupport theoryTestingSupport : getSubConstraintTheoryTestingSupports()) {
			Set<Entry<String, Type>> variableNamesAndTypeNameEntries = theoryTestingSupport.getVariableNamesAndTypesForTesting().entrySet();
			for (Map.Entry<String, Type> variableNameAndTypeName : variableNamesAndTypeNameEntries) {
				String variableName = variableNameAndTypeName.getKey();
				Type type = variableNameAndTypeName.getValue();
				if (variableNamesAndTypesForTesting.containsKey(variableName)) {
					variableName = primedUntilUnique(
							variableName, 
							s -> variableNamesAndTypesForTesting.containsKey(s.toString()));
				}
				variableNamesAndTypesForTesting.put(variableName, type);
			}
		}
		setVariableNamesAndTypesForTesting(variableNamesAndTypesForTesting);
	}
	
	private Collection<TheoryTestingSupport> getSubConstraintTheoryTestingSupports() {
		return getTheoryToTestingSupport().values();
	}
	
	private Map<Theory, TheoryTestingSupport> getTheoryToTestingSupport() {
		if (theoryToTestingSupport == null) {
			theoryToTestingSupport = new LinkedHashMap<>();
			for (Theory subConstraintTheory : getTheory().getSubTheories()) {
				theoryToTestingSupport.put(subConstraintTheory, TheoryTestingSupport.make(getRandom(), subConstraintTheory));
			}
		}
		
		return theoryToTestingSupport;
	}
	
	private TheoryTestingSupport getTheoryTestingSupport(String variable) {
		Type variableType = getTestingVariableType(variable);
		Theory subConstraintTheory = getTheory().getTheory(parse(variable), variableType);
		check(() -> subConstraintTheory != null, () -> "There is no sub-theory suitable for " + variable + ", which has type " + variableType);
		TheoryTestingSupport result = getTheoryToTestingSupport().get(subConstraintTheory);
		return result;
	}
	
	private static CompoundTheory newCompoundTheory(TheoryTestingSupport... subTheoryTestingSupports) {
		Theory[] subConstraintTheoriesArray = new Theory[subTheoryTestingSupports.length];
		for (int i = 0; i < subTheoryTestingSupports.length; i++) {
			subConstraintTheoriesArray[i] = subTheoryTestingSupports[i].getTheory();
		}
		return new CompoundTheory(subConstraintTheoriesArray);
	}
}