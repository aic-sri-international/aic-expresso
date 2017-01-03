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
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.mapIntoArrayList;
import static com.sri.ai.util.Util.randomPick;
import static com.sri.ai.util.Util.removeFromArrayListNonDestructively;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.FormulaUtil;
import com.sri.ai.grinder.sgdpllt.tester.TheoryTestingSupport;
import com.sri.ai.util.base.Pair;

@Beta
abstract public class AbstractTheoryTestingSupport implements TheoryTestingSupport {
	private static Categorical _someType;
	private static int CONSTANT_ARITY = -1;
	//
	private Theory theory;
	//
	private Random random;
	private boolean generalizedVariableSupportEnabled;
	//
	/**
	 * Raw information about testing variables and their types,
	 * excluding random function type argument types.
	 * The reason we keep this is that we delay the processing of function type argument types
	 * as much as possible, because sometimes a theory does not have enough types for filling them
	 * all without creating recursive types, so we don't want to fill those up as soon as the theory
	 * is created. Instead, we allow it to be created with this raw information, which may
	 * be useful for creating compound theories.
	 */
	private Map<String, Type> rawVariableNamesAndTypesForTesting;
	private Map<String, Type> variableNamesAndTypesForTesting;
	private Collection<Type> typesOfTestingVariables;
	//
	private Collection<Type> cachedTypesForTesting;
	private List<String> cachedVariableNamesForTesting;
	
	public AbstractTheoryTestingSupport(Theory theory, Random random, boolean generalizedVariableSupportEnabled) {
		this.theory = theory;
		this.random = random;
		this.generalizedVariableSupportEnabled = generalizedVariableSupportEnabled;
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
	public boolean isGeneralizedVariableSupportEnabled() {
		return generalizedVariableSupportEnabled;
	}
	
	@Override
	public void setVariableNamesAndTypesForTesting(Map<String, Type> variableNamesAndTypesForTesting) {
		this.rawVariableNamesAndTypesForTesting = variableNamesAndTypesForTesting;
		this.typesOfTestingVariables = variableNamesAndTypesForTesting.values();
		this.variableNamesAndTypesForTesting = null;
	}

	@Override
	public Map<String, Type> getVariableNamesAndTypesForTesting() {
		if (variableNamesAndTypesForTesting == null) {
			processVariableNamesAndTypesForTesting();
		}
		return variableNamesAndTypesForTesting;
	}

	/**
	 * @throws Error
	 */
	public void processVariableNamesAndTypesForTesting() throws Error {
		if (rawVariableNamesAndTypesForTesting == null) {
			throw new Error("Testing variable names and types is being requested, but have not been provided yet through setVariableNamesAndTypesForTesting() in theory " + this);
		}

		this.variableNamesAndTypesForTesting = new LinkedHashMap<>();
		
		List<TestingFunctionType> testingFunctionTypesToBeUpdated = new ArrayList<>();
		
		for (Map.Entry<String, Type> variableNameAndType : rawVariableNamesAndTypesForTesting.entrySet()) {
			String variableSignature = variableNameAndType.getKey();
			Type   variableType      = variableNameAndType.getValue();
			
			Pair<String, Integer> nameAndArityInfo = extractNameAndArityInfo(variableSignature);
			String name = nameAndArityInfo.first;
			int   arity = nameAndArityInfo.second;
			if (arity == CONSTANT_ARITY) {
				this.variableNamesAndTypesForTesting.put(name, variableType);
			}
			else {
				if (isGeneralizedVariableSupportEnabled()) {
					if (variableType instanceof FunctionType) {
						// An explicit function type has been defined, we will use
						// but ensure its arity matches with that of the variable signature
						int functionTypeArity = ((FunctionType)variableType).getArity();
						if (arity != functionTypeArity) {
							throw new IllegalArgumentException("Arity specified on variable signature of "+arity+" does not match that of provided function type, which is "+functionTypeArity);
						}
						this.variableNamesAndTypesForTesting.put(name, variableType);
						// If we are in a compound theory, we will be pulling types across theories
						// in this case we will re-assign the argument types to take into account
						// the set of types across all theories, so we need to track these for 
						// updating as well.
						if (variableType instanceof TestingFunctionType) {
							testingFunctionTypesToBeUpdated.add((TestingFunctionType)variableType);
						}
					}
					else {												
						if (getTheory().isInterpretedInThisTheoryBesidesBooleanConnectives(parse(name)) ||
							FormulaUtil.isInterpretedInPropositionalLogicIncludingConditionals(parse(name))) {
							throw new IllegalArgumentException("Provided generalized variable functor = " + name + " is interpreted in this theory.");
						}
						// In this instance the variableType represents the codomain
						TestingFunctionType testingFunctionType = new TestingFunctionType(variableType, new Type[arity]);
						testingFunctionTypesToBeUpdated.add(testingFunctionType);						
						this.variableNamesAndTypesForTesting.put(name, testingFunctionType);
					}
				}
				else {
					// We are not supporting generalized variables, so just drop it
				}
			}
		}
		this.variableNamesAndTypesForTesting = Collections.unmodifiableMap(this.variableNamesAndTypesForTesting);
		
		// ensure cached values are cleared as will need to be recomputed.
		this.cachedTypesForTesting = null;
		this.cachedVariableNamesForTesting = null;
		
		// Now we update the testing function types, to have argument types
		// selected at random that are dependent on the theory being used.
		ArrayList<Type> allTestingTypes = new ArrayList<>(getTypesForTesting());
		List<Pair<TestingFunctionType, List<Type>>> updatesToReApply = new ArrayList<>();
		for (TestingFunctionType testingFunctionTypeToUpdate : testingFunctionTypesToBeUpdated) {
			List<Type> updatedArgTypes    = new ArrayList<>();
			ArrayList<Type> allTypesExceptThisOne = new ArrayList<>(allTestingTypes);
			// Ensure we do not allow recursive function type declarations.
			// by first excluding the types of args to be updated from the list
			// types to choose from.
			allTypesExceptThisOne.remove(testingFunctionTypeToUpdate);

			// only types not containing this one are admissible in order to avoid recursive types.
			ArrayList<Type> typesNotContainingThisOne = 
					removeFromArrayListNonDestructively(
							allTypesExceptThisOne, 
							t -> isTypeContainedIn(testingFunctionTypeToUpdate, t));

			if (typesNotContainingThisOne.isEmpty()) {
				throw new Error("There are no non-recursive types to fill in unspecified argument types given testing types " + allTestingTypes + ". Perhaps theories with primitive types were not included in the overall theory being tested.");
			}

			for (int i = 0; i < testingFunctionTypeToUpdate.getArity(); i++) {
				Type updatedArgType = randomPick(getRandom(), typesNotContainingThisOne);
				updatedArgTypes.add(updatedArgType);				
			}
			testingFunctionTypeToUpdate.updateTestArgumentTypes(updatedArgTypes);
			updatesToReApply.add(new Pair<>(testingFunctionTypeToUpdate, updatedArgTypes));
		}
		// NOTE: Due to Type's using getName() during equality checking (see AbstracType) 
		// we need to ensure we re-update all the compound types together so that
		// their string representations are updated correctly as well as this 
		// affects their identity (i.e. equality and hashCode checks).
		for (Pair<TestingFunctionType, List<Type>> updateToReApply : updatesToReApply) {
			updateToReApply.first.updateTestArgumentTypes(updateToReApply.second);
		}
	}
	
	@Override
	public void setVariableNamesAndTypesAsIsForTesting(Map<String, Type> variableNamesAndTypesForTesting) {
		this.variableNamesAndTypesForTesting = new LinkedHashMap<>(variableNamesAndTypesForTesting);
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
			cachedTypesForTesting = new LinkedHashSet<Type>(typesOfTestingVariables);
			cachedTypesForTesting.addAll(getTheory().getNativeTypes());
			cachedTypesForTesting = Collections.unmodifiableCollection(cachedTypesForTesting);
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
		
		Context result = (Context) GrinderUtil.extendRegistryWith(mapFromSymbolNamesToTypeNames, getTypesForTesting(), context);
		return result;
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
	
	private static Pair<String, Integer> extractNameAndArityInfo(String variableName) {
		String name = variableName;
		int  arity = CONSTANT_ARITY; 
		if (variableName.contains("/")) {
			String[] parts = variableName.split("/");
			if (parts.length != 2) {
				throw new IllegalArgumentException("Variable name syntax is not recognized : "+variableName);
			}
			name  = parts[0];
			arity = Integer.parseInt(parts[1]);
		}
		
		if (parse(name) == null) {
			name = "'"+name+"'"; // Quote the name
		}
		
		Pair<String, Integer> result = new Pair<>(name, arity);
		return result;
	}
	
	private static class TestingFunctionType extends FunctionType {
		private static final long serialVersionUID = 1L;
		
		public TestingFunctionType(Type codomain, Type... argumentTypes) {
			super(codomain, argumentTypes);
		}
		
		public void updateTestArgumentTypes(List<Type> updatedArgumentTypes) {
			super.updateTestArgumentTypes(updatedArgumentTypes);
		}
	}
	
	private boolean isTypeContainedIn(Type type, Type containedInType) {
		boolean result = false;
		
		if (type == containedInType) {
			result = true;
		}
		else if (containedInType instanceof FunctionType) {
			FunctionType containedInFunctionType = (FunctionType) containedInType;
			for (int i = 0; i < containedInFunctionType.getArity(); i++) {
				if (isTypeContainedIn(type, containedInFunctionType.getArgumentTypes().get(i))) {
					result = true;
					break;
				}
			}
		}
		
		return result;
	}
}