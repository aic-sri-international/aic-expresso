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
package com.sri.ai.grinder.sgdpllt.theory.function;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.util.Util.map;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.FunctionType;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.library.Disequality;
import com.sri.ai.grinder.sgdpllt.library.Equality;

@Beta
public class BruteForceFunctionTheoryTestingSupport extends AbstractTheoryTestingSupport {
	private Map<String, Type> termVariableNamesAndTypesForTesting;
	
	public BruteForceFunctionTheoryTestingSupport(BruteForceFunctionTheory theory, Random random) {
		super(theory, random);
		setVariableNamesAndTypesForTesting(				
				map("f", new FunctionType(BOOLEAN_TYPE, BOOLEAN_TYPE), 
					"g", new FunctionType(getDefaultTestingType(), BOOLEAN_TYPE, getDefaultTestingType()), 
					"h", new FunctionType(getDefaultTestingType(), getDefaultTestingType()),
					"i", new FunctionType(BOOLEAN_TYPE)));
		setTermVariableNamesAndTypesForTesting(
				map("X", BOOLEAN_TYPE,
					"Y", getDefaultTestingType(),
					"Z", getDefaultTestingType()));
		
	}
	
	public void setTermVariableNamesAndTypesForTesting(Map<String, Type> termVariableNamesAndTypesForTesting) {
		this.termVariableNamesAndTypesForTesting = Collections.unmodifiableMap(termVariableNamesAndTypesForTesting);
	}

	public Map<String, Type> getTermVariableNamesAndTypesForTesting() {
		return termVariableNamesAndTypesForTesting;
	}
	
	@Override
	public Map<String, Type> getExtendedVariableNamesAndTypesForTesting() {
		return getTermVariableNamesAndTypesForTesting();
	}
	
	@Override
	public Expression makeRandomAtomOn(String mainVariable, Context context) {
		Expression result;
		
		Type         mainType                = getTestingVariableType(mainVariable);
		FunctionType mainFunctionType        = ensureFunctionType(mainType);
		Expression   mainFunctionApplication = makeFunctionApplication(mainVariable, mainFunctionType);
		// If its co-domain is boolean, generate a function application and return
		// it as the atom.
		if (mainFunctionType.getCodomain().equals(BOOLEAN_TYPE)) {
			result = mainFunctionApplication;
		}
		else {
			// If it is of some other type, pick another (or the same) function 
			// with the same co-domain. 
			String otherVariable = pickTestingVariableAtRandom(mainFunctionType.getCodomain(), variableName -> true);
			// Generate another function application
			Type         otherType                = getTestingVariableType(otherVariable);
			FunctionType otherFunctionType        = ensureFunctionType(otherType);
			Expression   otherFunctionApplication = makeFunctionApplication(otherVariable, otherFunctionType);
			// With it, form an equality or an inequality
			if (getRandom().nextBoolean()) {
				result = Equality.make(mainFunctionApplication, otherFunctionApplication);
			}
			else {
				result = Disequality.make(mainFunctionApplication, otherFunctionApplication);
			}
		}

		return result;
	}
	
	protected FunctionType ensureFunctionType(Type type) {
		FunctionType result;
		if (type instanceof FunctionType) {
			result = (FunctionType) type;
		}
		else {
			throw new IllegalArgumentException("Type of variable must be a function type, is: "+type);
		}
		return result;
	}
	 
	protected Expression makeFunctionApplication(String functorName, FunctionType functionType) {
		// Generate arguments for the application
		List<Expression> args = new ArrayList<>();
		for (Type argType : functionType.getArgumentTypes()) {
			// If constants supported, use at random
			if (argType.isSampleUniquelyNamedConstantSupported() && getRandom().nextBoolean()) {
				args.add(argType.sampleUniquelyNamedConstant(getRandom()));
			}
			else {
				// Otherwise retrieve a term variable matching that type and use it
				String termVariable = pickTestingVariableAtRandom(getTermVariableNamesAndTypesForTesting(), argType, variableName -> true);
				Type   termType     = getTermVariableNamesAndTypesForTesting().get(termVariable);
				if (termType instanceof FunctionType) {
					// Allow for nested function applications
					args.add(makeFunctionApplication(termVariable, (FunctionType) termType));
				}
				else {
					// Otherwise just assign the variable for the term.
					args.add(parse(termVariable));
				}
			}
		}
				
		Expression result = Expressions.apply(functorName, args.toArray(new Object[args.size()]));
				
		return result;
	}
}
