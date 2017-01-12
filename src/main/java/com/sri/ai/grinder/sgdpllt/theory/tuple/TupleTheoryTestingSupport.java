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
package com.sri.ai.grinder.sgdpllt.theory.tuple;

import static com.sri.ai.expresso.helper.Expressions.parse;
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
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractTheoryTestingSupport;
import com.sri.ai.grinder.sgdpllt.library.Disequality;
import com.sri.ai.grinder.sgdpllt.library.Equality;
@Beta
public class TupleTheoryTestingSupport extends AbstractTheoryTestingSupport {
	//
	public static final TupleType TUPLE_TYPE = new TupleType(getDefaultTestingType(), getDefaultTestingType());
	
	//
	private Map<String, Type> elementVariableNamesAndTypesForTesting;
	
	public TupleTheoryTestingSupport(TupleTheory theory, Random random) {
		super(theory, random);
		setVariableNamesAndTypesForTesting(
				map("L", TUPLE_TYPE, 
					"M", TUPLE_TYPE, 
					"N", TUPLE_TYPE));
		setElementVariableNamesAndTypesForTesting(
				map("X", getDefaultTestingType(),
					"Y", getDefaultTestingType(),
					"Z", getDefaultTestingType()));
	}
	
	public void setElementVariableNamesAndTypesForTesting(Map<String, Type> elementVariableNamesAndTypesForTesting) {
		this.elementVariableNamesAndTypesForTesting = Collections.unmodifiableMap(elementVariableNamesAndTypesForTesting);
	}

	public Map<String, Type> getElementVariableNamesAndTypesForTesting() {
		return elementVariableNamesAndTypesForTesting;
	}
	
	@Override
	public Expression makeRandomAtomOn(String mainVariable, Context context) {
		Expression result;
		
		// Construct an instance of the main tuple
		Type       mainType      = getTestingVariableType(mainVariable);
		TupleType  mainTupleType = ensureTupleType(mainType);
		Expression mainTuple     = makeTuple(mainTupleType); 
		
		// Pick another (or the same) variable having a compatible tuple type
		String otherVariable = pickTestingVariableAtRandom(mainType, variableName -> true);
		
		// Construct an instance of the other tuple
		Type       otherType      = getTestingVariableType(otherVariable);
		TupleType  otherTupleType = ensureTupleType(otherType);
		Expression otherTuple     = makeTuple(otherTupleType);
		
		// With it, form an equality or an inequality
		if (getRandom().nextBoolean()) {
			result = Equality.make(mainTuple, otherTuple);
		}
		else {
			result = Disequality.make(mainTuple, otherTuple);
		}
		
		return result;
	}
	
	protected TupleType ensureTupleType(Type type) {
		TupleType result;
		if (type instanceof TupleType) {
			result = (TupleType) type;
		}
		else {
			throw new IllegalArgumentException("Type of variable must be a tuple type, is: "+type);
		}
		return result;
	}
	
	protected Expression makeTuple(TupleType tupleType) {
		// Generate elements for the tuple
		List<Expression> elements = new ArrayList<>();
		
		for (Type elementType : tupleType.getElementTypes()) {
			// If constants supported, use at random
			if (elementType.isSampleUniquelyNamedConstantSupported() && getRandom().nextBoolean()) {
				elements.add(elementType.sampleUniquelyNamedConstant(getRandom()));
			}
			else {
				String elementVariable = pickTestingVariableAtRandom(getElementVariableNamesAndTypesForTesting(), elementType, variableName -> true);
				elements.add(parse(elementVariable));
			}
		}
		
		Expression result = Expressions.makeTuple(elements.toArray(new Expression[elements.size()]));
		
		return result;
	}
}
