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
package com.sri.ai.expresso.type;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.library.FunctorConstants.TUPLE_TYPE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.StringJoiner;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Tuple;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.core.DefaultRegistry;
import com.sri.ai.grinder.helper.AssignmentMapsIterator;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.collect.FunctionIterator;
import com.sri.ai.util.math.Rational;

/**
 * Represents tuple types.
 * 
 * @author oreilly
 *
 */
@Beta
public class TupleType extends AbstractType {
	private static final long serialVersionUID = 1L;

	private List<Type> elementTypes;
	//
	private String cachedString;
	// NOTE: Following used for iteration logic
	private Registry cachedIterateRegistry;
	private List<Expression> elementVariables;
	private Expression genericTuple;

	public TupleType(Type... elementTypes) {
		this(Arrays.asList(elementTypes));
	}
	
	public TupleType(List<Type> elementTypes) {
		this.elementTypes = Collections.unmodifiableList(new ArrayList<>(elementTypes));
	}

	public int getArity() {
		return getElementTypes().size();
	}

	public List<Type> getElementTypes() {
		return elementTypes;
	}

	@Override
	public String getName() {
		return toString();
	}

	@Override
	public Iterator<Expression> iterator() {
		if (!getElementTypes().stream().allMatch(Type::isDiscrete)) {
			throw new Error("Only tuple types with discrete element types can be enumerated.");
		}
		
		if (cachedIterateRegistry == null) {
			// Pre-compute
			elementVariables = new ArrayList<>();
			cachedIterateRegistry = new DefaultRegistry();
			Map<Expression, Expression> symbolsAndTypes = new LinkedHashMap<>();
			for (int i = 0; i < getArity(); i++) {
				Expression elementVariableI = makeSymbol("E" + (i + 1));
				elementVariables.add(elementVariableI);
				symbolsAndTypes.put(elementVariableI, parse(getElementTypes().get(i).getName()));
				cachedIterateRegistry = cachedIterateRegistry.makeNewContextWithAddedType(elementTypes.get(i));
			}
			cachedIterateRegistry = cachedIterateRegistry.setSymbolsAndTypes(symbolsAndTypes);
			
			StringJoiner tupleVariableRepresentation = new StringJoiner(", ", "tuple(", ")");
			for (Expression eleVar : elementVariables) {
				tupleVariableRepresentation.add(eleVar.toString());
			}
			
			genericTuple = parse(tupleVariableRepresentation.toString());
		}
		
		return FunctionIterator.functionIterator(new AssignmentMapsIterator(elementVariables, cachedIterateRegistry), assignment -> {
			
			Expression tuple = genericTuple;
			for (int i = 0; i < elementVariables.size(); i++) {
				Expression elementVariable = elementVariables.get(i);
				// NOTE: There will only be one occurrence of the element variable to replace, 
				// so can avoid doing an AllOccurrences replacement.
				tuple = tuple.replaceFirstOccurrence(elementVariable, assignment.get(elementVariable), cachedIterateRegistry);
			}
			
			return tuple;
		});
	}

	@Override
	public boolean contains(Expression uniquelyNamedConstant) {
		boolean result = false;
		
		if (Tuple.isTuple(uniquelyNamedConstant)) {
			if (uniquelyNamedConstant.numberOfArguments() == getArity()) {
				result = true; // Assume does contain unless find out otherwise
				for (int i = 0; i < getArity(); i++) {
					if (!getElementTypes().get(i).contains(uniquelyNamedConstant.get(i))) {
						result = false;
						break;
					}
				}				
			}
		}
		
		return result;
	}
	
	@Override
	public boolean isSampleUniquelyNamedConstantSupported() {
		return getElementTypes().stream().allMatch(et -> et.isSampleUniquelyNamedConstantSupported());
	}

	@Override
	public Expression sampleUniquelyNamedConstant(Random random) {
		List<Expression> elements = new ArrayList<>();
		for (Type elementType : getElementTypes()) {
			elements.add(elementType.sampleUniquelyNamedConstant(random));
		}
		Expression result = Expressions.makeTuple(elements);
		return result;
	}

	@Override
	public Expression cardinality() {
		if (isFinite()) {
			Rational cardinality = getElementTypes().stream()
							.map(Type::cardinality)
							.map(Expression::rationalValue)
							.reduce(Rational.ONE, Rational::multiply);
			return makeSymbol(cardinality);
		}
		return Expressions.INFINITY;
	}
	
	@Override
	public boolean isDiscrete() {
		return getElementTypes().stream().allMatch(Type::isDiscrete);
	}
	
	@Override
	public boolean isFinite() {
		return getElementTypes().stream().allMatch(Type::isFinite);
	}
	
	@Override
	public Set<Type> getEmbeddedTypes() {
		Set<Type> result = new LinkedHashSet<>();
		elementTypes.forEach(elementType -> {
			result.add(elementType);
			result.addAll(elementType.getEmbeddedTypes());
		});
		return result;
	}

	@Override
	public String toString() {
		if (cachedString == null) {
			cachedString = apply(TUPLE_TYPE, getElementTypes()).toString();
		}
		return cachedString;
	}

	/**
	 * Make a tuple type expression.
	 * 
	 * @param elementTypes
	 *            0 or more element types to tuple type.
	 * @return a tuple type expression.
	 */
	public static Expression make(Expression... elementTypes) {
		Expression result = make(Arrays.asList(elementTypes));
		return result;
	}


	/**
	 * Make a tuple type expression.
	 * 
	 * @param elementTypes
	 *            0 or more element types to tuple type.
	 * @return a tuple type expression.
	 */
	public static Expression make(List<Expression> elementTypes) {
		Expression result = apply(TUPLE_TYPE, elementTypes);

		return result;
	}
	
	public static boolean isTupleType(Expression expression) {
		boolean result = expression.hasFunctor(FunctorConstants.TUPLE_TYPE);
		return result;		
	}
}