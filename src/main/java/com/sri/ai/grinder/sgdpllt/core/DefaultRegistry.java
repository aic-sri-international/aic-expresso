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
package com.sri.ai.grinder.sgdpllt.core;

import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.fromTypeExpressionToItsIntrinsicMeaning;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeOfFunctor;
import static com.sri.ai.util.Util.map;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.IsVariable;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * A default implementation of {@link Registry}.
 * <p>
 * By default, the
 * predicate indicating variables uses {@link PrologVariableConvention}.
 * 
 * @author braz
 * @author oreilly
 */
@Beta
public class DefaultRegistry implements Registry, Serializable {
	private static final long serialVersionUID = 1L;
	
	private Map<Expression, Expression>  symbolsAndTypes;
	private Map<Expression, Type> fromTypeExpressionToType;

	private Predicate<Expression> isUniquelyNamedConstantPredicate;

	private Map<Object, Object> globalObjects = null;
	
	//
	// START - Constructors

	public DefaultRegistry() {
		this(
				new LinkedHashMap<Expression, Expression>(),
				new PrologConstantPredicate(), // symbolsAndTypes
				new LinkedHashMap<Object, Object>()); // globalObjects
	}

	public DefaultRegistry(Map<Object, Object> globalObjects) {
		this(
				new LinkedHashMap<Expression, Expression>(),
				new PrologConstantPredicate(), 
				globalObjects);
	}

	public DefaultRegistry(
			Map<Expression, Expression> symbolsAndTypes,
			Predicate<Expression> isUniquelyNamedConstantPredicate,
			Map<Object, Object> globalObjects) {

		this.symbolsAndTypes = symbolsAndTypes;
		this.isUniquelyNamedConstantPredicate = isUniquelyNamedConstantPredicate;
		//
		this.globalObjects = globalObjects;
		//
		this.fromTypeExpressionToType = map();
	}
	
	/**
	 * Creates a {@link DefaultRegistry} containing the basic information
	 * from another context.
	 * The basic information are the theory, symbols and types, is unique constant predicate,
	 * and global objects.
	 * Uses {@link #TrueContext(Theory, Map, Predicate, Map)}.
	 * @param another
	 */
	public DefaultRegistry(Context another) {
		this(
				another.getSymbolsAndTypes(), 
				another.getIsUniquelyNamedConstantPredicate(), 
				another.getGlobalObjects());
	}

	// END-Constructors
	//
	
	

	// END-Context
	//
	
	//
	//  PROTECTED METHODS
	//
	
	//
	// PRIVATE METHODS
	//
	
	@Override
	public DefaultRegistry clone() {
		DefaultRegistry result = null;
		try {
			result = (DefaultRegistry) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}

	//
	// START-Context
	@Override
	public boolean isUniquelyNamedConstant(Expression expression) {
		return getIsUniquelyNamedConstantPredicate().apply(expression);
	}
	
	@Override
	public boolean isVariable(Expression expression) {
		boolean result = IsVariable.isVariable(expression, isUniquelyNamedConstantPredicate);
		return result;
	}

	@Override
	public Predicate<Expression> getIsUniquelyNamedConstantPredicate() {
		return isUniquelyNamedConstantPredicate;
	}

	@Override
	public DefaultRegistry setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate) {
		DefaultRegistry result = clone();
		result.isUniquelyNamedConstantPredicate = isUniquelyNamedConstantPredicate;
		return result;
	}

	@Override
	public Set<Expression> getSymbols() {
		return symbolsAndTypes.keySet();
	}

	@Override
	public Map<Expression, Expression> getSymbolsAndTypes() {
		return symbolsAndTypes;
	}

	@Override
	public Registry setSymbolsAndTypes(Map<Expression, Expression> newSymbolsAndTypes) {
		DefaultRegistry result = clone();
		result.symbolsAndTypes = newSymbolsAndTypes;
		return result;
	}

	@Override
	public boolean containsSymbol(Expression symbol) {
		return symbolsAndTypes.containsKey(symbol);
	}
	
	@Override
	public Expression getTypeExpressionOfRegisteredSymbol(Expression symbol) {
		return symbolsAndTypes.get(symbol);
	}

	@Override
	public Map<Object, Object> getGlobalObjects() {
		return globalObjects;
	}

	@Override
	public DefaultRegistry putAllGlobalObjects(Map<Object, Object> objects) {
		DefaultRegistry result = clone();
		result.globalObjects = new StackedHashMap<>(objects, result.getGlobalObjects());
		return result;
	}

	@Override
	public DefaultRegistry putGlobalObject(Object key, Object value) {
		return putAllGlobalObjects(map(key, value));
	}

	@Override
	public boolean containsGlobalObjectKey(Object key) {
		return globalObjects.containsKey(key);
	}
	
	@Override
	public Object getGlobalObject(Object key) {
		return globalObjects.get(key);
	}

	// END-Context
	//
	
	//
	//  PROTECTED METHODS
	//
	
	//
	// PRIVATE METHODS
	//
	
	@Override
	public String toString() {
		return "Context with: " + getSymbolsAndTypes();
	}

	@Override
	public DefaultRegistry makeCloneWithAddedType(Type type) {
		DefaultRegistry result = clone();
		String name = type.getName();
		Expression typeExpression = parse(name);
		LinkedHashMap<Expression, Type> additionalTypeMap = map(typeExpression, type);
		result.fromTypeExpressionToType = new StackedHashMap<>(additionalTypeMap, fromTypeExpressionToType);
		return result;
	}

	@Override
	public Type getType(String name) {
		Expression typeExpression = parse(name);
		Type result = getTypeFromTypeExpression(typeExpression);
		return result;
	}

	@Override
	public Type getTypeFromTypeExpression(Expression typeExpression) {
		Type result = fromTypeExpressionToType.get(typeExpression);
		if (result == null) {
			result = fromTypeExpressionToItsIntrinsicMeaning(typeExpression, this);
		}
		return result;
	}

	@Override
	public Collection<Type> getTypes() {
		return Collections.unmodifiableCollection(fromTypeExpressionToType.values());
	}

	@Override
	public DefaultRegistry makeCloneWithAdditionalRegisteredSymbolsAndTypes(
			Map<Expression, Expression> symbolsAndTypes) {
		if (symbolsAndTypes.isEmpty()) { // nothing to do
			return this;
		}
		
		Map<Expression, Expression> newSymbolsAndTypes = 
				createNewSymbolsAndTypes(symbolsAndTypes);
		
		DefaultRegistry result = clone();
		result.symbolsAndTypes = newSymbolsAndTypes;
		
		return result;
	}

	private Map<Expression, Expression> createNewSymbolsAndTypes(Map<Expression, Expression> additionalSymbolsAndTypes) {
		Map<Expression, Expression> symbolsAndTypes = 
				getTypesOfIndicesFunctorsOrSymbols(additionalSymbolsAndTypes); // returns a fresh map, so we can use it below without copying
		Map<Expression, Expression> newSymbolsAndTypes = 
				new StackedHashMap<Expression, Expression>(symbolsAndTypes, getSymbolsAndTypes());
		return newSymbolsAndTypes;
	}

	/**
	 * Gets a freshly built map from indices to their types and returns a map from their functors-or-symbols
	 * (that is, their functors if they are function application, and themselves if they are symbols)
	 * to their types.
	 * A function has a type of the form <code>'->'('x'(T1, ..., Tn), R)</code>, where <code>T1,...,Tn</code>
	 * are the types of their arguments and <code>R</code> are their co-domain.
	 */
	private Map<Expression, Expression> getTypesOfIndicesFunctorsOrSymbols(Map<Expression, Expression> fromIndicesToType) {
		Map<Expression, Expression> result = new LinkedHashMap<Expression, Expression>();
		for (Map.Entry<Expression, Expression> entry : fromIndicesToType.entrySet()) {
			Expression index     = entry.getKey();
			Expression indexType = entry.getValue();
			if (index.getSyntacticFormType().equals(Symbol.SYNTACTIC_FORM_TYPE)) {
				result.put(index, indexType);
			}
			else if (index.getSyntacticFormType().equals(FunctionApplication.SYNTACTIC_FORM_TYPE)) {
				Expression typeOfFunctor = getTypeOfFunctor(index, indexType, this);
				result.put(index.getFunctorOrSymbol(), typeOfFunctor);
			}
			else {
				throw new Error("getTypesOfIndicesFunctorsOrSymbols not supported for expressions other than symbols and function applications, but invoked on " + index);
			}
		}
		return result;
	}
}
