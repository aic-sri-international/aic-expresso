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
package com.sri.ai.grinder.api;

import static com.sri.ai.grinder.helper.GrinderUtil.makeIndexExpressionsFromSymbolsAndTypes;
import static com.sri.ai.grinder.helper.GrinderUtil.makeListOfSymbolsAndTypesExpressionsFromSymbolsAndTypesStrings;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IndexExpressionsSet;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.core.ExtensionalIndexExpressionsSet;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.util.Util;

/**
 * An object containing information about global symbols and their types,
 * user-defined global objects, and a predicate for defining constants.
 * 
 * @author braz
 */
@Beta
public interface Registry extends Cloneable {

	Registry clone();
	
	/**
	 * Returns the predicate indicating uniquely named constants.
	 */
	Predicate<Expression> getIsUniquelyNamedConstantPredicate();
	
	/**
	 * Return a clone of this registry with the given predicate indicating uniquely named constants.
	 * @return
	 */
	Registry setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate);
	
	/** Indicates whether a given expression is a uniquely named constant (assumed to be distinct from all other uniquely named constants). */
	boolean isUniquelyNamedConstant(Expression expression);

	/** Indicates whether a given expression is not a uniquely named constant. */
	boolean isVariable(Expression expression);

	/**
	 * @return the set of known symbols.
	 */
	Set<Expression> getSymbols();
	
	/**
	 * @return the types of all registered symbols.
	 */
	Map<Expression, Expression> getSymbolsAndTypes();
	
	/**
	 * @return sets the map from symbols to types
	 */
	Registry setSymbolsAndTypes(Map<Expression, Expression> newSymbolsAndTypes);
	
	/**
	 * Indicates whether the registry contains a symbol. 
	 */
	boolean containsSymbol(Expression symbol);
	
	/**
	 * @return the type expression of a registered symbol.
	 */
	Expression getTypeExpressionOfRegisteredSymbol(Expression symbol);
	
	/**
	 * @return the type of a registered symbol (equivalent to <code>getType(getTypeExpressionOfRegisteredSymbol(symbol))</code>.
	 */
	default Type getTypeOfRegisteredSymbol(Expression symbol) {
		Expression typeExpression = getTypeExpressionOfRegisteredSymbol(symbol);
		Type result = getTypeFromTypeExpression(typeExpression);
		return result;
	}
	
	/**
	 * Create a new sub-registry and registers the symbols
	 * in the indices-and-types map (an index can be a symbol or a function application).
	 */
	Registry makeNewRegistryWithRegisteredAdditionalSymbolsAndTypes(Map<Expression, Expression> indicesAndTypes);

	/**
	 * Creates a new registry identical to a given one but for additional global objects.
	 * @param objects
	 * @return
	 */
	Registry putAllGlobalObjects(Map<Object, Object> objects);
	
	/**
	 * Gets map of global objects.
	 */
	Map<Object, Object> getGlobalObjects();
	
	/**
	 * Returns a cloned registry with a value in a map of global objects under key.
	 */
	Registry putGlobalObject(Object key, Object value);
	
	/**
	 * Indicates whether map of global objects contains key.
	 */
	boolean containsGlobalObjectKey(Object key);
	
	/**
	 * Gets a value from a map of global objects under key.
	 */
	Object getGlobalObject(Object key);
	
	Registry add(Type type);

	default Registry addAll(Collection<Type> types) {
		Registry result = this;
		for (Type type : types) {
			result = result.add(type);
		}
		return result;
	}
	
	/**
	 * Returns the {@link Type} represented by the given type string representation.
	 * @param typeExpression
	 * @return
	 */
	Type getType(String typeStringRepresentation);
	
	/**
	 * Returns the {@link Type} represented by the given type expression.
	 * @param typeExpression
	 * @return
	 */
	Type getTypeFromTypeExpression(Expression typeExpression);
	
	Collection<Type> getTypes();
	
	/**
	 * Extends with pairs of symbols and their respective types represented as strings.
	 * @param symbolsAndTypes
	 * @return
	 */
	default Registry extendWithSymbolsAndTypes(Expression... symbolsAndTypes) {
		Util.myAssert(symbolsAndTypes.length % 2 == 0, () -> "Need to extend registry with a sequence of symbols and their types");
		List<Expression> indexExpressions = makeIndexExpressionsFromSymbolsAndTypes(symbolsAndTypes);
		Registry result = extendWith(new ExtensionalIndexExpressionsSet(indexExpressions));
		return result;
	}

	/**
	 * Extends with pairs of symbols and their respective types represented as strings.
	 * @param symbolsAndTypes
	 * @return
	 */
	default Registry extendWithSymbolsAndTypes(String... symbolsAndTypes) {
		Expression[] symbolsAndTypesExpressions = makeListOfSymbolsAndTypesExpressionsFromSymbolsAndTypesStrings(symbolsAndTypes);
		Registry result = extendWithSymbolsAndTypes(symbolsAndTypesExpressions);
		return result;
	}

	/**
	 * Extends the registry with the given index expressions.
	 * @param indexExpressions
	 * @return
	 */
	default Registry extendWith(IndexExpressionsSet indexExpressions) {
		Map<Expression, Expression> indexToTypeMap = IndexExpressions.getIndexToTypeMapWithDefaultNull(indexExpressions);
		Registry result = makeNewRegistryWithRegisteredAdditionalSymbolsAndTypes(indexToTypeMap);
		return result;
	}
}
