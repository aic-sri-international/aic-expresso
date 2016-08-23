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

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;

/**
 * An object containing information about global symbols and their types,
 * user-defined global objects, and a predicate for defining constants.
 * 
 * @author braz
 */
@Beta
public interface GlobalRegistry extends Cloneable {

	GlobalRegistry clone();
	
	/**
	 * Returns the predicate indicating uniquely named constants.
	 */
	Predicate<Expression> getIsUniquelyNamedConstantPredicate();
	
	/**
	 * Return a clone of this context with the given predicate indicating uniquely named constants.
	 * @return
	 */
	GlobalRegistry setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate);
	
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
	 * @return the type of a registered symbol.
	 */
	Expression getTypeOfRegisteredSymbol(Expression symbol);
	
	/**
	 * Create a new sub-context and registers the symbols
	 * in the indices-and-types map (an index can be a symbol or a function application).
	 */
	GlobalRegistry registerIndicesAndTypes(Map<Expression, Expression> indicesAndTypes);

	/**
	 * Creates a new context identical to a given one but for additional global objects.
	 * @param objects
	 * @return
	 */
	GlobalRegistry putAllGlobalObjects(Map<Object, Object> objects);
	
	/**
	 * Gets map of global objects.
	 */
	Map<Object, Object> getGlobalObjects();
	
	/**
	 * Returns a cloned context with a value in a map of global objects under key.
	 */
	GlobalRegistry putGlobalObject(Object key, Object value);
	
	/**
	 * Indicates whether map of global objects contains key.
	 */
	boolean containsGlobalObjectKey(Object key);
	
	/**
	 * Gets a value from a map of global objects under key.
	 */
	Object getGlobalObject(Object key);
	
	GlobalRegistry add(Type type);

	default GlobalRegistry addAll(Collection<Type> types) {
		GlobalRegistry result = this;
		for (Type type : types) {
			result = result.add(type);
		}
		return result;
	}
	
	Type getType(String typeStringRepresentation);
	
	Type getType(Expression typeExpression);
	
	Collection<Type> getTypes();
	
	// Constraint method (specializing return value to Context):

}
