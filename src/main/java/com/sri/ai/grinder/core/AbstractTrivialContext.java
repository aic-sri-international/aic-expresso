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
package com.sri.ai.grinder.core;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.IsVariable;

/**
 * An implementation of {@link Context} containing a {@link DefaultRegistry}
 * with a trivial boolean constraint.
 * <p>
 * By default, the
 * predicate indicating variables uses {@link PrologVariableConvention}.
 * 
 * @author braz
 * @author oreilly
 */
@Beta
public abstract class AbstractTrivialContext extends AbstractExpressionWrapper implements Context {
	
	private static final long serialVersionUID = 1L;

	private Theory theory;

	private Registry registry;
	
	//
	// START - Constructors

	public AbstractTrivialContext(
			Theory theory,
			Map<Expression, Expression> symbolsAndTypes,
			Predicate<Expression> isUniquelyNamedConstantPredicate,
			Map<Object, Object> globalObjects) {

		this.theory = theory;
		this.registry = 
				new DefaultRegistry(
						symbolsAndTypes,
						isUniquelyNamedConstantPredicate,
						globalObjects);
	}

	public AbstractTrivialContext() {
		this(
				null,
				new LinkedHashMap<Expression, Expression>(), // symbolsAndTypes
				new PrologConstantPredicate(), 
				new LinkedHashMap<Object, Object>()); // globalObjects
	}

	public AbstractTrivialContext(Theory theory) {
		this(
				theory,
				new LinkedHashMap<Expression, Expression>(), // symbolsAndTypes
				new PrologConstantPredicate(),
				new LinkedHashMap<Object, Object>()); // globalObjects
	}
	
	/**
	 * Creates a {@link AbstractTrivialContext} containing the basic information
	 * from another context.
	 * The basic information are the theory, symbols and types, is unique constant predicate,
	 * and global objects.
	 * Uses {@link #TrueContext(Theory, Map, Predicate, Map)}.
	 * @param another
	 */
	public AbstractTrivialContext(Context another) {
		this(
				another.getTheory(), 
				another.getSymbolsAndTypes(), 
				another.getIsUniquelyNamedConstantPredicate(), 
				another.getGlobalObjects());
	}

	// END-Constructors
	//
	
	@Override
	public Theory getTheory() {
		return theory;
	}

	@Override
	public AbstractTrivialContext clone() {
		AbstractTrivialContext result = null;
		try {
			result = (AbstractTrivialContext) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}

	// END-TrueContext
	//

	//
	// START-Registry

	@Override
	public boolean isUniquelyNamedConstant(Expression expression) {
		boolean result = registry.isUniquelyNamedConstant(expression);
		return result;
	}
	
	@Override
	public boolean isVariable(Expression expression) {
		// This is not just a matter of forwarding the method to the registry,
		// because now we have a theory that needs to be taken into account.
		boolean result = IsVariable.isVariable(expression, getIsUniquelyNamedConstantPredicate(), getTypes(), getTheory());
		return result;
	}

	@Override
	public Predicate<Expression> getIsUniquelyNamedConstantPredicate() {
		Predicate<Expression> result = registry.getIsUniquelyNamedConstantPredicate();
		return result;
	}

	@Override
	public AbstractTrivialContext setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate) {
		AbstractTrivialContext result = clone();
		result.registry = result.registry.setIsUniquelyNamedConstantPredicate(isUniquelyNamedConstantPredicate);
		return result;
	}

	@Override
	public Set<Expression> getSymbols() {
		return registry.getSymbols();
	}

	@Override
	public Map<Expression, Expression> getSymbolsAndTypes() {
		return registry.getSymbolsAndTypes();
	}

	@Override
	public Context setSymbolsAndTypes(Map<Expression, Expression> newSymbolsAndTypes) {
		AbstractTrivialContext result = clone();
		result.registry = result.registry.setSymbolsAndTypes(newSymbolsAndTypes);
		return result;
	}

	@Override
	public boolean containsSymbol(Expression symbol) {
		return registry.containsSymbol(symbol);
	}

	@Override
	public Expression getTypeExpressionOfRegisteredSymbol(Expression symbol) {
		return registry.getTypeExpressionOfRegisteredSymbol(symbol);
	}

	@Override
	public Map<Object, Object> getGlobalObjects() {
		return registry.getGlobalObjects();
	}

	@Override
	public AbstractTrivialContext putAllGlobalObjects(Map<Object, Object> objects) {
		AbstractTrivialContext result = clone();
		result.registry = result.registry.putAllGlobalObjects(objects);
		return result;
	}

	@Override
	public AbstractTrivialContext putGlobalObject(Object key, Object value) {
		AbstractTrivialContext result = clone();
		result.registry = result.registry.putGlobalObject(key, value);
		return result;
	}

	@Override
	public void putInplaceGlobalObject(Object key, Object value) {
		registry.putInplaceGlobalObject(key, value);
	}

	@Override
	public boolean containsGlobalObjectKey(Object key) {
		return registry.containsGlobalObjectKey(key);
	}
	
	@Override
	public Object getGlobalObject(Object key) {
		return registry.getGlobalObject(key);
	}

	@Override
	public AbstractTrivialContext makeNewContextWithAddedType(Type type) {
		AbstractTrivialContext result = clone();
		result.registry = result.registry.makeNewContextWithAddedType(type);
		return result;
	}

	@Override
	public Type getType(String name) {
		Type result = registry.getType(name);
		return result;
	}

	@Override
	public Type getTypeFromTypeExpression(Expression typeExpression) {
		Type result = registry.getTypeFromTypeExpression(typeExpression);
		return result;
	}

	@Override
	public Collection<Type> getTypes() {
		return registry.getTypes();
	}

	@Override
	public AbstractTrivialContext makeCloneWithAdditionalRegisteredSymbolsAndTypes(Map<Expression, Expression> symbolsAndTypes) {
		AbstractTrivialContext result = clone();
		result.registry = result.registry.makeCloneWithAdditionalRegisteredSymbolsAndTypes(symbolsAndTypes);
		return result;
	}

	// END-Registry
	//

	//
	// START-Context
	
	@Override
	public Expression binding(Expression variable) {
		return null;
	}

	@Override
	abstract public Context conjoinWithLiteral(Expression literal, Context context);

	@Override
	abstract public boolean isContradiction();

	@Override
	abstract public Context makeContradiction();

	// END-Context
	//

	//
	// START-AbstractExpressionWrapper
	
	@Override
	abstract protected Expression computeInnerExpression();

	// END-AbstractExpressionWrapper
	//

}
