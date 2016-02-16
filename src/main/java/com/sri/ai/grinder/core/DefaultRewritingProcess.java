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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.fromTypeExpressionToItsIntrinsicMeaning;
import static com.sri.ai.grinder.library.FunctorConstants.CARDINALITY;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.myAssert;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * A default implementation of {@link RewritingProcess}. By default, the
 * predicate indicating variables uses {@link PrologVariableConvention}.
 * 
 * @author braz
 * @author oreilly
 */
@Beta
public class DefaultRewritingProcess implements RewritingProcess {
	
	private int recursionLevel = 0;
	
	private Map<Expression, Expression>  contextualSymbolsAndTypes = null;
	private Map<Expression, Type> types = new LinkedHashMap<Expression, Type>();

	private Expression contextualConstraint = Expressions.TRUE;
	
	private Predicate<Expression> isUniquelyNamedConstantPredicate = null;

	private Map<Object, Object> globalObjects = null;
	
	//
	// START - Constructors

	public DefaultRewritingProcess() {
		this(
				new LinkedHashMap<Expression, Expression>(), // contextualSymbolsAndTypes
				new PrologConstantPredicate(), 
				new LinkedHashMap<Object, Object>()); // globalObjects
	}
	
	public DefaultRewritingProcess(Map<Object, Object> globalObjects) {
		this(
				new LinkedHashMap<Expression, Expression>(), 
				new PrologConstantPredicate(), 
				globalObjects);
	}

	public DefaultRewritingProcess(
			Map<Expression, Expression> contextualSymbolsAndTypes,
			Predicate<Expression> isUniquelyNamedConstantPredicate,
			Map<Object, Object> globalObjects) {
		
		initialize(null,
				contextualSymbolsAndTypes, 
				Expressions.TRUE,
				isUniquelyNamedConstantPredicate,
				new ConcurrentHashMap<Object, Object>(globalObjects),
				map());
	}

	// END-Constructors
	//
	
	//
	// START-RewritingProcess
	@Override
	public boolean isUniquelyNamedConstant(Expression expression) {
		return getIsUniquelyNamedConstantPredicate().apply(expression);
	}
	
	@Override
	public boolean isVariable(Expression expression) {
		boolean result = IsVariable.isVariable(expression,
				isUniquelyNamedConstantPredicate);
		return result;
	}

	@Override
	public Predicate<Expression> getIsUniquelyNamedConstantPredicate() {
		return isUniquelyNamedConstantPredicate;
	}

	@Override
	public RewritingProcess setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate) {
		DefaultRewritingProcess result = clone();
		result.isUniquelyNamedConstantPredicate = isUniquelyNamedConstantPredicate;
		return result;
	}

	@Override
	public Set<Expression> getContextualSymbols() {
		return contextualSymbolsAndTypes.keySet();
	}

	@Override
	public Map<Expression, Expression> getContextualSymbolsAndTypes() {
		return contextualSymbolsAndTypes;
	}

	@Override
	public Expression getContextualSymbolType(Expression variable) {
		return contextualSymbolsAndTypes.get(variable);
	}

	@Override
	public Expression getContextualConstraint() {
		return contextualConstraint;
	}

	@Override
	public RewritingProcess newSubProcessWithContext(
			Map<Expression, Expression> subProcesscontextualSymbolsAndTypes, 
			Expression subProcessContextualConstraint) {

		DefaultRewritingProcess result = 
				new DefaultRewritingProcess(
						this, 
						subProcesscontextualSymbolsAndTypes,
						subProcessContextualConstraint);
		
		return result;
	}

	@Override
	public RewritingProcess extendGlobalObjects(Map<Object, Object> objects, RewritingProcess process) {
		myAssert(() -> process instanceof DefaultRewritingProcess, () -> "Not implemented for other implementations of " + RewritingProcess.class);
		DefaultRewritingProcess result = new DefaultRewritingProcess((DefaultRewritingProcess) process);
		Map<Object, Object> newGlobalObjects = new StackedHashMap<>(objects, result.getGlobalObjects());
		result.globalObjects = newGlobalObjects;
		return result;
	}

	@Override
	public Map<Object, Object> getGlobalObjects() {
		return globalObjects;
	}

	@Override
	public DefaultRewritingProcess putGlobalObject(Object key, Object value) {
		DefaultRewritingProcess result = clone();
		Map<Object, Object> newGlobalObjects = new StackedHashMap<>(map(key, value), result.getGlobalObjects());
		result.globalObjects = newGlobalObjects;
		return result;
	}
	
	@Override
	public boolean containsGlobalObjectKey(Object key) {
		return globalObjects.containsKey(key);
	}
	
	@Override
	public Object getGlobalObject(Object key) {
		return globalObjects.get(key);
	}

	@Override
	public int getRecursionLevel() {
		return recursionLevel;
	}
	
	// END-RewritingProcess
	//
	
	//
	//  PROTECTED METHODS
	//
	
	//
	// PRIVATE METHODS
	//
	
	// Note: private constructors for sub-processes			                        
	private DefaultRewritingProcess(
			DefaultRewritingProcess parentProcess,
			Map<Expression, Expression> contextualSymbolsAndTypes,
			Expression contextualConstraint) {
		initialize(
				parentProcess,
				contextualSymbolsAndTypes,
				contextualConstraint,
				parentProcess.isUniquelyNamedConstantPredicate,
				parentProcess.globalObjects,
				parentProcess.types
				);
		
	}

	/** A copy constructor with clean contextual constraint and clean caches. */
	private DefaultRewritingProcess(DefaultRewritingProcess process) {
		initialize(
				null, // parentProcess,
				process.getContextualSymbolsAndTypes(),
				Expressions.TRUE,
				process.getIsUniquelyNamedConstantPredicate(),
				process.getGlobalObjects(),
				process.types);
	}
	
	
	@Override
	public DefaultRewritingProcess clone() {
		return new DefaultRewritingProcess(this);
	}
	
	private void initialize(
			DefaultRewritingProcess parentProcess,
			Map<Expression, Expression> contextualSymbolsAndTypes,
			Expression contextualConstraint,
			Predicate<Expression> isUniquelyNamedConstantPredicate,
			Map<Object, Object> globalObjects,
			Map<Expression, Type> types) {
		
		if (parentProcess != null) {
			recursionLevel = parentProcess.getRecursionLevel() + 1;
		}
		this.contextualSymbolsAndTypes = contextualSymbolsAndTypes;
		this.contextualConstraint = contextualConstraint;
		this.isUniquelyNamedConstantPredicate = isUniquelyNamedConstantPredicate;
		//
		this.globalObjects = globalObjects;
		//
		this.types = types;
	}
	
	@Override
	public String toString() {
		return "Rewriting process with context " + getContextualSymbolsAndTypes();
	}

	@Override
	public RewritingProcess newRewritingProcessWith(Type type) {
		DefaultRewritingProcess result = new DefaultRewritingProcess(this);
		result.types = new LinkedHashMap<>(result.types);
		result.types.put(parse(type.getName()), type);
		Expression unknownTypeSize = apply(CARDINALITY, type.getName());
		if ( ! type.cardinality().equals(unknownTypeSize)) { // the reason for this test is not storing two equal but distinct instances in case some code replaces one by the other, creating a new expression that is equal but not the same instance, which we assume throughout expresso not to happen
			result = result.putGlobalObject(unknownTypeSize, type.cardinality());
		}
		return result;
	}

	@Override
	public Type getType(String name) {
		Expression typeExpression = parse(name);
		Type result = getType(typeExpression);
		return result;
	}

	@Override
	public Type getType(Expression typeExpression) {
		Type result = types.get(typeExpression);
		if (result == null) {
			result = fromTypeExpressionToItsIntrinsicMeaning(typeExpression);
		}
		return result;
	}

	@Override
	public Collection<Type> getTypes() {
		return Collections.unmodifiableCollection(types.values());
	}
}
