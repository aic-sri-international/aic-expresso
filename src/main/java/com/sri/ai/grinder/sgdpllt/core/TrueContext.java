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

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.parse;
import static com.sri.ai.grinder.helper.GrinderUtil.fromTypeExpressionToItsIntrinsicMeaning;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeOfFunctor;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.myAssert;

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
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.core.PrologConstantPredicate;
import com.sri.ai.grinder.sgdpllt.api.Constraint;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.constraint.CompleteMultiVariableContext;
import com.sri.ai.grinder.sgdpllt.library.IsVariable;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * An implementation of {@link Context} extending a {@link DefaultRegistry}
 * with a {@link TRUE} constraint.
 * <p>
 * By default, the
 * predicate indicating variables uses {@link PrologVariableConvention}.
 * 
 * @author braz
 * @author oreilly
 */
@Beta
public class TrueContext extends AbstractExpressionWrapper implements Context {
	
	private static final long serialVersionUID = 1L;

	private Theory theory;

	private Registry registry;
	
	private Map<Expression, Type> fromTypeExpressionToType;

	//
	// START - Constructors

	public TrueContext() {
		this(
				null,
				new LinkedHashMap<Expression, Expression>(),
				new PrologConstantPredicate(), // symbolsAndTypes
				new LinkedHashMap<Object, Object>()); // globalObjects
	}

	public TrueContext(Theory theory) {
		this(
				theory,
				new LinkedHashMap<Expression, Expression>(),
				new PrologConstantPredicate(), // symbolsAndTypes
				new LinkedHashMap<Object, Object>()); // globalObjects
	}
	
	public TrueContext(Theory theory, Map<Object, Object> globalObjects) {
		this(
				theory,
				new LinkedHashMap<Expression, Expression>(),
				new PrologConstantPredicate(), 
				globalObjects);
	}

	public TrueContext(
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
		//
		this.fromTypeExpressionToType = map();
	}
	
	/**
	 * Creates a {@link TrueContext} containing the basic information
	 * from another context.
	 * The basic information are the theory, symbols and types, is unique constant predicate,
	 * and global objects.
	 * Uses {@link #TrueContext(Theory, Map, Predicate, Map)}.
	 * @param another
	 */
	public TrueContext(Context another) {
		this(
				another.getTheory(), 
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
	public TrueContext clone() {
		TrueContext result = null;
		try {
			result = (TrueContext) super.clone();
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
		boolean result = IsVariable.isVariable(expression, getIsUniquelyNamedConstantPredicate());
		return result;
	}

	@Override
	public Predicate<Expression> getIsUniquelyNamedConstantPredicate() {
		return registry.getIsUniquelyNamedConstantPredicate();
	}

	@Override
	public TrueContext setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate) {
		TrueContext result = clone();
		result.registry = registry.setIsUniquelyNamedConstantPredicate(isUniquelyNamedConstantPredicate);
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
	public Registry setSymbolsAndTypes(Map<Expression, Expression> newSymbolsAndTypes) {
		TrueContext result = clone();
		result.registry = registry.setSymbolsAndTypes(newSymbolsAndTypes);
		return result;
	}

	@Override
	public Expression getTypeOfRegisteredSymbol(Expression symbol) {
		return registry.getTypeOfRegisteredSymbol(symbol);
	}

	@Override
	public Map<Object, Object> getGlobalObjects() {
		return registry.getGlobalObjects();
	}

	@Override
	public TrueContext putAllGlobalObjects(Map<Object, Object> objects) {
		TrueContext result = clone();
		result.registry = registry.putAllGlobalObjects(objects);
		return result;
	}

	@Override
	public TrueContext putGlobalObject(Object key, Object value) {
		TrueContext result = clone();
		result.registry = registry.putGlobalObject(key, value);
		return result;
	}

	@Override
	public boolean containsGlobalObjectKey(Object key) {
		return registry.containsGlobalObjectKey(key);
	}
	
	@Override
	public Object getGlobalObject(Object key) {
		return registry.getGlobalObject(key);
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
	public TrueContext add(Type type) {
		TrueContext result = clone();
		String name = type.getName();
		Expression typeExpression = parse(name);
		LinkedHashMap<Expression, Type> additionalTypeMap = map(typeExpression, type);
		result.fromTypeExpressionToType = new StackedHashMap<>(additionalTypeMap, fromTypeExpressionToType);
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
	public TrueContext registerIndicesAndTypes(
			Map<Expression, Expression> expressionsAndTypes) {
		if (expressionsAndTypes.isEmpty()) { // nothing to do
			return this;
		}
		
		Map<Expression, Expression> newSymbolsAndTypes = 
				createNewSymbolsAndTypes(expressionsAndTypes);
		
		TrueContext result = clone();
		result.registry = registry.setSymbolsAndTypes(newSymbolsAndTypes);
		
		return result;
	}

	private Map<Expression, Expression> createNewSymbolsAndTypes(Map<Expression, Expression> expressionsAndTypes) {
		Map<Expression, Expression> symbolsAndTypes = 
				getTypesOfIndicesFunctorsOrSymbols(expressionsAndTypes); // returns a fresh map, so we can use it below without copying
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

	@Override
	public Theory getTheory() {
		myAssert( 
				() -> theory != null, 
				() -> "Trying to obtain a theory from a " + TrueContext.class + " instance without one.");
		return theory;
	}

	private CompleteMultiVariableContext makeTrueConstraint(Theory theory) {
		CompleteMultiVariableContext result = new CompleteMultiVariableContext(theory, this);
		return result;
	}
	
	private Theory theoryToUse(Expression conjoinant) {
		Theory result;
		if (theory != null) {
			result = theory;
		}
		else if (conjoinant instanceof Constraint) {
			result = ((Constraint) conjoinant).getTheory();
		}
		else {
			throw new Error("Conjoining with default context but there is no theory available");
		}
		return result;
	}
	
	@Override
	public Context conjoin(Expression formula, Context context) {
		Context result = 
				makeTrueConstraint(theoryToUse(formula))
				.conjoin(formula, context);
		return result;
	}

	@Override
	public Context conjoinWithConjunctiveClause(Expression conjunctiveClause, Context context) {
		Context result = 
				makeTrueConstraint(theoryToUse(conjunctiveClause))
				.conjoinWithConjunctiveClause(conjunctiveClause, context);
		return result;
	}

	@Override
	public Context conjoinWithLiteral(Expression literal, Context context) {
		Context result = 
				makeTrueConstraint(theoryToUse(literal))
				.conjoinWithLiteral(literal, context);
		return result;
	}

	@Override
	public Expression binding(Expression variable) {
		return null;
	}

	@Override
	public boolean isContradiction() {
		return false;
	}

	@Override
	public Context makeContradiction() {
		if (theory == null) {
			throw new Error("Should not be making a contradiction out of a TrueContext without a constraint");
		}
		Context result = makeTrueConstraint(theory).makeContradiction();
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		return TRUE;
	}
}
