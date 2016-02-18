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
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.IsVariable;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.core.constraint.CompleteMultiVariableContext;
import com.sri.ai.util.collect.StackedHashMap;

/**
 * An implementation of {@link Context} that contains type and symbol
 * information but no constraint (that is, it stands for the expression {@link TRUE}.
 * <p>
 * By default, the
 * predicate indicating variables uses {@link PrologVariableConvention}.
 * 
 * @author braz
 * @author oreilly
 */
@Beta
public class TypeContext extends AbstractExpressionWrapper implements Context {
	
	private static final long serialVersionUID = 1L;

	private ConstraintTheory constraintTheory;

	private Map<Expression, Expression>  symbolsAndTypes;
	private Map<Expression, Type> fromTypeExpressionToType;

	private Predicate<Expression> isUniquelyNamedConstantPredicate;

	private Map<Object, Object> globalObjects = null;
	
	//
	// START - Constructors

	public TypeContext() {
		this(
				null,
				new LinkedHashMap<Expression, Expression>(),
				new PrologConstantPredicate(), // symbolsAndTypes
				new LinkedHashMap<Object, Object>()); // globalObjects
	}

	public TypeContext(ConstraintTheory constraintTheory) {
		this(
				constraintTheory,
				new LinkedHashMap<Expression, Expression>(),
				new PrologConstantPredicate(), // symbolsAndTypes
				new LinkedHashMap<Object, Object>()); // globalObjects
	}
	
	public TypeContext(ConstraintTheory constraintTheory, Map<Object, Object> globalObjects) {
		this(
				constraintTheory,
				new LinkedHashMap<Expression, Expression>(),
				new PrologConstantPredicate(), 
				globalObjects);
	}

	public TypeContext(
			ConstraintTheory constraintTheory,
			Map<Expression, Expression> symbolsAndTypes,
			Predicate<Expression> isUniquelyNamedConstantPredicate,
			Map<Object, Object> globalObjects) {

		this.constraintTheory = constraintTheory;
		
		this.symbolsAndTypes = symbolsAndTypes;
		this.isUniquelyNamedConstantPredicate = isUniquelyNamedConstantPredicate;
		//
		this.globalObjects = globalObjects;
		//
		this.fromTypeExpressionToType = map();
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
	public TypeContext clone() {
		TypeContext result = null;
		try {
			result = (TypeContext) super.clone();
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
	public TypeContext setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate) {
		TypeContext result = clone();
		result.isUniquelyNamedConstantPredicate = isUniquelyNamedConstantPredicate;
		return result;
	}

	@Override
	public Set<Expression> getRegisteredSymbols() {
		return symbolsAndTypes.keySet();
	}

	@Override
	public Map<Expression, Expression> getSymbolsAndTypes() {
		return symbolsAndTypes;
	}

	@Override
	public Expression getTypeOfRegisteredSymbol(Expression symbol) {
		return symbolsAndTypes.get(symbol);
	}

	@Override
	public Map<Object, Object> getGlobalObjects() {
		return globalObjects;
	}

	@Override
	public TypeContext putAllGlobalObjects(Map<Object, Object> objects) {
		TypeContext result = clone();
		result.globalObjects = new StackedHashMap<>(objects, result.getGlobalObjects());
		return result;
	}

	@Override
	public TypeContext putGlobalObject(Object key, Object value) {
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
	public TypeContext add(Type type) {
		TypeContext result = clone();
		LinkedHashMap<Expression, Type> additionalTypeMap = map(parse(type.getName()), type);
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
			result = fromTypeExpressionToItsIntrinsicMeaning(typeExpression);
		}
		return result;
	}

	@Override
	public Collection<Type> getTypes() {
		return Collections.unmodifiableCollection(fromTypeExpressionToType.values());
	}

	@Override
	public TypeContext registerIndicesAndTypes(
			Map<Expression, Expression> expressionsAndTypes) {
		if (expressionsAndTypes.isEmpty()) { // nothing to do
			return this;
		}
		
		Map<Expression, Expression> newSymbolsAndTypes = 
				createNewSymbolsAndTypes(expressionsAndTypes);
		
		TypeContext result = clone();
		result.symbolsAndTypes = newSymbolsAndTypes;
		
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
			if (index.getSyntacticFormType().equals("Symbol")) {
				result.put(index, indexType);
			}
			else if (index.getSyntacticFormType().equals("Function application")) {
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
	public ConstraintTheory getConstraintTheory() {
		myAssert( 
				() -> constraintTheory != null, 
				() -> "Trying to obtain a constraint theory from a " + TypeContext.class + " instance without one.");
		return constraintTheory;
	}

	private CompleteMultiVariableContext makeTrueConstraint(ConstraintTheory constraintTheory) {
		CompleteMultiVariableContext result = new CompleteMultiVariableContext(constraintTheory, this);
		return result;
	}
	
	private ConstraintTheory constraintTheoryToUse(Expression conjoinant) {
		ConstraintTheory result;
		if (constraintTheory != null) {
			result = constraintTheory;
		}
		else if (conjoinant instanceof Constraint) {
			result = ((Constraint) conjoinant).getConstraintTheory();
		}
		else {
			throw new Error("Conjoining with default context but there is no constraint theory available");
		}
		return result;
	}
	
	@Override
	public Context conjoin(Expression formula, Context context) {
		Context result = 
				makeTrueConstraint(constraintTheoryToUse(formula))
				.conjoin(formula, context);
		return result;
	}

	@Override
	public Context conjoinWithConjunctiveClause(Expression conjunctiveClause, Context context) {
		Context result = 
				makeTrueConstraint(constraintTheoryToUse(conjunctiveClause))
				.conjoinWithConjunctiveClause(conjunctiveClause, context);
		return result;
	}

	@Override
	public Context conjoinWithLiteral(Expression literal, Context context) {
		Context result = 
				makeTrueConstraint(constraintTheoryToUse(literal))
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
		if (constraintTheory == null) {
			throw new Error("Should not be making a contradiction out of a TypeContext without a constraint");
		}
		Context result = makeTrueConstraint(constraintTheory).makeContradiction();
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		return TRUE;
	}
}
