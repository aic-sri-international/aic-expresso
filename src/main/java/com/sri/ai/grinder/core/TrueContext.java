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
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeOfFunctor;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.RESULT;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.code;
import static com.sri.ai.util.explanation.logging.api.ThreadExplanationLogger.explanationBlock;

import java.util.Collection;
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
import com.sri.ai.grinder.api.Constraint;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Registry;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.constraint.CompleteMultiVariableContext;
import com.sri.ai.grinder.library.IsVariable;
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
	
	//
	// START - Constructors

	public TrueContext() {
		this(
				null,
				new LinkedHashMap<Expression, Expression>(), // symbolsAndTypes
				new PrologConstantPredicate(), 
				new LinkedHashMap<Object, Object>()); // globalObjects
	}

	public TrueContext(Theory theory) {
		this(
				theory,
				new LinkedHashMap<Expression, Expression>(), // symbolsAndTypes
				new PrologConstantPredicate(),
				new LinkedHashMap<Object, Object>()); // globalObjects
	}
	
	public TrueContext(Theory theory, Map<Object, Object> globalObjects) {
		this(
				theory,
				new LinkedHashMap<Expression, Expression>(), // symbolsAndTypes
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
		boolean result = IsVariable.isVariable(expression, getIsUniquelyNamedConstantPredicate(), getTypes(), getTheory());
		return result;
	}

	@Override
	public Predicate<Expression> getIsUniquelyNamedConstantPredicate() {
		return registry.getIsUniquelyNamedConstantPredicate();
	}

	@Override
	public TrueContext setIsUniquelyNamedConstantPredicate(Predicate<Expression> isUniquelyNamedConstantPredicate) {
		TrueContext result = clone();
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
	public Registry setSymbolsAndTypes(Map<Expression, Expression> newSymbolsAndTypes) {
		TrueContext result = clone();
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
	public TrueContext putAllGlobalObjects(Map<Object, Object> objects) {
		TrueContext result = clone();
		result.registry = result.registry.putAllGlobalObjects(objects);
		return result;
	}

	@Override
	public TrueContext putGlobalObject(Object key, Object value) {
		TrueContext result = clone();
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

	// END-Context
	//
	
	//
	//  PROTECTED METHODS
	//
	
	//
	// PRIVATE METHODS
	//
	
//	@Override
//	public String toString() {
//		return "Context with: " + getSymbolsAndTypes();
//	}

	@Override
	public TrueContext makeNewContextWithAddedType(Type type) {
		TrueContext result = clone();
		result.registry = result.registry.makeNewContextWithAddedType(type);
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
		Type result = registry.getTypeFromTypeExpression(typeExpression);
		return result;
	}

	@Override
	public Collection<Type> getTypes() {
		return registry.getTypes();
	}

	@Override
	public TrueContext makeCloneWithAdditionalRegisteredSymbolsAndTypes(
			Map<Expression, Expression> symbolsAndTypes) {
		if (symbolsAndTypes.isEmpty()) { // nothing to do
			return this;
		}
		
		Map<Expression, Expression> newSymbolsAndTypes = 
				createAugmentedSymbolsAndTypes(symbolsAndTypes);
		
		TrueContext result = clone();
		result.registry = result.registry.setSymbolsAndTypes(newSymbolsAndTypes);
		
		return result;
	}

	private Map<Expression, Expression> createAugmentedSymbolsAndTypes(Map<Expression, Expression> additionalSymbolsAndTypes) {
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

	@Override
	public Theory getTheory() {
		return theory;
	}

	private CompleteMultiVariableContext makeTrueCompleteMultiVariableContext(Theory theory) {
		return explanationBlock("Making CompleteMultiVariableContext", code(() -> {
			CompleteMultiVariableContext result = new CompleteMultiVariableContext(theory, this);
			return result;
		}), "Made ", RESULT);
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
				makeTrueCompleteMultiVariableContext(theoryToUse(formula))
				.conjoin(formula, context);
		
//		Context result2 = Context.super.conjoin(formula, context);
//		result = DebuggingComparativeContextInvocationHandler.makeComparativeContext(result, result2);
//		if (!result.toString().equals(result2.toString())) {
//			println("Divergence in context constraints!");
//			println("original: " + context);
//			println("formula: " + formula);
//			println("result: " + result);
//			println("result: " + result2);
//			System.exit(-1);
//		}
//		
//		if (result.isContradiction() != result2.isContradiction()) {
//			println("Divergence in isContradiction!");
//			println("original: " + context);
//			println("formula: " + formula);
//			println("result: " + result);
//			println("result: " + result2);
//			System.exit(-1);
//		}
		
		return result;
	}

//	@Override
//	public Context conjoinWithConjunctiveClause(Expression conjunctiveClause, Context context) {
//		Context result = 
//				makeTrueCompleteMultiVariableContext(theoryToUse(conjunctiveClause))
//				.conjoinWithConjunctiveClause(conjunctiveClause, context);
//		return result;
//	}

	@Override
	public Context conjoinWithLiteral(Expression literal, Context context) {
		Context result = 
				makeTrueCompleteMultiVariableContext(theoryToUse(literal))
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
			throw new Error("Should not be making a contradiction out of a TrueContext without a theory");
		}
		Context result = makeTrueCompleteMultiVariableContext(theory).makeContradiction();
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		return TRUE;
	}
}
