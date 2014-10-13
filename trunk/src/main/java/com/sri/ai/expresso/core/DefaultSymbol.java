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
package com.sri.ai.expresso.core;

import static java.util.Collections.emptyList;

import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxLeaf;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractNonQuantifiedExpression;
import com.sri.ai.util.AICUtilConfiguration;
import com.sri.ai.util.Util;
import com.sri.ai.util.math.Rational;

/**
 * An (atomic) {@link Expression} representing a symbol.
 * 
 * @author braz
 */
@Beta
public class DefaultSymbol extends AbstractNonQuantifiedExpression implements Symbol {
	private static final long serialVersionUID = 1L;
	
	private Object value;
	
	// this field is merely a cache; this Expression class is not based on syntax trees like previous ones did; it merely provides a syntax tree when requested.
	private SyntaxLeaf cachedSyntaxTree;

	@Override
	public Object getValue() {
		return value;
	}

	/**
	 * Even though {@link Symbol} is not a {@link FunctionApplication} and does not have arguments,
	 * we offer this method (returning an empty list) for convenience,
	 * because it is often the case that we want to treat function applications and symbols uniformly
	 * and talk about their arguments,
	 * and testing every time whether an expression is a symbol becomes cumbersome.
	 */
	@Override
	public List<Expression> getArguments() {
		return emptyList();
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(RewritingProcess process) {
		return Util.iterator();
	}

	@Override
	public Object getSyntacticFormType() {
		return "Symbol";
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return cachedSyntaxTree;
	}

	@Override
	public Expression renameSymbol(Expression symbol, Expression newSymbol, RewritingProcess process) {
		Expression result = this;
		if (this.equals(symbol)) {
			result = newSymbol;
		}
		return result;
	}

	//
	private int hashCode = -1; // lazy init and re-use the calculated hashCode.

	@Override
	public int hashCode() {
		if (hashCode == -1) {
			hashCode = getValue().hashCode();
		}
		return hashCode;
	}

	/**
	 * The semantics of comparison of expressions based on Symbols 
	 * is the comparison of the underlying syntax trees.
	 * If the other object is a syntax tree itself, it is considered its own underlying syntax tree.
	 * If it is not an Expression or a SyntaxTree, a Symbol is built on it and used.
	 */
	@Override
	public boolean equals(Object another) {
		if (this == another) {
			return true;
		}
	
		SyntaxTree anotherSyntaxTree;
		
		if (another instanceof SyntaxTree) {
			anotherSyntaxTree = (SyntaxTree) another;
		}
		else if (another instanceof Expression) {
			anotherSyntaxTree = ((Expression) another).getSyntaxTree();
		}
		else {
			anotherSyntaxTree = SyntaxTrees.makeSyntaxLeaf(another);
		}
		
		boolean result = getSyntaxTree().equals(anotherSyntaxTree);
		return result;
	}

	@Override
	public Expression clone() {
		return Expressions.makeSymbol(getValue());
	}

	@Override
	public boolean booleanValue() {
		if (getSyntaxTree().getLabel() instanceof Boolean) {
			return ((Boolean) getSyntaxTree().getLabel()).booleanValue();
		}
		throw new Error("Expression.intValue() invoked on " + this + ", which is not a boolean.");
	}

	@Override
	public int intValue() {
		if (getSyntaxTree().getLabel() instanceof Number) {
			return ((Number) getSyntaxTree().getLabel()).intValue();
		}
		throw new Error("Expression.intValue() invoked on " + this + ", which is not a number.");
	}

	@Override
	public int intValueExact() throws ArithmeticException {
		if (getSyntaxTree().getLabel() instanceof Rational) {
			return ((Rational) getSyntaxTree().getLabel()).intValueExact();
		}
		throw new Error("Expression.intValueExact() invoked on " + this + ", which is not a number.");
	}

	@Override
	public double doubleValue() {
		if (getSyntaxTree().getLabel() instanceof Number) {
			return ((Number) getSyntaxTree().getLabel()).doubleValue();
		}
		throw new Error("Expression.doubleValue() invoked on " + this + ", which is not a number.");
	}

	@Override
	public Rational rationalValue() {
		if (getSyntaxTree().getLabel() instanceof Number) {
			return (Rational) getSyntaxTree().getLabel();
		}
		throw new Error("Expression.rationalValue() invoked on " + this + ", which is not a number.");
	}

	public static Symbol createSymbol(Object value) {
		Symbol result = null;
		// If global symbol table to be used and the symbol's value is not
		// an expression - i.e. quoted expressions of the form:
		// <X>
		// as these need to be instantiated each time in order to be
		// parsed correctly.
		if (useGlobalSymbolTable && !(value instanceof Expression)) {
			
			result = globalSymbolTable.getIfPresent(value);
			if (result == null) {
				result = new DefaultSymbol(value);
				if (!(!cacheNumericSymbols && result.getValue() instanceof Number)) {
					globalSymbolTable.put(value, result);
				}
			}
		} 
		else {
			result = new DefaultSymbol(value);
		}
		
		return result;
	}

	// Note: End users can only instantiate Symbols via the factory method.
	private DefaultSymbol(Object value) {
		
		if (value instanceof Number && !(value instanceof Rational)) {
			value = new Rational(((Number)value).doubleValue());
		} 
		else if (value.equals("true")) {
			value = Boolean.TRUE;
		} 
		else if (value.equals("false")) {
			value = Boolean.FALSE;
		} 
		else if (value instanceof String) {
			try {
				value = new Rational((String)value);
			}
			catch (NumberFormatException e) {
				// ignore
			}
		}
	
		cachedSyntaxTree = DefaultSyntaxLeaf.createSyntaxLeaf(value);
		this.value = value;
	}

	public static void flushGlobalSymbolTable() {
		if (AICUtilConfiguration.isRecordCacheStatistics()) {
			System.out.println("Global Symbol Table Cache Stats="+globalSymbolTable.stats());
		}
		// Causes relevant flags to be reset.
		useGlobalSymbolTable = ExpressoConfiguration.isUseGlobalSymbolTable();
		cacheNumericSymbols  = ExpressoConfiguration.isGlobalSymbolTableToCacheNumerics();
		
		if (globalSymbolTable != null) {
			globalSymbolTable.invalidateAll();
		}
		globalSymbolTable = newSymbolTable();
		// Add well known symbols to the table
		// The Booleans.
		globalSymbolTable.put(true,          SYMBOL_TRUE);
		globalSymbolTable.put(Boolean.TRUE,  SYMBOL_TRUE);
		globalSymbolTable.put("true",        SYMBOL_TRUE);
		globalSymbolTable.put(false,         SYMBOL_FALSE);
		globalSymbolTable.put(Boolean.FALSE, SYMBOL_FALSE);
		globalSymbolTable.put("false",       SYMBOL_FALSE);
		// Common Numbers
		globalSymbolTable.put("0",             SYMBOL_0);
		globalSymbolTable.put(new Integer(0),  SYMBOL_0);
		globalSymbolTable.put(new Rational(0), SYMBOL_0);
		globalSymbolTable.put("1",             SYMBOL_1);
		globalSymbolTable.put(new Integer(1),  SYMBOL_1);
		globalSymbolTable.put(new Rational(1), SYMBOL_1);
		globalSymbolTable.put("2",             SYMBOL_2);
		globalSymbolTable.put(new Integer(2),  SYMBOL_2);
		globalSymbolTable.put(new Rational(2), SYMBOL_2);
		globalSymbolTable.put("3",             SYMBOL_3);
		globalSymbolTable.put(new Integer(3),  SYMBOL_3);
		globalSymbolTable.put(new Rational(3), SYMBOL_3);
		globalSymbolTable.put("4",             SYMBOL_4);
		globalSymbolTable.put(new Integer(4),  SYMBOL_4);
		globalSymbolTable.put(new Rational(4), SYMBOL_4);
		globalSymbolTable.put("5",             SYMBOL_5);
		globalSymbolTable.put(new Integer(5),  SYMBOL_5);
		globalSymbolTable.put(new Rational(5), SYMBOL_5);
		globalSymbolTable.put("6",             SYMBOL_6);
		globalSymbolTable.put(new Integer(6),  SYMBOL_6);
		globalSymbolTable.put(new Rational(6), SYMBOL_6);
		globalSymbolTable.put("7",             SYMBOL_7);
		globalSymbolTable.put(new Integer(7),  SYMBOL_7);
		globalSymbolTable.put(new Rational(7), SYMBOL_7);
		globalSymbolTable.put("8",             SYMBOL_8);
		globalSymbolTable.put(new Integer(8),  SYMBOL_8);
		globalSymbolTable.put(new Rational(8), SYMBOL_8);
		globalSymbolTable.put("9",             SYMBOL_9);
		globalSymbolTable.put(new Integer(9),  SYMBOL_9);
		globalSymbolTable.put(new Rational(9), SYMBOL_9);
	}

	private static Cache<Object, Symbol> newSymbolTable() {
		CacheBuilder<Object, Object> cb = CacheBuilder.newBuilder();
		
		long maximumSize = ExpressoConfiguration.getGlobalSymbolTableMaximumSize();
		// Note: a maximumSize of 
		// < 0 means no size restrictions
		// = 0 means no cache
		// > 0 means maximum size of cache
		if (maximumSize >= 0L) {			
			cb.maximumSize(maximumSize);
		}
		if (AICUtilConfiguration.isRecordCacheStatistics()) {
			cb.recordStats();
		}
		
		Cache<Object, Symbol> result = cb.build();
		
		return result;
	}

	//
	// Commonly used symbol expressions
	private static final Symbol SYMBOL_TRUE  = Expressions.makeSymbol(true);

	private static final Symbol SYMBOL_FALSE = Expressions.makeSymbol(false);

	private static final Symbol SYMBOL_0     = Expressions.makeSymbol(new Rational(0));

	private static final Symbol SYMBOL_1     = Expressions.makeSymbol(new Rational(1));

	private static final Symbol SYMBOL_2     = Expressions.makeSymbol(new Rational(2));

	private static final Symbol SYMBOL_3     = Expressions.makeSymbol(new Rational(3));

	private static final Symbol SYMBOL_4     = Expressions.makeSymbol(new Rational(4));

	private static final Symbol SYMBOL_5     = Expressions.makeSymbol(new Rational(5));

	private static final Symbol SYMBOL_6     = Expressions.makeSymbol(new Rational(6));

	private static final Symbol SYMBOL_7     = Expressions.makeSymbol(new Rational(7));

	private static final Symbol SYMBOL_8     = Expressions.makeSymbol(new Rational(8));

	private static final Symbol SYMBOL_9     = Expressions.makeSymbol(new Rational(9));

	//
	private static boolean                      useGlobalSymbolTable = ExpressoConfiguration.isUseGlobalSymbolTable();

	private static boolean                      cacheNumericSymbols  = ExpressoConfiguration.isGlobalSymbolTableToCacheNumerics();

	private static Cache<Object, Symbol>        globalSymbolTable    = newSymbolTable();

	static {
		flushGlobalSymbolTable();
	}

	@Override
	public String makeToString() {
		return getSyntaxTree().toString();
	}
}
