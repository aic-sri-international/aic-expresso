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

import com.google.common.annotations.Beta;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.core.AbstractSyntaxTreeBasedExpression;
import com.sri.ai.util.AICUtilConfiguration;
import com.sri.ai.util.math.Rational;

/**
 * An Expression based on {@link Symbol} syntax trees.
 * 
 * @author braz
 */
@Beta
public class ExpressionOnSymbol extends AbstractSyntaxTreeBasedExpression {
	private static final long serialVersionUID = 1L;
	
	public ExpressionOnSymbol(SyntaxTree syntaxTree) {
		this.syntaxTree = syntaxTree;
	}
	
	public static Expression createSymbol(Object value) {
		Expression result = null;
		// If global symbol table to be used and the symbol's value is not
		// an expression - i.e. quoted expressions of the form:
		// <X>
		// as these need to be instantiated each time in order to be
		// parsed correctly.
		if (_useGlobalSymbolTable && !(value instanceof Expression)) {
			
			result = _globalSymbolTable.getIfPresent(value);
			if (result == null) {
				result = new ExpressionOnSymbol(value);
				if (!(!_cacheNumericSymbols && result.getValue() instanceof Number)) {
					_globalSymbolTable.put(value, result);
				}
			}
		} 
		else {
			result = new ExpressionOnSymbol(value);
		}
		
		return result;
	}

	//
	// PRIVATE METHODS
	//
	// Note: Can only instantiate Symbols via the factory method.
	private ExpressionOnSymbol(Object value) {
		
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
	
		syntaxTree = DefaultSymbol.createSymbol(value);
	}

	@Override
	public Object getValue() {
		return getSyntaxTree().getValue();
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
			anotherSyntaxTree = SyntaxTrees.makeSymbol(another);
		}
		
		boolean result = getSyntaxTree().equals(anotherSyntaxTree);
		return result;
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

	@Override
	public Expression clone() {
		return Expressions.makeSymbol(getValue());
	}

	public static void flushGlobalSymbolTable() {
		if (AICUtilConfiguration.isRecordCacheStatistics()) {
			System.out.println("Global Symbol Table Cache Stats="+_globalSymbolTable.stats());
		}
		// Causes relevant flags to be reset.
		_useGlobalSymbolTable = ExpressoConfiguration.isUseGlobalSymbolTable();
		_cacheNumericSymbols  = ExpressoConfiguration.isGlobalSymbolTableToCacheNumerics();
		
		if (_globalSymbolTable != null) {
			_globalSymbolTable.invalidateAll();
		}
		_globalSymbolTable = newSymbolTable();
		// Add well known symbols to the table
		// The Booleans.
		_globalSymbolTable.put(true,          SYMBOL_TRUE);
		_globalSymbolTable.put(Boolean.TRUE,  SYMBOL_TRUE);
		_globalSymbolTable.put("true",        SYMBOL_TRUE);
		_globalSymbolTable.put(false,         SYMBOL_FALSE);
		_globalSymbolTable.put(Boolean.FALSE, SYMBOL_FALSE);
		_globalSymbolTable.put("false",       SYMBOL_FALSE);
		// Common Numbers
		_globalSymbolTable.put("0",             SYMBOL_0);
		_globalSymbolTable.put(new Integer(0),  SYMBOL_0);
		_globalSymbolTable.put(new Rational(0), SYMBOL_0);
		_globalSymbolTable.put("1",             SYMBOL_1);
		_globalSymbolTable.put(new Integer(1),  SYMBOL_1);
		_globalSymbolTable.put(new Rational(1), SYMBOL_1);
		_globalSymbolTable.put("2",             SYMBOL_2);
		_globalSymbolTable.put(new Integer(2),  SYMBOL_2);
		_globalSymbolTable.put(new Rational(2), SYMBOL_2);
		_globalSymbolTable.put("3",             SYMBOL_3);
		_globalSymbolTable.put(new Integer(3),  SYMBOL_3);
		_globalSymbolTable.put(new Rational(3), SYMBOL_3);
		_globalSymbolTable.put("4",             SYMBOL_4);
		_globalSymbolTable.put(new Integer(4),  SYMBOL_4);
		_globalSymbolTable.put(new Rational(4), SYMBOL_4);
		_globalSymbolTable.put("5",             SYMBOL_5);
		_globalSymbolTable.put(new Integer(5),  SYMBOL_5);
		_globalSymbolTable.put(new Rational(5), SYMBOL_5);
		_globalSymbolTable.put("6",             SYMBOL_6);
		_globalSymbolTable.put(new Integer(6),  SYMBOL_6);
		_globalSymbolTable.put(new Rational(6), SYMBOL_6);
		_globalSymbolTable.put("7",             SYMBOL_7);
		_globalSymbolTable.put(new Integer(7),  SYMBOL_7);
		_globalSymbolTable.put(new Rational(7), SYMBOL_7);
		_globalSymbolTable.put("8",             SYMBOL_8);
		_globalSymbolTable.put(new Integer(8),  SYMBOL_8);
		_globalSymbolTable.put(new Rational(8), SYMBOL_8);
		_globalSymbolTable.put("9",             SYMBOL_9);
		_globalSymbolTable.put(new Integer(9),  SYMBOL_9);
		_globalSymbolTable.put(new Rational(9), SYMBOL_9);
	}

	private static Cache<Object, Expression> newSymbolTable() {
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
		
		Cache<Object, Expression> result = cb.build();
		
		return result;
	}

	//
	// Commonly used symbol expressions
	private static final Expression SYMBOL_TRUE  = Expressions.makeSymbol(true);

	private static final Expression SYMBOL_FALSE = Expressions.makeSymbol(false);

	private static final Expression SYMBOL_0     = Expressions.makeSymbol(new Rational(0));

	private static final Expression SYMBOL_1     = Expressions.makeSymbol(new Rational(1));

	private static final Expression SYMBOL_2     = Expressions.makeSymbol(new Rational(2));

	private static final Expression SYMBOL_3     = Expressions.makeSymbol(new Rational(3));

	private static final Expression SYMBOL_4     = Expressions.makeSymbol(new Rational(4));

	private static final Expression SYMBOL_5     = Expressions.makeSymbol(new Rational(5));

	private static final Expression SYMBOL_6     = Expressions.makeSymbol(new Rational(6));

	private static final Expression SYMBOL_7     = Expressions.makeSymbol(new Rational(7));

	private static final Expression SYMBOL_8     = Expressions.makeSymbol(new Rational(8));

	private static final Expression SYMBOL_9     = Expressions.makeSymbol(new Rational(9));

	//
	private static boolean                      _useGlobalSymbolTable = ExpressoConfiguration.isUseGlobalSymbolTable();

	private static boolean                      _cacheNumericSymbols  = ExpressoConfiguration.isGlobalSymbolTableToCacheNumerics();

	private static Cache<Object, Expression>    _globalSymbolTable    = newSymbolTable();

	static {
		flushGlobalSymbolTable();
	}
}
