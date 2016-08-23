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

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.sri.ai.expresso.ExpressoConfiguration;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndSyntacticContext;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.SyntaxLeaf;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.core.AbstractNonQuantifiedExpression;
import com.sri.ai.grinder.sgdpll.api.Context;
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
	
	private SyntaxLeaf syntxLeaf;

	@Override
	public Object getValue() {
		return syntxLeaf.getValue();
	}
	
	@Override
	public boolean isStringLiteral() {
		return syntxLeaf.isStringLiteral();
	}

	@Override
	public Iterator<ExpressionAndSyntacticContext> getImmediateSubExpressionsAndContextsIterator() {
		return Util.iterator();
	}

	@Override
	public Object getSyntacticFormType() {
		return "Symbol";
	}

	@Override
	public SyntaxTree getSyntaxTree() {
		return syntxLeaf;
	}

	@Override
	public Expression replaceSymbol(Expression symbol, Expression newSymbol, Context context) {
		Expression result = this;
		if (this.equals(symbol)) {
			result = newSymbol;
		}
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
	public long longValue() {
		if (getSyntaxTree().getLabel() instanceof Number) {
			return ((Number) getSyntaxTree().getLabel()).longValue();
		}
		throw new Error("Expression.longValue() invoked on " + this + ", which is not a number.");
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
		// throw new Error("Expression.rationalValue() invoked on " + this + ", which is not a number.");
		return null; // returning null for consistency with rationalValue() behavior for other expressions
	}
	
	public static Symbol createSymbol(Object value) {
		return createSymbol(value, false);
	}

	public static Symbol createSymbol(Object value, boolean isStringLiteral) {
		Symbol result = null;
		// If global symbol table to be used and the symbol's value is not
		// an expression - i.e. quoted expressions of the form:
		// <X>
		// as these need to be instantiated each time in order to be
		// parsed correctly.
		if (useGlobalSymbolTable && !(value instanceof Expression)) {
			
			if (isStringLiteral) {
				result = globalStringLiteralTable.getIfPresent(value);
			}
			else {
				result = globalSymbolTable.getIfPresent(value);
			}
			if (result == null) {
				result = new DefaultSymbol(value, isStringLiteral);
				if (!(!cacheNumericSymbols && result.getValue() instanceof Number)) {
					if (isStringLiteral) {
						globalStringLiteralTable.put(value, result);
					}
					else {
						globalSymbolTable.put(value, result);
					}
				}
			}
		} 
		else {
			result = new DefaultSymbol(value, isStringLiteral);
		}
		
		return result;
	}

	// Note: End users can only instantiate Symbols via the factory method.
	private DefaultSymbol(Object value, boolean isStringLiteral) {	
		syntxLeaf = DefaultSyntaxLeaf.createSyntaxLeaf(value, isStringLiteral);
	}

	public static void flushGlobalSymbolTable() {
		if (AICUtilConfiguration.isRecordCacheStatistics()) {
			System.out.println("Global Symbol Table Cache Stats="+globalSymbolTable.stats());
			System.out.println("Global String Literal Table Cache Stats="+globalStringLiteralTable.stats());
		}
		// Causes relevant flags to be reset.
		useGlobalSymbolTable = ExpressoConfiguration.isUseGlobalSymbolTable();
		cacheNumericSymbols  = ExpressoConfiguration.isGlobalSymbolTableToCacheNumerics();
		
		if (globalSymbolTable != null) {
			globalSymbolTable.invalidateAll();
		}
		if (globalStringLiteralTable != null) {
			globalStringLiteralTable.invalidateAll();
		}
		globalSymbolTable        = newSymbolTable();
		globalStringLiteralTable = newSymbolTable();
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

	private static Cache<Object, Symbol>        globalSymbolTable         = newSymbolTable();
	private static Cache<Object, Symbol>        globalStringLiteralTable  = newSymbolTable();
	
	static {
		flushGlobalSymbolTable();
	}

	@Override
	public String makeToString() {
		return getSyntaxTree().toString();
	}

	@Override
	public Expression getFunctor() {
		return null;
	}

	@Override
	public List<Expression> getArguments() {
		 return Collections.emptyList();
	}

	@Override
	public Expression set(int i, Expression newIthArgument) {
		throw new Error("set(int, Expression) not defined for " + getClass());
	}
}
