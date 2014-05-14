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
package com.sri.ai.expresso;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.core.ExpressionSyntaxTreeToStringFunction;
import com.sri.ai.util.Configuration;
import com.sri.ai.util.cache.CacheMap;

/**
 * Configuration information for the classes in the expresso package.
 * 
 * @author oreilly
 *
 */
@Beta
public class ExpressoConfiguration extends Configuration {
	public static final String  KEY_DISPLAY_NUMERIC_PRECISION_FOR_SYMBOLS                     = "expresso.display.numeric.precision.for.symbols";
	public static final Integer DEFAULT_VALUE_DISPLAY_NUMERIC_PRECISION_FOR_SYMBOLS           = new Integer(9);
	//
	public static final String  KEY_DISPLAY_SCIENTIFIC_GREATER_N_INTEGER_PLACES               = "expresso.display.scientific.greater.n.integer.places";
	public static final Integer DEFAULT_VALUE_DISPLAY_SCIENTIFIC_GREATER_N_INTEGER_PLACES     = new Integer(40);
	//
	public static final String  KEY_DISPLAY_SCIENTIFIC_AFTER_N_DECIMAL_PLACES                 = "expresso.display.scientific.after.n.decimal.places";
	public static final Integer DEFAULT_VALUE_DISPLAY_SCIENTIFIC_AFTER_N_DECIMAL_PLACES       = new Integer(40);
	//
	public static final String  KEY_USE_GLOBAL_SYMBOL_TABLE                                   = "expresso.use.global.symbol.table";
	public static final Boolean DEFAULT_VALUE_USE_GLOBAL_SYMBOL_TABLE                         = Boolean.TRUE; 
	// Note: < 0 means no limit, 0 means no caching, > 0 means cache to that size.
	public static final String  KEY_GLOBAL_SYMBOL_TABLE_MAXIMUM_SIZE                          = "expresso.global.symbol.table.maximum.size";
	public static final Long    DEFAULT_VALUE_GLOBAL_SYMBOL_TABLE_MAXIMUM_SIZE                = CacheMap.NO_MAXIMUM_SIZE;
	//
	public static final String  KEY_GLOBAL_SYMBOL_TABLE_CACHES_NUMERICS                       = "expresso.global.symbol.table.cache.numerics";
	public static final Boolean DEFAULT_VALUE_GLOBAL_SYMBOL_TABLE_CACHES_NUMERICS             = Boolean.FALSE;
 	//
	public static final String KEY_DEFAULT_SYNTAX_TO_STRING_UNARY_FUNCTION_CLASS              = "expresso.syntax.to.string.unary.function.class";
	public static final String DEFAULT_VALUE_DEFAULT_SYNTAX_TO_STRING_UNARY_FUNCTION_CLASS    = ExpressionSyntaxTreeToStringFunction.class.getName();
	//
	public static final String KEY_SYNTAX_TO_STRING_THREAD_CACHE_TIMEOUT_IN_SECONDS           = "expresso.syntax.to.string.thread.cache.timeout";
	public static final Long   DEFAULT_VALUE_SYNTAX_TO_STRING_THREAD_CACHE_TIMEOUT_IN_SECONDS = new Long(60);
	
			
	public static int getDisplayNumericPrecisionForSymbols() {
		int result = getInt(KEY_DISPLAY_NUMERIC_PRECISION_FOR_SYMBOLS, DEFAULT_VALUE_DISPLAY_NUMERIC_PRECISION_FOR_SYMBOLS);
	
		return result;
	}
	
	public static int getDisplayScientificGreaterNIntegerPlaces() {
		int result = getInt(KEY_DISPLAY_SCIENTIFIC_GREATER_N_INTEGER_PLACES, DEFAULT_VALUE_DISPLAY_SCIENTIFIC_GREATER_N_INTEGER_PLACES);
	
		return result;
	}
	
	public static int getDisplayScientificAfterNDecimalPlaces() {
		int result = getInt(KEY_DISPLAY_SCIENTIFIC_AFTER_N_DECIMAL_PLACES, DEFAULT_VALUE_DISPLAY_SCIENTIFIC_AFTER_N_DECIMAL_PLACES);
	
		return result;
	}
	
	public static boolean isUseGlobalSymbolTable() {
		boolean result = getBoolean(KEY_USE_GLOBAL_SYMBOL_TABLE, DEFAULT_VALUE_USE_GLOBAL_SYMBOL_TABLE);
		
		return result;
	}
	
	public static long getGlobalSymbolTableMaximumSize() {
		long result = getLong(KEY_GLOBAL_SYMBOL_TABLE_MAXIMUM_SIZE, DEFAULT_VALUE_GLOBAL_SYMBOL_TABLE_MAXIMUM_SIZE);
		
		return result;
	}
	
	public static boolean isGlobalSymbolTableToCacheNumerics() {
		boolean result = getBoolean(KEY_GLOBAL_SYMBOL_TABLE_CACHES_NUMERICS, DEFAULT_VALUE_GLOBAL_SYMBOL_TABLE_CACHES_NUMERICS);
		
		return result;
	}
	
	public static String getDefaultSyntaxToStringUnaryFunctionClass() {
		String result = getString(KEY_DEFAULT_SYNTAX_TO_STRING_UNARY_FUNCTION_CLASS, DEFAULT_VALUE_DEFAULT_SYNTAX_TO_STRING_UNARY_FUNCTION_CLASS);
		
		return result;
	}
	
	public static long getSyntaxToStringThreadCacheTimeoutInSeconds() {
		long result = getLong(KEY_SYNTAX_TO_STRING_THREAD_CACHE_TIMEOUT_IN_SECONDS, DEFAULT_VALUE_SYNTAX_TO_STRING_THREAD_CACHE_TIMEOUT_IN_SECONDS);
		
		return result;
	}
}
