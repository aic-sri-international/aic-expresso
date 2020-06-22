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

import java.math.MathContext;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.expresso.helper.SyntaxTrees;
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
	public static final String  KEY_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_EXACT_REPRESENTATION_OF_NUMERICAL_SYMBOLS = "expresso.display.numerics.most.decimal.places.in.exact.representation.of.numerical.symbols";
	public static final Integer DEFAULT_VALUE_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_EXACT_REPRESENTATION_OF_NUMERICAL_SYMBOLS = Integer.valueOf(10000);
	//
	public static final String  KEY_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_APPROXIMATE_REPRESENTATION_OF_NUMERICAL_SYMBOLS = "expresso.display.numerics.most.decimal.places.in.approximate.representation.of.numerical.symbols";
	public static final Integer DEFAULT_VALUE_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_APPROXIMATE_REPRESENTATION_OF_NUMERICAL_SYMBOLS = Integer.valueOf(MathContext.DECIMAL128.getPrecision());
	//
	public static final String  KEY_DISPLAY_NUMERICS_EXACTLY_FOR_SYMBOLS                      = "expresso.display.numerics.exactly.for.symbols";
	public static final boolean DEFAULT_VALUE_DISPLAY_NUMERICS_EXACTLY_FOR_SYMBOLS            = true;
	//
	public static final String  KEY_DISPLAY_NUMERICS_MOST_INTEGER_PLACES_BEFORE_SWITCHING_TO_SCIENTIFIC_NOTATION               = "expresso.display.numerics.most.integer.places.before.switching.to.scientific.notation";
	public static final Integer DEFAULT_VALUE_DISPLAY_NUMERICS_MOST_INTEGER_PLACES_BEFORE_SWITCHING_TO_SCIENTIFIC_NOTATION     = Integer.valueOf(40);
	//
	public static final String  KEY_DISPLAY_NUMERICS_GREATEST_INITIAL_NON_ZERO_DECIMAL_PLACE_POSITION_BEFORE_SWITCHING_TO_SCIENTIFIC_NOTATION = "expresso.display.numerics.greatest.initial.non.zero.decimal.place.position.before.switching.to.scientific.notation";
	public static final Integer DEFAULT_VALUE_DISPLAY_SCIENTIFIC_AFTER_N_DECIMAL_PLACES       = Integer.valueOf(40);
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
	public static final String KEY_SYNTAX_TO_STRING_THREAD_CACHE_TIMEOUT_IN_SECONDS           = "expresso.syntax.to.string.thread.cache.timeout";
	public static final Long   DEFAULT_VALUE_SYNTAX_TO_STRING_THREAD_CACHE_TIMEOUT_IN_SECONDS = Long.valueOf(60);
	
			
	public static boolean isDisplayNumericsExactlyForSymbols() {
		boolean result = getBoolean(KEY_DISPLAY_NUMERICS_EXACTLY_FOR_SYMBOLS, DEFAULT_VALUE_DISPLAY_NUMERICS_EXACTLY_FOR_SYMBOLS);
		return result;
	}

	public static boolean setDisplayNumericsExactlyForSymbols(boolean newValue) {
		boolean result = isDisplayNumericsExactlyForSymbols();
		SyntaxTrees.flushGlobalSymbolTable();
		DefaultSymbol.flushGlobalSymbolTable();
		Configuration.setProperty(KEY_DISPLAY_NUMERICS_EXACTLY_FOR_SYMBOLS, Boolean.toString(newValue));
		return result;
	}

	/**
	 * Get the number of maximum number of decimal places to use when exactly representing a numeric symbol.
	 */
	public static int getDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols() {
		int result = getInt(KEY_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_EXACT_REPRESENTATION_OF_NUMERICAL_SYMBOLS, DEFAULT_VALUE_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_EXACT_REPRESENTATION_OF_NUMERICAL_SYMBOLS);
		return result;
	}

	public static int setDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols(int newValue) {
		int result = getDisplayNumericsMostDecimalPlacesInExactRepresentationOfNumericalSymbols();
		SyntaxTrees.flushGlobalSymbolTable();
		DefaultSymbol.flushGlobalSymbolTable();
		Configuration.setProperty(KEY_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_EXACT_REPRESENTATION_OF_NUMERICAL_SYMBOLS, Integer.toString(newValue));
		return result;
	}
	
	/**
	 * Get the number of maximum number of decimal places to use when approximately representing a numeric symbol.
	 * TODO: this may give the erroneous impression that it regulates precision for symbols with double values,
	 * but that is regulated by the precision for exact representation (because the double being stored will
	 * be exactly represented). It would be best to clarify all that.
	 */
	public static int getDisplayNumericsMostDecimalPlacesInApproximateRepresentationOfNumericalSymbols() {
		int result = getInt(KEY_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_APPROXIMATE_REPRESENTATION_OF_NUMERICAL_SYMBOLS, DEFAULT_VALUE_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_APPROXIMATE_REPRESENTATION_OF_NUMERICAL_SYMBOLS);
	
		return result;
	}
	
	public static int setDisplayNumericsMostDecimalPlacesInApproximateRepresentationOfNumericalSymbols(int newValue) {
		int result = getDisplayNumericsMostDecimalPlacesInApproximateRepresentationOfNumericalSymbols();
		SyntaxTrees.flushGlobalSymbolTable();
		DefaultSymbol.flushGlobalSymbolTable();
		Configuration.setProperty(KEY_DISPLAY_NUMERICS_MOST_DECIMAL_PLACES_IN_APPROXIMATE_REPRESENTATION_OF_NUMERICAL_SYMBOLS, Integer.toString(newValue));
		return result;
	}
	
	public static int getDisplayNumericsMostIntegerPlacesBeforeSwitchingToScientificNotation() {
		int result = getInt(KEY_DISPLAY_NUMERICS_MOST_INTEGER_PLACES_BEFORE_SWITCHING_TO_SCIENTIFIC_NOTATION, DEFAULT_VALUE_DISPLAY_NUMERICS_MOST_INTEGER_PLACES_BEFORE_SWITCHING_TO_SCIENTIFIC_NOTATION);
		return result;
	}
	
	public static int setDisplayNumericsMostIntegerPlacesBeforeSwitchingToScientificNotation(int newValue) {
		int result = getDisplayNumericsMostIntegerPlacesBeforeSwitchingToScientificNotation();
		SyntaxTrees.flushGlobalSymbolTable();
		DefaultSymbol.flushGlobalSymbolTable();
		Configuration.setProperty(KEY_DISPLAY_NUMERICS_MOST_INTEGER_PLACES_BEFORE_SWITCHING_TO_SCIENTIFIC_NOTATION, Integer.toString(newValue));
		return result;
	}
	
	public static int getDisplayNumericsGreatestInitialNonZeroDecimalPlacePositionBeforeSwitchingToScientificNotation() {
		int result = getInt(KEY_DISPLAY_NUMERICS_GREATEST_INITIAL_NON_ZERO_DECIMAL_PLACE_POSITION_BEFORE_SWITCHING_TO_SCIENTIFIC_NOTATION, DEFAULT_VALUE_DISPLAY_SCIENTIFIC_AFTER_N_DECIMAL_PLACES);
		return result;
	}
	
	public static int setDisplayNumericsGreatestInitialNonZeroDecimalPlacePositionBeforeSwitchingToScientificNotation(int newValue) {
		int result = getDisplayNumericsGreatestInitialNonZeroDecimalPlacePositionBeforeSwitchingToScientificNotation();
		SyntaxTrees.flushGlobalSymbolTable();
		DefaultSymbol.flushGlobalSymbolTable();
		Configuration.setProperty(KEY_DISPLAY_NUMERICS_GREATEST_INITIAL_NON_ZERO_DECIMAL_PLACE_POSITION_BEFORE_SWITCHING_TO_SCIENTIFIC_NOTATION, Integer.toString(newValue));
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
	
	public static long getSyntaxToStringThreadCacheTimeoutInSeconds() {
		long result = getLong(KEY_SYNTAX_TO_STRING_THREAD_CACHE_TIMEOUT_IN_SECONDS, DEFAULT_VALUE_SYNTAX_TO_STRING_THREAD_CACHE_TIMEOUT_IN_SECONDS);
		
		return result;
	}
}
