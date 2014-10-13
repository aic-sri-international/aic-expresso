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
package com.sri.ai.brewer;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.core.CommonGrammar;
import com.sri.ai.util.Configuration;

/**
 * Configuration information for the classes in the brewer package.
 * 
 * @author oreilly
 *
 */
@Beta
public class BrewerConfiguration extends Configuration {

	// 
	public static final String  KEY_USE_PARSING_CACHE                                                               = "brewer.use.parsing.cache";
	public static final Boolean DEFAULT_VALUE_USE_PARSING_CACHE                                                     = Boolean.TRUE;
	//
	public static final String  KEY_PARSER_FLAG_LOOK_AHEAD                                                          = "brewer.parser.flag.look.ahead";
	public static final Boolean DEFAULT_VALUE_PARSER_FLAG_LOOK_AHEAD                                                = Boolean.FALSE;
	//
	public static final String  KEY_PARSER_FLAG_SET_LIMIT_BASED_ON_PARENTHETICAL_TOKENS                             = "brewer.parser.flag.set.limit.based.on.parenthetical.tokens";
	public static final Boolean DEFAULT_VALUE_PARSER_FLAG_SET_LIMIT_BASED_ON_PARENTHETICAL_TOKENS                   = Boolean.TRUE;
	//
	public static final String  KEY_PARSER_FLAG_PRUNE_BY_LENGTH_LOWER_BOUND_AND_LIMIT                               = "brewer.parser.flag.prune.by.length.lower.bound.and.limit";
	public static final Boolean DEFAULT_VALUE_PARSER_FLAG_PRUNE_BY_LENGTH_LOWER_BOUND_AND_LIMIT                     = Boolean.TRUE;
	//
	public static final String  KEY_PARSER_FLAG_LOG_IS_ON                                                           = "brewer.parser.flag.log.is.on";
	public static final Boolean DEFAULT_VALUE_PARSER_FLAG_LOG_IS_ON                                                 = Boolean.FALSE;
	//
	public static final String  KEY_PARSER_FLAG_LOG_CACHE_UNLESS_LOGGING_IS_OFF                                     = "brewer.parser.flag.log.cache.usage.unless.logging.is.off";
	public static final Boolean DEFAULT_VALUE_PARSER_FLAG_LOG_CACHE_UNLESS_LOGGING_IS_OFF                           = Boolean.TRUE;
	//
	public static final String  KEY_PARSER_FLAG_LOG_LIST_OF_CACHE_ITEMS_UNLESS_CACHE_USAGE_LOGGING_IS_OFF           = "brewer.parser.flag.log.list.of.cache.items.unless.cache.usage.logging.is.off";
	public static final Boolean DEFAULT_VALUE_PARSER_FLAG_LOG_LIST_OF_CACHE_ITEMS_UNLESS_CACHE_USAGE_LOGGING_IS_OFF = Boolean.FALSE;
	//
	public static final String  KEY_OUTPUT_PARSING_TIME_INFO                                                        = "brewer.output.parsing.time.info";
	public static final Boolean DEFAULT_VALUE_OUTPUT_PARSING_TIME_INFO                                              = Boolean.TRUE;
	//
	public static boolean isUseParsingCache() {
		boolean result = getBoolean(KEY_USE_PARSING_CACHE, DEFAULT_VALUE_USE_PARSING_CACHE);
		
		return result;
	}
	
	public static boolean isParserFlagLookAhead() {
		boolean result = getBoolean(KEY_PARSER_FLAG_LOOK_AHEAD, DEFAULT_VALUE_PARSER_FLAG_LOOK_AHEAD);
		
		return result;
	}
	
	public static boolean isParserFlagSetLimitBasedOnParentheticalTokens() {
		boolean result = getBoolean(KEY_PARSER_FLAG_SET_LIMIT_BASED_ON_PARENTHETICAL_TOKENS, DEFAULT_VALUE_PARSER_FLAG_SET_LIMIT_BASED_ON_PARENTHETICAL_TOKENS);
		
		return result;
	}
	
	public static boolean isParserFlagPruneByLengthLowerBoundAndLimit() {
		boolean result = getBoolean(KEY_PARSER_FLAG_PRUNE_BY_LENGTH_LOWER_BOUND_AND_LIMIT, DEFAULT_VALUE_PARSER_FLAG_PRUNE_BY_LENGTH_LOWER_BOUND_AND_LIMIT);
		
		return result;
	}
	
	public static boolean isParserFlagLogIsOn() {
		boolean result = getBoolean(KEY_PARSER_FLAG_LOG_IS_ON, DEFAULT_VALUE_PARSER_FLAG_LOG_IS_ON);
		
		return result;
	}
	
	public static boolean isParserFlagLogCacheUsageUnlessLoggingIsOff() {
		boolean result = getBoolean(KEY_PARSER_FLAG_LOG_CACHE_UNLESS_LOGGING_IS_OFF, DEFAULT_VALUE_PARSER_FLAG_LOG_CACHE_UNLESS_LOGGING_IS_OFF);
		
		return result;
	}

	public static boolean isParserFlagLogListOfCacheItemsUnlessCacheUsageLoggingIsOff() {
		boolean result = getBoolean(KEY_PARSER_FLAG_LOG_LIST_OF_CACHE_ITEMS_UNLESS_CACHE_USAGE_LOGGING_IS_OFF, DEFAULT_VALUE_PARSER_FLAG_LOG_LIST_OF_CACHE_ITEMS_UNLESS_CACHE_USAGE_LOGGING_IS_OFF);
		
		return result;
	}
	
	public static boolean isOutputParsingTimeInfo() {
		boolean result = getBoolean(KEY_OUTPUT_PARSING_TIME_INFO, DEFAULT_VALUE_OUTPUT_PARSING_TIME_INFO);
		
		return result;
	}
}
