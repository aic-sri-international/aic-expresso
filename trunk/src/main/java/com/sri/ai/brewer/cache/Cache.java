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
package com.sri.ai.brewer.cache;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import com.google.common.annotations.Beta;
import com.google.common.base.Stopwatch;
import com.google.common.collect.Lists;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.core.DefaultParsingResult;
import com.sri.ai.brewer.core.ParserFlags;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IdentityWrapper;

/**
 * A Parsing Cache.
 * 
 * @author braz
 *
 */
@Beta
public class Cache {
	
	public static boolean logCacheUsage;
	public static boolean logListOfCacheItems;
	
	static {
		logCacheUsage = ParserFlags.logCacheUsageUnlessLoggingIsOff && ParserFlags.logIsOn;
		logListOfCacheItems = ParserFlags.logListOfCacheItemsUnlessCacheUsageLoggingIsOff && logCacheUsage;
	}
	
	private Map<List, Collection<CacheItem>> cache = new HashMap<List, Collection<CacheItem>>();
	// cache maps binary lists (position, identity of parsingExpression) to collections of cache items.
	// A cache item also contains the conditions on the parsing expression.
	// Those will have to be checked explicitly against the parsing process's current conditions
	// for first and last parse.

	public Collection<CacheItem> getCacheItems(int position, ParsingExpression parsingExpression) {
		Collection<CacheItem> cacheItem = cache.get(Util.list(position, new IdentityWrapper(parsingExpression)));
		return cacheItem == null? new HashSet<CacheItem>() : cacheItem;
	}

	public void putCacheItem(CacheItem cacheItem, ParsingProcess process) {
		Util.addToCollectionValuePossiblyCreatingIt(
				cache,
				Util.list(cacheItem.getPosition(), new IdentityWrapper(cacheItem.getParsingExpression())),
				cacheItem,
				HashSet.class);
		logCacheItem("Putting cache item:", cacheItem, process);
	}

	public CacheItem tryToUseCache(ParsingExpression parsingExpression, ParsingProcess process) {
		process.conditionalLogln(logCacheUsage, "Checking for cached parse for " + parsingExpression);
		process.pushLevel();
		Stopwatch stopwatch = Stopwatch.createStarted();
		Collection<CacheItem> cacheItemsAtThisPositionAndParsingExpression = getCacheItems(process.getTokenPosition(), parsingExpression);
		process.conditionalLogln(
				logCacheUsage,
				"Number of cache items to check conditions for, for current token and parsing expression: " +
				cacheItemsAtThisPositionAndParsingExpression.size());
		for (CacheItem cacheItem: cacheItemsAtThisPositionAndParsingExpression) {
			logCacheItem(
					Lists.newArrayList(
							"Checking " + parsingExpression,
							"Against cache item:"
					),
					cacheItem, process);
			if (
					process.currentConjunctionOfPrecedenceConditionsOnFirstParseIsEquivalentTo(cacheItem.getPrecedenceConditionsOnFirstParse()) &&
					process.currentConjunctionOfPrecedenceConditionsOnLastParseIsEquivalentTo(cacheItem.getPrecedenceConditionsOnLastParse())
					&&
					process.currentTokenizerPositionLimitIsSatisfiedByCachedConditions(cacheItem)
					) {
				logCacheItem("Using", cacheItem, process);
				if (DefaultParsingResult.isSuccessful(cacheItem.getParsingResult())) {
					process.advanceNTokens(cacheItem.getParsingResult().getTokens().size());
				}
				process.popLevel();
				long time = stopwatch.elapsed(TimeUnit.MILLISECONDS);
				process.conditionalLogln(logCacheUsage, "Using " + cacheItem.getParsingResult());
				process.conditionalLogln(logCacheUsage, "Took " + time + " ms to find.");
				return cacheItem;
			}
			else if (logCacheUsage) {
				process.conditionalLogln(logCacheUsage, "Failed comparison");
			}
		}
		process.popLevel();
		long time = stopwatch.elapsed(TimeUnit.MILLISECONDS);
		process.conditionalLogln(logCacheUsage, "No cached parse");
		process.conditionalLogln(logCacheUsage, "Took " + time + " ms to go over " + cacheItemsAtThisPositionAndParsingExpression.size() + " items.");
		return null;
	}

	public void logCacheItem(Collection<String> messages, CacheItem cacheItem, ParsingProcess process) {
		if (logCacheUsage) {
			for (String message : messages) {
				process.logln(message);
			}
			logCacheItem(cacheItem, process);
		}
	}

	public void logCacheItem(String message, CacheItem cacheItem, ParsingProcess process) {
		if (logCacheUsage) {
			process.logln(message);
			logCacheItem(cacheItem, process);
		}
	}
	
	public void logCacheItem(CacheItem cacheItem, ParsingProcess process) {
		if (logCacheUsage) {
			process.logln("On " + cacheItem.getParsingExpression());
			process.pushLevel();
			process.logln("On first: " + cacheItem.getPrecedenceConditionsOnFirstParse());
			process.logln("On last: " + cacheItem.getPrecedenceConditionsOnLastParse());
			process.logln("On position: " + cacheItem.getPosition());
			if (DefaultParsingResult.isSuccessful(cacheItem.getParsingResult())) {
				process.logln("With result: " + cacheItem.getParsingResult().getParse());
			}
			else {
				process.logln("No parsing result.");
			}
			process.logln("Under token position limit: " + cacheItem.getTokenPositionLimit());
			if (cacheItem.getParsingResult().tokenPositionLimitInfluencedResult()) {
				process.logln("Limit made a difference");
			}
			else {
				process.logln("Limit made no difference");
			}
			process.popLevel();
		}
	}
	
	public void logCache(ParsingProcess process) {
		if (logListOfCacheItems) {
			process.logln("Cache inside:");
			process.pushLevel();
		}
		
		int numberOfItems = 0;
		for (Collection<CacheItem> cacheItemsInAPosition : cache.values()) {
			for (CacheItem cacheItem : cacheItemsInAPosition) {
				if (logListOfCacheItems) {
					logCacheItem(cacheItem, process);
				}
				numberOfItems++;
			}
		}
		
		if (logListOfCacheItems) {
			process.popLevel();
		}
		
		if (logCacheUsage) {
			process.logln("Number of cache items: " + numberOfItems);
		}
	}
}
