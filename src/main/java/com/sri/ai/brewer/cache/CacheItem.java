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
import java.util.HashSet;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.core.ParsingResult;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.Util;

/**
 * A caching item for parsing expressions that parsed successfully at some
 * point. Usually, they were part of larger parsing expressions that failed, but
 * it is worth keeping them in a cache so their parsing does not need to be
 * repeated.
 * 
 * @author braz
 */
@Beta
public class CacheItem {
	private int position;
	private ParsingExpression parsingExpression;
	private ParsingResult parsingResult;
	private Collection<Expression> precedenceConditionsOnFirstParse;
	private Collection<Expression> precedenceConditionsOnLastParse;
	private int tokenPositionLimit;

	public CacheItem(
			int position,
			ParsingExpression parsingExpression,
			ParsingResult parsingResult,
			Collection<Expression> precedenceConditionsOnFirstParse,
			Collection<Expression> precedenceConditionsOnLastParse,
			int tokenizerPositionLimit) {
		super();
		this.position = position;
		this.parsingExpression = parsingExpression;
		this.parsingResult = parsingResult;
		this.precedenceConditionsOnFirstParse = new HashSet<Expression>(precedenceConditionsOnFirstParse);
		this.precedenceConditionsOnLastParse = new HashSet<Expression>(precedenceConditionsOnLastParse);
		this.tokenPositionLimit = tokenizerPositionLimit;
	}

	public int getPosition() {
		return position;
	}

	public ParsingExpression getParsingExpression() {
		return parsingExpression;
	}

	public ParsingResult getParsingResult() {
		return parsingResult;
	}

	public Collection<Expression> getPrecedenceConditionsOnFirstParse() {
		return precedenceConditionsOnFirstParse;
	}

	public Collection<Expression> getPrecedenceConditionsOnLastParse() {
		return precedenceConditionsOnLastParse;
	}

	/**
	 * Returns the tokenizer position limit constraint at the time of parsing.
	 */
	public int getTokenPositionLimit() {
		return tokenPositionLimit;
	}

	/**
	 * Returns the tokenizer position limit for the cached parse, returning a {@link NullPointerException}
	 * if the parse has not been successful.
	 */
	public int getParseTokenizerPositionLimit() {
		return position + getParsingResult().getTokens().size();
	}

	@Override
	public String toString() {
		return "Cached item at position " + position + ": " + parsingResult + " with conditions " + Util.join(precedenceConditionsOnFirstParse) + " on first parse and " + Util.join(precedenceConditionsOnLastParse) + " on last parse and token position limit " + tokenPositionLimit;
	}
}
