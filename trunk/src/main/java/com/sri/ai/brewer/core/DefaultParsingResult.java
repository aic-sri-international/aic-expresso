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
package com.sri.ai.brewer.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.expresso.api.Expression;

/**
 * The result of parsing, containing the corresponding parsing expression, the
 * parse, and the list of tokens used.
 * 
 * @author braz
 */
@Beta
public class DefaultParsingResult implements ParsingResult {
	private ParsingExpression parsingExpression;
	private Expression parse;
	private List<String> tokens;
	private boolean isSuccessful;
	
	/** Indicates whether the parsing result might have been different had the token position limit been different. */
	private boolean tokenPositionLimitInfluencedResult;

	/** Constructs a successful parsing result. */
	public DefaultParsingResult(ParsingExpression parsingExpression, Collection<String> tokens, Expression expression, boolean tokenPositionLimitInfluencedResult) {
		this.parsingExpression = parsingExpression;
		this.tokens = new ArrayList<String>(tokens);
		this.parse = expression;
		this.isSuccessful = true;
		this.tokenPositionLimitInfluencedResult = tokenPositionLimitInfluencedResult;
	}

	/** Constructs an unsuccessful parsing result. */
	public DefaultParsingResult(boolean tokenPositionLimitInfluencedResult) {
		this.isSuccessful = false;
		this.tokenPositionLimitInfluencedResult = tokenPositionLimitInfluencedResult;
	}

	public ParsingExpression getParsingExpression() {
		return parsingExpression;
	}

	public Expression getParse() {
		return parse;
	}

	@Override
	public void setParse(Expression newParse) {
		parse = newParse;
	}

	public List<String> getTokens() {
		return tokens;
	}
	
	public void putBack(ParsingProcess process) {
		for (int i = tokens.size(); i != 0; i--) {
			process.putBack(tokens.get(i - 1));
		}
	}
	
	public String toString() {
		return parse + " for " + parsingExpression;
	}

	@Override
	public boolean isSuccessful() {
		return isSuccessful;
	}

	public static boolean isSuccessful(ParsingResult parsingResult) {
		return parsingResult.isSuccessful();
	}

	public static ParsingResult makeFailedParsingResult(boolean ranOutOfTokens) {
		return new DefaultParsingResult(ranOutOfTokens);
	}

	@Override
	public boolean tokenPositionLimitInfluencedResult() {
		return tokenPositionLimitInfluencedResult;
	}
}
