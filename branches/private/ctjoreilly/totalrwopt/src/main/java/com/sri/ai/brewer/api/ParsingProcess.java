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
package com.sri.ai.brewer.api;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.cache.CacheItem;
import com.sri.ai.brewer.core.ParsingResult;
import com.sri.ai.brewer.core.TokenIterator;
import com.sri.ai.expresso.api.Expression;

/**
 * A parsing process, passed from parsing expression to parsing expression as
 * the process goes on.
 * 
 * It keeps several pieces of information:
 * 
 * <ul>
 * 
 * <li>A stack of parsing expressions which are the ancestors of the currently
 * parsed one.
 * 
 * <li>precedence conditions: there is always a conjunction of
 * {@link ParsingConstraint}s on both the first and last parse of the current
 * parsing expression. When the process recurses into a sub-parsing expression,
 * the current set of conditions can change, but is restored after the
 * sub-parsing is done. For this reason, there is a stack of conjunctions of
 * precedence condition conjunctions, and its associated methods. Each
 * conjunction is a stack itself, so this is a stack of stacks.
 * 
 * <li>current grammar being used.
 * 
 * <li>level of nesting of parsing, useful for printing debugging information,
 * for example.
 * 
 * <li>caching interface: if a cache mechanism is available, successfully parsed
 * sub-parsing expressions of failed parsing expressions can still be used by
 * other parsing expressions attempting to parse them at the same position.
 * 
 * </ul>
 * 
 * @author braz
 */
@Beta
public interface ParsingProcess {
	TokenIterator getTokenIterator();

	int getTokenPosition();
	
	void advanceNTokens(int n);
	
	boolean hasNextTokenAccordingToCurrentConditions();
	
	String nextTokenAccordingToCurrentConditions();
	
	void putBack(String token);
	
	void putBack(List<String> tokens);
	
	void putBackFromParsingResults(LinkedList<ParsingResult> tokens);
	
	List<String> remainingTokens();
	
	ParsingResult parsingResultOfNonTerminal(String nonTerminalName);
	
	Expression parseOfNonTerminal(String nonTerminalName);
	
	/** 
	 * Register parsing expression, that is, take whatever information needs to be taken from it,
	 * such as special tokens, and returns whether it was necessary to actually do it (that is, it had not been seen before).
	 */
	boolean registerParsingExpressionsAndSubParsingExpressions(ParsingExpression parsingExpression, ParsingProcess process);

	void collectGlobalFollowingTokensInformationFromRegisteredInformation();
	
	boolean globalFollowingTokensInformationAlreadyDefined();
	
	Set<String> getCloserTokensOf(String token);
	
	CloserTokensResult consumeCloserTokensOrMoveUpToMisplacedCloserTokenOrLimit(Set<String> closerTokens);

	CloserTokensResult moveUpToFirstCloserTokenOrMisplacedCloserTokenOrLimit(Set<String> closerTokens);

	void pushParsingExpression(ParsingExpression parent);
	
	ParsingExpression popParsingExpressions();
	
	/** The parent of the parsing expression being currently parsed (<code>null</code> if none). */
	ParsingExpression getParent();
	
	void log(Object object);

	void logln(Object object);

	void conditionalLogln(boolean condition, Object object);

	void logrestln(Object object);

	void pushPrecedenceConditionToCurrentConjunctionOnFirstParse(Expression condition);

	void popPrecedenceConditionFromCurrentConjunctionOnFirstParse();
	
	void pushNewConjunctionOfPrecedenceConditionsOnFirstParse();
	
	void popConjunctionOfPrecedenceConditionsOnFirstParse();
	
	void pushTokenizerPositionLimit(int limitPosition);

	int  popTokenizerPositionLimit();
	
	int currentTokenizerPositionLimit();

	/**
	 * Indicates whether a cached parse satisfies the current tokenizer position limit,
	 * assuming that all other conditions, including initial position and parsing expression, are the same.
	 */
	boolean currentTokenizerPositionLimitIsSatisfiedByCachedConditions(
			CacheItem cacheItem);

	/**
	 * Returns the position of the first token beyond position i and before position limit
	 * that is equal to the given token, returning -1 if there is no such token,
	 * leaving the token iterator in the same state as it found it.
	 */
	int lookAheadForTokenFromPositionWithLimit(String token, int i, int limit);

	/**
	 * Returns position of last occurrence of token in token iterator before position i (not inclusive),
	 * or -1 if it does not occur, while leaving the token iterator in the same state it was found.
	 */
	int lookAheadForTokenBackwardsFromGivenPosition(String token, int i);

	void setInitialTokenPositionLimitIfTokenIteratorIsBasedOnString();

	boolean currentConjunctionOfPrecedenceConditionsOnFirstParseIsSatisfiedBy(ParsingExpression parsingExpression);
	
	boolean currentConjunctionOfPrecedenceConditionsOnFirstParseIsEquivalentTo(Collection<Expression> precedenceConditions);
	
	boolean currentConjunctionOfPrecedenceConditionsOnFirstParseIsSatisfiedBy(Collection<Expression> precedenceConditions);
	
	Collection<Expression> getPrecedenceConditionsOnFirstParse();
	
	void pushPrecedenceConditionToCurrentConjunctionsOnLastParse(Expression condition);
	
	void popPrecedenceConditionFromCurrentConjunctionOnLastParse();
	
	void pushNewConjunctionOfPrecedenceConditionsOnLastParse();
	
	void popConjunctionOfPrecedenceConditionsOnLastParse();

	boolean currentConjunctionOfPrecedenceConditionsOnLastParseIsSatisfiedBy(ParsingExpression parsingExpression);
	
	boolean currentConjunctionOfPrecedenceConditionsOnLastParseIsEquivalentTo(Collection<Expression> precedenceConditions);

	boolean currentConjunctionOfPrecedenceConditionsOnLastParseIsSatisfiedBy(Collection<Expression> precedenceConditions);
	
	Collection<Expression> getPrecedenceConditionsOnLastParse();

	
	boolean currentConjunctionOfPrecedenceConditionsIsSatisfiedBy(ParsingExpression parsingExpression);
	
	//////////////// Grammar ////////////////
	
	Map<String, ParsingExpression> getGrammar();
	
	//////////////// Length ////////////////
	
	int getLengthLowerBound(ParsingExpression parsingExpression);
	
	////////////// Level ///////////////////
	
	int getLevel();
	
	void pushLevel();
	
	void popLevel();
	
	//////////// Cache ////////////////
	
	void putCacheItem(CacheItem cacheItem);

	public CacheItem getCacheItemOrNull(ParsingExpression parsingExpression);
	
	//////////// Closer Tokens ////////////////

	public static class CloserTokensResult {
		public boolean success;
		public int position;
		public List<String> tokens;
		public CloserTokensResult(boolean success, int position, List<String> tokens) {
			this.success = success;
			this.position = position;
			this.tokens = tokens;
		}
	};
	
	public static class FirstCloserTokenFound extends CloserTokensResult {
		public String token;
		public FirstCloserTokenFound(String token, int position, List<String> tokens) {
			super(true, position, tokens);
			this.token = token;
		}
	};

	public static class AllCloserTokensConsumed extends CloserTokensResult {
		public AllCloserTokensConsumed(int position, List<String> tokens) {
			super(true, position, tokens);
		}
	};

	public static class MismatchedCloserTokenFound extends CloserTokensResult {
		public String token;
		public MismatchedCloserTokenFound(String token, int position, List<String> tokens) {
			super(false, position, tokens);
			this.token = token;
		}
	};

	public static class LimitOrEndOfTokensReached extends CloserTokensResult {
		public LimitOrEndOfTokensReached(int position, List<String> tokens) {
			super(false, position, tokens);
		}
	};
}
