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

import java.io.IOException;
import java.io.PrintStream;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.brewer.api.Grammar;
import com.sri.ai.brewer.api.ParsingConstraint;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.cache.Cache;
import com.sri.ai.brewer.cache.CacheItem;
import com.sri.ai.brewer.parsingconstraint.AbstractAtomicParsingConstraint;
import com.sri.ai.brewer.parsingconstraint.GreaterThanOrEqualToOrUnrelated;
import com.sri.ai.brewer.parsingconstraint.GreaterThanOrUnrelated;
import com.sri.ai.brewer.parsingexpression.core.Sequence;
import com.sri.ai.brewer.parsingexpression.core.Terminal;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.ManyToManyRelation;

/**
 * A default implementation for {@link ParsingProcess}.
 * 
 * @author braz
 */
@Beta
public class DefaultParsingProcess implements ParsingProcess {
	private Stack<List<Expression>> precedenceConditionConjunctionsOnFirstParseStack = new Stack<List<Expression>>();

	private Stack<List<Expression>> precedenceConditionConjunctionsOnLastParseStack = new Stack<List<Expression>>();

	protected Grammar grammar;
	
	HashSet<ParsingExpression> registeredParsingExpressions = new HashSet<ParsingExpression>();

	protected int level = 1;
	
	private PrintStream log = ParserFlags.logIsOn ? Util.getPrintStream("log.txt") : null;
	
	private Cache cache = new Cache();

	private TokenIterator tokenIterator;

	private Stack<ParsingExpression> stackOfParsingExpressions = new Stack<ParsingExpression>();
	
	private Map<String, Set<String>> tokensAlwaysFollowingAToken = new HashMap<String, Set<String>>();
	private Map<String, Set<String>> tokensAlwaysPrecedingAToken = new HashMap<String, Set<String>>();
	private Set<String> notParenthetical = new HashSet<String>();
	private IsNotParenthetical isNotParenthetical = new IsNotParenthetical();
	private Map<String, Set<Set<String>>> setsOfTokensPrecedingATokenInIndividualSequences = new HashMap<String, Set<Set<String>>>(); // closing tokens
	private Map<String, Set<String>> closersOfAToken = new HashMap<String, Set<String>>(); // closing tokens
	private Set<String> closersOfSomeToken;
	private ManyToManyRelation<String, String> parentheticalPairs;

	// TODO: rename "closer" to "closing"
	// TODO: is parenthetical optimization independent of closers? If so, separate in code
	
	private void collectPrecedingFollowingAndNonParentheticalTokens(ParsingExpression parsingExpression) {
		ArrayList<String> tokens = new ArrayList<String>();
		for (Expression subExpression : parsingExpression.getArguments()) {
			if (subExpression instanceof Terminal) {
				Terminal terminal = (Terminal) subExpression;
				String token = (String) terminal.get(0).getValue();
				tokens.add(token);
			}
		}
		
		for (int i = 0; i != tokens.size(); i++) {
			String token = tokens.get(i);
			if (parsingExpression instanceof Sequence) {
				if ( ! notParenthetical.contains(token)) {
					if ( ! tokensAlwaysFollowingAToken.containsKey(token)) {
						// token is being observed for the first time,
						// so all following tokens are considered to have "always" followed it
						Set<String> tokensAlwaysFollowingToken = Util.getValuePossiblyCreatingIt(tokensAlwaysFollowingAToken, token, HashSet.class);
						for (int j = i + 1; j != tokens.size(); j++) {
							tokensAlwaysFollowingToken.add(tokens.get(j));
						}
					}
					else {
						// token has been observed before, so we already have a set of token that have always followed it;
						// we simply remove the ones in this set that fail to follow it in this sequence as well
						Set<String> tokensAlwaysFollowingToken = tokensAlwaysFollowingAToken.get(token);
						Set<String> tokensNotFollowingTokenInThisSequence = new HashSet<String>();
						for (String anotherToken : tokensAlwaysFollowingToken) {
							if ( ! Util.listFromIContains(tokens, i + 1, anotherToken)) {
								tokensNotFollowingTokenInThisSequence.add(anotherToken);
							}
						}
						tokensAlwaysFollowingToken.removeAll(tokensNotFollowingTokenInThisSequence);
					}
					
					// analogous thing for preceding tokens:
					
					if ( ! tokensAlwaysPrecedingAToken.containsKey(token)) {
						// token is being observed for the first time,
						// so all preceding tokens are considered to have "always" preceded it
						Set<String> tokensAlwaysPrecedingToken = Util.getValuePossiblyCreatingIt(tokensAlwaysPrecedingAToken, token, HashSet.class);
						for (int j = i - 1; j != -1; j--) {
							tokensAlwaysPrecedingToken.add(tokens.get(j));
						}
					}
					else {
						// token has been observed before, so we already have a set of token that have always preceded it;
						// we simply remove the ones in this set that fail to precede it in this sequence as well
						Set<String> tokensAlwaysPrecedingToken = tokensAlwaysPrecedingAToken.get(token);
						Set<String> tokensNotPrecedingTokenInThisSequence = new HashSet<String>();
						for (String anotherToken : tokensAlwaysPrecedingToken) {
							if ( ! Util.listUpToIExclusiveContains(tokens, i, anotherToken)) {
								tokensNotPrecedingTokenInThisSequence.add(anotherToken);
							}
						}
						tokensAlwaysPrecedingToken.removeAll(tokensNotPrecedingTokenInThisSequence);
					}
					
					// collect sets of tokens preceding each token in each sequence.
					Set<String> tokensPrecedingTokenInThisSequence = new HashSet<String>();
					for (int j = i - 1; j != -1; j--) {
						tokensPrecedingTokenInThisSequence.add(tokens.get(j));
					}
					Set<Set<String>> setsOfTokensPrecedingTokenInIndividualSequences =
						Util.getValuePossiblyCreatingIt(setsOfTokensPrecedingATokenInIndividualSequences, token, HashSet.class);
					setsOfTokensPrecedingTokenInIndividualSequences.add(tokensPrecedingTokenInThisSequence);
				}
			}
			else {
				// token appears in a parsing expression that is not a sequence;
				// this means it cannot be used as a parenthetical token,
				// because we only know how to use them in a sequence context.
				notParenthetical.add(token);
				tokensAlwaysFollowingAToken.remove(token);
				tokensAlwaysPrecedingAToken.remove(token);
			}
		}
		
		// remove collected tokens that are not parenthetical (that is, that appear in non-sequence parsing expressions)
		Iterator<Set<String>> iterator = tokensAlwaysFollowingAToken.values().iterator();
		while (iterator.hasNext()) {
			Set<String> value = iterator.next();
			Util.removeAll(value, isNotParenthetical);
		}
		iterator = tokensAlwaysPrecedingAToken.values().iterator();
		while (iterator.hasNext()) {
			Set<String> value = iterator.next();
			Util.removeAll(value, isNotParenthetical);
		}
		Iterator<Set<Set<String>>> iterator2 = setsOfTokensPrecedingATokenInIndividualSequences.values().iterator();
		while (iterator.hasNext()) {
			Set<Set<String>> set = iterator2.next();
			iterator = set.iterator();
			while (iterator.hasNext()) {
				Set<String> value = iterator.next();
				Util.removeAll(value, isNotParenthetical);
			}
		}
	}
	
	private class IsNotParenthetical implements Predicate<String> {
		@Override
		public boolean apply(String object) {
			return notParenthetical.contains(object);
		}
	}

	@Override
	public void collectGlobalFollowingTokensInformationFromRegisteredInformation() {
		logln("Global following-tokens information, if any:");
		pushLevel();
		parentheticalPairs = new ManyToManyRelation<String, String>();
		for (String token : tokensAlwaysFollowingAToken.keySet()) {
			Set<String> tokensAlwaysFollowingToken = Util.getValuePossiblyCreatingIt(tokensAlwaysFollowingAToken, token, HashSet.class);
			for (String tokenAlwaysFollowingToken : tokensAlwaysFollowingToken) {
				Set<String> tokensAlwaysPrecedingTokenAlwaysFollowingToken =  Util.getValuePossiblyCreatingIt(tokensAlwaysPrecedingAToken, tokenAlwaysFollowingToken, HashSet.class);
				if (tokensAlwaysPrecedingTokenAlwaysFollowingToken.contains(token)) {
					parentheticalPairs.add(token, tokenAlwaysFollowingToken);
				}
			}
		}

//		Iterator<Pair<String,String>> iterator = parentheticalPairs.iterator();
//		logln("Parenthetical tokens:");
//		System.out.println("Parenthetical tokens:");
//		pushLevel();
//		while (iterator.hasNext()) {
//			Pair<String,String> pair = iterator.next();
//			String pairRepresentation = pair.first + " " + pair.second;
//			logln(pairRepresentation);
//			System.out.println(pairRepresentation);
//		}
//		popLevel();

		closersOfSomeToken = new HashSet<String>();
		for (String token : tokensAlwaysFollowingAToken.keySet()) {
			Set<String> closersOfToken = new HashSet<String>();

			Set<String> tokensAlwaysFollowingToken = tokensAlwaysFollowingAToken.get(token);
			Util.collect(tokensAlwaysFollowingToken, closersOfToken, new DoesNotAppearOnItsOwn());
			// we require closers to be always preceded by some opener (as opposed to appearing on their own).
			// This guarantees that they never appear on their own,
			// so we can safely conclude that they are really closing when we find them after an opener.
			// Note that openers do not need to be parenthetical, in the sense that the closer may appear without that particular opener having been present,
			// but closers are always parenthetical in the sense that their openers cannot appear without that particular closer following them later.
			if ( ! closersOfToken.isEmpty()) {
				closersOfAToken.put(token, closersOfToken);
				closersOfSomeToken.addAll(closersOfSomeToken);
			}
		}
		
		// TODO: Automatic detection of closer tokens is still not working properly.
		// The way it is right now, ( on ... ) causes ) to be a closer of both ( and on.
		// This won't work because when we read 'on', we will consume ')' as its closer
		// and '(' will not be closed. I am not sure yet how to fix this.
		// Right now there are other things with greater priority so I will just hard code the main parentheses for now.
		closersOfAToken.clear();
		closersOfSomeToken.clear();
		closersOfAToken.put( "(", Util.set( ")"));
		closersOfAToken.put( "[", Util.set( "]"));
		closersOfAToken.put( "{", Util.set( "}"));
		closersOfAToken.put("_{", Util.set( "}"));
		closersOfAToken.put("{{", Util.set("}}"));
		List<String> someTokens = Util.list(")", "]", "}", "}}");
		closersOfSomeToken.addAll(someTokens);
		
//		logln("Closers of each opener:");
//		System.out.println("Closers of each opener:");
//		pushLevel();
//		Iterator<Entry<String, Set<String>>> mapIterator = closersOfAToken.entrySet().iterator();
//		while (mapIterator.hasNext()) {
//			Entry<String,Set<String>> entry = mapIterator.next();
//			String entryRepresentation = entry.getKey() + " ---> " + Util.join(entry.getValue());
//			logln(entryRepresentation);
//			System.out.println(entryRepresentation);
//		}
//		popLevel();

		popLevel();
	}
	
	@Override
	public Set<String> getCloserTokensOf(String token) {
		return closersOfAToken.get(token);
	}
	
	private class DoesNotAppearOnItsOwn implements Predicate<String> {
		@Override
		public boolean apply(String token) {
			Set<Set<String>> setsOfTokensPrecedingTokenInIndividualSequences =
				setsOfTokensPrecedingATokenInIndividualSequences.get(token);
			for (Set<String> setOfTokensPrecedingTokenInAnIndividualSequence
					: setsOfTokensPrecedingTokenInIndividualSequences) {
				if (appearsOnItsOwnInThisSequence(setOfTokensPrecedingTokenInAnIndividualSequence)) {
					return false;
				}
			}
			return true;
		}

		public boolean appearsOnItsOwnInThisSequence(Set<String> setOfTokensPrecedingTokenInAnIndividualSequence) {
			boolean result =
				Util.forAll(setOfTokensPrecedingTokenInAnIndividualSequence, isNotParenthetical);
			return result;
		}
	}
	
	@Override
	public boolean globalFollowingTokensInformationAlreadyDefined() {
		return parentheticalPairs != null;
	}
	
	@Override
	public CloserTokensResult consumeCloserTokensOrMoveUpToMisplacedCloserTokenOrLimit(Set<String> closerTokens) {
		return processCloserTokensOrMoveUpToMisplacedCloserTokenOrLimit(closerTokens, false /* do not stop at first closing token */);
	}

	@Override
	public CloserTokensResult moveUpToFirstCloserTokenOrMisplacedCloserTokenOrLimit(Set<String> closerTokens) {
		return processCloserTokensOrMoveUpToMisplacedCloserTokenOrLimit(closerTokens, true /* stop at first closing token */);
	}

	/**
	 * Looks tokens ahead looking for a set of tokens always following some previously seen (and not represented here) token.
	 * Multiple types of results are returned (see hierarchy of {@link CloserTokensResult}).
	 */
	private CloserTokensResult processCloserTokensOrMoveUpToMisplacedCloserTokenOrLimit(Set<String> closerTokens, boolean stopAtFirstCloserToken) {
		List<String> tokens = new LinkedList<String>();
		closerTokens = new HashSet<String>(closerTokens); // make copy of closerTokens because they are removed from set as method proceeds.
		while (closerTokens.size() > 0 &&
				tokenIterator.hasNext() &&
				getTokenPosition() < currentTokenizerPositionLimit()) {
			String token = tokenIterator.nextToken();
			tokens.add(token);
			if (closerTokens.contains(token)) {
				if (stopAtFirstCloserToken) {
					return new FirstCloserTokenFound(token, getTokenPosition() - 1, tokens);
				}
				else {
					closerTokens.remove(token);
				}
			}
			else {
				// at this point, we know the token is not one of those we are seeking.
				// Now we check whether it is not a closing token of some other token,
				// which must be missing because otherwise it would have started a recursive call of this method.
				if (closersOfSomeToken.contains(token)) {
					return new MismatchedCloserTokenFound(token, getTokenPosition() - 1, tokens);
				}

				Set<String> newCloserTokens = closersOfAToken.get(token);
				if (newCloserTokens != null) {
					CloserTokensResult recursiveResult =
						consumeCloserTokensOrMoveUpToMisplacedCloserTokenOrLimit(newCloserTokens);
					if (recursiveResult.success) {
						tokens.addAll(recursiveResult.tokens);
					}
					else {
						recursiveResult.tokens.addAll(0, tokens);
						return recursiveResult;
					}
				}
				// Obs: note that this check for new following tokens is in the else branch in which 'token' is not one of the sought following branches.
				// A sought following token could also have its own following tokens.
				// For example, in T1 T2 T3, T3 follows both T1 and T2.
				// It is unnecessary to recursively seek the followers of T2, because by transitivity they are also followers of T1 and are already being sought.
				// In fact, it would be problematic to do so, because T3 would be consumed as a follower of T2 and not found later as a follower of T1.
			}
		}
		CloserTokensResult result;
		if (closerTokens.isEmpty()) {
			result = new AllCloserTokensConsumed(getTokenPosition() - 1, tokens); // everything's all right
		}
		else {
			result = new LimitOrEndOfTokensReached(getTokenPosition() - 1, tokens); // reached limit or ran out of tokens
		}
		return result;
	}

	public DefaultParsingProcess(Reader reader, Grammar grammar) {
		try {
			this.tokenIterator = new TokenIterator(reader);
		} catch (IOException e) {
			throw new Error(e);
		}

		this.grammar = grammar;
		for (ParsingExpression parsingExpression : grammar.values()) {
			registerParsingExpressionsAndSubParsingExpressions(parsingExpression, this);
		}
		
		pushNewConjunctionOfPrecedenceConditionsOnFirstParse();
		pushNewConjunctionOfPrecedenceConditionsOnLastParse();
	}

	public DefaultParsingProcess(String string, Grammar grammar) {
		this(new StringReader(string), grammar);
	}

	@Override
	public TokenIterator getTokenIterator() {
		return tokenIterator;
	}
	
	@Override
	public int getTokenPosition() {
		return tokenIterator.getPosition();
	}

	@Override
	public void advanceNTokens(int n) {
		tokenIterator.advanceNTokens(n);
	}

	@Override
	public boolean hasNextTokenAccordingToCurrentConditions() {
		boolean tokenIteratorHasNextToken = tokenIterator.hasNext();
		boolean tokenPositionHasNotReachedLimitYet =
				Util.compareIntegersWithMinusOneMeaningInfinite(
						getTokenPosition(),
						currentTokenizerPositionLimit()) < 0;
		boolean result = tokenIteratorHasNextToken && tokenPositionHasNotReachedLimitYet;
		return result;
	}
	
	@Override
	public String nextTokenAccordingToCurrentConditions() {
		return tokenIterator.nextToken();
	}
	
	@Override
	public void putBack(String token) {
		tokenIterator.putBack(token);
	}

	@Override
	public void putBack(List<String> tokens) {
		tokenIterator.putBack(tokens);
	}

	@Override
	public void putBackFromParsingResults(LinkedList<ParsingResult> parsingResults) {
		Iterator<ParsingResult> iterator = parsingResults.descendingIterator();
		while (iterator.hasNext()) {
			ParsingResult parsingResult = iterator.next();
			tokenIterator.putBack(parsingResult.getTokens());
		}
	}

	@Override
	public List<String> remainingTokens() {
		return Util.listFrom(tokenIterator);
	}

	/** 
	 * Register parsing expression, that is, take whatever information needs to be taken from it,
	 * such as special tokens, and returns whether it was necessary to actually do it (that is, it had not been seen before).
	 */
	@Override
	public boolean registerParsingExpressionsAndSubParsingExpressions(ParsingExpression parsingExpression, ParsingProcess process) {
		parsingExpression = parsingExpression.equivalentSimplerParsingExpression(this);

		if (registeredParsingExpressions.contains(parsingExpression)) {
			return false;
		}
		
		registeredParsingExpressions.add(parsingExpression);
		
		registerIndividualParsingExpression(parsingExpression);
		
		for (ParsingExpression toBeRegistered : parsingExpression.parsingExpressionsToBeRegistered(process)) {
			registerParsingExpressionsAndSubParsingExpressions(toBeRegistered, process);
		}

		Iterator<Expression> subParsingExpressionsIterator = parsingExpression.getArguments().iterator();
		while (subParsingExpressionsIterator.hasNext()) {
			Expression subExpression = subParsingExpressionsIterator.next();
			if (subExpression instanceof ParsingExpression) {
				registerParsingExpressionsAndSubParsingExpressions((ParsingExpression) subExpression, process);
				//					System.out.println("Registering " + parsingExpression.hashCode() + ": " + parsingExpression);
			}
		}
		return true;
	}

	public void registerIndividualParsingExpression(ParsingExpression parsingExpression) {
		if (parsingExpression.hasFunctor("terminal") && parsingExpression.numberOfArguments() > 0) {
			String terminalString = parsingExpression.get(0).toString();
			tokenIterator.putSpecialTokenIfItIsOne(terminalString);
		}
		collectPrecedingFollowingAndNonParentheticalTokens(parsingExpression);
//		System.out.println("Registering " + parsingExpression.hashCode() + ": " + parsingExpression);
	}

	private boolean haveAlreadySetLimitBasedOnString = false;

	@Override
	public void setInitialTokenPositionLimitIfTokenIteratorIsBasedOnString() {
		if (ParserFlags.lookAhead && !haveAlreadySetLimitBasedOnString && tokenIterator.isBasedOnString() && tokenIterator.getPosition() == 0) {
			// there is a definite end to the token stream, so we set that limit to take advantage of lookahead (see Sequence).
			List<String> tokens = new LinkedList<String>();
			while (tokenIterator.hasNext()) {
				String token = tokenIterator.nextToken();
				tokens.add(token);
			}
			pushTokenizerPositionLimit(tokens.size());
			tokenIterator.putBack(tokens);
			logln("Number of tokens: " + tokens.size());
			haveAlreadySetLimitBasedOnString = true;
		}
	}

	@Override
	public Expression parseOfNonTerminal(String nonTerminalName) {
		Expression result;
		result = getGrammar().get(nonTerminalName).parse(this);
		return result;
	}

	@Override
	public ParsingResult parsingResultOfNonTerminal(String nonTerminalName) {
		return getGrammar().get(nonTerminalName).parsingResult(this);
	}

	@Override
	public ParsingExpression getParent() {
		if (stackOfParsingExpressions.size() < 2) {
			return null;
		}
		ParsingExpression current = stackOfParsingExpressions.pop();
		ParsingExpression parent = stackOfParsingExpressions.peek();
		stackOfParsingExpressions.push(current);
		return parent;
	}
	
	@Override
	public void pushParsingExpression(ParsingExpression parent) {
		stackOfParsingExpressions.push(parent);
	}
	
	@Override
	public ParsingExpression popParsingExpressions() {
		return stackOfParsingExpressions.pop();
	}
	
	@Override
	public void log(Object object) {
		if (ParserFlags.logIsOn) {
			log.print(Util.times(getLevel(), "*") + object);
		}
	}

	@Override
	public void logln(Object object) {
		if (ParserFlags.logIsOn) {
			log(object + "\n");		
		}
	}

	@Override
	public void conditionalLogln(boolean condition, Object object) {
		if (condition && ParserFlags.logIsOn) {
				log(object + "\n");
		}
	}
	
	@Override
	public void logrestln(Object object) {
		if (ParserFlags.logIsOn) {
			log.println(object);
		}
	}

	public void logf(String format, Object object) {
		if (ParserFlags.logIsOn) {
			log.printf(Util.times(getLevel(), "*") + format, object);
		}
	}

	public void loglnf(String format, Object object) {
		if (ParserFlags.logIsOn) {
			logf(format + "\n", object);		
		}
	}

	public void logrestlnf(String format, Object object) {
		if (ParserFlags.logIsOn) {
			log.printf(format + "\n", object);		
		}
	}

	@Override
	public void pushPrecedenceConditionToCurrentConjunctionOnFirstParse(Expression condition) {
		addConditionToNormalizedConjunctionOnTopOfStack(condition, precedenceConditionConjunctionsOnFirstParseStack);
	}

	private void addConditionToNormalizedConjunctionOnTopOfStack(
			Expression condition,
			final Stack<List<Expression>> stackOfPrecedenceConditionsConjunctions) {
		final List<Expression> currentConjunction = stackOfPrecedenceConditionsConjunctions.peek();
		LinkedList<Expression> newConjunction = new LinkedList<Expression>(currentConjunction);
		addConditionToNormalizedConjunction(newConjunction, (AbstractAtomicParsingConstraint) condition);
		stackOfPrecedenceConditionsConjunctions.push(newConjunction);
	}
	
	private static void addConditionToNormalizedConjunction(Collection<Expression> normalizedConjunction, AbstractAtomicParsingConstraint newCondition) {
		AbstractAtomicParsingConstraint sameDisjunctionIfAny =
			removeAndReturnConjunctWithSameDisjunction(normalizedConjunction, newCondition);
		AbstractAtomicParsingConstraint newConjunct;
		if (sameDisjunctionIfAny != null) {
			List<Expression> conjunction = Util.list((Expression) sameDisjunctionIfAny, newCondition);
			newConjunct = normalizePrecedenceConditionsOnSameDisjunction(conjunction);
		}
		else {
			newConjunct = newCondition;
		}
		normalizedConjunction.add(newConjunct);
	}

	private static AbstractAtomicParsingConstraint removeAndReturnConjunctWithSameDisjunction(
			Collection<Expression> normalizedConjunction,
			AbstractAtomicParsingConstraint newCondition) {
		for (Expression expression : normalizedConjunction) {
			AbstractAtomicParsingConstraint condition = (AbstractAtomicParsingConstraint) expression;
			if (condition.getDisjunction() == newCondition.getDisjunction()) {
				normalizedConjunction.remove(condition);
				return condition;
			}
		}
		return null;
	}
	
	private static AbstractAtomicParsingConstraint normalizePrecedenceConditionsOnSameDisjunction(Collection<Expression> conjunction) {
		int maxIndex = -1;
		Expression maxDisjunct = null;
		Expression disjunction = null;
		boolean equalIsOk = false; // indicates whether final condition will be "OrEqualTo"
		for (Expression expression : conjunction) {
			if (expression instanceof GreaterThanOrUnrelated) {
				GreaterThanOrUnrelated condition = (GreaterThanOrUnrelated) expression;
				if (condition.getIndexOfDisjunctInDisjunction() > maxIndex) {
					maxIndex = condition.getIndexOfDisjunctInDisjunction();
					maxDisjunct = condition.getDisjunct();
					disjunction = condition.getDisjunction();
					equalIsOk = false;
				}
			}
			else if (expression instanceof GreaterThanOrEqualToOrUnrelated) {
				GreaterThanOrEqualToOrUnrelated condition = (GreaterThanOrEqualToOrUnrelated) expression;
				if (condition.getIndexOfDisjunctInDisjunction() > maxIndex) {
					maxIndex = condition.getIndexOfDisjunctInDisjunction();
					maxDisjunct = condition.getDisjunct();
					disjunction = condition.getDisjunction();
					equalIsOk = true;
				}
			}
		}
		
		if (equalIsOk) {
			return new GreaterThanOrEqualToOrUnrelated((ParsingExpression)maxDisjunct, (ParsingExpression) disjunction);
		}

		return new GreaterThanOrUnrelated((ParsingExpression)maxDisjunct, (ParsingExpression) disjunction);
	}
	
	@Override
	public void popPrecedenceConditionFromCurrentConjunctionOnFirstParse() {
		precedenceConditionConjunctionsOnFirstParseStack.pop();
	}

	///////////////// TOKENIZER POSITION LIMIT //////////////////
	
	private Stack<Integer> stackOfTokenizerPositionLimits = new Stack<Integer>();
	
	@Override
	public void pushTokenizerPositionLimit(int limitPosition) {
		stackOfTokenizerPositionLimits.push(limitPosition);
		logln("Pushed limit to " + limitPosition);
		logln("Limit stack is " + stackOfTokenizerPositionLimits);
	}
	
	@Override
	public int  popTokenizerPositionLimit() {
		Integer result = stackOfTokenizerPositionLimits.pop();
		logln("Pop limit " + result);
		logln("Limit stack is " + stackOfTokenizerPositionLimits);
		return result;
	}
	
	@Override
	public int currentTokenizerPositionLimit() {
		if (stackOfTokenizerPositionLimits.isEmpty()) {
			return -1;
		}
		return stackOfTokenizerPositionLimits.peek();
	}
	
	///////////////////////////////////////////////////////////////
	
	@Override
	public boolean currentTokenizerPositionLimitIsSatisfiedByCachedConditions(CacheItem cacheItem) {
		int tokenizerPositionLimit = cacheItem.getTokenPositionLimit();
		if ( ! DefaultParsingResult.isSuccessful(cacheItem.getParsingResult())) {
			if ( ! cacheItem.getParsingResult().tokenPositionLimitInfluencedResult()) {
				// if the cause of failure was not hitting the token position limit, then we know that a new parsing, no matter what limit, will also fail.
				return true; // use the cache item, that is, indicate parsing failure.
			}
			// if there was no parse due to the token position limit,
			// the only chance for the current limit to do better is to be greater than the cached limit.
			if (Util.compareIntegersWithMinusOneMeaningInfinite(currentTokenizerPositionLimit(), tokenizerPositionLimit) > 0) {
				return false; // false means the cache item will not be used and parsing will be attempted.
			}
			return true; // it failed due to limit, and the current limit is less than or equal to cached limit, so reuse cache item and indicate parsing failure.
		}
		// Now we deal with the situation in which the cache item represents a successful parse.
		int parseTokenizerPositionLimit = cacheItem.getParseTokenizerPositionLimit();
		// The parseTokenizerPositionLimit is always <= tokenizerPositionLimit (because it was parsed under that constraint),
		// so we have three possibilities:
		// the current tokenizer position limit falls before both of them,
		// or in between them, or after both of them.
		// If it falls before both of them, then the cached conditions are not good enough
		// because the parse goes beyond the current limit.
		// if it falls after both of them, then the cached conditions are not good enough
		// because perhaps parsing the current parsing expression with a greater limit will provide a longer parse.
		// If parseTokenizerPositionLimit <= current limit <= tokenizerPositionLimit,
		// then the conditions satisfy the current limit.
		if (
				Util.compareIntegersWithMinusOneMeaningInfinite(
						parseTokenizerPositionLimit, currentTokenizerPositionLimit()) <= 0 &&
				Util.compareIntegersWithMinusOneMeaningInfinite(
						currentTokenizerPositionLimit(), tokenizerPositionLimit) <= 0) {
			return true;
		}
		return false;
	}
	
	@Override
	public int lookAheadForTokenFromPositionWithLimit(String token, int i, int limit) {
		LinkedList<String> tokens = new LinkedList<String>();
		int result = -1;
		while (tokenIterator.getPosition() != i && tokenIterator.hasNext()) {
			tokens.add(tokenIterator.nextToken());
		}
		// now we are at i
		while (
				Util.compareIntegersWithMinusOneMeaningInfinite(tokenIterator.getPosition(), limit) != 0
				&&
				tokenIterator.hasNext()) {
			String nextToken = tokenIterator.nextToken();
			tokens.add(nextToken);
			if (nextToken.equals(token)) {
				result = tokenIterator.getPosition() - 1;
			}
		}
		tokenIterator.putBack(tokens);
		return result;
	}

//	@Override
	public int lookAheadForTokenBackwardsFromGivenPositionOld(String token, int i) {
		LinkedList<String> tokens = new LinkedList<String>();
		int result = -1;
		while (tokenIterator.getPosition() != i && tokenIterator.hasNext()) {
			String currentToken = tokenIterator.nextToken();
			if (currentToken.equals(token)) {
				result = tokenIterator.getPosition() - 1; // the -1 is due to the fact that the getPosition() method indicates the position of the next token.
			}
			tokens.add(currentToken);
		}
		tokenIterator.putBack(tokens);
		return result;
	}

	@Override
	public int lookAheadForTokenBackwardsFromGivenPosition(String token, int i) {
		if (i > tokenIterator.getPosition()) {
			i--;
			tokenIterator.makeSureIthPositionIsInRandomAccess(i);
			while (i >= tokenIterator.getPosition()) {
				String iThToken = tokenIterator.peekAtIthTokenAfterMakingSureItIsInRandomAccess(i);
				if (iThToken.equals(token)) {
					return i;
				}
				i--;
			}
		}
		return -1;
	}

	@Override
	public boolean currentConjunctionOfPrecedenceConditionsOnFirstParseIsSatisfiedBy(ParsingExpression parsingExpression) {
		Collection<Expression> set = precedenceConditionConjunctionsOnFirstParseStack.peek();
		return satisfiesSetOfPrecedenceConditions(parsingExpression, set);
	}

	public boolean currentConjunctionOfPrecedenceConditionsOnFirstParseIsSatisfiedByOld(
			Collection<Expression> precedenceConditions) {
		return precedenceConditions.containsAll(precedenceConditionConjunctionsOnFirstParseStack.peek());
	}

	@Override
	public boolean currentConjunctionOfPrecedenceConditionsOnFirstParseIsEquivalentTo(
			Collection<Expression> precedenceConditions) {
		return equivalent(precedenceConditions, precedenceConditionConjunctionsOnFirstParseStack.peek());
	}

	private boolean equivalent(Collection<Expression> precedenceConditions, final List<Expression> conjunction) {
		if (precedenceConditions.size() != conjunction.size()) {
			return false;
		}
		return new HashSet<Expression>(precedenceConditions).equals(new HashSet<Expression>(conjunction));
	}

	@Override
	public boolean currentConjunctionOfPrecedenceConditionsOnFirstParseIsSatisfiedBy(
			Collection<Expression> precedenceConditions) {
		return topOfStackIsSatisfiedBy(precedenceConditions, precedenceConditionConjunctionsOnFirstParseStack);
	}

	private boolean topOfStackIsSatisfiedBy(
			Collection<Expression> precedenceConditions,
			final Stack<List<Expression>> stackOfPrecedenceConditionsConjunctions) {
		final List<Expression> conjunction = stackOfPrecedenceConditionsConjunctions.peek();
		for (Expression condition : conjunction) {
			if (!implies(precedenceConditions, (ParsingConstraint) condition)) {
				return false;
			}
		}
		return true;
	}

	private boolean implies(Collection<Expression> precedenceConditions,
			ParsingConstraint condition) {
		for (Expression condition1 : precedenceConditions) {
			if (implies((ParsingConstraint)condition1, condition)) {
				return true;
			}
		}
		return false;
	}

	private boolean implies(ParsingConstraint condition1, ParsingConstraint condition2) {
		if (condition1 instanceof GreaterThanOrUnrelated && condition2 instanceof GreaterThanOrUnrelated) {
			GreaterThanOrUnrelated c1 = (GreaterThanOrUnrelated) condition1;
			GreaterThanOrUnrelated c2 = (GreaterThanOrUnrelated) condition2;
			return c1.getDisjunction() == c2.getDisjunction() &&
			c1.getIndexOfDisjunctInDisjunction() >= c2.getIndexOfDisjunctInDisjunction();
		}
		else if (condition1 instanceof GreaterThanOrUnrelated && condition2 instanceof GreaterThanOrEqualToOrUnrelated) {
			GreaterThanOrUnrelated          c1 = (GreaterThanOrUnrelated) condition1;
			GreaterThanOrEqualToOrUnrelated c2 = (GreaterThanOrEqualToOrUnrelated) condition2;
			return c1.getDisjunction() == c2.getDisjunction() &&
			c1.getIndexOfDisjunctInDisjunction() >= c2.getIndexOfDisjunctInDisjunction();
		}
		else if (condition1 instanceof GreaterThanOrEqualToOrUnrelated && condition2 instanceof GreaterThanOrUnrelated) {
			GreaterThanOrEqualToOrUnrelated c1 = (GreaterThanOrEqualToOrUnrelated) condition1;
			GreaterThanOrUnrelated          c2 = (GreaterThanOrUnrelated) condition2;
			return c1.getDisjunction() == c2.getDisjunction() &&
			c1.getIndexOfDisjunctInDisjunction() > c2.getIndexOfDisjunctInDisjunction();
		}
		else if (condition1 instanceof GreaterThanOrEqualToOrUnrelated && condition2 instanceof GreaterThanOrEqualToOrUnrelated) {
			GreaterThanOrEqualToOrUnrelated c1 = (GreaterThanOrEqualToOrUnrelated) condition1;
			GreaterThanOrEqualToOrUnrelated c2 = (GreaterThanOrEqualToOrUnrelated) condition2;
			return c1.getDisjunction() == c2.getDisjunction() &&
			c1.getIndexOfDisjunctInDisjunction() >= c2.getIndexOfDisjunctInDisjunction();
		}
		Util.fatalError("Uncovered case in implies: " + condition1 + ", " + condition2);
		return false;
	}

	@Override
	public void pushNewConjunctionOfPrecedenceConditionsOnFirstParse() {
		precedenceConditionConjunctionsOnFirstParseStack.push(new LinkedList<Expression>());
	}

	@Override
	public void popConjunctionOfPrecedenceConditionsOnFirstParse() {
		precedenceConditionConjunctionsOnFirstParseStack.pop();
	}
	
	@Override
	public void pushPrecedenceConditionToCurrentConjunctionsOnLastParse(Expression condition) {
		addConditionToNormalizedConjunctionOnTopOfStack(condition, precedenceConditionConjunctionsOnLastParseStack);
	}
	
	@Override
	public void popPrecedenceConditionFromCurrentConjunctionOnLastParse() {
		precedenceConditionConjunctionsOnLastParseStack.pop();	
	}

	@Override
	public boolean currentConjunctionOfPrecedenceConditionsOnLastParseIsSatisfiedBy(ParsingExpression parsingExpression) {
		Collection<Expression> set = precedenceConditionConjunctionsOnLastParseStack.peek();
		return satisfiesSetOfPrecedenceConditions(parsingExpression, set);
	}

	@Override
	public boolean currentConjunctionOfPrecedenceConditionsOnLastParseIsEquivalentTo(
			Collection<Expression> precedenceConditions) {
		return equivalent(precedenceConditions, precedenceConditionConjunctionsOnLastParseStack.peek());
	}

	@Override
	public boolean currentConjunctionOfPrecedenceConditionsOnLastParseIsSatisfiedBy(
			Collection<Expression> precedenceConditions) {
		return topOfStackIsSatisfiedBy(precedenceConditions, precedenceConditionConjunctionsOnLastParseStack);
	}

	@Override
	public void pushNewConjunctionOfPrecedenceConditionsOnLastParse() {
		precedenceConditionConjunctionsOnLastParseStack.push(new LinkedList<Expression>());
	}

	@Override
	public void popConjunctionOfPrecedenceConditionsOnLastParse() {
		precedenceConditionConjunctionsOnLastParseStack.pop();
	}

	
	
	@Override
	public boolean currentConjunctionOfPrecedenceConditionsIsSatisfiedBy(ParsingExpression parsingExpression) {
		return
		currentConjunctionOfPrecedenceConditionsOnFirstParseIsSatisfiedBy(parsingExpression) &&
		currentConjunctionOfPrecedenceConditionsOnLastParseIsSatisfiedBy(parsingExpression);
	}

	@Override
	public Map<String, ParsingExpression> getGrammar() {
		return grammar;
	}

	@Override
	public int getLevel() {
		return level;
	}

	@Override
	public void pushLevel() {
		level++;
	}

	@Override
	public void popLevel() {
		level--;
	}

	@Override
	public Collection<Expression> getPrecedenceConditionsOnFirstParse() {
		return precedenceConditionConjunctionsOnFirstParseStack.peek();
	}

	@Override
	public Collection<Expression> getPrecedenceConditionsOnLastParse() {
		return precedenceConditionConjunctionsOnLastParseStack.peek();
	}

	@Override
	public void putCacheItem(CacheItem cacheItem) {
		cache.putCacheItem(cacheItem, this);
	}

	@Override
	public CacheItem getCacheItemOrNull(ParsingExpression parsingExpression) {
		return cache.tryToUseCache(parsingExpression, this);
	}
	
	public void logCache() {
		cache.logCache(this);
	}

	public void logConditions() {
		if (ParserFlags.logIsOn) {
			logln("Conditions");
			pushLevel();

			logln("--- On first parse");
			for (Expression condition : precedenceConditionConjunctionsOnFirstParseStack.peek()) {
				logln(condition);
			}

			logln("--- On last parse");
			for (Expression condition : precedenceConditionConjunctionsOnFirstParseStack.peek()) {
				logln(condition);
			}

			popLevel();
		}
	}

	////////////////////////////////////////////////////////////////////////
	// Methods for precedence conditions
	////////////////////////////////////////////////////////////////////////
	
	private boolean satisfiesSetOfPrecedenceConditions(
			ParsingExpression parsingExpression, Collection<Expression> set) {
		for (Expression condition : set) {
			if (! satisfies(parsingExpression, condition)) {
				return false;
			}
		}
		return true;
	}

	public static boolean satisfies(ParsingExpression parsingExpression, Expression condition) {
		return ((ParsingConstraint)condition).apply(parsingExpression);
	}

	Map<ParsingExpression, Integer> lengthLowerBoundCache = new HashMap<ParsingExpression, Integer>();
	
	@Override
	public int getLengthLowerBound(ParsingExpression parsingExpression) {
		Integer lengthLowerBound = lengthLowerBoundCache.get(parsingExpression);
		if (lengthLowerBound == null) {
			lengthLowerBound = parsingExpression.computeLengthLowerBound(new Stack<ParsingExpression>(), this);
		}
		return lengthLowerBound.intValue();
	}
}
