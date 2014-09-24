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
package com.sri.ai.brewer.parsingexpression.core;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.concurrent.TimeUnit;

import com.google.common.annotations.Beta;
import com.google.common.base.Stopwatch;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.cache.CacheItem;
import com.sri.ai.brewer.core.DefaultParsingProcess;
import com.sri.ai.brewer.core.DefaultParsingResult;
import com.sri.ai.brewer.core.ParserFlags;
import com.sri.ai.brewer.core.ParsingResult;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.ExpressionOnCompoundSyntaxTree;
import com.sri.ai.util.Util;

/**
 * Abstract class implementing the functionality shared by most types of parsing
 * expression, particularly that concerning bookkeeping such as nesting level
 * and the use of equivalent functions. Extenders should define either
 * {@link #equivalentSimplerParsingExpression(ParsingProcess)} or
 * {@link #parsingResultAfterBookkeeping(ParsingProcess)} for its specific
 * functionality. It also defines some helper methods useful in extender
 * implementations.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractParsingExpression extends ExpressionOnCompoundSyntaxTree implements ParsingExpression {
	private static final long serialVersionUID = 1L;
	//
	public static boolean caching = true;
	
	public AbstractParsingExpression(Object functor, Object... args) {
		super(functor, args);
	}
	
	@Override
	public ParsingExpression equivalentSimplerParsingExpression(ParsingProcess process) {
		return this;
	}
	
	@Override
	public Expression parse(ParsingProcess process) {
		ParsingResult result = parsingResult(process);
		if (DefaultParsingResult.isSuccessful(result)) {
			return result.getParse();
		}
		return null;
	}

//	private static Set<ParsingExpression> alreadyShown = new HashSet<ParsingExpression>();
	
	@Override
	public final ParsingResult parsingResult(ParsingProcess process) {
		boolean actuallyRegistered = process.registerParsingExpressionsAndSubParsingExpressions(this, process);
		if (actuallyRegistered || ! process.globalFollowingTokensInformationAlreadyDefined()) {
			process.collectGlobalFollowingTokensInformationFromRegisteredInformation();
		}
		process.setInitialTokenPositionLimitIfTokenIteratorIsBasedOnString();
		
		ParsingResult parsingResult = null;

		if (ParserFlags.pruneByLengthLowerBoundAndLimit) {
			int lengthLowerBound = process.getLengthLowerBound(this);
//			if ( ! alreadyShown.contains(this)) {
//				System.out.println("\nLength lower bound of " + this);
//				System.out.println(lengthLowerBound);
//				alreadyShown.add(this);
//			}
			if (process.currentTokenizerPositionLimit() != -1 &&
					lengthLowerBound >
					(process.currentTokenizerPositionLimit() - process.getTokenPosition())) {
				parsingResult = DefaultParsingResult.makeFailedParsingResult(true /* limit influenced result */);
//				System.out.println("Length lower bound would work here: ");
//				System.out.println("parsing expression: " + this);
//				System.out.println("length lower bound: " + lengthLowerBound);
//				System.out.println("token position: " + process.getTokenPosition());
//				System.out.println("token position limit: " + process.currentTokenizerPositionLimit());
				return parsingResult;
			}
		}
		
		ParsingExpression equivalent = equivalentSimplerParsingExpression(process);
		if (equivalent != this) {
			process.logln("Going to parse equivalent parsing expression " + equivalent);
			ParsingResult parsingResultOfEquivalent = equivalent.parsingResult(process);
			if ( ! DefaultParsingResult.isSuccessful(parsingResultOfEquivalent)) {
				parsingResult = DefaultParsingResult.makeFailedParsingResult(parsingResultOfEquivalent.tokenPositionLimitInfluencedResult());
			}
			else {
				parsingResult = new DefaultParsingResult(this, parsingResultOfEquivalent.getTokens(), parsingResultOfEquivalent.getParse(), parsingResultOfEquivalent.tokenPositionLimitInfluencedResult());
			}
		}
		else {
			process.logln("Trying to parse " + this);
			process.logln("Position of tokenizer before parsing attempt: " + process.getTokenPosition() + ", limit " + process.currentTokenizerPositionLimit());
			((DefaultParsingProcess)process).logConditions();
			process.logln("Parsing attempt details of " + this);
			
			process.pushParsingExpression(this);
			process.pushLevel();

			CacheItem cacheItem = null;
			if (caching) {
				cacheItem = process.getCacheItemOrNull(this);
			}
			
			if (cacheItem == null) {
				Stopwatch stopwatch = Stopwatch.createStarted();
				int initialPositionOfParse = process.getTokenPosition();
				parsingResult = parsingResultAfterBookkeeping(process);
				long time = stopwatch.elapsed(TimeUnit.MILLISECONDS);
				process.popLevel();
				process.popParsingExpressions();

				process.log(time + " ms to decide ");
				if (DefaultParsingResult.isSuccessful(parsingResult)) {
					process.logrestln("parsed: " + parsingResult.getParse());
				}
				else {
					process.logrestln("failure of " + this);
				}

				if (caching) {
					if (DefaultParsingResult.isSuccessful(parsingResult)) {
						process.logln("Tokens: " + parsingResult.getTokens());
					}
					cacheItem = new CacheItem(
							initialPositionOfParse,
							this,
							parsingResult,
							process.getPrecedenceConditionsOnFirstParse(),
							process.getPrecedenceConditionsOnLastParse(),
							process.currentTokenizerPositionLimit());
					process.putCacheItem(cacheItem);
				}

				process.logln("Position of tokenizer after parsing attempt: " + process.getTokenPosition());
				((DefaultParsingProcess)process).logCache();
			}
			else {
				parsingResult = cacheItem.getParsingResult();
				process.popLevel();
				process.popParsingExpressions();
			}
		}
		
		if (DefaultParsingResult.isSuccessful(parsingResult)) {
			parsingResult.setParse(postProcessParse(parsingResult.getParse())); // this can be used, for example, to transform nested applications of associative operators into a single application
		}
		
		return parsingResult;
	}

	/** 
	 * A method for post-processing parsing result's parse.
	 * The default behavior is to do nothing.
	 */
	protected Expression postProcessParse(Expression parse) {
		return parse;
	}

	/**
	 * We implement this stub (instead of leaving it abstract) because some extensions may use equivalences
	 * and not implement it at all, and yet be fully instantiable classes.
	 */
	protected ParsingResult parsingResultAfterBookkeeping(ParsingProcess process) {
		Util.fatalError("parsingResultAfterBookkeeping(ParsingProcess) not implemented for " + getClass());
		return null;
	}

	/**
	 * Helper method collecting the list of tokens in a list of parsing results.
	 */
	protected Collection<String> tokens(List<ParsingResult> results) {
		List<String> tokens = new LinkedList<String>();
		for (ParsingResult result : results) {
			tokens.addAll(result.getTokens());
		}
		return tokens;
	}

	/** Gets the list of arguments as parsing expressions, rather than simply Expressions. */
	@SuppressWarnings("unchecked")
	public List<ParsingExpression> getParsingExpressionArguments() {
		return (List) getArguments();
	}
	
	/** Returns parsing expression we arrive to by following {@link #equivalentSimplerParsingExpression(ParsingProcess)}. */
	@Override
	public ParsingExpression ultimateEquivalence(ParsingProcess process) {
		ParsingExpression current = this;
		ParsingExpression next;
		while ((next = current.equivalentSimplerParsingExpression(process)) != current) {
			current = next;
		}
		return current;
	}

	@Override
	public Collection<ParsingExpression> parsingExpressionsToBeRegistered(ParsingProcess process) {
		Collection<ParsingExpression> result =
			Util.set(
					this,
					equivalentSimplerParsingExpression(process));
		return result;
	}

	abstract public int computeLengthLowerBoundAfterBookkeeping(Stack<ParsingExpression> beingComputed, ParsingProcess process);

	@Override
	public int computeLengthLowerBound(Stack<ParsingExpression> beingComputed, ParsingProcess process) {
		if (beingComputed.contains(this)) {
			return 0;
		}
		beingComputed.push(this);
		int result = computeLengthLowerBoundAfterBookkeeping(beingComputed, process);
		beingComputed.pop();
		return result;
	}
}
