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

import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.core.DefaultParsingResult;
import com.sri.ai.brewer.core.ParsingResult;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.DefaultCompoundSyntaxTree;
import com.sri.ai.util.Util;

/**
 * A Kleene parsing expression tries to parse a base parsing expression as many
 * times as possible (including zero times), each terminated by a given
 * delimiter parsing expression (with default being an optional comma) The
 * delimiter can occur after the last parse, as in, for example, Perl. The
 * resulting parsed expression is an application of "kleene list" on the
 * successive parses, with the exception of cases with a single parse, which is
 * then just returned directly. For example, you can get "kleene(a,b)" or
 * "kleene()", but if the list is formed only by "a", you get "a". The one
 * exception to that no-application-of-kleene-to-single-elements is if that
 * single parse is a kleene list itself, say "kleene(a, b)". In this case you
 * get a kleene list of it, "kleene(kleene(a, b))". The reason for this
 * exception is that returning such single element would be ambiguous with
 * another kleene list.
 * 
 * @author braz
 */
@Beta
public class Kleene extends AbstractParsingExpression {
	private static final long serialVersionUID = 1L;
	
	private int minimumNumberOfElements = -1;

	public Kleene(Object arg) {
		super("kleene", arg, new Terminal(","));
	}

	public Kleene(Object arg, ParsingExpression delimiter) {
		super("kleene", arg, delimiter);
	}

	public Kleene(Object arg, int minimumNumberOfElements) {
		super("kleene", arg, new Terminal(","));
		this.minimumNumberOfElements = minimumNumberOfElements;
	}

	public Kleene(Object arg, ParsingExpression delimiter, int minimumNumberOfElements) {
		super("kleene", arg, delimiter);
		this.minimumNumberOfElements = minimumNumberOfElements;
	}

	@Override
	protected ParsingResult parsingResultAfterBookkeeping(ParsingProcess process) {
		LinkedList<ParsingResult> results = new LinkedList<ParsingResult>();
		boolean tokenPositionLimitInfluencedResult = false;
		ParsingExpression subParsingExpression = (ParsingExpression) get(0);
		ParsingExpression delimiter = (ParsingExpression) get(1);
		do {
			ParsingResult result =
				parsingResultOfSubParsingExpression(subParsingExpression, process);
			tokenPositionLimitInfluencedResult = tokenPositionLimitInfluencedResult || result.tokenPositionLimitInfluencedResult();
			
			if (DefaultParsingResult.isSuccessful(result)) {
				results.add(result);
				ParsingResult separatorResult = delimiter.parsingResult(process);
				if ( ! DefaultParsingResult.isSuccessful(separatorResult)) {
					tokenPositionLimitInfluencedResult = tokenPositionLimitInfluencedResult || separatorResult.tokenPositionLimitInfluencedResult();
					return makeParsingResult(results, tokenPositionLimitInfluencedResult, process);
				}
				else {
					results.add(separatorResult);
				}
			}
			else {
				// need to put last separator back and discard the corresponding parsing result,
				// because it was not really a separator, since this last thing parsed was not one of the elements.
				if ( ! results.isEmpty()) {
					ParsingResult lastSeparatorParsingResult = results.get(results.size()-1);
					lastSeparatorParsingResult.putBack(process);
					results.remove(results.size()-1);
				}
				return makeParsingResult(results, tokenPositionLimitInfluencedResult, process);
			}
		} while (true);
	}

	private ParsingResult parsingResultOfSubParsingExpression(
			ParsingExpression subParsingExpression, ParsingProcess process) {
//		if ( ! results.isEmpty()) { // only first position inherits conditions on first parse, others get clean ones
//			process.pushNewConjunctionOfPrecedenceConditionsOnFirstParse();
//		}
//		// as opposed to sequence, any sub-parse can be the last one,
//		// so they all inherit conditions on last parse.
		
		// the above is not quite correct. We should check for conditions on last parse only
		// if it is indeed the last.
		// This means trying to parse without conditions and requiring it not to be the last.
		// If it is, backtrack and parse again enforcing its being the last parse.
		// This has been entered in to do list.
		
		ParsingResult result = subParsingExpression.parsingResult(process);
		
//		if ( ! results.isEmpty()) { // get back to previous situation
//			process.popConjunctionOfPrecedenceConditionsOnFirstParse();
//		}
		return result;
	}

	private ParsingResult makeParsingResult(LinkedList<ParsingResult> results, boolean tokenPositionLimitInfluencedResult, ParsingProcess process) {
		LinkedList<SyntaxTree> subTrees = new LinkedList<SyntaxTree>();
		for (ParsingResult result : results) {
			if (result.getParsingExpression() != get(1)) {
				subTrees.add(result.getParse() == null? null : result.getParse().getSyntaxTree());
			}
		}
		if (minimumNumberOfElements != -1 && subTrees.size() < minimumNumberOfElements) {
			process.putBackFromParsingResults(results);
			return new DefaultParsingResult(tokenPositionLimitInfluencedResult);
		}
		if (subTrees.size() == 1 && ! firstResultIsKleeneListItself(results)) {
			return new DefaultParsingResult(this, results.get(0).getTokens(), results.get(0).getParse(), tokenPositionLimitInfluencedResult);
		}
		return new DefaultParsingResult(this, tokens(results), new DefaultCompoundSyntaxTree("kleene list", subTrees.toArray()), tokenPositionLimitInfluencedResult);
	}

	private boolean firstResultIsKleeneListItself(List<ParsingResult> results) {
		return results.get(0).getParse() != null && Util.equals(results.get(0).getParse().getSyntaxTree().getRootTree(), "kleene list");
	}

	@Override
	public int computeLengthLowerBoundAfterBookkeeping(Stack<ParsingExpression> beingComputed, ParsingProcess process) {
		return 0;
	}
}
