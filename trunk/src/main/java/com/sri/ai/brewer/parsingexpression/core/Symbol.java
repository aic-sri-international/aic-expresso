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
import java.util.Set;
import java.util.Stack;

import com.google.common.annotations.Beta;
import com.google.common.collect.Lists;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.core.DefaultParsingResult;
import com.sri.ai.brewer.core.ParsingResult;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.util.Util;

/**
 * A parsing expression for symbols. Its parse is a {@link DefaultSymbol}. It
 * accepts Unicode identifiers, numbers and single symbols, but rejects strings
 * in {@link #exceptions}, which defaults to
 * <code>"(", ")", "[", "]", "{", "}", ","</code>.
 * 
 * @author braz
 */
@Beta
public class Symbol extends AbstractParsingExpression {
	private static final long serialVersionUID = 1L;

	private static final Set<String> exceptions = Util.set("(", ")", "[", "]", "{", "}", ",");

	private final String quotedNonTerminalName = getQuotedNonTerminalName();
	
	private final Sequence quoteTermParsingExpression =
			quotedNonTerminalName == null
			? null : new Sequence(new Terminal("<"), new NonTerminal(getQuotedNonTerminalName()), new Terminal(">"));

	public Symbol() {
		super("symbol");
	}

	public Symbol(String quotedNonTerminalName) {
		super("symbol", quotedNonTerminalName);
	}
	
	@Override
	protected ParsingResult parsingResultAfterBookkeeping(ParsingProcess process) {
		if (getQuotedNonTerminalName() != null) { // if quoted symbols are accepted
			ParsingResult result = quoteTermParsingExpression.parsingResult(process);

			if (DefaultParsingResult.isSuccessful(result)) { // if this is a quoted term
				// return Symbol named by that term
				return new DefaultParsingResult(this, result.getTokens(), DefaultSymbol.createSymbol(result.getParse().getSyntaxTree().getSubTree(0)), result.tokenPositionLimitInfluencedResult());
			}
		}

		if ( ! process.hasNextTokenAccordingToCurrentConditions()) {
			return DefaultParsingResult.makeFailedParsingResult(true /* limit did influence result */);
		}
		String token = process.nextTokenAccordingToCurrentConditions();
		if ( ! exceptions.contains(token)) {
			return new DefaultParsingResult(this, Lists.newArrayList(token), DefaultSymbol.createSymbol(token), false);
		}
		process.putBack(token);
		return DefaultParsingResult.makeFailedParsingResult(false /* limit did not influence result */);
	}

	private String getQuotedNonTerminalName() {
		if (numberOfArguments() == 0) {
			return null;
		}
		com.sri.ai.expresso.api.Symbol argument = (com.sri.ai.expresso.api.Symbol) get(0);
		Object value = argument.getValue();
		return value.toString();
		// we get value first instead of using toString on the symbol straight because the latter includes quotes when there are spaces.
	}

	@Override
	public Collection<ParsingExpression> parsingExpressionsToBeRegistered(ParsingProcess process) {
		Collection<ParsingExpression> result =
			Util.set(
					this,
					equivalentSimplerParsingExpression(process));
		if (quoteTermParsingExpression != null) {
			result.add(quoteTermParsingExpression);
		}
		return result;
	}

	@Override
	public int computeLengthLowerBoundAfterBookkeeping(Stack<ParsingExpression> beingComputed, ParsingProcess process) {
		return 1;
	}
}
