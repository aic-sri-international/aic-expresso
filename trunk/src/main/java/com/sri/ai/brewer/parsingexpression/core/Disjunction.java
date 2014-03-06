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


import java.util.Stack;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.brewer.api.ParsingProcess;
import com.sri.ai.brewer.core.DefaultParsingProcess;
import com.sri.ai.brewer.core.DefaultParsingResult;
import com.sri.ai.brewer.core.ParsingResult;
import com.sri.ai.brewer.parsingconstraint.GreaterThanOrEqualToOrUnrelated;
import com.sri.ai.brewer.parsingconstraint.GreaterThanOrUnrelated;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.Util;

/**
 * A parsing expression formed as a disjunction of other parsing expressions,
 * and parsing successfully if any of the disjuncts is parsed successfully. The
 * order of disjuncts is significant. They are checked in the order in the
 * disjunct. This can be used to determine operator precedence; smaller
 * precedence expressions are the first ones in the disjunction, and therefore
 * checked first. This causes the desired effect of higher precedence
 * expressions being subexpressions.
 * 
 * @author braz
 */
@Beta
public class Disjunction extends AbstractParsingExpression {
	private static final long serialVersionUID = 1L;

	public Disjunction(Object... args) {
		super("disjunction", args);
	}

	@Override
	protected ParsingResult parsingResultAfterBookkeeping(ParsingProcess process) {
		// Note: we used to iterate over disjuncts until one of them parsed.
		// Now, we check the first one satisfying conditions, and if that fails then try to disjunction as a whole again,
		// with conditions to avoid testing that first one again.
		// The reason for this more convoluted way of parsing disjunctions is that
		// trying to parse the disjunction as a whole allows the cache to identify previous attempts
		// of parsing it with that set of conditions.
		
		final DefaultParsingProcess finalModifiedProcess = (DefaultParsingProcess) process;
		finalModifiedProcess.logConditions();
		ParsingExpression disjunct =
			Util.getFirstSatisfyingPredicateOrNull(
					getParsingExpressionArguments(), new Predicate<ParsingExpression>() { 
						@Override
						public boolean apply(ParsingExpression disjunct) {
							final boolean result = finalModifiedProcess.currentConjunctionOfPrecedenceConditionsIsSatisfiedBy(disjunct);
							return result;
						}});

		if (disjunct != null) {
			finalModifiedProcess.logln("First disjunct satisfying conditions: " + disjunct);

			// enter conditions that must be satisfied by subexpressions of current disjunct
			process.pushPrecedenceConditionToCurrentConjunctionOnFirstParse(new GreaterThanOrUnrelated(disjunct, this));
			process.pushPrecedenceConditionToCurrentConjunctionsOnLastParse(new GreaterThanOrEqualToOrUnrelated(disjunct, this));

			ParsingResult disjunctParsingResult = disjunct.parsingResult(process);

			// remove them
			process.popPrecedenceConditionFromCurrentConjunctionOnFirstParse();
			process.popPrecedenceConditionFromCurrentConjunctionOnLastParse();

			ParsingResult finalParsingResult;
			if (DefaultParsingResult.isSuccessful(disjunctParsingResult)) {
				finalParsingResult = new DefaultParsingResult(this, disjunctParsingResult.getTokens(), disjunctParsingResult.getParse(), disjunctParsingResult.tokenPositionLimitInfluencedResult());
			}
			else {
				// after failed for first satisfying disjunct, place conditions so that only the next disjunct is selected:
				process.pushPrecedenceConditionToCurrentConjunctionOnFirstParse(new GreaterThanOrUnrelated(disjunct, this));
				process.pushPrecedenceConditionToCurrentConjunctionsOnLastParse(new GreaterThanOrUnrelated(disjunct, this));
				
				// now parse this same disjunction, but effectively selecting the next disjunct:
				ParsingResult remainingDisjunctsParsingResult = this.parsingResult(process);
				
				process.popPrecedenceConditionFromCurrentConjunctionOnFirstParse();
				process.popPrecedenceConditionFromCurrentConjunctionOnLastParse();
				
				boolean tokenPositionLimitInfluencedResult =
					disjunctParsingResult.tokenPositionLimitInfluencedResult()
					||
					remainingDisjunctsParsingResult.tokenPositionLimitInfluencedResult();
				
				if (DefaultParsingResult.isSuccessful(remainingDisjunctsParsingResult)) {
					finalParsingResult = new DefaultParsingResult(
							this,
							remainingDisjunctsParsingResult.getTokens(),
							remainingDisjunctsParsingResult.getParse(),
							tokenPositionLimitInfluencedResult);
				}
				else {
					finalParsingResult = new DefaultParsingResult(tokenPositionLimitInfluencedResult);
				}
			}
			return finalParsingResult;
		}
		else {
			finalModifiedProcess.logln("No disjunct satisfies conditions.");
			return DefaultParsingResult.makeFailedParsingResult(false /* limit did not influence result */);
		}
	}

	@Override
	public ParsingExpression equivalentSimplerParsingExpression(ParsingProcess process) {
		return this;
	}

	@Override
	public int computeLengthLowerBoundAfterBookkeeping(Stack<ParsingExpression> beingComputed, ParsingProcess process) {
		int minimumSubLengthLowerBound = Integer.MAX_VALUE;
		for (Expression subExpression : this.getArguments()) {
			ParsingExpression disjunct = (ParsingExpression) subExpression;
			// Note: if the disjunct has already been computed, we ignore it
			// because that will be zero and will set the minimumSubLengthLowerBound unfairly.
			// If no other disjunction is not already being computed,
			// then the only options are disjuncts already being computed,
			// which means this parsing expression recurses infinitely anyway,
			// in which case MAX_VALUE is an appropriate answer.
			if (!beingComputed.contains(disjunct)) {
				int subLengthLowerBound = disjunct.computeLengthLowerBound(beingComputed, process);
				if (subLengthLowerBound < minimumSubLengthLowerBound) {
					minimumSubLengthLowerBound = subLengthLowerBound;
				}
			}
		}
		return minimumSubLengthLowerBound;
	}
}
