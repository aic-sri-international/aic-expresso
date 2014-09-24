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
import java.util.Stack;

import com.google.common.annotations.Beta;
import com.sri.ai.brewer.core.DefaultParsingResult;
import com.sri.ai.brewer.core.ParsingResult;
import com.sri.ai.expresso.api.Expression;

/**
 * A parsing expression is an expression used for, given a parsing process,
 * produce a {@link DefaultParsingResult}, which contains a resulting parsed
 * expression as well as extra information.
 * 
 * @author braz
 */
@Beta
public interface ParsingExpression extends Expression {

	/** Parses within given parsing process. */
	ParsingResult parsingResult(ParsingProcess process);

	/** Short for <code>parsingResult(constraint, process).getParse()</code>. */
	Expression parse(ParsingProcess process);
	
	/**
	 * Returns <code>this</code>, or, if the parsing expression is processed by replacement by a simpler equivalent parsing expression P,
	 * returns this expression P.
	 */
	ParsingExpression equivalentSimplerParsingExpression(ParsingProcess process);

	/**
	 * Provides a collection of parsing expressions that the parsing expression may use in the course of its parsing.
	 * These are usually auxiliary, intermediate parsing expressions.
	 */
	Collection<ParsingExpression> parsingExpressionsToBeRegistered(ParsingProcess process);
	
	/** Returns parsing expression we arrive to by following {@link #equivalentSimplerParsingExpression(ParsingProcess)}. */
	ParsingExpression ultimateEquivalence(ParsingProcess process);

	/**
	 * Computes the minimum length (in tokens) of this parsing expression in the context of a parsing process.
	 */
	int computeLengthLowerBound(Stack<ParsingExpression> beingComputed, ParsingProcess process);
}
