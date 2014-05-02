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
package com.sri.ai.brewer.parsingconstraint;


import com.google.common.annotations.Beta;
import com.sri.ai.brewer.api.ParsingExpression;
import com.sri.ai.expresso.helper.Expressions;

/**
 * A precedence condition with respect to a given disjunct in a given
 * disjunction, satisfied by parsing expressions that have precedence greater
 * than or equal to the original disjunct in that disjunction, or is not in the
 * disjunction at all.
 * 
 * @braz
 */
@Beta
public class GreaterThanOrEqualToOrUnrelated extends AbstractAtomicParsingConstraint {
	private static final long serialVersionUID = 1L;
	
	public GreaterThanOrEqualToOrUnrelated(ParsingExpression disjunct, ParsingExpression disjunction) {
		super(disjunct, disjunction);
		valueOrRootSyntaxTree = Expressions.createSymbol(">= or unrelated");
	}

	public GreaterThanOrEqualToOrUnrelated(Side side, ParsingExpression disjunct, ParsingExpression disjunction) {
		super(side, disjunct, disjunction);
		valueOrRootSyntaxTree = Expressions.createSymbol(">= or unrelated");
	}

	@Override
	public boolean apply(ParsingExpression parsingExpression) {
		return
		!disjunction.getArguments().contains(parsingExpression) ||
		getIndexOf(parsingExpression) >= indexOfDisjunctInDisjunction;
	}
	
	public String toString() {
		return ">= " + disjunct + (getSide() != null? " on " + getSide() : "")/*+ " in " + disjunction + " or unrelated"*/;
	}
}
