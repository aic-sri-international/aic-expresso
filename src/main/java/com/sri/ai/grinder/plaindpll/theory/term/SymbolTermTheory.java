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
package com.sri.ai.grinder.plaindpll.theory.term;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.grinder.plaindpll.theory.EqualityTheory.EqualityTheoryConstraint;

@Beta
/** 
 * A {@link TermTheory} for symbol terms.
 */
public class SymbolTermTheory implements TermTheory {


	@Override
	public boolean isTerm(Expression expression, RewritingProcess process) {
		boolean result = expression.getSyntacticFormType().equals("Symbol");
		return result;
	}

	@Override
	public boolean isVariableTerm(Expression expression, RewritingProcess process) {
		boolean result = isTerm(expression, process) && process.isVariable(expression);
		return result;
	}

	@Override
	public boolean equalityBetweenTermsImpliesFurtherFacts() {
		return false;
	}

	@Override
	public boolean disequalityBetweenTermsImpliesFurtherFacts() {
		return false;
	}

	@Override
	public Expression getSplitterTowardDisunifyingDistinctTerms(Expression term, Expression anotherTerm, RewritingProcess process) {
		return null; // if two symbols are distinct, then they are disunified.
	}

	@Override
	public Expression normalizeTermInEquality(Expression term, EqualityTheoryConstraint constraint, RewritingProcess process) {
		return term;
	}

	@Override
	public boolean termsHaveNoArguments() {
		return true;
	}
}