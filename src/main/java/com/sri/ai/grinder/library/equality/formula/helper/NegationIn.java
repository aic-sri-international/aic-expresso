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
package com.sri.ai.grinder.library.equality.formula.helper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.TotalRewriter;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.ForAll;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;

/**
 * Moves negations in as far as literals in a formula: 
 * 
 * not(true)               -> false
 * not(false)              -> true
 * not(X = Y)              -> X != Y
 * not(X != Y)             -> X = Y
 * not(not(F))             -> F
 * not(F1 and F2)          -> not(F1) or not(F2)
 * not(F1 or F2)           -> not(F1) and not(F2)
 * not(for all X : F)      -> there exists X : not(F)
 * not(there exists X : F) -> for all X : not(F)
 */
@Beta
public class NegationIn extends AbstractRewriter {
	
	public static Expression negationsIn(Expression formula, RewritingProcess process) {
		TotalRewriter cnfRewriter = new TotalRewriter(Arrays.asList((Rewriter)
				new NegationIn()
			));
		Expression result = cnfRewriter.rewrite(formula, process);	
		return result;
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression,
			RewritingProcess process) {
		Expression result = expression;
		
		if (expression.hasFunctor(FunctorConstants.NOT)) {
			Expression negated = expression.get(0);
			
			// not(true) -> false
			if (negated.equals(Expressions.TRUE)) {
				result = Expressions.FALSE;
			} // not(false) -> true
			else if (negated.equals(Expressions.FALSE)) {
				result = Expressions.TRUE;
			} // not(X = Y) -> X != Y
			else if (Equality.isEquality(negated) && negated.numberOfArguments() == 2) {
				result = Disequality.make(negated.get(0), negated.get(1));
			} // not(X != Y) -> X = Y
			else if (Disequality.isDisequality(negated)) {
				result = Equality.make(negated.get(0), negated.get(1));
			} // not(not(F)) -> F
			else if (negated.hasFunctor(FunctorConstants.NOT)) {
				result = negated.get(0);
			} // not(F1 and F2) -> not(F1) or not(F2)
			else if (And.isConjunction(negated) && negated.numberOfArguments() > 0) {
				List<Expression> negatedConjuncts = new ArrayList<Expression>();
				for (Expression conjunct : negated.getArguments()) {
					negatedConjuncts.add(Not.make(conjunct));
				}
				result = Or.make(negatedConjuncts);
			} // not(F1 or F2) -> not(F1) and not(F2)
			else if (Or.isDisjunction(negated) && negated.numberOfArguments() > 0) {
				List<Expression> negatedDisjuncts = new ArrayList<Expression>();
				for (Expression disjunct : negated.getArguments()) {
					negatedDisjuncts.add(Not.make(disjunct));
				}
				result = And.make(negatedDisjuncts);
			} // not(for all X : F) -> there exists X : not(F)
			else if (ForAll.isForAll(negated)) {
				result = ThereExists.make(ForAll.getIndex(negated), Not.make(ForAll.getBody(negated)));
			} // not(there exists X : F) -> for all X : not(F)
			else if (ThereExists.isThereExists(negated)) {
				result = ForAll.make(ThereExists.getIndex(negated), Not.make(ThereExists.getBody(negated)));
			}
		}
		
		return result;
	}
}
