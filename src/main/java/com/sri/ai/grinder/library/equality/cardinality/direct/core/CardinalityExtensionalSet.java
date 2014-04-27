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
package com.sri.ai.grinder.library.equality.cardinality.direct.core;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;

/**
 * Default implementation of R_cardExtensionalSet( | {t1,...,tn} | ).
 * 
 * @author oreilly
 *
 */
@Beta
public class CardinalityExtensionalSet extends AbstractHierarchicalRewriter implements CardinalityRewriter {
	
	public CardinalityExtensionalSet() {
	}
	
	@Override
	public String getName() {
		return R_cardExtensionalSet;
	}

	/**
	 * @see CardinalityRewriter#R_cardExtensionalSet
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		Expression extensionalSet = null;
		if (expression.hasFunctor(FunctorConstants.CARDINALITY) &&
			expression.numberOfArguments() == 1  &&
			Sets.isExtensionalUniSet(extensionalSet = expression.get(0))) {
			
			Set<Expression> constantElements = new LinkedHashSet<Expression>();
			Set<Expression> variableElements = new LinkedHashSet<Expression>();
			for (Expression element : ExtensionalSet.getElements(extensionalSet)) {
				if (process.isConstant(element)) {
					constantElements.add(element);
				} 
				else {
					variableElements.add(element);
				}
			}
			
			if (constantElements.size() == 0 && variableElements.size() == 0) {
				Trace.log("if n = 0");
				Trace.log("    return 0");
				result = Expressions.ZERO;
			} 
			else if (variableElements.size() == 0) {
				// Is a set of constants just return their size
				Trace.log("if all constant elements");
				Trace.log("    return number of constants");
				result = Expressions.createSymbol(constantElements.size());
			} 
			else {
				Trace.log("N <- R_cardExtensionalSet( | {t2,...,tk} | ) ");
				List<Expression> elements = new ArrayList<Expression>();
				elements.addAll(variableElements);
				elements.addAll(constantElements);
				
				Expression       t1                   = elements.get(0);
				List<Expression> t2ToTk               = elements.subList(1, elements.size());
				Expression       t2ToTkExtensionalSet = ExtensionalSet.makeUniSetExpression(t2ToTk);
				Expression       cardinalityT2ToTk    = Expressions.make(FunctorConstants.CARDINALITY, t2ToTkExtensionalSet);
				Expression       n                    = process.rewrite(R_cardExtensionalSet, cardinalityT2ToTk);
				
				Trace.log("Irrelevant <- R_normalize(t1 = t2 or ... or t1 = tn)");
				boolean          shortCircuited = false;
				Expression       t1EqualTn      = null;
				List<Expression> t1Equalities   = new ArrayList<Expression>();
				for (Expression tn : t2ToTk) {
					t1EqualTn = Equality.make(t1, tn);
					t1EqualTn = process.rewrite(R_normalize, t1EqualTn);
					if (t1EqualTn.equals(Expressions.TRUE)) {
						shortCircuited = true;
						break;
					}
					t1Equalities.add(t1EqualTn);
				}
				Expression irrelevant = null;
				if (shortCircuited) {
					irrelevant = Expressions.TRUE;
				} 
				else {
					irrelevant = Or.make(t1Equalities);
				}
				
				Trace.log("return if Irrelevant then N else plusOne(N)");
				if (irrelevant.equals(Expressions.TRUE)) {
					result = n;
				} 
				else if (irrelevant.equals(Expressions.FALSE)) {
					result = plusOne(n);
				} 
				else {
					result = IfThenElse.make(irrelevant, n, plusOne(n));
				}
			}
		} 
		else {
			throw new IllegalArgumentException("Not an input expression of the form |{t1,...,tn}| :"+expression);
		}
		
		return result;
	}
	
	/**
	 * Takes a counting solution C and returns a counting solution equivalent to C + 1.
	 */
	private static Expression plusOne(Expression countingSolution) {
		Expression result = null;

		if (countingSolution.getSyntaxTree() instanceof Symbol) {
			Number value = (Number) ((Symbol) countingSolution.getSyntaxTree()).getValue();
			result = Expressions.createSymbol(value.intValue() + 1);
		}
		else { // counting solution is an if then else
			Expression condition = IfThenElse.getCondition(countingSolution);
			Expression thenCountingSolution = IfThenElse.getThenBranch(countingSolution);
			Expression elseCountingSolution = IfThenElse.getElseBranch(countingSolution);

			Expression newThenCountingSolution = plusOne(thenCountingSolution);
			Expression newElseCountingSolution = plusOne(elseCountingSolution);
			result = IfThenElse.make(condition, newThenCountingSolution, newElseCountingSolution);
		}
		
		return result;
	}
}
