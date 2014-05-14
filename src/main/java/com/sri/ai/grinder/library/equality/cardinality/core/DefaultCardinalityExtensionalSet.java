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
package com.sri.ai.grinder.library.equality.cardinality.core;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.library.Basic;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSet;

/**
 * Default implementation of R_cardExtensional.
 * 
 * @author saadati
 *
 */
@Beta
public class DefaultCardinalityExtensionalSet extends AbstractHierarchicalRewriter {

	private Basic rBasic = new Basic();
	private boolean assumeConstantsAreDistinct = true; // Set this to true to use a slightly faster algorithm
	public DefaultCardinalityExtensionalSet() {
	}
	
	//
	// START-CardinalityExtensionalSet
	@Override
	public String getName() {
		return "R_cardExtensional";
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression res = null;
//		long begin, end;
//		begin = System.currentTimeMillis();
//		if ( assumeConstantsAreDistinct ) {
//			res = rewriteInternalConstantsAssumedToBeDistinct(expression, process);
//		} 
//		else {
//			res = rewriteInternalGeneral(expression, process);
//		}
		res = rewriteInternalGeneral(expression, process);

		//res = rBasic.rewrite(res);
		//end = System.currentTimeMillis();
		//long diff = end - begin;
		//System.out.println("The time took for extensional set " + expression + " to compute: " + diff);
		return res;
	}
	
	//
	// PRIVATE METHODS
	//
	private Expression rewriteInternalGeneral(Expression expression, RewritingProcess process) {
		Expression result = null;
		if (Sets.isExtensionalMultiSet(expression) ) {
			result = Expressions.makeSymbol(ExtensionalSet.getElements(expression).size());
		} 
		else if ( Sets.isExtensionalUniSet(expression) ) {
			List<Expression> elements = new ArrayList<Expression>();
			for (Expression e: ExtensionalSet.getElements(expression)) {
				if ( !elements.contains(e) ) {
					if ( process.isVariable(e) ) {
						elements.add(0, e); 
					} 
					else {
						elements.add(e);
					}
				}
			}
			if ( elements.size() < 2 ) {
				return Expressions.makeSymbol(elements.size());
			} 
			else {
				Expression first = elements.get(0);
				elements.remove(0);
				boolean isFirstConstant = process.isConstant(first);
				ArrayList<Expression> equalities = new ArrayList<Expression>();
				for (Expression oth: elements) {
					if ( isFirstConstant ) {
						if ( assumeConstantsAreDistinct ) {
							boolean isOtherConstant = process.isConstant(oth);
							if ( isOtherConstant ) { // Constants are different and therefore distinct
								continue;
							}
						}
					}
					Expression eq = Equality.make(first, oth);
					equalities.add(eq);
				}
				Expression cond = null;
				if ( equalities.isEmpty() ) {
					cond = Expressions.FALSE;
				} 
				else {
					cond = Or.make(equalities);
				}
				Expression otherCard = rewriteInternalGeneral(ExtensionalSet.makeUniSetExpression(elements), process);
				Expression otherCardPlusOne = addTo(otherCard, Expressions.ONE, process);
				result = IfThenElse.make(cond, otherCard, otherCardPlusOne);
			}
		} 
		else {
			throw new IllegalArgumentException("Expression does not have the proper form");
		}
		return result;		
	}
	
	private Expression addTo(Expression e1, Expression e2, RewritingProcess process) {
		Expression result = null;
		if ( IfThenElse.isIfThenElse(e1) ) {
			Expression condition = IfThenElse.getCondition(e1);
			Expression thenBranch = IfThenElse.getThenBranch(e1);
			Expression elseBranch = IfThenElse.getElseBranch(e1);
			Expression newThen = addTo(thenBranch, e2, process);
			Expression newElse = addTo(elseBranch, e2, process);
			result = IfThenElse.make(condition, newThen, newElse);
		} 
		else {
			ArrayList<Expression> w1 = new ArrayList<Expression>();
			w1.add(e1);
			w1.add(e2);
			result = rBasic.rewrite(Plus.make(w1), process);
		}
		return result;
	}
}
