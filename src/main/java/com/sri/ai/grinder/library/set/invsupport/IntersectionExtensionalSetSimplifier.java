/*
 * Copyright (c) 2017, SRI International
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
package com.sri.ai.grinder.library.set.invsupport;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.IntensionalSet;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.boole.ThereExists;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.set.Sets;
import com.sri.ai.grinder.library.set.extensional.ExtensionalSets;
import com.sri.ai.grinder.rewriter.api.Simplifier;

/**
 * <pre>
 * Between an extensional set and another Set (either intensional or extensional, 
 * since it resorts to "in" that works for both):
 * 
 * { e1, ..., e_m }  intersection  Set   
 * ---> 
 * (if e1 in Set then {e1} else {}) 
 * union 
 * ... 
 * union (if e_m in Set then {e_m} else {})
 * 
 * 
 * "In":
 * intensional: 
 *     e in {{ (on I) H : C }}  
 *     --->  
 *     evaluate(there exists I : C and H = e)
 * extensional: 
 *     e in { e1, ..., en } ---> 
 *     evaluate(e = e1) or ... or evaluate(e = en) 
 *     (evaluate each equality as you go along for potential short-circuiting 
 *      -- note this covers e in {} if n = 0). 
 * </pre>
 *  
 * @author oreilly
 *
 */
public class IntersectionExtensionalSetSimplifier implements Simplifier {
	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression, context);
	}

	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (expression.hasFunctor(FunctorConstants.INTERSECTION) && 
				expression.numberOfArguments() > 1) {

			Expression firstExtensionalSet = null;
			List<Expression> otherSets     = new ArrayList<>();
			List<Expression> notSets       = new ArrayList<>();
			boolean emptySetArg = false;
			for (Expression possibleSet : expression.getArguments()) {
				if (Sets.isEmptySet(possibleSet)) {
					emptySetArg = true;
					break; // short circuit, intersection on empty set results in empty set
				}
				if (firstExtensionalSet == null && Sets.isExtensionalSet(possibleSet)) {
					firstExtensionalSet = possibleSet;
				}
				else if (Sets.isSet(possibleSet)) {
					otherSets.add(possibleSet);
				}
				else {
					notSets.add(possibleSet);
				}
			}
			if (emptySetArg) {
				result = Sets.EMPTY_SET;
			} else if (firstExtensionalSet != null && otherSets.size() > 0) {
				List<Expression> unionArgs = new ArrayList<>();
				for (Expression e : ExtensionalSets.getElements(firstExtensionalSet)) {
					List<Expression> conjuncts = new ArrayList<>();
					for (Expression otherSet : otherSets) {
						Expression inCondition;
						if (Sets.isIntensionalSet(otherSet)) {
							inCondition = inIntensionalSet(e, (IntensionalSet) otherSet, context);
						}
						else {
							inCondition = inExtensionalSet(e, otherSet, context);
						}
						conjuncts.add(inCondition);
						if (inCondition.equals(false)) {
							break; // short circuit
						}
					}
					Expression condition = And.simplify(And.make(conjuncts));
					if (condition.equals(true)) {
						unionArgs.add(ExtensionalSets.makeUniSetExpression(Arrays.asList(e)));
					}
					else if (!condition.equals(false)) {
						// Not true or false (i.e. false implies empty set and can be dropped from the union up front).
						Expression thenBranch = ExtensionalSets.makeUniSetExpression(Arrays.asList(e));
						Expression elseBranch = Sets.EMPTY_SET;
						Expression ifThenElse = IfThenElse.make(condition, thenBranch, elseBranch, true);
						unionArgs.add(ifThenElse);
					}
				}
				if (unionArgs.size() == 0) {
					// Holds even in there are non-set (i.e. conditional) terms in the original intersection
					result = Sets.EMPTY_SET; 
				}
				else {
					Expression union = Sets.makeUnion(unionArgs.toArray(new Expression[unionArgs.size()]));
					if (notSets.size() == 0) {
						result = union;
					} 
					else{
						Expression[] intersectionArgs = new Expression[notSets.size()+1];
						intersectionArgs[0] = union;
						for (int i = 0; i < notSets.size(); i++) {
							intersectionArgs[i+1] = notSets.get(i);
						}
						result = Sets.makeIntersection(intersectionArgs);
					}
				}
			}
		}
		
		return result;
	}
	
	

	public static Expression inIntensionalSet(Expression element, IntensionalSet iSet, Context context) {
		// e in {{ (on I) H : C }}  
	    // --->  
	    // evaluate(there exists I : C and H = e)
		//
		Expression thereExistsBody = And.make(iSet.getCondition(), Equality.make(iSet.getHead(), element));
		Expression thereExists     = ThereExists.make(iSet.getIndexExpressions(), thereExistsBody);
		
		Expression result = context.getTheory().evaluate(thereExists, context);
		return result;
	}
	
	public static Expression inExtensionalSet(Expression element, Expression eSet, Context context) {
		// e in { e1, ..., en } 
		// ---> 
	    // evaluate(e = e1) or ... or evaluate(e = en) 
	    // (evaluate each equality as you go along for potential short-circuiting 
	    //  -- note this covers e in {} if n = 0).
		Expression result;
		List<Expression> eSetElements = ExtensionalSets.getElements(eSet);
		if (eSetElements.size() == 0) { // can't be in the empty set.
			result = Expressions.FALSE;
		}
		else {
			List<Expression> disjuncts    = new ArrayList<>();
			for (Expression eSetElement : eSetElements) {
				Expression elementEquality = Equality.make(element, eSetElement);
				Expression inCondition     = context.getTheory().evaluate(elementEquality, context);
				disjuncts.add(inCondition);
				if (inCondition.equals(true)) {
					break; // short-circuit
				}
			}
			result = Or.simplify(Or.make(disjuncts));
		}
		
		return result;
	}

}