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
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;

/**
 * Default implementation of replace_conjunct_and_top_simplify(F, i, F1 and ... and Fn).
 * 
 * @author oreilly
 * 
 */
@Beta
public class ReplaceConjunctAndTopSimplify {
	
	/**
	 * <pre>
	 * replace_conjunct_and_top_simplify(F, i, F1 and ... and Fn)
	 * F and each Fi is any formula
	 * i is an integer in {1,..., n}
	 * This function is intended for simplifying conjunctions obtained from replacing a given conjunct by 
	 * another in a conjunction that has already been checked for obvious simplifications.
	 * Returns a possibly simplified expression equivalent to
	 * F1 and ... and Fi-1 and F and Fi+1 and ... and Fn, in linear time.
	 * 
	 * if F is False
	 *     return False
	 * if incomplete_linear_implies(Fj, F), for some j != i
	 *     return F1 and ... and Fi-1 and Fi+1 and ... and Fn
	 * if incomplete_linear_implies(F, not Fj) or incomplete_linear_implies(Fj, not F) for some j != i
	 *     return False
	 * C <- F1 and ... and Fi-1 and F and Fi+1 and ... and Fn
	 * C <- remove from C all conjuncts Fj, j != i, such that incomplete_linear_implies(F, Fj)
	 * return C
	 * </pre>
	 * 
	 * @param expressionF
	 *            any formula, which is to replace the current conjunct at the
	 *            specified location.
	 * @param i
	 *            the location in F1...Fn that F is to replace the existing
	 *            conjunct.
	 * @param conjunctionF1ToFn
	 *            the conjunction to have its i'th argument replaced with F.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a possibly simplified expression equivalent to F1 and ... and
	 *         Fi-1 and F and Fi+1 and ... and Fn, in linear time
	 */
	public static Expression replaceConjunctAndTopSimplify(Expression expressionF, int i, Expression conjunctionF1ToFn, RewritingProcess process) {
		Expression result = null;
		
		if (And.isConjunction(conjunctionF1ToFn)) {
			if (i < 0 || i >= (conjunctionF1ToFn.numberOfArguments())) {
				throw new IllegalArgumentException("Index i is out of range:"+i);
			}
			List<Expression> conjuncts = conjunctionF1ToFn.getArguments();
			if (expressionF.equals(Expressions.FALSE)) {
				Trace.log("if F is False");
				Trace.log("    return False");
				result = Expressions.FALSE;
			}
			// if incomplete_linear_implies(Fj, F), for some j != i
			// if incomplete_linear_implies(F, not Fj) or incomplete_linear_implies(Fj, not F) for some j != i
			else if ((result = meetsIncompletLinearImpliesCriteria(expressionF, i, conjuncts, process)) == null) {
				// Otherwise
				Trace.log("C <- F1 and ... and Fi-1 and F and Fi+1 and ... and Fn");
				List<Expression> conjunctsC = new ArrayList<Expression>();
				conjunctsC.addAll(conjuncts.subList(0, i));
				conjunctsC.add(expressionF);
				conjunctsC.addAll(conjuncts.subList(i+1, conjuncts.size()));
				
				Trace.log("C <- remove from C all conjuncts Fj, j != i, such that incomplete_linear_implies(F, Fj)");
				List<Expression> finalConjunctsC = new ArrayList<Expression>();
				Expression       fj;
				for (int j = 0; j < conjunctsC.size(); j++) {
					fj = conjunctsC.get(j);
					if (j != i) {
						if (!IncompleteLinearImplies.implies(expressionF, fj, process)) {
							finalConjunctsC.add(fj);
						}
					} 
					else {
						// i.e. fi as fj = fi
						finalConjunctsC.add(fj);
					}
				}
				Trace.log("return C");
				result = And.make(finalConjunctsC);
			}
		} 
		else {
			// Is an implied conjunct
			if (i != 0) {
				throw new IllegalArgumentException("Index i is out of range:"+i);
			}
			result = expressionF;
		}
		
		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	// Note: As an optimization, this performs both types of incomplete_linear_implies tests with
	// 1 pass thru the list of conjuncts.
	private static Expression meetsIncompletLinearImpliesCriteria(Expression f, int i, List<Expression> conjuncts, RewritingProcess process) {
		Expression result = null;
		
		Expression fj;
		Expression notF = CardinalityUtil.makeNot(f);
		int conjunctsSize = conjuncts.size();
		for (int j = 0; result == null && j < conjunctsSize; j++) {
			if (j != i) {
				fj = conjuncts.get(j);
				
				// if incomplete_linear_implies(Fj, F), for some j != i
				if (IncompleteLinearImplies.implies(fj, f, process)) {
					Trace.log("if incomplete_linear_implies(Fj, F), for some j != i");
					Trace.log("    return F1 and ... and Fi-1 and Fi+1 and ... and Fn");
					List<Expression> conjunctsFiRemoved = new ArrayList<Expression>();
					conjunctsFiRemoved.addAll(conjuncts.subList(0, i));
					conjunctsFiRemoved.addAll(conjuncts.subList(i+1, conjuncts.size()));
					result = And.make(conjunctsFiRemoved);
				} 
				else {
					Expression notFj = CardinalityUtil.makeNot(fj);
					// if incomplete_linear_implies(F, not Fj) or incomplete_linear_implies(Fj, not F) for some j != i
					if (IncompleteLinearImplies.implies(f, notFj, process) || IncompleteLinearImplies.implies(fj, notF, process)) {
						Trace.log("if incomplete_linear_implies(F, not Fj) or incomplete_linear_implies(Fj, not F) for some j != i");
						Trace.log("    return False");
						result = Expressions.FALSE;
					} 
				}
			}
		}
		
		return result;
	}
}
