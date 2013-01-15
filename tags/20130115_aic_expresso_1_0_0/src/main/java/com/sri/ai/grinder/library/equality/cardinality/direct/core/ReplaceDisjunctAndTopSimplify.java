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
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;

/**
 * Default implementation of replace_disjunct_and_top_simplify(F, i, F1 or ... or Fn).
 * 
 * @author oreilly
 * 
 */
@Beta
public class ReplaceDisjunctAndTopSimplify {
	
	/**
	 * <pre>
	 * replace_disjunct_and_top_simplify(F, i, F1 or ... or Fn)
	 * F and each Fi is any formula
	 * i is an integer in {1,..., n}
	 * This function is intended for simplifying disjunctions obtained from replacing a given disjunct 
	 * by another in a disjunction that has already been checked for obvious simplifications.
	 * Returns a possibly simplified expression equivalent to
	 * F1 or ... or Fi-1 or F or Fi+1 or ... or Fn, in linear time.
	 * 
	 * if F is True
	 *     return True
	 * if incomplete_linear_implies(F, Fj), for some j != i
	 *     return F1 or ... or Fi-1 or Fi+1 or ... or Fn
	 * if incomplete_linear_implies(not F, Fj) or incomplete_linear_implies(not Fj, F) for some j != i
	 *     return True
	 * D <- F1 or ... or Fi-1 or F or Fi+1 or ... or Fn
	 * D <- remove from D all disjuncts Fj, j != i, such that incomplete_linear_implies(Fj, F)
	 * return D
	 * </pre>
	 * 
	 * @param expressionF
	 *            any formula, which is to replace the current disjunct at the
	 *            specified location.
	 * @param i
	 *            the location in F1...Fn that F is to replace the existing
	 *            disjunct.
	 * @param disjunctionF1ToFn
	 *            the disjunction to have its i'th argument replaced with F.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a possibly simplified expression equivalent to F1 or ... or Fi-1
	 *         or F or Fi+1 or ... or Fn, in linear time
	 */
	public static Expression replaceDisjunctAndTopSimplify(Expression expressionF, int i, Expression disjunctionF1ToFn, RewritingProcess process) {
		Expression result = null;
		
		if (Or.isDisjunction(disjunctionF1ToFn)) {
			if (i < 0 || i >= (disjunctionF1ToFn.numberOfArguments())) {
				throw new IllegalArgumentException("Index i is out of range:"+i);
			}
			List<Expression> disjuncts = disjunctionF1ToFn.getArguments();
			if (expressionF.equals(Expressions.TRUE)) {
				Trace.log("if F is True");
				Trace.log("    return True");
				result = Expressions.TRUE;
			} 
			// if incomplete_linear_implies(F, Fj), for some j != i
			// if incomplete_linear_implies(not F, Fj) or incomplete_linear_implies(not Fj, F) for some j != i
			else if ((result = meetsIncompletLinearImpliesCriteria(expressionF, i, disjuncts, process)) == null) {
				// Otherwise
				Trace.log("D <- F1 or ... or Fi-1 or F or Fi+1 or ... or Fn");
				List<Expression> disjunctsD = new ArrayList<Expression>();
				disjunctsD.addAll(disjuncts.subList(0, i));
				disjunctsD.add(expressionF);
				disjunctsD.addAll(disjuncts.subList(i+1, disjuncts.size()));
				
				Trace.log("D <- remove from D all disjuncts Fj, j != i, such that incomplete_linear_implies(Fj, F)");
				List<Expression> finalDisjunctsD = new ArrayList<Expression>();
				Expression       fj;
				for (int j = 0; j < disjunctsD.size(); j++) {
					fj = disjunctsD.get(j);
					if (j != i) {
						if (!IncompleteLinearImplies.implies(fj, expressionF, process)) {
							finalDisjunctsD.add(fj);
						}
					} 
					else {
						// i.e. fi as fj = fi
						finalDisjunctsD.add(fj);
					}
				}
				Trace.log("return D");
				result = Or.make(finalDisjunctsD);
			}
		}
		else {
			throw new IllegalArgumentException("Is not a disjunction:"+disjunctionF1ToFn);
		}
		
		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	// Note: As an optimization, this performs both types of incomplete_linear_implies tests with
	// 1 pass thru the list of disjuncts.
	private static Expression meetsIncompletLinearImpliesCriteria(Expression f, int i, List<Expression> disjuncts, RewritingProcess process) {
		Expression result = null;
		
		Expression fj;
		Expression notF = CardinalityUtil.makeNot(f);
		int disjunctsSize = disjuncts.size();
		for (int j = 0; result == null && j < disjunctsSize; j++) {
			if (j != i) {
				fj = disjuncts.get(j);
				
				// if incomplete_linear_implies(F, Fj), for some j != i
				if (IncompleteLinearImplies.implies(f, fj, process)) {
					Trace.log("if incomplete_linear_implies(F, Fj), for some j != i");
					Trace.log("    return F1 or ... or Fi-1 or Fi+1 or ... or Fn");
					List<Expression> disjunctsFiRemoved = new ArrayList<Expression>();
					disjunctsFiRemoved.addAll(disjuncts.subList(0, i));
					disjunctsFiRemoved.addAll(disjuncts.subList(i+1, disjuncts.size()));
					result = Or.make(disjunctsFiRemoved);
				} 
				else {
					Expression notFj = CardinalityUtil.makeNot(fj);
					// if incomplete_linear_implies(not F, Fj) or incomplete_linear_implies(not Fj, F) for some j != i
					if (IncompleteLinearImplies.implies(notF, fj, process) || IncompleteLinearImplies.implies(notFj, f, process)) {
						Trace.log("if incomplete_linear_implies(not F, Fj) or incomplete_linear_implies(not Fj, F) for some j != i");
						Trace.log("    return True");
						result = Expressions.TRUE;
					} 
				}
			}
		}
		
		return result;
	}
}
