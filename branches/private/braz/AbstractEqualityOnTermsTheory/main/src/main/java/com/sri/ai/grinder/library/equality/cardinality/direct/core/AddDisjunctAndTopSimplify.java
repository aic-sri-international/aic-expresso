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
 * Default implementation of add_disjunct_and_top_simplify(F, F1 or ... or Fn).
 * 
 * @author oreilly
 * 
 */
@Beta
public class AddDisjunctAndTopSimplify {
	
	/**
	 * <pre>
	 * add_disjunct_and_top_simplify(F, F1 or ... or Fn)
	 * F and each Fi is any formula
	 * Returns a top-simplified equivalent of F1 or ... or Fn and F in linear time.
	 * 
	 * if F is True
	 *     return True
	 * if incomplete_linear_implies(F, Fi), for some i
	 *     return F1 or ... or Fn
	 * if incomplete_linear_implies(not F, Fi) or incomplete_linear_implies(not Fi, F) for some i
	 *     return True
	 * D <- remove from F1 or ... or Fn all disjuncts Fi such that incomplete_linear_implies(Fi, F)
	 * return D or F
	 * </pre>
	 * 
	 * @param expressionF
	 *            any formula.
	 * @param f1ToFn
	 *            a disjunction where each fi is any formula.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a top-simplified equivalent of F1 or ... or Fn or F in linear
	 *         time.
	 */
	public static Expression addDisjunctAndTopSimplify(Expression f, Expression f1ToFn, RewritingProcess process) {
		Expression result = null;
		
		List<Expression> f1ToFnDisjuncts = new ArrayList<Expression>();
		if (Or.isDisjunction(f1ToFn)) {
			f1ToFnDisjuncts.addAll(f1ToFn.getArguments());
		} 
		else {
			// Is an implied disjunct
			f1ToFnDisjuncts.add(f1ToFn);
		}
		
		if (f.equals(Expressions.TRUE)) {
			Trace.log("if F is True");
			Trace.log("    return True");
			result = Expressions.TRUE;
		} 
		// if incomplete_linear_implies(Fi, F), for some i
		// if incomplete_linear_implies(not F, Fi) or incomplete_linear_implies(not Fi, F) for some i
		else if ((result = meetsIncompletLinearImpliesCriteria(f, f1ToFn, f1ToFnDisjuncts, process)) == null) {
			// Otherwise
			Trace.log("D <- remove from F1 or ... or Fn all disjuncts Fi such that incomplete_linear_implies(Fi, F)");
			List<Expression> finalDisjunctsD = new ArrayList<Expression>();
			for (Expression fi : f1ToFnDisjuncts) {
				if (!IncompleteLinearImplies.implies(fi, f, process)) {
					finalDisjunctsD.add(fi);
				}
			}
			Trace.log("return D or F");
			finalDisjunctsD.add(f);
			result = Or.make(finalDisjunctsD);
		}
		
		return result;
	}
	
	//
	// PRIVATE
	//
	// Note: As an optimization, this performs both types of incomplete_linear_implies tests with
	// 1 pass thru the list of disjuncts.
	private static Expression meetsIncompletLinearImpliesCriteria(Expression f, Expression f1ToFn, List<Expression> disjuncts, RewritingProcess process) {
		Expression result = null;
		
		Expression fi;
		Expression notF = CardinalityUtil.makeNot(f);
		int disjunctsSize = disjuncts.size();
		for (int i = 0; result == null && i < disjunctsSize; i++) {
			fi = disjuncts.get(i);
				
			// if incomplete_linear_implies(F, Fi), for some i
			if (IncompleteLinearImplies.implies(f, fi, process)) {
				Trace.log("if incomplete_linear_implies(F, Fi), for some i");
				Trace.log("    return F1 or ... or Fn");
				result = f1ToFn;
			} 
			else {
				Expression notFi = CardinalityUtil.makeNot(fi);
				//  if incomplete_linear_implies(not F, Fi) or incomplete_linear_implies(not Fi, F) for some i
				if (IncompleteLinearImplies.implies(notF, fi, process) || IncompleteLinearImplies.implies(notFi, f, process)) {
					Trace.log("if incomplete_linear_implies(not F, Fi) or incomplete_linear_implies(not Fi, F) for some i");
					Trace.log("    return True");
					result = Expressions.TRUE;
				} 
			}
		}
		
		return result;
	}
}
