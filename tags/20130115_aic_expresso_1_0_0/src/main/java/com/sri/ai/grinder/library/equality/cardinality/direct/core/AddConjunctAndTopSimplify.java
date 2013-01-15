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
 * Default implementation of add_conjunct_and_top_simplify(F, F1 and ... and Fn).
 * 
 * @author oreilly
 * 
 */
@Beta
public class AddConjunctAndTopSimplify {
	
	/**
	 * <pre>
	 * add_conjunct_and_top_simplify(F, F1 and ... and Fn)
	 * F and each Fi is any formula
	 * Returns a top-simplified equivalent of F1 and ... and Fn and F in linear time.
	 * 
	 * if F is False
	 *     return False
	 * if incomplete_linear_implies(Fi, F), for some i
	 *     return F1 and ... and Fn
	 * if incomplete_linear_implies(F, not Fi) or incomplete_linear_implies(Fi, not F) for some i
	 *     return False
	 * C <- remove from F1 and ... and Fn all conjuncts Fi such that incomplete_linear_implies(F, Fi)
	 * return C and F
	 * </pre>
	 * 
	 * @param expressionF
	 *            any formula.
	 * @param f1ToFn
	 *            a conjunction where each fi is any formula.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return a top-simplified equivalent of F1 and ... and Fn and F in linear
	 *         time.
	 */
	public static Expression addConjunctAndTopSimplify(Expression f, Expression f1ToFn, RewritingProcess process) {
		Expression result = null;
		
		List<Expression> f1ToFnConjuncts = new ArrayList<Expression>(); 
		if (And.isConjunction(f1ToFn)) {
			f1ToFnConjuncts.addAll(f1ToFn.getArguments());
		} 
		else {
			// Is an implied conjunct
			f1ToFnConjuncts.add(f1ToFn);
		}

		if (f.equals(Expressions.FALSE)) {
			Trace.log("if F is False");
			Trace.log("    return False");
			result = Expressions.FALSE;
		} 
		// if incomplete_linear_implies(Fi, F), for some i
		// if incomplete_linear_implies(F, not Fi) or incomplete_linear_implies(Fi, not F) for some i
		else if ((result = meetsIncompletLinearImpliesCriteria(f, f1ToFn, f1ToFnConjuncts, process)) == null) {
			// Otherwise
			Trace.log("remove from F1 and ... and Fn all conjuncts Fi such that incomplete_linear_implies(F, Fi)");
			List<Expression> finalConjunctsC = new ArrayList<Expression>();
			for (Expression fi : f1ToFnConjuncts) {
				if (!IncompleteLinearImplies.implies(f, fi, process)) {
					finalConjunctsC.add(fi);
				}
			}
			Trace.log("return C and F");
			finalConjunctsC.add(f);
			result = And.make(finalConjunctsC);
		}
		
		return result;
	}
	
	//
	// PRIVATE METHODS
	//
	// Note: As an optimization, this performs both types of incomplete_linear_implies tests with
	// 1 pass thru the list of conjuncts.
	private static Expression meetsIncompletLinearImpliesCriteria(Expression f, Expression f1ToFn, List<Expression> conjuncts, RewritingProcess process) {
		Expression result = null;
		
		Expression fi;
		Expression notF = CardinalityUtil.makeNot(f);
		int conjunctsSize = conjuncts.size();
		for (int i = 0; result == null && i < conjunctsSize; i++) {
			fi = conjuncts.get(i);
				
			// if incomplete_linear_implies(Fi, F), for some i
			if (IncompleteLinearImplies.implies(fi, f, process)) {
				Trace.log("if incomplete_linear_implies(Fi, F), for some i");
				Trace.log("    return F1 and ... and Fn");
				result = f1ToFn;
			} 
			else {
				Expression notFi = CardinalityUtil.makeNot(fi);
				// if incomplete_linear_implies(F, not Fi) or incomplete_linear_implies(Fi, not F) for some i
				if (IncompleteLinearImplies.implies(f, notFi, process) || IncompleteLinearImplies.implies(fi, notF, process)) {
					Trace.log("if incomplete_linear_implies(F, not Fi) or incomplete_linear_implies(Fi, not F) for some i");
					Trace.log("    return False");
					result = Expressions.FALSE;
				} 
			}
		}
		
		return result;
	}
}
