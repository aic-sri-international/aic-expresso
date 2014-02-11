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
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;

/**
 * Default implementation of incomplete_linear_implies(G, H).
 * 
 * @author oreilly
 *
 */
@Beta
public class IncompleteLinearImplies {
	
	/**
	 * <pre>
	 * incomplete_linear_implies(G, H)
	 * G and H formulas
	 * Runs in linear time on the size of G and H.
	 * Returns whether G is known to imply H according the methods used by this function (a return 
	 * value of False does not mean that G does not imply H, it merely means that this function does 
	 * not know that it does).
	 * 
	 * if G and H are the same expression
	 *         or
	 *    G is "t1 = t2" or "not(t1 != t2)" and
	 *    H is "t2 = t1" or "not(t2 != t1)"
	 *         or
	 *    G is "t1 = c1" or G is "c1 = t1" with c1 a constant,
	 *          and H is "not t1 = c2" or H is "not c2 = t1" or H is "t1 != c2" or H is "c2 != t1"
	 *          with c2 a constant distinct from c1
	 *         or
	 *    G is t1 != t2 or "not (t1 = t2)" and H is "t2 != t1" or "not (t2 = t1)"
	 *        return true
	 * return false
	 * </pre>
	 * 
	 * @param expressionG
	 *            a formula.
	 * @param expressionH
	 *            a formula.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return true if G is known to imply H according the methods used by this
	 *         function, false otherwise.
	 */
	public static boolean implies(Expression expressionG, Expression expressionH, RewritingProcess process) {
		boolean result = false;
		
		// if G and H are the same expression
		if (expressionG.equals(expressionH) ||
		    // or
		    // G is "t1 = t2" or "not (t1 != t2)" and 
			// H is "t2 = t1" or "not (t2 != t1)"
			isSameEqualsButDifferentForm(expressionG, expressionH) ||			
			// or
			// G is "t1 = c1" or G is "c1 = t1" with c1 a constant,
			// and H is "not t1 = c2" or H is "not c2 = t1" or H is "t1 != c2" or H is "c2 != t1"
			// with c2 a constant distinct from c1
			isEqualNotEqualOnDifferentConstants(expressionG, expressionH, process) ||
			// or
			// G is t1 != t2 or "not (t1 = t2)" and H is "t2 != t1" or "not (t2 = t1)"
			isSameNotEqualsButDifferentOrdering(expressionG, expressionH)
			) {
			result = true;
		}
		
		return result;
	}

	//
	// PRIVATE
	//
	private static boolean isSameEqualsButDifferentForm(Expression g, Expression h) {
		boolean result = false;
	    // G is "t1 = t2" or "not (t1 != t2)" and 
		// H is "t2 = t1" or "not (t2 != t1)"
		Set<Expression> gArgs;
		Set<Expression> hArgs;
		result =
				(gArgs = argumentsOfEquality(g)) != null &&
				(hArgs = argumentsOfEquality(h)) != null &&
				gArgs.equals(hArgs);
		return result;
	}
	
	/**
	 * Returns the arguments of an equality either of the form t1 = t2, or the form !(t1 != t2),
	 * or null if the given expression is not an equality.
	 */
	private static Set<Expression> argumentsOfEquality(Expression expression) {
		if (expression.hasFunctor(FunctorConstants.EQUAL)) {
			return new LinkedHashSet<Expression>(expression.getArguments());
		}
		if (expression.hasFunctor(FunctorConstants.NOT) && expression.get(0).hasFunctor(FunctorConstants.INEQUALITY)) {
			return new LinkedHashSet<Expression>(expression.get(0).getArguments());
		}
		return null;
	}
	
	private static boolean isEqualNotEqualOnDifferentConstants(Expression g, Expression h, RewritingProcess process) {
		boolean result = false;
		// G is "t1 = c1" or G is "c1 = t1" with c1 a constant,
		// and H is "not t1 = c2" or H is "not c2 = t1" or H is "t1 != c2" or H is "c2 != t1"
		// with c2 a constant distinct from c1
		if (g.hasFunctor(FunctorConstants.EQUAL)) {
			Set<Expression> gConstants = new LinkedHashSet<Expression>();
			Set<Expression> gVariables = new LinkedHashSet<Expression>();
			for (Expression gArg : g.getArguments()) {
				if (process.isConstant(gArg)) {
					gConstants.add(gArg);
				} 
				else {
					gVariables.add(gArg);
				}
			}
			
			if (gConstants.size() == 1) {
				List<Expression> hArgs      = new ArrayList<Expression>();
				Set<Expression>  hConstants = new LinkedHashSet<Expression>();
				Set<Expression>  hVariables = new LinkedHashSet<Expression>();
				if (h.hasFunctor(FunctorConstants.NOT) &&
					h.numberOfArguments() == 1 &&
					h.get(0).hasFunctor(FunctorConstants.EQUAL)) {
					hArgs.addAll(h.get(0).getArguments());
				} 
				else if (h.hasFunctor(FunctorConstants.INEQUALITY)) {
					hArgs.addAll(h.getArguments());
				}
				
				for (Expression hArg : hArgs) {
					if (process.isConstant(hArg)) {
						hConstants.add(hArg);
					} 
					else {
						hVariables.add(hArg);
					}
				}
				
				if (gConstants.size() == hConstants.size() && 
					!gConstants.equals(hConstants) && 
					gVariables.equals(hVariables) &&
					gVariables.size() > 0) { // this ensure a = a => not(b = b) is handled correctly
					result = true;
				}
			}
		}
		
		return result;
	}
	
	private static boolean isSameNotEqualsButDifferentOrdering(Expression g, Expression h) {
		boolean result = false;
		//  G is t1 != t2 or "not (t1 = t2)" and H is "t2 != t1" or "not (t2 = t1)"
		Set<Expression> gArgs = new LinkedHashSet<Expression>();
		if (g.hasFunctor(FunctorConstants.INEQUALITY)) {
			gArgs.addAll(g.getArguments());
		} 
		else if (g.hasFunctor(FunctorConstants.NOT) &&
				g.numberOfArguments() == 1 &&
				g.get(0).hasFunctor(FunctorConstants.EQUAL)) {
			gArgs.addAll(g.get(0).getArguments());
		}
		
		if (gArgs.size() > 0) {
			Set<Expression> hArgs = new LinkedHashSet<Expression>();
			if (h.hasFunctor(FunctorConstants.INEQUALITY)) {
				hArgs.addAll(h.getArguments());
			} 
			else if (h.hasFunctor(FunctorConstants.NOT) &&
					h.numberOfArguments() == 1 &&
					h.get(0).hasFunctor(FunctorConstants.EQUAL)) {
				hArgs.addAll(h.get(0).getArguments());
			}
		
			if (gArgs.equals(hArgs)) {
				result = true;
			}
		}

		return result;
	}
}
