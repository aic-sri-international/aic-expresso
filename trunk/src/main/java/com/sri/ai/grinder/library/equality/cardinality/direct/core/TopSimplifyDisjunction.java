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
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.equality.cardinality.CardinalityUtil;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;

/**
 * Default implementation of R_top_simplify_disjunction(F1 or ... or Fn).
 * 
 * @author oreilly
 *
 */
@Beta
public class TopSimplifyDisjunction extends AbstractHierarchicalRewriter implements CardinalityRewriter {
	
	public TopSimplifyDisjunction() {
	}
	
	@Override
	public String getName() {
		return R_top_simplify_disjunction;
	}
	
	/**
	 * @see CardinalityRewriter#R_top_simplify_disjunction
	 */
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = null;
		
		// Assert input arguments
		if (Or.isDisjunction(expression)) {	
			Trace.log("T <- empty tuple");
			Trace.log("for all i");
			List<Expression> t = new ArrayList<Expression>();
			for (Expression fi : expression.getArguments()) {
				if (fi.equals(Expressions.TRUE) || CardinalityUtil.isEqualityOnSameTerms(fi)) {
					Trace.log("    if Fi is True or Alpha = Alpha");
					Trace.log("        return True");
					result = Expressions.TRUE;
					break;
				} 
				else if (!fi.equals(Expressions.FALSE) && !CardinalityUtil.isDisequalityOnSameTerms(fi)) {
					Trace.log("    if Fi not \"False\" and Fi is not Alpha != Alpha");
					Trace.log("        add Fi to T");
					t.add(fi);
				}
			}
			
			// Note: This is an optimization so that we can reduce
			// the number of Not(Ti) expressions we create in the 
			// inner loop.
			List<Expression> notT = new ArrayList<Expression>();
			for (Expression ti : t) {
				notT.add(CardinalityUtil.makeNot(ti));
			}
			
			if (result == null) {
				Trace.log("T' <- empty tuple");
				Trace.log("irrelevant <- empty set");
				List<Expression> tPrime     = new ArrayList<Expression>();
				Set<Expression>  irrelevant = new LinkedHashSet<Expression>();
				
				Trace.log("for all i in 1,..., |T|");
				Expression ti, tj, notTi, notTj;
				int        sizeT = t.size();
				boolean    iIsIrrelevant;
				for (int i = 0; i < sizeT; i++) {
					ti = t.get(i);
					if (irrelevant.contains(ti)) {
						Trace.log("    if Ti is in irrelevant");
						Trace.log("        continue to next i");
						continue;
					}
					Trace.log("    i_is_irrelevant <- false");
					iIsIrrelevant = false;
					Trace.log("    for all j in i + 1, ..., |T|");
					for (int j = i+1; result == null && j < sizeT; j++) {
						tj = t.get(j);
						if (IncompleteLinearImplies.implies(ti, tj, process)) {
							Trace.log("        if incomplete_linear_implies(Ti, Tj)");
							Trace.log("            i_is_irrelevant <- true");
							Trace.log("            continue to next i");
							iIsIrrelevant = true;
							break;
						}
						
						if (IncompleteLinearImplies.implies(tj, ti, process)) {
							Trace.log("        if incomplete_linear_implies(Tj, Ti)");
							Trace.log("            irrelevant <- irrelevant union { Tj }");
							irrelevant.add(tj);
						} 
						else {
							notTi = notT.get(i);
							notTj = notT.get(j);
							if (IncompleteLinearImplies.implies(notTi, tj, process) || IncompleteLinearImplies.implies(notTj, ti, process)) {
								Trace.log("        else if incomplete_linear_implies(not Ti, Tj)");
								Trace.log("                or incomplete_linear_implies(not Tj, Ti)");
								Trace.log("            return True // disjunction is tautology");
								result = Expressions.TRUE;
								break;
							}
						}
					}
					
					if (!iIsIrrelevant && result == null) {
						Trace.log("    if not i_is_irrelevant");
						Trace.log("        add Ti to T'");
						tPrime.add(ti);
					}
				}
				
				if (result == null) {
					Trace.log("return disjunction on elements of T'");
					result = Or.make(tPrime);
				}
			}
		} 
		else {
			// Not a disjunction, just return as is.
			result = expression;
		}
		
		// This is needed to avoid infinite loops in ExhaustiveRewriter usage. Ideally we should never have this condition to be true.
		if (result.equals(expression)) {
			return expression;
		}
		
		return result;
	}
}
