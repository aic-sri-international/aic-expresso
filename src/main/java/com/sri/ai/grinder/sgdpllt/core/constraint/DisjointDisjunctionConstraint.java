/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.grinder.sgdpllt.core.constraint;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Constraint;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.boole.Or;

/**
 * A DisjointDisjunctionConstraint is a disjunction (a list of Constraints) in
 * which the disjuncts are guaranteed to be mutually exclusive.
 * 
 * @author oreilly
 */
@Beta
public class DisjointDisjunctionConstraint extends AbstractConstraint {
	private static final long serialVersionUID = 1L;
	
	private List<Constraint> disjointDisjuncts;

	public static Constraint make(Theory theory, Constraint... disjointDisjuncts) {
		Constraint result = make(theory, Arrays.asList(disjointDisjuncts));
		return result;
	}
	
	public static Constraint make(Theory theory, List<Constraint> disjointDisjuncts) {
		Constraint result;
		if (disjointDisjuncts.size() == 1) {
			result = disjointDisjuncts.get(0);
		}
		else {
			result = new DisjointDisjunctionConstraint(theory, disjointDisjuncts);
		}
		return result;
	}

	@Override
	public Constraint conjoinWithLiteral(Expression literal, Context context) {
		Constraint result;
		if (isContradiction()) {
			result = null; // indicates is a contradiction
		}
		else {
			// To conjoin a literal with a DisjointDisjunctionConstraint, we simply conjoin it with
			// every disjunct and form a new DisjointDisjunctionConstraint with the
			// non-contradictory resulting disjuncts:
			// (d1, ..., dn) and L = (d1 and L) or ... or (dn and L) (some of the "di
			// and L" will be contradictions and can be discarded).
			List<Constraint> newDisjointDisjuncts = new ArrayList<>(disjointDisjuncts.size());			
			for (Constraint disjunct : disjointDisjuncts) {
				Constraint disjunctConjoinedWithLiteral = disjunct.conjoinWithLiteral(literal, context);
				if (disjunctConjoinedWithLiteral != null && !disjunctConjoinedWithLiteral.isContradiction()) {
					newDisjointDisjuncts.add(disjunctConjoinedWithLiteral);
				}
			}
			
			result = make(getTheory(), newDisjointDisjuncts);
		}
		return result;
	}

	@Override
	public Expression binding(Expression variable) {
		Expression result = null;
		
		for (Constraint disjunct : disjointDisjuncts) {
			Expression disjunctBinding = disjunct.binding(variable);
			if (disjunctBinding == null) {
				result = null;
				break;
			}
			if (result == null) {
				// i.e. first binding
				result = disjunctBinding;
			}
			else if (!result.equals(disjunctBinding)) {
				result = null;
				break;
			}
		}
		
		return result;
	}

	@Override
	protected Expression computeInnerExpressionIfNotContradiction() {
		List<Expression> disjuncts = new ArrayList<>();
		for (Constraint disjunct : disjointDisjuncts) {
			disjuncts.add(disjunct);
		}
		Expression result = Or.make(disjuncts);
		return result;
	}

	private DisjointDisjunctionConstraint(Theory theory, List<Constraint> disjointDisjuncts) {
		super(theory);
		this.disjointDisjuncts = new ArrayList<>(disjointDisjuncts);
		if (this.disjointDisjuncts.size() == 0) {
			this.isContradiction = true;
		}
	}
}
