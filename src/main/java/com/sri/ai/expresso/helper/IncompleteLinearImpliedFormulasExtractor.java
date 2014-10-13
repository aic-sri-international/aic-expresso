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
package com.sri.ai.expresso.helper;

import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.Not;

/**
 * A method for extracting an (incomplete, but obtained in linear time) set of
 * formulas implied by a formula. This is written with the intention of getting
 * a cheap formula simplifier given a context. We will extract basic formulas
 * implied by the context and replace them by true in the simplified formula.
 * 
 * @author braz
 */
@Beta
public class IncompleteLinearImpliedFormulasExtractor {

	public static List<Expression> get(Expression formula) {
		List<Expression> result = new LinkedList<Expression>();
		get(formula, result);
		return result;
	}

	public static void get(Expression formula, List<Expression> result) {
		// if formula is a conjunction, implied facts are the union of each conjunct's implied facts.
		if (formula.hasFunctor(FunctorConstants.AND)){
			for (Expression conjunct : formula.getArguments()) {
				get(conjunct, result);
			}
		}
		// if formula is the negation of a disjunction,
		// we use DeMorgan's to obtain a conjunction
		else if (formula.hasFunctor(FunctorConstants.NOT)) {
			Expression argument = formula.get(0);
			if (argument.hasFunctor(FunctorConstants.OR)) {
				for (Expression disjunct : argument.getArguments()) {
					Expression conjunct = Not.make(disjunct);
					get(conjunct, result);
				}
			}
			// if it is a negation that is not a disjunction, the formula is the implied fact itself
			else {
				result.add(formula);
			}
		}
		// if it is not a conjunction or the negation of a disjunction, the formula is the implied fact itself
		else {
			result.add(formula);
		}
	}
}
