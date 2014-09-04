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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Equals;
import com.sri.ai.util.base.Not;

@Beta
/** 
 * A class for plain, 
 * non-rewriter simplification of equality boolean formulas.
 */
public class SimplifyFormula {

	public static Expression simplify(Expression formula, RewritingProcess process) {
		formula = topSimplify(formula, process);
		if (formula.getSyntacticFormType().equals("Function application")) {
			ArrayList<Expression> simplifiedArguments =
					Util.mapIntoArrayList(formula.getArguments(), new SimplifyAsFunction(process));
			formula = Expressions.apply(formula.getFunctor(), simplifiedArguments);
			formula = topSimplify(formula, process);
		}
		return formula;
	}
	
	public static Expression topSimplify(Expression formula, RewritingProcess process) {
		
		Expression result = formula;
		
		if (formula.hasFunctor(FunctorConstants.EQUALITY)) {
			if (Util.allEqual(formula.getArguments())) {
				result = Expressions.TRUE;
			}
			else {
				Set<Expression> constants = new LinkedHashSet<Expression>();
				Util.collect(formula.getArguments(), constants, process.getIsConstantPredicate());
				if (constants.size() > 1) {
					result = Expressions.FALSE;
				}
				else {
					result = formula;
				}
			}
		}
		else if (formula.hasFunctor(FunctorConstants.DISEQUALITY)) {
			if (Util.allEqual(formula.getArguments())) {
				result = Expressions.FALSE;
			}
			else {
				Set<Expression> constants = new LinkedHashSet<Expression>();
				Util.collect(formula.getArguments(), constants, process.getIsConstantPredicate());
				if (constants.size() > 1) {
					result = Expressions.TRUE;
				}
				else {
					result = formula;
				}
			}
		}
		else if (formula.hasFunctor(FunctorConstants.AND)) {
			if (formula.getArguments().contains(Expressions.FALSE)) {
				result = Expressions.FALSE;
			}
			else {
				Not<Expression> notEqualToTrue = Not.make(Equals.make(Expressions.TRUE));
				Set<Expression> distinctArgumentsNotEqualToTrue = new LinkedHashSet<Expression>();
				Util.collect(formula.getArguments(), distinctArgumentsNotEqualToTrue, notEqualToTrue);
				if (distinctArgumentsNotEqualToTrue.size() != formula.getArguments().size()) {
					if (distinctArgumentsNotEqualToTrue.size() == 0) {
						result = Expressions.TRUE;
					}
					else if (distinctArgumentsNotEqualToTrue.size() == 1) {
						result = Util.getFirst(distinctArgumentsNotEqualToTrue);
					}
					else {
						result = Expressions.apply(FunctorConstants.AND, distinctArgumentsNotEqualToTrue);
					}
				}
				else {
					result = formula;
				}
			}
		}
		else if (formula.hasFunctor(FunctorConstants.OR)) {
			if (formula.getArguments().contains(Expressions.TRUE)) {
				result = Expressions.TRUE;
			}
			else {
				Not<Expression> notEqualToFalse = Not.make(Equals.make(Expressions.FALSE));
				Set<Expression> distinctArgumentsNotEqualToTrue = new LinkedHashSet<Expression>();
				Util.collect(formula.getArguments(), distinctArgumentsNotEqualToTrue, notEqualToFalse);
				if (distinctArgumentsNotEqualToTrue.size() != formula.getArguments().size()) {
					if (distinctArgumentsNotEqualToTrue.size() == 0) {
						result = Expressions.FALSE;
					}
					else if (distinctArgumentsNotEqualToTrue.size() == 1) {
						result = Util.getFirst(distinctArgumentsNotEqualToTrue);
					}
					else {
						result = Expressions.apply(FunctorConstants.OR, distinctArgumentsNotEqualToTrue);
					}
				}
				else {
					result = formula;
				}
			}
		}
		else if (formula.hasFunctor(FunctorConstants.NOT)) {
			if (formula.get(0).equals(Expressions.TRUE)) {
				result = Expressions.FALSE;
			}
			else if (formula.get(0).equals(Expressions.FALSE)) {
				result = Expressions.TRUE;
			}
			else if (formula.get(0).hasFunctor(FunctorConstants.NOT)) {
				result = formula.get(0).get(0);
			}
			else {
				result = formula;
			}
		}
		else if (formula.hasFunctor(FunctorConstants.IF_THEN_ELSE)) {
			result = IfThenElse.simplify(formula);
		}
		
		return result;
	}

	private static class SimplifyAsFunction implements Function<Expression, Expression> {
	
		RewritingProcess process;
		
		public SimplifyAsFunction(RewritingProcess process) {
			this.process = process;
		}
		
		@Override
		public Expression apply(Expression input) {
			Expression result = simplify(input, process);
			return result;
		}
	}

	/**
	 * Applies an equality between two terms to a formula by replacing the first one by the second everywhere and simplifying the result.
	 */
	protected static Expression applyEqualityTo(Expression formula, Expression equalityOfTwoTerms, RewritingProcess process) {
		Expression term1 = equalityOfTwoTerms.get(0);
		Expression term2 = equalityOfTwoTerms.get(1);
		Expression result = formula.replaceAllOccurrences(term1, term2, process);
		result = simplify(result, process);
		return result;
	}

	protected static Expression applyDisequalityTo(Expression formula, Expression disequality, RewritingProcess process) {
		Expression term1 = disequality.get(0);
		Expression term2 = disequality.get(1);
		Expression result = formula.replaceAllOccurrences(new SimplifyAtomGivenDisequality(term1, term2), process);
		result = simplify(result, process);
		return result;
	}
}
