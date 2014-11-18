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

import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.base.Pair;

/**
 * Defines a DPLL-type problem by using a semi-ring and a conversion from expression to value to be summed.
 * For example, satisfiability uses the boolean semi-ring and does not convert expressions ("sums" the expressions themselves, that is, takes their disjunction).
 * Model counting uses the number semi-ring and converts boolean formula F to 'if F then 1 else 0' to count satisfying models.
 * 
 * @author braz
 *
 */
public interface ProblemType {

	/**
	 * Performs the semi-ring's additive operation on two values.
	 */
	Expression add(Expression value1, Expression value2, RewritingProcess process);

	/**
	 * The result of adding a value (constant in the sense of having no background theory literals,
	 * but possibly symbolic) to itself n times.
	 */
	Expression addNTimes(Expression constantValue, Expression n, RewritingProcess process);

	/**
	 * Indicates whether given value is a maximum value in the semi-ring, that is,
	 * using the additive operation on it with any other value will produce itself.
	 */
	boolean isMaximum(Expression value);

	/** Converts expression value without literals to the value to be summed (useful for model counting of boolean formulas, for example: for boolean formula F, we want to sum 'if F then 1 else 0') */
	Expression fromExpressionValueWithoutLiteralsToValueToBeSummed(Expression expression);

	/**
	 * Gets an expression passed to a rewriter solving this type of problem, and returns a pair containing the expression
	 * and indices for DPLL to solve.
	 * The index types are assumed to be stored in the rewriting process.
	 * @param expression
	 * @param process
	 * @return
	 */
	Pair<Expression, List<Expression>> getExpressionAndIndexExpressionsFromRewriterProblemArgument(Expression expression, RewritingProcess process);
}
