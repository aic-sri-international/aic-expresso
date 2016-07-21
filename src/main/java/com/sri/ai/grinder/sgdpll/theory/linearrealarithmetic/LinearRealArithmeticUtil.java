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
package com.sri.ai.grinder.sgdpll.theory.linearrealarithmetic;

import static com.sri.ai.expresso.helper.Expressions.ZERO;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.condition;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.elseBranch;
import static com.sri.ai.grinder.library.controlflow.IfThenElse.isIfThenElse;
import static com.sri.ai.util.Util.getFirst;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.helper.IsolateUtil;

/**
 * A collection of methods for manipulating linear real arithmetic literals.
 *
 * @author braz
 *
 */
@Beta
public class LinearRealArithmeticUtil {

	private static final Symbol X = makeSymbol("x");
	private static final Expression ZERO_DISTINCT_FROM_ZERO = apply(DISEQUALITY, ZERO, ZERO);

	/**
	 * Simplify a linear real arithmetic literal.
	 * @param expression
	 * @return
	 */
	public static Expression simplify(Expression expression) {
		// we isolate variable x in the expression, even if there is no variable x in it.
		// If it does not exist, then we will get the equivalent "if 0 != 0 then <irrelevant> else 0 <operator> <polynomial without x>"
		return simplify(expression, X);
	}

	/**
	 * Simplify a linear real arithmetic literal using {@link #simplify(Expression, Symbol)}
	 * with respect to the first variable found in the expression 
	 * (variable is determined in accordance to {@link LinearRealArithmeticTheory#isVariable(Expression, Context)};
	 * the same expression is returned if there are no liner real arithmetic variable.
	 * @param expression
	 * @param theory
	 * @param context
	 * @return
	 */
	public static Expression simplify(Expression expression, LinearRealArithmeticTheory theory, Context context) {
		Expression result;
		Collection<Expression> variables = theory.getVariablesIn(expression, context);
		if (variables.isEmpty()) { // no linear real arithmetic variable, so it is not a linear real arithmetic literal
			result = expression;
		}
		else {
			Expression variable = getFirst(variables);
			result = simplify(expression, variable);
		}
		return result;
	}

	/**
	 * Simplify a literal with respect to a variable by isolating it.
	 * If the variable is not present, all terms are move to the right-hand side.
	 * @param expression
	 * @param variable
	 * @return
	 * @throws Error
	 */
	public static Expression simplify(Expression expression, Expression variable) throws Error {
		Expression result = isolateVariable(variable, expression);
		
		// not strictly required because caller is the one who knows about context and simplification, 
		// but why return an ugly thing if we can avoid it?
		if (isIfThenElse(result) && condition(result).equals(ZERO_DISTINCT_FROM_ZERO)) {
			result = elseBranch(result);
		}
		
		if (result != expression && result.equals(expression)) { // comply to requirement that if there are no changes, then instance is the same
			result = expression;
		}
		
		return result;
	}

	/**
	 * Returns an expression equivalent to linear arithmetic expression <code>numericalComparison</code> in which terms are cancelled out,
	 * numerical constants are summed together, and the given variable occurs alone in one of the sides.
	 * @param variable
	 * @param numericalComparison
	 * @return
	 * @throws Error
	 */
	public static Expression isolateVariable(Expression variable, Expression numericalComparison) throws Error {
		return IsolateUtil.isolate(numericalComparison, variable);
	}
}