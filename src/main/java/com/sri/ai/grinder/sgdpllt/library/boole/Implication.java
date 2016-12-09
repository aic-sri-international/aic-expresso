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
package com.sri.ai.grinder.sgdpllt.library.boole;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.IMPLICATION;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;

/**
 * @author braz
 *
 */
@Beta
public class Implication implements Simplifier {

	/**
	 * Make a material implication:<br>
	 * p => q
	 * 
	 * @param antecedent
	 *            p => q, the p part.
	 * @param consequent
	 *            p => q, the q part.
	 * @return antecedent => consequent.
	 */
	public static Expression make(Expression antecedent, Expression consequent) {
		return Expressions.apply(IMPLICATION, antecedent, consequent);
	}

	public static boolean isImplication(Expression expressionF) {
		boolean result = expressionF.hasFunctor(IMPLICATION) && expressionF.numberOfArguments() == 2;
		return result;
	}

	@Override
	public Expression applySimplifier(Expression expression, Context context) {
		return simplify(expression);
	}
	
	public static Expression simplify(Expression expression) {
		if (expression.get(0).equals(expression.get(1))) {
			return Expressions.TRUE;
		}
		
		if (expression.get(0).equals(Expressions.TRUE)){
			return expression.get(1);
		}
		else if (expression.get(0).equals(Expressions.FALSE)){
			return Expressions.TRUE;
		}

		if (expression.get(1).equals(Expressions.TRUE)){
			return Expressions.TRUE;
		}
		else if (expression.get(1).equals(Expressions.FALSE)){
			return Not.make(expression.get(0));
		}

		return expression;
	}
}
