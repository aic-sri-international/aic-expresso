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
package com.sri.ai.grinder.library.number;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.ExpressionIsSymbolOfType;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasFunctor;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.util.Util;
import com.sri.ai.util.math.Rational;

/**
 * Implements a rewriter for the division operation.
 * 
 * @author braz
 *
 */
@Beta
public class Division extends AbstractRewriter {
	
	public Division() {
		this.setReifiedTests(new HasFunctor(FunctorConstants.DIVISION),
						     new HasNumberOfArguments(2));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (expression.get(0).equals(expression.get(1))) { // if numerator and denominator are equal, result is 1.
			return Expressions.ONE;
		}

		if (expression.get(0).equals(0)) { // if numerator is 0, fraction is 0.
			return Expressions.ZERO;
		}

		if (expression.get(1).equals(1)) { // if denominator is 1, fraction is numerator.
			return expression.get(0);
		}

		if (ExpressionIsSymbolOfType.apply(expression.get(0), Number.class) &&
				ExpressionIsSymbolOfType.apply(expression.get(1), Number.class)) {

			Rational numerator   = expression.get(0).rationalValue();
			Rational denominator = expression.get(1).rationalValue();

			Rational quotient = Util.divisionWithArbitraryPrecisionIfPossible(numerator, denominator);
			if (quotient != null) {
				return Expressions.createSymbol(quotient);
			}
		}

		return expression;
	}
		
	public static Expression make(Expression numerator, Expression denominator) {
		if (numerator.equals(denominator)) {
			return Expressions.ONE;
		}
		return Expressions.apply("/", numerator, denominator);
	}
}
