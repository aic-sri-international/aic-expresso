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
package com.sri.ai.grinder.library.equality.formula.helper;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasFunctor;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.FunctorConstants;

/**
 * Performs the following normalizations on formulas:
 * 
 * a != X      -> X != a
 * B != A      -> A != B
 * X != X      -> false
 * a != b      -> true
 */
@Beta
public class NormalizeDisequalityLiteral extends AbstractRewriter {
	
	public NormalizeDisequalityLiteral() {
		this.setReifiedTests(new HasFunctor(FunctorConstants.INEQUALITY));
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression,
			RewritingProcess process) {
		Expression result = expression;			

		// a != X -> X != a
		Expression normalized = Equality.normalize(expression, process);
		if (normalized != expression) {
			result = normalized;
		}
		else {
			// B != A -> A != B
			String e0 = expression.get(0).toString();
			String e1 = expression.get(1).toString();
		    if (e0.compareTo(e1) > 0 && process.isVariable(expression.get(0)) && process.isVariable(expression.get(1))) {
		    	result = Expressions.makeFunctionApplication(expression.getFunctor(), expression.get(1), expression.get(0));
		    }
			// X != X -> false
		    else if (expression.get(0).equals(expression.get(1))) {
				result = Expressions.FALSE;
			}
			else {
				// a != b -> true
				if (process.isConstant(expression.get(0)) && process.isConstant(expression.get(1))) {
					result = Expressions.TRUE;
				}
			}
		}

		return result;
	}
}
