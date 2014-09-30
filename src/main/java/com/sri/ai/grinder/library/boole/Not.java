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
package com.sri.ai.grinder.library.boole;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.library.FunctorConstants;

/**
 * An atomic rewriter of Boolean "not" expressions. Includes related helper methods.
 * 
 * @author braz
 *
 */
@Beta
public class Not extends AbstractRewriter {

	public static final Expression FUNCTOR = Expressions.makeSymbol(FunctorConstants.NOT);
	
	public Not() {
		this.setReifiedTests(new HasKind(FUNCTOR), 
				             new HasNumberOfArguments(1));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		if (expression.get(0).equals(Expressions.TRUE)) {
			return Expressions.FALSE;
		}
		else if (expression.get(0).equals(Expressions.FALSE)) {
			return Expressions.TRUE;
		}
		else if (expression.get(0).hasFunctor(FUNCTOR) && expression.get(0).numberOfArguments() == 1) {
			return expression.get(0).get(0);
		}
		
		return expression;
	}

	public static boolean isNegation(Expression expression) {
		return expression.hasFunctor(FunctorConstants.NOT);
	}

	/** Make a "not" application on given expression. */
	public static Expression make(Expression expression) {
		if (expression.equals(Expressions.TRUE)){
			return Expressions.FALSE;
		}
		if (expression.equals(Expressions.FALSE)){
			return Expressions.TRUE;
		}
		return Expressions.apply("not", expression);
	}

	public static Expression getFunctor() {
		return FUNCTOR;
	}
}
