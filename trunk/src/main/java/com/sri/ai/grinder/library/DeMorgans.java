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
package com.sri.ai.grinder.library;

import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewriterTest;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.DefaultRewriterTest;
import com.sri.ai.grinder.core.HasFormula;
import com.sri.ai.grinder.core.HasFunctor;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.util.Util;

/**
 * @author braz
 */
@Beta
public class DeMorgans extends AbstractRewriter {

	public DeMorgans() {
		// Set up the relevant reified tests
		List<DefaultRewriterTest> reifiedTests =
				Util.list(
						new HasFunctor(FunctorConstants.NOT),
						new HasNumberOfArguments(1),
						new HasFormula());
		this.setReifiedTests(reifiedTests.toArray(new RewriterTest[reifiedTests.size()]));
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression argument = expression.get(0);
		if (argument.hasFunctor(FunctorConstants.AND) || argument.hasFunctor(FunctorConstants.OR)) {
			String newFunctor = theOther(argument.getFunctor());
			List<Expression> newArguments = Util.mapIntoArrayList(argument.getArguments(), negator);
			expression = Expressions.apply(newFunctor, newArguments);
		}
		return expression;
	}

	private Function<Expression, Expression> negator = new Function<Expression, Expression>() {
		@Override
		public Expression apply(Expression input) {
			return Not.make(input);
		}
	};
	
	private String theOther(Expression functor) {
		if (functor.equals(FunctorConstants.AND)){
			return FunctorConstants.OR;
		}
		else {
			return FunctorConstants.AND;
		}
	}
}
