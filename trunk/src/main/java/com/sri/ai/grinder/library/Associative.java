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

import java.util.LinkedHashSet;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.google.common.base.Predicates;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasFunctor;
import com.sri.ai.util.Util;

/**
 * A rewriter collapsing nested applications of a given iterator into a single
 * one. For example, it can rewrite <code>f(x, f(y,z))</code> into
 * <code>f(x,y,z)</code>.
 * 
 * @author braz
 */
@Beta
public class Associative extends AbstractRewriter {

	private Set<Expression> operators = new LinkedHashSet<Expression>();

	public Associative(Object... operators) {
		super();
		this.operators.addAll(Expressions.wrap(operators));
		// If a single operator and corresponding absorbing element
		// then I can add a reified test for the operator/functor
		if (this.operators.size() == 1) {
			this.setReifiedTests(new HasFunctor(operators[0]));
			this.setName(""+operators[0]+" "+Util.camelCaseToSpacedString(getClass().getSimpleName()));
		}
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression,
			final RewritingProcess process) {
		Expression functor = expression.getFunctor();
		if (operators.contains(functor)) {
			Predicate<Expression> alwaysTrue = Predicates.alwaysTrue();
			Expression result = Expressions
					.associateWhenSureOperatorIsAssociative(
							expression, alwaysTrue);
			return result;
		}
		return expression;
	}
}
