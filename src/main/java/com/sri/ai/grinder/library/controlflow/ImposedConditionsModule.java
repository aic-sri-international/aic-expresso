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
package com.sri.ai.grinder.library.controlflow;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.util.base.Pair;

/**
 * This module concentrates the functionality for registering and using
 * providers that know the conditions that an expression imposes on its
 * sub-expressions. For example, the expression:
 * 
 * <pre>
 * if A = B then 1 else 2
 * 
 * imposes the condition 'A = B' on the then branch sub-expression '1' and
 * 'not(A = B)' on the else branch sub-expression '2'.
 * </pre>
 * 
 * @author oreilly
 * 
 */
@Beta
public class ImposedConditionsModule extends AbstractRewriter implements NoOpRewriter {
	private Set<Provider> providers = new LinkedHashSet<Provider>();

	/**
	 * An interface for objects that know how to determine the conditions that
	 * an expression imposes on its sub-expressions.
	 * 
	 * @author oreilly
	 * 
	 */
	public static interface Provider {
		/**
		 * Get a list of the conditions an expression imposes on its
		 * sub-expressions, which can be a sub-set of the sub-expressions if not
		 * all of them have conditions imposed on them.
		 * 
		 * @param expression
		 *            the expression to be checked for whether or not it imposes
		 *            conditions on its sub-expression.
		 * @param process
		 *            the rewriting process in which the provider is being
		 *            called.
		 * @return null if there are no conditions imposed by expression on its
		 *         subexpressions. Otherwise, a list of pairs where the first
		 *         element is the condition imposed and the second element is
		 *         the path to the sub-expression within the expression. A path
		 *         is used so that the same expression can be a sub-expression
		 *         of the same parent expression twice, and it can even get
		 *         different conditions in different positions.
		 */
		List<Pair<Expression, List<Integer>>> getConditionsExpressionImposesOnSubExpressions(
				Expression expression, RewritingProcess process);
	}

	/**
	 * Default Constructor.
	 */
	public ImposedConditionsModule() {

	}

	/**
	 * Register a provider with this module.
	 * 
	 * @param provider
	 *            a provider to be registered.
	 */
	public void register(Provider provider) {
		providers.add(provider);
	}

	/**
	 * 
	 * @see Provider#getConditionsExpressionImposesOnSubExpressions
	 */
	public List<Pair<Expression, List<Integer>>> getConditionsExpressionImposesOnSubExpressions(
			Expression expression, RewritingProcess process) {
		List<Pair<Expression, List<Integer>>> result = null;

		for (Provider provider : providers) {
			result = provider.getConditionsExpressionImposesOnSubExpressions(
					expression, process);
			if (result != null) {
				break;
			}
		}

		return result;
	}

	/**
	 * Static utility routine.
	 * 
	 * @see Provider#getConditionsExpressionImposesOnSubExpressions
	 */
	public static List<Pair<Expression, List<Integer>>> get(
			Expression expression, RewritingProcess process) {

		ImposedConditionsModule module = (ImposedConditionsModule) process
				.findModule(ImposedConditionsModule.class);
		if (module == null) {
			throw new Error(
					"ImposedConditionsModule module not found");
		}

		List<Pair<Expression, List<Integer>>> result = module
				.getConditionsExpressionImposesOnSubExpressions(expression,
						process);

		return result;
	}

	//
	// START-Rewriter
	@Override
	public void rewritingProcessFinalized(RewritingProcess process) {
		providers.clear();
	}

	// END-Rewriter
	//

	@Override
	/*
	 * This will eventually be removed when we introduce mechanism to deal with
	 * modules.
	 */
	public Expression rewriteAfterBookkeeping(Expression expression,
			RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression;
	}
}
