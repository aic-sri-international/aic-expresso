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
package com.sri.ai.grinder.api;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;

/**
 * A rewriter rewrites a expression into another, for example by simplifying it,
 * or returns same if it does not apply to it. A rewriting process keeps
 * contextual information and is passed along from rewriter to rewriter as they
 * may invoke each other.
 * 
 * Implementations must be able to be given a <i>sub-rewriter intercepter
 * object</i> (see {@link #setSubRewriterIntercepter(Intercepter)} and
 * {@link #getSubRewriterIntercepter()}). If given an intercepter,
 * implementations must delegate all invocations of the sub-rewriters' rewrite
 * method (or at least those meant to be possibly intercepted) to the
 * {@link Intercepter#invokeSubRewriter(Rewriter, Expression, RewritingProcess)}
 * method. If an intercepter is not provided, the implementation must invoke the
 * sub-rewriter directly as the default behavior.
 * 
 * @author braz
 */
@Beta
public interface Rewriter {
	/**
	 * Return value returned if the rewriter is called within a
	 * rewriting process whose contextual constraint evaluates to false.
	 */
	Expression FALSE_CONTEXTUAL_CONTRAINT_RETURN_VALUE = Expressions.makeSymbol("whatever");

	/**
	 * An unique identifying name for the rewriter (intended to correspond to
	 * pseudo-code descriptions of the rewriter).
	 * 
	 * @return the unique human readable name for identifying the rewriter.
	 */
	String getName();
	
	/**
	 * 
	 * @return the (optional) list of conjunctive tests that a rewriter applies
	 *         to a given expression to help in determining whether or not it
	 *         should attempt to actually rewrite a given expression.
	 */
	List<RewriterTest> getReifiedTests();

	/**
	 * Rewrites an expression within the context of a rewriting process.
	 * 
	 * @param expression
	 *            the expression to be rewritten.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @return the original input expression if no rewriting occurred, otherwise
	 *         a new rewritten expression.
	 */
	Expression rewrite(Expression expression, RewritingProcess process);
	
	/**
	 * Rewrites an expression within the context of a rewriting process.
	 * 
	 * @param expression
	 *            the expression to be rewritten.
	 * @param process
	 *            the process in which the rewriting is occurring.
	 * @param bypassTests
	 *            set to true if the RewriterTest ojbects returned by getTests()
	 *            should not be tested (assumes the caller has already done so),
	 *            false otherwise.
	 * @return the original input expression if no rewriting occurred, otherwise
	 *         a new rewritten expression.
	 */
	Expression rewrite(Expression expression, RewritingProcess process, boolean bypassTests);

	/**
	 * Same as {@link #rewrite(Expression, RewritingProcess)}, with a default
	 * rewriting process object.
	 */
	Expression rewrite(Expression expression);

	/**
	 * Same as {@link #rewrite(Expression, RewritingProcess)}, with a default
	 * rewriting process object using a given predicate for indicating
	 * constants.
	 */
	Expression rewrite(Expression expression, Predicate<Expression> isConstantPredicate);

	/**
	 * Same as {@link #rewrite(Expression, RewritingProcess)}, with a default
	 * rewriting process object using a given map of global objects.
	 */
	Expression rewrite(Expression expression, Map<Object, Object> globalObjects);

	/**
	 * Returns an iterator ranging over the children rewriters of this rewriter.
	 */
	Iterator<Rewriter> getChildrenIterator();

	/**
	 * A method to be invoked by rewriting process when it starts. This is
	 * useful for cases when rewriters need to communicate with other rewriters.
	 * Only at this point can they be sure all other rewriters are present (and
	 * find then with {@link RewritingProcess#findModule(Predicate)}.
	 */
	void rewritingProcessInitiated(RewritingProcess process);

	/**
	 * A method to be invoked by rewriting process when it ends.
	 * 
	 * @see #rewritingProcessInitiated(RewritingProcess).
	 */
	void rewritingProcessFinalized(RewritingProcess process);
}
