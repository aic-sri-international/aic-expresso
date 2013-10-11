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

import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.util.base.IdentityWrapper;
import com.sri.ai.util.base.Pair;

/**
 * A rewriting process gathers all information that needs to be kept and
 * manipulated during a concerted application of several rewritings on an
 * expression. It is available to every rewriting during its operation as part
 * of the process. It keeps several pieces of data, such as the root expression
 * being manipulated, global objects, annotations on expressions, etc. It also
 * provides methods for pre- and post-processing bookkeeping.
 * <p>
 * The reason it keeps a root expression is for cache maintenance. Rewritten
 * expressions are kept in a cache. When the cache reaches a certain size, the
 * cache is purged of expressions not reachable from the root expression (they
 * are assumed to have been part of the process in the past, but are considered
 * irrelevant now).
 * <p>
 * 
 * @author braz
 */
@Beta
public interface RewritingProcess {
	
	/** Indicates whether a given expression is a constant. */
	boolean isConstant(Expression expression);

	/** Indicates whether a given expression is a constant. */
	boolean isVariable(Expression expression);
	// the two above are Expression-specific, should be moved to ExpressionRewritingProcess

	/**
	 * Returns the predicate indicating constants.
	 */
	Predicate<Expression> getIsConstantPredicate();
	
	/**
	 * Sets the predicate indicating constants.
	 */
	void setIsConstantPredicate(Predicate<Expression> isConstantPredicate);
	
	/**
	 * Returns the root expression being rewritten by this process.
	 */
	Expression getRootExpression();

	/**
	 * Sets the root expression being rewritten by this process.
	 */
	void setRootExpression(Expression newRoot);

	/** Returns root rewriter of process. */
	Rewriter getRootRewriter();
	
	Rewriter setRootRewriter(Rewriter newRootRewriter);
	
	/**
	 * Rewrites an expression within the context of this rewriting process.
	 * 
	 * @param rewriterName
	 *            the name of the rewriter to perform the actual rewriting
	 *            within the context of this process.
	 * @param expression
	 *            the expression to be rewritten.
	 * @return a rewritten version of the input expression or the original input
	 *         expression if no rewriting occurred.
	 */
	Expression rewrite(String rewriterName, Expression expression);
	
	/**
	 * Rewrites an expression within the context of this rewriting process.
	 * 
	 * @param rewriterName
	 *            the name of the rewriter to perform the actual rewriting
	 *            within the context of this process.
	 * @param expression
	 *            the expression to be rewritten.
	 * @param childCallIntercepter
	 *            a rewrite intercepter to be invoked when any child rewrite
	 *            calls that the rewriter identified by rewriterName makes via
	 *            this API.
	 * @return a rewritten version of the input expression or the original input
	 *         expression if no rewriting occurred.
	 */
	Expression rewrite(String rewriterName, Expression expression, ChildRewriterCallIntercepter childCallIntercepter);
	
	/**
	 * @return the set of logical variables that should be considered free in
	 *         this specific context.
	 */
	Set<Expression> getContextualVariables();
	
	/**
	 * @return the domains of all contextual variables.
	 */
	Map<Expression, Expression> getContextualVariablesDomains();
	
	/**
	 * @return the domains of a contextual variable.
	 */
	Expression getContextualVariableDomain(Expression variable);
	
	/**
	 * 
	 * @return the process's current contextual constraint. This is a constraint
	 *         on the values of the free variables for expressions being passed
	 *         to rewriters, which is a subset of the contextual variables
	 *         associated with the process (i.e. not all these variables need
	 *         necessarily be constrained).
	 */
	Expression getContextualConstraint();

	/**
	 * Create a new sub-rewriting process with it own context.
	 */
	RewritingProcess newSubProcessWithContext(
			Set<Expression> contextualVariables,
			Map<Expression, Expression> subProcessContextualVariablesDomains,
			Expression contextualConstraint);

	/**
	 * A method to be called by rewriters in advance of their own rewriting.
	 * Rewriters are required to directly return its returned value if it is not <code>null</code>.
	 * This gives the RewritingProcess to chance to do bookkeeping and sometimes intervene,
	 * such as when it is doing caching, for example.
	 */
	Expression rewritingPreProcessing(Rewriter rewriter, Expression expression);	

	/**
	 * A method to be called by rewriters after their own rewriting
	 * (they should not do it if they simply used the result of {@link #rewritingPreProcessing(Rewriter, Expression)}).
	 */
	void rewritingPostProcessing(Rewriter rewriter, Expression expression, Expression result);

	/**
	 * Method called when process is completely constructed and ready to start.
	 */
	void notifyReadinessOfRewritingProcess();
	
	/**
	 * Method called when process is completed.
	 */
	void notifyEndOfRewritingProcess();
	
	/**
	 * Returns the first rewriter among the ones used by this process which satisfies the given predicate.
	 */
	Rewriter findModule(Predicate<Rewriter> predicate);
	
	/**
	 * Returns the first rewriter among the ones used by this process which is of a given class.
	 */
	Rewriter findModule(Class<?> clazz);
	
	/**
	 * Gets map of global objects.
	 */
	Map<Object, Object> getGlobalObjects();
	
	/**
	 * Sets map of global objects to a new given one.
	 */
	void setGlobalObjects(Map<Object, Object> newMap);
	
	/**
	 * Puts a value in a map of global objects under key.
	 */
	Object putGlobalObject(Object key, Object value);
	
	/**
	 * Gets a value from a map of global objects under key.
	 */
	Object getGlobalObject(Object key);
	
	boolean isRecursive();
	
	/** The recursion level of a process. A top process has level 0. */
	int getRecursionLevel();
	
	/**
	 * A method returning any object meant as an equivalence class for the given expression
	 * for the purpose of dead-ends detection (see {@link RewriteOnce}).
	 * Specific types of processes can override this method for the type of expressions it uses.
	 */
	Pair<IdentityWrapper, Expression> getExpressionEquivalenceClassForDeadEnd(Expression expression);
	
	/**
	 * Interrupt the rewriting process, will cause a RuntimeException to be thrown during execution at key points in the logic. 
	 */
	void interrupt();
}
