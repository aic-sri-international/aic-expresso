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
package com.sri.ai.grinder.rewriterrefiner;

import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSymbol;
import com.sri.ai.grinder.api.ChildRewriterCallIntercepter;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.grinder.library.set.tuple.Tuple;
import com.sri.ai.util.functionalsequence.AbstractFunctionalRefiner;

/**
 * A {@link RewriterRefiner} is an {@link AbstractFunctionalRefiner} taking and
 * producing {@link Expression}s, with the corresponding function being
 * implemented by a {@link Rewriter} applied to a given expression under a given
 * {@link RewritingProcess}.
 * <p>
 * Rewriter results may be cached, so invoking a rewriter multiple times will
 * always produce the same results. In order for the refiner to actually refine
 * its results based on the same rewriter and argument expression,
 * {@link RewriterRefiner} makes a new argument expression equal to a tuple
 * (expression, timestep), where 'expression' is the actual original argument
 * and timestep is incremented at each invocation.
 * <p>
 * The arguments to this functional refiner are invocations of child rewriters
 * made by the rewriter. The rewriter refiner allows implementations to
 * stipulate which child rewriters are wrapper in a rewriter refiner of their
 * own, through the methods
 * {@link #callMustBeWrappedInRewriterRefiner(String, Expression)} and
 * {@link #makeChildRewriterRefiner(String, Expression, RewritingProcess)}. The
 * default implementation of the latter always returns null, so it must be
 * overridden if some of the argument child rewriters do need to be wrapper in
 * RewriterRefiners.
 * <p>
 * It may happen that certain rewritten values indicate that no further
 * refinement is possible. In this case, implementations can override
 * {@link #resultIndicatesThatRefinerIsDone(Expression)} (the default always
 * returns false).
 * 
 * @author braz
 * 
 */
@Beta
public abstract class RewriterRefiner extends AbstractFunctionalRefiner<Expression, Expression> {

	protected String rewriterName;
	protected Expression expression;
	protected RewritingProcess process;
	protected Map<ChildRewriterInvocation, RewriterRefiner> sharedRewriterRefiners;
	protected RewriterRefinerIntercepter intercepter = new RewriterRefinerIntercepter();
	protected boolean isRootRefiner = false;
	protected int timestep = 0;

	public RewriterRefiner(Expression initialValue) {
		super(initialValue);
	}

	protected Expression computeFunction() {		
		if (isRootRefiner) {
			// This is the root refiner, therefore increment the timestep.
			timestep++;
			// REMOVE DEFAULTSYMBOL IF POSSIBLE
			expression = Tuple.make(Tuple.get(expression, 0), DefaultSymbol.createSymbol(timestep));
		}
		Expression result = process.rewrite(rewriterName, expression, intercepter);
		// If a message value returned then the bounds rewriter is done.
		// other rewriters are done on initialization (as their values are cached).
		if (resultIndicatesThatRefinerIsDone(result)) {
			this.knownToBeOver = true;				
		}
		
		Trace.log("// RewriterRefiner.computeFunction({}, {})={}", rewriterName, expression, result);
		 
		return result;
	}

	protected boolean callMustBeWrappedInRewriterRefiner(String rewriterName, Expression expression) {
		return false;
	}

	protected RewriterRefiner makeChildRewriterRefiner(String childRewriterName, Expression childExpression, RewritingProcess childProcess) {
		return null;
	}

	protected RewriterRefiner getChildRewriterFunctionalRefiner(ChildRewriterInvocation childRewriterInvocation, String childRewriterName, Expression childExpression, RewritingProcess childProcess) {
		RewriterRefiner result = null;

		result = sharedRewriterRefiners.get(childRewriterInvocation);
		if (result == null) {
			result = makeChildRewriterRefiner(childRewriterName, childExpression, childProcess);
			sharedRewriterRefiners.put(childRewriterInvocation, result);
		}

		// TODO - need a thread safe version of these assignments (also, super class logic to be reviewed for thread safety)!
		//
		// Note: Need to always update the expression for the bounds rewriters as the timestep will
		// have been incremented.
		result.expression = childExpression;
		result.process    = childProcess;
		
		return result;
	}

	protected boolean resultIndicatesThatRefinerIsDone(Expression result) {
		return false;
	}

	protected class RewriterRefinerIntercepter implements ChildRewriterCallIntercepter {
		@Override
		public Expression intercept(String rewriterName, Expression expression, RewritingProcess process) {

			Expression result;
			
			Trace.log("+RewriterRefiner.intercept({}, {})", rewriterName, expression);
	
			if (callMustBeWrappedInRewriterRefiner(rewriterName, expression)) {
				// 'expression' is a tuple (realArgumentExpression, timestep)
				// We only want to use the realArgumentExpression to do the child rewriter
				// invocation lookup (otherwise a new refiner
				// would be instantiated on each lookup).
				Expression realArgumentExpression = Tuple.get(expression, 0);

				ChildRewriterInvocation childRewriterInvocation = new ChildRewriterInvocation(rewriterName, realArgumentExpression, process);

				// Now we use 'expression' again because we want the timestep to be included.
				// Not including the timestep would make rewriting caching to return the same result every time, instead of more refined values.
				RewriterRefiner childRewriterFunctionalRefiner = getChildRewriterFunctionalRefiner(childRewriterInvocation, rewriterName, expression, process);
				
				result = getCurrentArgumentValue(childRewriterFunctionalRefiner);
			}
			else {
				result = process.rewrite(rewriterName, expression, null);
			}
			
			Trace.log("-RewriterRefiner.intercept({}, {})={}", rewriterName, expression, result);
			
			return result;
		}
	}

	protected static class ChildRewriterInvocation {
		private String childRewriterName;
		private Expression arguments;
		private Expression contextualConstraint;
		private Set<Expression> contextualVariables;
		
		public ChildRewriterInvocation(String rewriterName, Expression arguments, RewritingProcess process) {
			super();
			this.childRewriterName    = rewriterName;
			this.arguments            = arguments;
			this.contextualConstraint = process.getContextualConstraint();
			this.contextualVariables  = process.getContextualVariables();
		}
	
		@Override
		public boolean equals(Object another) {
			ChildRewriterInvocation anotherChildRewriterInvocation = (ChildRewriterInvocation) another;
			boolean result =
					childRewriterName.equals(anotherChildRewriterInvocation.childRewriterName) && // this is why we cannot just use a list and equals()
					arguments.equals(anotherChildRewriterInvocation.arguments) &&
					contextualConstraint.equals(anotherChildRewriterInvocation.contextualConstraint) &&
					contextualVariables.equals(anotherChildRewriterInvocation.contextualVariables);
			return result;
		}
	
		@Override
		public int hashCode() {
			int result =
					childRewriterName.hashCode() +
					arguments.hashCode() +
					contextualConstraint.hashCode() +
					contextualVariables.hashCode(); // is there a better way for combining hash codes?
			
			return result;
		}
	}
}
