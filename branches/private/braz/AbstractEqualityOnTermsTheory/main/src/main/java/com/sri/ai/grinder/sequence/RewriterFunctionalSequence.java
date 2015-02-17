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
package com.sri.ai.grinder.sequence;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.ChildRewriterCallIntercepter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.util.functionalsequence.AbstractFunctionalSequence;

/**
 * An extension of {@link AbstractFunctionalSequence} for using rewriters as the
 * underlying function implementation.
 * 
 * The functional sequence arguments are {@link RewriterFunctionalSequence}s of
 * child-rewriters invoked by the given rewriter. These objects are
 * automatically constructed and kept in a map (that should be shared with
 * sub-rewriter functional sequences when they are instantiated), so that
 * invocations of the same child-rewriter with the same arguments under the same
 * context will share the {@link RewriterFunctionalSequence}.
 * 
 * IMPORTANT: because rewriters are repeatedly invoked on the same parameters
 * under the same process, they must not have cached results, but actually be
 * run.
 * 
 * The class is still abstract because it does not implement
 * {@link #initialValue()}. The class keeps the default implementations of
 * {@link #isFinalValue(Expression)}, {@link #nextArgumentToUpdate()} and
 * {@link #computeFunctionIncrementally(Expression, java.util.Iterator, Expression, Expression)}
 * , which always return false, pick the first argument sequence with available
 * values, and computes the function normally, respectively.
 * 
 * Because each invocation of a rewriter or sub-rewriter may involve a new
 * rewriting process instance, this class provides a getter and setter for the
 * rewriting process. This is used right before a new value in the rewriter's
 * functional sequence is requested, so it may be that the rewriter in a
 * RewriterFunctionalSequence is run under a different process than the one it
 * was constructed with. However, in these cases the context should still be the
 * same, since it is used to locate the RewriterFunctionalSequence to being
 * with.
 * 
 * @author braz
 */
@Beta
public abstract class RewriterFunctionalSequence extends AbstractFunctionalSequence<Expression, Expression> {
	protected String rewriterName;
	protected Expression arguments;
	protected RewritingProcess process;
	protected Map<ChildRewriterInvocation, RewriterFunctionalSequence> sharedRewriterFunctionalSequences;
	protected RewriterFunctionalSequenceIntercepter intercepter = new RewriterFunctionalSequenceIntercepter();

	
	// BEGINNING OF MAIN METHODS AND INNER CLASSES

	public RewriterFunctionalSequence(String rewriterName, Expression arguments, RewritingProcess process) {
		this(rewriterName, arguments, process, new ConcurrentHashMap<ChildRewriterInvocation, RewriterFunctionalSequence>());
	}
	
	protected RewriterFunctionalSequence(String rewriterName, Expression arguments, RewritingProcess process, Map<ChildRewriterInvocation, RewriterFunctionalSequence> sharedRewriterFunctionalSequences) {
		super();
		this.rewriterName                      = rewriterName;
		this.arguments                         = arguments;
		this.process                           = process;
		this.sharedRewriterFunctionalSequences = sharedRewriterFunctionalSequences;
	}
	
	protected abstract RewriterFunctionalSequence newInstance(String childRewriterName, Expression childCallExpression, RewritingProcess childCallProcess);

	@Override
	protected Expression computeFunction() {	
		Expression result = process.rewrite(rewriterName, arguments, intercepter);
		return result;
	}
	
	protected class RewriterFunctionalSequenceIntercepter implements ChildRewriterCallIntercepter {
		@Override
		public Expression intercept(String rewriterName, Expression expression, RewritingProcess process) {

			Trace.log("+functionalSequence.intercept({}, {})", rewriterName, expression);
	
			// When the inner rewriter requests a child-rewriter to rewrite something,
			// we must instead give it the current value of the rewriter functional sequence
			// corresponding to that child-rewriter and arguments.
			ChildRewriterInvocation childRewriterInvocation = new ChildRewriterInvocation(rewriterName, expression, process);

			RewriterFunctionalSequence childRewriterFunctionalSequence = getChildRewriterFunctionalSequence(childRewriterInvocation, rewriterName, expression, process);
			
			Expression result = getCurrentArgumentValue(childRewriterFunctionalSequence);

			Trace.log("-functionalSequence.intercept({}, {})={}", rewriterName, expression, result);
			
			return result;
		}
	}

	protected static class ChildRewriterInvocation {
		private String childRewriterName;
		private Expression arguments;
		private Expression contextualConstraint;
		private Set<Expression> contextualSymbols;
		
		public ChildRewriterInvocation(String rewriterName, Expression arguments, RewritingProcess process) {
			super();
			this.childRewriterName    = rewriterName;
			this.arguments            = arguments;
			this.contextualConstraint = process.getContextualConstraint();
			this.contextualSymbols  = process.getContextualSymbols();
		}

		@Override
		public boolean equals(Object another) {
			ChildRewriterInvocation anotherChildRewriterInvocation = (ChildRewriterInvocation) another;
			boolean result =
					childRewriterName.equals(anotherChildRewriterInvocation.childRewriterName) && // this is why we cannot just use a list and equals()
					arguments.equals(anotherChildRewriterInvocation.arguments) &&
					contextualConstraint.equals(anotherChildRewriterInvocation.contextualConstraint) &&
					contextualSymbols.equals(anotherChildRewriterInvocation.contextualSymbols);
			return result;
		}

		@Override
		public int hashCode() {
			int result =
					childRewriterName.hashCode() +
					arguments.hashCode() +
					contextualConstraint.hashCode() +
					contextualSymbols.hashCode(); // is there a better way for combining hash codes?
			
			return result;
		}
	}
	
	// END OF MAIN METHODS AND INNER CLASSES
	
	// BEGINNING OF UTILITY METHODS
	private RewriterFunctionalSequence getChildRewriterFunctionalSequence(ChildRewriterInvocation childRewriterInvocation, String childRewriterName, Expression childExpression, RewritingProcess childProcess) {
		RewriterFunctionalSequence result = null;

		result = sharedRewriterFunctionalSequences.get(childRewriterInvocation);
		if (result == null) {
			result = newInstance(childRewriterName, childExpression, childProcess);
			sharedRewriterFunctionalSequences.put(childRewriterInvocation, result);
		}
		
		// TODO - need a better way for the functional sequence
		// to the childRequest information.
		result.process = process;
		
		return result;
	}

	// END OF UTILITY METHODS
}
