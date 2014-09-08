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
package com.sri.ai.grinder.core;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.google.common.collect.Lists;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewriterTest;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.helper.Justification;
import com.sri.ai.grinder.helper.RewriterLogging;
import com.sri.ai.grinder.helper.Trace;
import com.sri.ai.util.Util;
import com.sri.ai.util.Util.SelectPairResult;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.base.BinaryPredicate;
import com.sri.ai.util.base.Pair;

/**
 * A basic, default implementation of some of the {@link Rewriter} methods.
 * 
 * @author braz
 */
@Beta
public abstract class AbstractRewriter implements Rewriter {
	
//	// FOR DEBUGGING
//	// This is a way of setting a breakpoint condition in AbstractRewriter with classes from projects
//	// using aic-expresso using their own classes, which would not be recognized here.
//	// I was using it right before AbstractRewriter invoked rewriteAfterBookkeeping
//	// in order to intercept a particular rewriting action by a rewriter in aic-praise. @author braz
//	public static TernaryPredicate<Rewriter, Expression, Expression> conditionForBreakpoint;
	
	private static final List<Rewriter> _emptyChildList = Collections.unmodifiableList(new ArrayList<Rewriter>());
	//
	private String name = null;
	private List<RewriterTest> reifiedTests = Collections.emptyList(); 
	private boolean traceInAndOutOfRewriter = GrinderConfiguration.isTraceInAndOutOfAtomicRewriterEnabled();

	/**
	 * A general rewriting utility which receives an expression, looks for a
	 * pair of its arguments satisfying a given set of predicates (both
	 * individually and as a pair), performs a binary operation on them, and
	 * returns a new expression with the same functor as the original, with the
	 * pair of arguments removed and the result of the binary operation in the
	 * original position of the first one. If such a pair is not found, simply
	 * returns the original expression. If true, argument
	 * <code>noUnaryApplication</code> prevents the new operation from being a
	 * unary application, returning instead its only argument (this is useful
	 * for operators whose singleton application is the same as the identity,
	 * such as conjunction, disjunction, addition, etc).
	 */
	public static Expression expressionAfterBinaryOperationIsPerformedOnPairOfArgumentsSatisfyingPredicates(
			Expression expression, BinaryFunction<Expression, Expression, Expression> binaryOperation, Predicate<Expression> unaryPredicate1, Predicate<Expression> unaryPredicate2, BinaryPredicate<Expression, Expression> binaryPredicate,
			boolean noUnaryApplication) {

		SelectPairResult<Expression> pair =
			Util.selectPairInEitherOrder(expression.getArguments(), unaryPredicate1, unaryPredicate2, binaryPredicate);

		if (pair != null) {
			Expression operationResult = binaryOperation.apply(pair.satisfiesFirstPredicate, pair.satisfiesSecondPredicate);

			Pair<List<Expression>, List<Expression>> slices =
				Util.slicesBeforeIAndRestWithoutJ(
						expression.getArguments(),
						pair.indexOfFirst, pair.indexOfSecond);

			@SuppressWarnings("unchecked")
			List<Expression> arguments =
					Util.addAllToANewList(
							(Collection<Expression>) slices.first,
							(Collection<Expression>) Lists.newArrayList(operationResult),
							(Collection<Expression>) slices.second);

			if (noUnaryApplication && arguments.size() == 1) {
				return arguments.get(0);
			}

			Expression result = Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(expression.getFunctor(), arguments);
			return result;
		}
		return expression;
	}
	
	public AbstractRewriter() {
		super();
	}
	
	//
	// START-Rewriter
	@Override
	public String getName() {
		if (name == null) {
			name = Util.camelCaseToSpacedString(getClass().getSimpleName());
		}
		return name;
	}
	
	@Override
	public List<RewriterTest> getReifiedTests() {
		return reifiedTests;
	}

	@Override
	public Expression rewrite(Expression expression, boolean bypassTests, RewritingProcess process){
		Expression result   = expression;
		Expression original = expression;
		
		String previousRewriterName = null;
		if (Trace.isEnabled() || Justification.isEnabled()) {
			previousRewriterName = RewriterLogging.setCurrentRewriterName(getName());
		}
		
		if (isTraceInAndOutOfRewriter()) {
			if (Trace.isEnabled()) {
				Trace.in("+"+getName()+"({}) - under context variables = {}, constrained by {}", expression, process.getContextualSymbols(), process.getContextualConstraint());
			}
		}
		
		if (process.getRootRewriter() == null) {
			process.setRootRewriter(this);
		}
		Expression preProcessing = process.rewritingPreProcessing(this, expression);
		if (preProcessing != null) {
			result = preProcessing;
		} 
		else if (process.getContextualConstraint().equals(Expressions.FALSE)) {
			result = Rewriter.FALSE_CONTEXTUAL_CONTRAINT_RETURN_VALUE;
		} 
		else {
			
			if (bypassTests || runReifiedTests(expression, process)) {
				result = rewriteAfterBookkeeping(expression, process);
			}
			
			if (result != original && original == process.getRootExpression()) {
				process.setRootExpression(result);
			}
			process.rewritingPostProcessing(this, original, result);
		}
		
		if (isTraceInAndOutOfRewriter()) {
			if (Trace.isEnabled()) {
				if (result != expression) {
					Trace.out(RewriterLogging.REWRITER_PROFILE_INFO, "-"+getName()+"={}", result);
				}
				else {
					Trace.out(RewriterLogging.REWRITER_PROFILE_INFO, "-"+getName()+" did not apply");
				}
			}
		}
		
		if (Trace.isEnabled() || Justification.isEnabled()) {
			RewriterLogging.setCurrentRewriterName(previousRewriterName);
		}
		
		return result;
	}
	
	@Override
	public Expression rewrite(Expression expression, RewritingProcess process) {
		// Don't bypass tests by default.
		return rewrite(expression, false, process);
	}
	
	@Override
	public Expression rewrite(Expression expression) {
		RewritingProcess process = makeRewritingProcess(expression);
		process = GrinderUtil.extendContextualSymbolsWithFreeVariablesInExpressionwithUnknownTypeForSetUpPurposesOnly(expression, process);
		return rewrite(expression, process);
	}

	@Override
	public Iterator<Rewriter> getChildrenIterator() {
		return _emptyChildList.iterator();
	}
	
	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
	}

	@Override
	public void rewritingProcessFinalized(RewritingProcess process) {
	}
	
	// END-Rewriter
	//
	
	public abstract Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process);

	@Override
	public RewritingProcess makeRewritingProcess(Expression expression) {
		DefaultRewritingProcess result = new DefaultRewritingProcess(expression, this);
		return result;
	}

	@Override
	public String toString() {
		return Util.camelCaseToSpacedString(getClass().getSimpleName());
	}
	
	//
	// PROTECTED METHODS
	//	
	protected void setName(String name) {
		this.name = name;
	}
	
	protected void setReifiedTests(RewriterTest... rewriterTests) {
		reifiedTests = new ArrayList<RewriterTest>();
		
		for (RewriterTest rt : rewriterTests) {
			reifiedTests.add(rt);
		}
		
		// For safety, make immutable
		reifiedTests = Collections.unmodifiableList(reifiedTests);
	}
	
	protected boolean runReifiedTests(final Expression expression, final RewritingProcess process) {		
		// Note: intentionally not using Util.forAll as this is a heavily
		// used routine I don't won't to have the overhead of creating
		// iterators.
		int numTests = reifiedTests.size();
		for (int i = 0; i < numTests; i++) {
			if (!reifiedTests.get(i).apply(expression, process)) {
				return false;
			}
		}
		return true;
	}
	
	protected boolean isTraceInAndOutOfRewriter() {
		return traceInAndOutOfRewriter;
	}
}