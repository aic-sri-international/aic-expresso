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
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewriterTest;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.Util;

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
	
	//
	private String name = null;
	private List<RewriterTest> reifiedTests = Collections.emptyList(); 
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
		return rewriteAfterBookkeeping(expression, process);
	}
	
	@Override
	public Expression rewrite(Expression expression, RewritingProcess process) {
		// Don't bypass tests by default.
		return rewrite(expression, false, process);
	}
	
	// END-Rewriter
	//
	
	public abstract Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process);

	@Override
	public RewritingProcess makeRewritingProcess(Expression expression) {
		DefaultRewritingProcess result = new DefaultRewritingProcess();
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
		// used routine I don't want to have the overhead of creating
		// iterators.
		int numTests = reifiedTests.size();
		for (int i = 0; i < numTests; i++) {
			if (!reifiedTests.get(i).apply(expression, process)) {
				return false;
			}
		}
		return true;
	}
}