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
package com.sri.ai.grinder.helper.concurrent;

import java.util.Map;

import org.slf4j.MDC;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.util.Configuration;

/**
 * Base class for calling RewriteOnBranch objects within a concurrent (i.e.
 * Callable) context.
 * 
 * @author oreilly
 * 
 */
@Beta
public class CallableRewriteOnBranch implements CallableRewriter {
	public static final String BRANCH_TYPE_THEN = "then";
	public static final String BRANCH_TYPE_ELSE = "else";
	public static final String BRANCH_TYPE_AND  = "and";
	public static final String BRANCH_TYPE_OR   = "or";
	public static final String BRANCH_TYPE_TASK = "task";
	
	//
	protected String           branchType         = "";
	protected RewriteOnBranch  rewriteOnBranch    = null;
	protected Expression[]     branchArguments    = null; 
	protected RewritingProcess process            = null;
	//
	private Thread  parentThread          = null;
	private Map     mdcParentContextMap   = null;            

	/**
	 * Constructor.
	 * 
	 * @param branchType
	 *            a descriptive name for the branch, e.g. 'then', 'else', etc...
	 * @param rewriteOnBranch
	 *            the object responsible for performing the actual rewrite.
	 * @param branchArguments
	 *            the arguments to be passed to the RewriteOnBranch
	 *            implementation when it is called.
	 * @param process
	 *            the current rewriting process.
	 */
	public CallableRewriteOnBranch(String branchType, RewriteOnBranch rewriteOnBranch, Expression[] branchArguments, RewritingProcess process) {
		this.branchType      = branchType;
		this.rewriteOnBranch = rewriteOnBranch;
		this.branchArguments = branchArguments;
		this.process         = process;
		//
		parentThread        = Thread.currentThread();
		mdcParentContextMap = MDC.getCopyOfContextMap();
	}

	@Override
	public Expression call() {
		Expression result = null;
		if (isCallRewriteOnBranch()) {
			beforeCallRewriteOnBranch();
			result = callRewriteOnBranch();
		}
		return result;
	}
	
	//
	// PROTECTED METHODS
	//
	/**
	 * 
	 * @return true is callRewriteOnBranch() is to be called, false otherwise.
	 */
	protected boolean isCallRewriteOnBranch() {
		return true;
	}

	/**
	 * Logic that needs to be performed before callRewriteOnBranch() is
	 * executed.
	 */
	protected void beforeCallRewriteOnBranch() {
		// If I'm being run concurrently, then certain objects need
		// to be setup and configured on the worker thread.
		if (parentThread != Thread.currentThread()) {
			// Ensure configuration information is inherited by the working thread
			Configuration.inheritConfiguration(parentThread, Thread.currentThread());
			// Ensure the Trace and Justification APIs are setup to
			// run correctly on this worker thread.
			MDC.setContextMap(mdcParentContextMap);
			// Ensure this information is available to any expressions
			// within the context of the worker thread.
			DefaultRewritingProcess.setGlobalRewritingProcessForKnowledgeBasedExpressions(getBranchProcess());
		}
	}
	
	/**
	 * 
	 * @return the rewriting process to be used on the branch.
	 */
	protected RewritingProcess getBranchProcess() {
		return process;
	}
	
	/**
	 * If isCallRewriteOnBranch() returns true, this method is called after
	 * beforeCallRewriteOnBranch() is performed.
	 * 
	 * @return the result from calling RewriteOnBranch.
	 */
	protected Expression callRewriteOnBranch() {
		Expression result = rewriteOnBranch.rewrite(branchArguments, getBranchProcess());
		
		return result;
	}
}
