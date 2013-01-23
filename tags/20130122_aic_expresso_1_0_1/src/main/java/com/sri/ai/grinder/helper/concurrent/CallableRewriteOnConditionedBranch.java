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

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.helper.GrinderUtil;

/**
 * An extension to CallableRewriteOnBranch for handling conditioned calls (i.e.
 * 'if {a condition} then {rewrite based on condition being true} else {rewrite
 * based on condition being false}')
 * 
 * @author oreilly
 * 
 */
@Beta
public class CallableRewriteOnConditionedBranch extends CallableRewriteOnBranch {

	private boolean               callRewrite                        = true;
	private Expression            branchCondition                    = null;
	private String                rewriterNameToCheckBranchReachable = null;
	//
	private RewritingProcess branchProcess                = null;
	
	/**
	 * Constructor.
	 * 
	 * @param callRewrite
	 *            set to true if callRewriteOnBranch() should be called, false
	 *            otherwise.
	 * @param branchType
	 *            a descriptive name for the branch, i.e.. 'then' or 'else'.
	 * @param branchCondition
	 *            the condition under which the RewriteOnBranch is being called
	 *            (i.e. for the then branch this is the 'condition' and for the
	 *            else branch is 'not condition').
	 * @param rewriterOnConditionedBranch
	 *            the RewriteOnBranch to be called to perform the acutal rewrite
	 *            logic.
	 * @param branchArguments
	 *            the arguments to be passed to the RewriteOnBranch
	 *            implementation when it is called.
	 * @param rewriterNameToCheckBranchReachable
	 *            a rewriter name to be used to check whether or not the branch is
	 *            actually reachable based on its branchCondition.
	 * @param process
	 *            the current rewriting process.
	 */
	public CallableRewriteOnConditionedBranch(
			boolean callRewrite,
			String branchType,
			Expression branchCondition,
			RewriteOnBranch rewriterOnConditionedBranch,
			Expression[] branchArguments,
			String rewriterNameToCheckBranchReachable,
			RewritingProcess process) {
		super(branchType, rewriterOnConditionedBranch, branchArguments, process);
		
		this.callRewrite                        = callRewrite;
		this.branchCondition                    = branchCondition;
		this.rewriterNameToCheckBranchReachable = rewriterNameToCheckBranchReachable;
		// default to the current process.
		this.branchProcess = process;
	}
	
	//
	// PROTECTED METHODS
	//
	@Override
	protected boolean isCallRewriteOnBranch() {
		boolean result = false;
		if (callRewrite) {
			branchProcess = GrinderUtil.extendContextualConstraint(branchCondition, process);
			// Only process the branch if legal within the current contextual constraint.
			Expression branchReachable =  GrinderUtil.currentContextBranchReachable(rewriterNameToCheckBranchReachable, process, branchProcess);
			if (!branchReachable.equals(Expressions.FALSE)) {
				result = true;
			}
		}
		return result;
	}
	
	@Override
	protected RewritingProcess getBranchProcess() {
		return branchProcess;
	}
}
