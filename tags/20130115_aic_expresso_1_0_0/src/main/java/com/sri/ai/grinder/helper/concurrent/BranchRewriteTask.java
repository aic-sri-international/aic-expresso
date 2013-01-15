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

/**
 * A utility class for bundling together a RewriteOnBranch instance and the
 * corresponding arguments that should be passed to it when it is called. The
 * rewriting process to be used, should be determined externally.
 * 
 * @author oreilly
 * 
 */
@Beta
public class BranchRewriteTask {
	private RewriteOnBranch rewriteOnBranch = null;
	private Expression[]    arguments       = null;

	/**
	 * Constructor.
	 * 
	 * @param rewriteOnBranch
	 *            the RewriteOnBranch instance to be called.
	 * @param arguments
	 *            the expression arguments to be used when calling the
	 *            RewriteOnBranch instance.
	 */
	public BranchRewriteTask(RewriteOnBranch rewriteOnBranch,
			Expression[] arguments) {
		this.rewriteOnBranch = rewriteOnBranch;
		this.arguments = arguments;
	}

	/**
	 * 
	 * @return the RewriteOnBranch instance to be called.
	 */
	public RewriteOnBranch getRewriteOnBranch() {
		return rewriteOnBranch;
	}

	/**
	 * 
	 * @return the arguments to be passed to the RewriteOnBranch instance.
	 */
	public Expression[] getArguments() {
		return arguments;
	}
}
