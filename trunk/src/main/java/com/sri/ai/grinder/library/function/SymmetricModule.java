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
package com.sri.ai.grinder.library.function;

import java.util.HashSet;
import java.util.LinkedHashSet;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;


/**
 * This module concentrates the functionality for registering and using pieces
 * of knowledge that can tell whether a function is symmetric
 * (meaning that all its arguments are symmetric, that is, their order does not matter).
 * 
 * @author braz
 */
@Beta
public class SymmetricModule extends AbstractRewriter implements NoOpRewriter {

	private HashSet<Provider> providers = new LinkedHashSet<Provider>();
	
	/**
	 * An interface for objects that know how to determine
	 * whether the a function is symmetric.
	 * Providers must notify {@link SymmetricModule} of their existence
	 * with the method {@link SymmetricModule#register(Provider)} so it can invoke them.
	 * as necessary.
	 */
	public static interface Provider {
		boolean isSymmetric(Expression function, RewritingProcess process);
	}

	public void register(Provider provider) {
		providers.add(provider);
	}

	@Override
	/* This will eventually be removed when we introduce mechanism to deal with modules. */
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression;
	}

	@Override
	public void rewritingProcessFinalized(RewritingProcess process) {
		providers.clear();
	}

	public boolean isSymmetric(Expression function, RewritingProcess process) {
		for (Provider provider : providers) {
			boolean result = provider.isSymmetric(function, process);
			if (result) {
				return result;
			}
		}
		return false;
	}
}
