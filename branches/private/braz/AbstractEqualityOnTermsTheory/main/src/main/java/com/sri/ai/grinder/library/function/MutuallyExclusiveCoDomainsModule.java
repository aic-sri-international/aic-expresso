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

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.AbstractModuleNoOpRewriter;
import com.sri.ai.grinder.api.Module;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * 
 * @author braz
 *
 */
@Beta
public class MutuallyExclusiveCoDomainsModule extends AbstractModuleNoOpRewriter {

	/**
	 * An interface for objects that know how to determine whether the possible
	 * interpretations of two expressions are never going to be the same due to
	 * their co-domains. Providers must notify
	 * {@link MutuallyExclusiveCoDomainsModule} of their existence with the
	 * method {@link MutuallyExclusiveCoDomainsModule#register(Provider)} so it
	 * can invoke them. as necessary.
	 * {@link MutuallyExclusiveCoDomainsModule#register(Provider, RewritingProcess)} is provided as a convenience for finding the module in the rewriting process.
	 */
	public static interface Provider extends Module.Provider {
		boolean haveMutuallyExclusiveCoDomains(Expression expression1, Expression expression2, RewritingProcess process);
	}

	/**
	 * Registers a {@link Provider} in the {@link MutuallyExclusiveCoDomainsModule} module of the given process,
	 * or throw an error if there is not one.
	 */
	public static void register(Provider provider, RewritingProcess process) throws Error {
		register(MutuallyExclusiveCoDomainsModule.class, provider, process);
	}

	/**
	 * An abstract implementation of {@link Provider} reducing
	 * {@link Provider#haveMutuallyExclusiveCoDomains(Expression, Expression, RewritingProcess)}
	 * to
	 * {@link Provider#haveMutuallyExclusiveCoDomains(Expression, Expression, RewritingProcess)}
	 * on the expressions's corresponding knowledge-based expressions.
	 */
	public static abstract class AbstractProvider implements Provider {
		@Override
		public boolean haveMutuallyExclusiveCoDomains(Expression expression1, Expression expression2, RewritingProcess process) {
			return haveMutuallyExclusiveCoDomains(expression1, expression2, process);
		}
	}

	public boolean haveMutuallyExclusiveCoDomains(Expression expression1, Expression expression2, RewritingProcess process) {
		boolean result;
		for (Module.Provider moduleProvider : providers.keySet()) {
			Provider provider = (Provider) moduleProvider;
			result = provider.haveMutuallyExclusiveCoDomains(expression1, expression2, process);
			if (result) {
				return result;
			}
		}
		return false;
	}
}
