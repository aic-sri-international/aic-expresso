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
package com.sri.ai.grinder.library.equality.injective;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.function.MutuallyExclusiveCoDomainsModule;

/**
 * A {@link MutuallyExclusiveCoDomainsModule.Provider} provider for declaring mutually exclusive co-domains.
 * 
 * @author braz
 *
 */
@Beta
public class DeclareMutuallyExclusiveCoDomains extends AbstractRewriter
implements NoOpRewriter, MutuallyExclusiveCoDomainsModule.Provider {

	private Collection<? extends Expression> functors;
	
	public DeclareMutuallyExclusiveCoDomains(Object... functors) {
		super();
		this.functors = Expressions.wrap(functors);
	}

	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
		MutuallyExclusiveCoDomainsModule.register(this, process);
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression; // will be removed eventually, not a real rewriter, just a module.
	}

	@Override
	public boolean haveMutuallyExclusiveCoDomains(Expression expression1,
			Expression expression2, RewritingProcess process) {
		if (expression1.hasFunctor(expression2.getFunctor()) ||
				expression1.equals(expression2)) {
			return false;
		}
		boolean containsFirst = functors.contains(expression1.getFunctor()) || functors.contains(expression1);
		boolean containsSecond = functors.contains(expression2.getFunctor()) || functors.contains(expression2);
		boolean result = containsFirst && containsSecond;
		return result;
	}
}
