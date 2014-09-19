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

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasFunctor;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.library.function.MutuallyExclusiveCoDomainsModule;

/**
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractOperationOnMutuallyExclusiveCoDomainExpressions
		extends AbstractRewriter {
	
	public AbstractOperationOnMutuallyExclusiveCoDomainExpressions() {
		this.setReifiedTests(new HasFunctor(getFunctor()),
				             new HasNumberOfArguments(2));
	}

	protected abstract String getFunctor();
	protected abstract Expression getResult();
	protected abstract Expression getResultExpression();

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {

		Expression expression1 = expression.get(0);
		Expression expression2 = expression.get(1);
		if (haveMutuallyExclusiveCoDomains(expression1, expression2, process)) {
			return getResultExpression();
		}
			
		return expression;
	}

	private boolean haveMutuallyExclusiveCoDomains(Expression expression1, Expression expression2, RewritingProcess process) {
		MutuallyExclusiveCoDomainsModule mutuallyExclusiveCoDomainsModule = (MutuallyExclusiveCoDomainsModule) process.findModule(MutuallyExclusiveCoDomainsModule.class);
		boolean haveMutuallyExclusiveCoDomains = mutuallyExclusiveCoDomainsModule != null && mutuallyExclusiveCoDomainsModule.haveMutuallyExclusiveCoDomains(expression1, expression2, process);
		return haveMutuallyExclusiveCoDomains;
	}
}
