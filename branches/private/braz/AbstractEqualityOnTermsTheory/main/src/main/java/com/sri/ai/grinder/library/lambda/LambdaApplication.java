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
package com.sri.ai.grinder.library.lambda;

import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.LambdaExpression;
import com.sri.ai.expresso.api.QuantifiedExpression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.util.Util;

/**
 * 
 * @author braz
 *
 */
@Beta
public class LambdaApplication extends AbstractRewriter {
	public interface PerformApplication {
		boolean isApplicationToBePerformed(Expression lambdaExpression, RewritingProcess process);
	}
	
	private PerformApplication performApplication = null;
	
	public LambdaApplication() {
		this(new PerformApplication() {
			@Override
			public boolean isApplicationToBePerformed(Expression lambdaExpression, RewritingProcess process) {
				// By default always perform
				return true;
			}
		});
	}
	
	public LambdaApplication(PerformApplication performApplication) {
		this.performApplication = performApplication;
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression functor = expression.getFunctor();
		if (functor instanceof LambdaExpression &&
			performApplication.isApplicationToBePerformed(functor, process)) {
			expression = performApplication(expression, process);
		}
		return expression;
	}

	public static Expression performApplication(Expression lambdaApplicationExpression, RewritingProcess process) throws Error {
		Expression lambdaExpression = lambdaApplicationExpression.getFunctor();
		List<Expression> arguments = lambdaApplicationExpression.getArguments();
		Expression result = performApplication(lambdaExpression, arguments, process);
		return result;
	}

	public static Expression performApplication(Expression lambdaExpression, List<Expression> arguments, RewritingProcess process) throws Error {
		List<Expression> parameters = GrinderUtil.getParameters((QuantifiedExpression) lambdaExpression);
		if (parameters.size() != arguments.size()) {
			throw new Error("Lambda application number of parameters and arguments does not match: " + lambdaExpression + "(" + Util.join(arguments) + ")");
		}
		Expression body = ((LambdaExpression) lambdaExpression).getBody();
		Expression result = performApplication(parameters, body, arguments, process);
		return result;
	}

	public static Expression performApplication(List<Expression> parameters, Expression body, List<Expression> arguments, RewritingProcess process) {
		Expression result = body;
		Iterator<Expression> parameterIterator = parameters.iterator();
		Iterator<Expression> argumentIterator = arguments.iterator();
		while (parameterIterator.hasNext()) {
			Expression parameter = parameterIterator.next();
			Expression argument = argumentIterator.next();
			result = SemanticSubstitute.replace(result, parameter, argument, process);
		}
		return result;
	}
}
