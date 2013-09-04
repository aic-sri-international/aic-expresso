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
import com.google.common.collect.Lists;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.SemanticSubstitute;
import com.sri.ai.grinder.library.boole.QuantifierSubExpressionAndScopedVariableProvider;

/**
 * A class providing basic methods for the manipulation of lambda expressions.
 * 
 * @braz
 */
@Beta
public class Lambda extends QuantifierSubExpressionAndScopedVariableProvider {

	public static final String ROOT = "lambda . : .";
	public static final String SYNTACTIC_FORM_TYPE = "Lambda expression";

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		return expression;
	}

	public static boolean isLambdaExpression(Expression expression) {
		return expression != null &&
		expression.getSyntaxTree().getRootTree() != null &&
		expression.getSyntaxTree().getRootTree().equals(ROOT);
	}

	public static List<Expression> getParameters(Expression expression) {
		return Expressions.ensureListFromKleeneList(expression.getSyntaxTree().getSubTree(0)); // does need to be sub tree
	}

	public static Expression getBody(Expression lambda) {
		return lambda.getSyntaxTree().getSubTree(1); // does need to be sub tree
	}

	/**
	 * Makes a lambda expression from a given set of parameters and a body.
	 */
	public static Expression make(List<Expression> parameters, Expression body) {
		Expression parameterList = Expressions.makeKleeneListIfNeeded(parameters);
		Expression result = Expressions.apply(ROOT, parameterList, body);
		return result;
	}
	
	public static Expression make(Expression variable, Expression body) {
		return make(Lists.newArrayList(variable), body);
	}
	
	/**
	 * Checks for equality of two lambda expressions up to renaming of
	 * single-variable parameters.
	 */
	public static boolean areKnownToBeEqual(Expression lambda1, Expression lambda2, RewritingProcess process) {
		List<Expression> parameters1 = getParameters(lambda1);
		List<Expression> parameters2 = getParameters(lambda2);
		if (parameters1.size() != parameters2.size()) {
			return false;
		}
		Expression body1 = getBody(lambda1);
		Expression body2 = getBody(lambda2);
		if ( ! parameters1.equals(parameters2)) {
			Iterator<Expression> parameter1Iterator = parameters1.iterator();
			Iterator<Expression> parameter2Iterator = parameters2.iterator();
			while (parameter1Iterator.hasNext()) {
				Expression parameter1 = parameter1Iterator.next();
				Expression parameter2 = parameter2Iterator.next();
				if ( ! parameter1.equals(parameter2)) {
					if (parameter1 instanceof Symbol && parameter2 instanceof Symbol) {
						body2 = SemanticSubstitute.replace(body2, parameter2, parameter1, process);
//						body2 = Substitute.replaceWithoutTryingToUnify(body2, parameter2, parameter1, process);
					}
					else {
						return false; // not renaming function applications, so just give up.
					}
				}
			}
		}
		boolean result = body1.equals(body2);
		return result;
		// We still need to take into account the situation in which a variable appears in different positions,
		// such as lambda X,Y ... and lambda Y,Z ...
		// This requires standardizing apart.
	}

	@Override
	protected String getRootTreeString() {
		return ROOT;
	}

	@Override
	protected String getSyntacticFormType() {
		return SYNTACTIC_FORM_TYPE;
	}
}
