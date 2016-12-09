/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.grinder.sgdpllt.library.lambda;

import static com.sri.ai.util.Util.map;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.FunctionApplication;
import com.sri.ai.expresso.api.LambdaExpression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Rewriter;
import com.sri.ai.grinder.sgdpllt.rewriter.api.Simplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.core.Switch;

/**
 * A lambda calculus beta reduction simplifier, which simply means a 
 * simplifier that applies a lambda expression to arguments:<br>
 * <br>
 * <pre>  
 * (lambda X in People : if X = ann then 0 else if X = bob then 0 else 0)(ann) 
 *    ---> 
 * if ann = ann then 0 else if ann = bob then 0 else 0
 * </pre>
 * 
 * @author oreilly
 *
 */
@Beta
public class LambdaBetaReductionSimplifier extends Switch<Object> {
	public LambdaBetaReductionSimplifier() {
		super(
				Switch.SYNTACTIC_FORM_TYPE,
				new HashMap<Object, Rewriter>(makeSyntacticFormTypeSimplifiers()));
	}
	
	public static Map<String, Simplifier> makeSyntacticFormTypeSimplifiers() {
		return map(FunctionApplication.SYNTACTIC_FORM_TYPE, (Simplifier) (f, context) -> simplify(f, context));
	}
	
	public static Expression simplify(Expression expression, Context context) {
		Expression result = expression;
		
		if (expression.getFunctor() instanceof LambdaExpression) {						
			LambdaExpression lambdaExpression           = (LambdaExpression) expression.getFunctor();
			List<Expression> lambdaIndexExpressionsList = IndexExpressions.getIndexExpressionsWithType(lambdaExpression.getIndexExpressions());
			List<Expression> lambdaArguments            = expression.getArguments();
			if (lambdaArguments.size() == lambdaIndexExpressionsList.size()) {
				result = lambdaExpression.getBody();
				for (int i = 0; i < lambdaIndexExpressionsList.size(); i++) {
					Expression indexExpression    = IndexExpressions.getIndex(lambdaIndexExpressionsList.get(i));
					Expression argumentExpression = lambdaArguments.get(i);
					result = result.replaceAllOccurrences(indexExpression, argumentExpression, context);
				}				
			}
		}
		
		return result;
	}
}