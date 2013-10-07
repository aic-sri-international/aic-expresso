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
package com.sri.ai.grinder.library.boole;

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;

/**
 * A collection of helper methods for handling first order logic 'there exists . : .' expressions.
 * 
 * @author braz
 *
 */
@Beta
public class ThereExists {

	public static final String LABEL = "there exists . : .";

	public static Expression make(List<Expression> indexExpressions, Expression body) {
		Expression current = body;
		for (int i = indexExpressions.size() - 1; i >= 0; i--) {
			current = make(indexExpressions.get(i), current);
		}
		return current;
	}

	public static Expression make(Expression indexExpression, Expression body) {
		Expression current = Expressions.apply(LABEL, indexExpression, body);
		return current;
	}

	public static final String SYNTACTIC_FORM_TYPE = "There exists";

	public static boolean isThereExists(Expression expression) {
		boolean result = expression.getSyntaxTree().getLabel().equals(ThereExists.LABEL) && expression.getSyntaxTree().numberOfImmediateSubTrees() != 0;
		return result;
	}
	
	public static Expression getIndex(Expression expression) {
		Expression indexExpression = getIndexExpression(expression);
		Expression result = IndexExpressions.getIndex(indexExpression);
		return result;
	}
	
	public static Expression getIndexExpression(Expression expression) {
		Expression result = expression.getSyntaxTree().getSubTree(0);
		return result;
	}
	
	public static Expression getBody(Expression expression) {
		Expression result = expression.getSyntaxTree().getSubTree(1);
		return result;
	}
}
