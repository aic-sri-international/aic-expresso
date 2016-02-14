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
package com.sri.ai.grinder.library.set.tuple;

import java.util.Arrays;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;

/**
 * API for tuples. Because parsing tuples with single elements is ambiguous with
 * parenthesized expressions, function applications of "tuple" are also treated
 * as tuples.
 * 
 * @author braz
 */
@Beta
public class Tuple extends AbstractRewriter {

	public static final String TUPLE_LABEL = "( . )";
	//

	@Override
	public Expression apply(Expression expression, RewritingProcess process) {
		return expression;
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression;
	}
	
	@SuppressWarnings("unchecked")
	public static Expression make(Object... elements) {
		List list = null;
		if (elements.length == 1 && elements[0] instanceof List) {
			list = (List) elements[0];
		}
		else {
			list = Arrays.asList(elements);
		}
		list = Expressions.makeSureItIsSyntaxTreeOrNonExpressionObject(list);
		Expression result =
				Expressions.makeExpressionOnSyntaxTreeWithLabelAndSubTrees(
						TUPLE_LABEL, SyntaxTrees.makeKleeneListIfNeeded(list));
		return result;
	}

	public static boolean isTuple(Expression expression) {
		boolean result =
				expression.getSyntaxTree().getLabel().equals(TUPLE_LABEL)
				||
				expression.getSyntaxTree().getLabel().equals("tuple");
		return result;
	}
	
	public static List<Expression> getElements(Expression expression) {
		List<Expression> result = expression.getSubExpressions();
		return result;
	}
	
	public static int size(Expression expression) {
		List<Expression> elements = getElements(expression);
		int result = elements.size();
		return result;
	}
	
	public static Expression get(Expression expression, int index) {
		return getElements(expression).get(index);
	}
}
