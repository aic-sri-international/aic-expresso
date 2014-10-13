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
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.core.DefaultExpressionAndContext;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SyntaxTrees;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.helper.BasePathIterator;
import com.sri.ai.grinder.library.equality.CheapDisequalityModule;
import com.sri.ai.grinder.library.function.InjectiveModule;
import com.sri.ai.grinder.library.function.MutuallyExclusiveCoDomainsModule;
import com.sri.ai.util.Util;
import com.sri.ai.util.collect.PairIterator;

/**
 * API for tuples. Because parsing tuples with single elements is ambiguous with
 * parenthesized expressions, function applications of "tuple" are also treated
 * as tuples.
 * 
 * @author braz
 */
@Beta
public class Tuple extends AbstractRewriter
implements
NoOpRewriter,
CheapDisequalityModule.Provider,
InjectiveModule.Provider,
MutuallyExclusiveCoDomainsModule.Provider {

	public static final String TUPLE_LABEL = "( . )";
	//
	private static final List<Integer> _pathZero = Collections.unmodifiableList(Arrays.asList(new Integer(0)));

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
		List<Expression> elements;
		if (expression.getSyntaxTree().getLabel().equals("tuple")) {
			elements = expression.getArguments();
			// this is incorrect because it is treating tuples as function applications, but they are not;
			// their syntactic form type is "Tuple", not "Function application".
			// So it is working based on chance implementation details.
			// It is part of what needs to be cleaned up regarding the whole representation of expressions.
		}
		else {
			elements = Expressions.ensureListFromKleeneList(Expressions.makeFromSyntaxTree(expression.getSyntaxTree().getSubTree(0)));
		}
		return elements;
	}
	
	public static int size(Expression expression) {
		List<Expression> elements = getElements(expression);
		int result = elements.size();
		return result;
	}
	
	public static Expression get(Expression expression, int index) {
		return getElements(expression).get(index);
	}

	@Override
	public boolean haveMutuallyExclusiveCoDomains(Expression expression1, Expression expression2, RewritingProcess process) {
		boolean result = isTuple(expression1) &&
				isTuple(expression2) &&
				size(expression1) != size(expression2);
		return result;
	}
	
	@Override
	public boolean isCheapDisequality(Expression e1, Expression e2, RewritingProcess process) {
		boolean result = false;
		
		if (isTuple(e1) && isTuple(e2)) {
			List<Expression> els1 = getElements(e1);
			List<Expression> els2 = getElements(e2);
			if (els1.size() != els2.size()) {
				// tuples of different length are guaranteed not to be equal.
				result = true;
			}
			else {
				// Both tuples are the same length, lets see if another provider
				// can determine inequality cheaply between the elements.
				CheapDisequalityModule cheapDisequalityModule = (CheapDisequalityModule) process.findModule(CheapDisequalityModule.class);
				if (cheapDisequalityModule != null) {
					int size = els1.size();
					for (int i = 0; i < size; i++) {
						result = cheapDisequalityModule.isCheapDisequality(els1.get(i), els2.get(i), process);
						if (result) {
							// The provider guarantees: e1 != e2
							break;
						}
					}
				}
			}
		}
		
		return result;
	}

	@Override
	public Object getInjectiveFunctionToken(Expression expression, RewritingProcess process) {
		if ( ! isTuple(expression)) {
			return null;
		}
		Expression token = Expressions.makeSymbol("tuple/" + size(expression));
		return token;
	}

	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
		CheapDisequalityModule.register(this, process);
		InjectiveModule.register(this, process);
		MutuallyExclusiveCoDomainsModule.register(this, process);
	}
}
