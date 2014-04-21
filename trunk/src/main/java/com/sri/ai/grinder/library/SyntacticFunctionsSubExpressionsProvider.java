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
package com.sri.ai.grinder.library;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;

/**
 * A provider indicating what are the sub-expressions and syntactic form of a
 * list of named syntactic functions. The functions are assumed to be unary.
 * Non-unary structures with the same names are ignored (they can exist and be
 * treated by other providers). They have no sub-expressions because their body
 * expression cannot be rewritten.
 * 
 * @author braz
 */
@Beta
public class SyntacticFunctionsSubExpressionsProvider extends AbstractRewriter
implements NoOpRewriter, ExpressionKnowledgeModule.Provider
{
	private Set<Expression> functionNameSymbols;

	public SyntacticFunctionsSubExpressionsProvider(String... names) {
		functionNameSymbols = new LinkedHashSet<Expression>(Expressions.wrap(names));
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression; // this method should be eliminated as some point because this is not really a rewriter.
	}

	@Override
	public Object getSyntacticFormType(Expression expression, RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			return "Syntactic function";
		}
		return null;
	}

	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(
			Expression expression, RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			List<ExpressionAndContext> emptyList = Collections.emptyList();
			return emptyList.iterator(); // no subexpressions because the queried expression is a syntactic parameter
		}
		return null;
	}

	private boolean knowledgeApplies(Expression expression) {
		boolean result =
			expression.getSyntaxTree().getRootTree() != null &&
			functionNameSymbols.contains(expression.getSyntaxTree().getRootTree()) &&
			expression.getSyntaxTree().numberOfImmediateSubTrees() == 1;
		return result;
	}

	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
		ExpressionKnowledgeModule.register(this, process);
	}
}
