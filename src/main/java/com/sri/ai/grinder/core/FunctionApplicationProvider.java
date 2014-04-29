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
package com.sri.ai.grinder.core;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.helper.ExpressionKnowledgeModule;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.NoOpRewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A function application provider.
 * 
 * @author braz
 *
 */
@Beta
public class FunctionApplicationProvider extends AbstractRewriter
implements
NoOpRewriter,
ExpressionKnowledgeModule.Provider
{
	public static final int INDEX_OF_FUNCTOR_IN_FUNCTION_APPLICATIONS = -1;
	
	@Override
	public Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIterator(Expression expression, final RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			Iterator<SyntaxTree> subTreesAndRootTreeIterator = expression.getSyntaxTree().getImmediateSubTreesIncludingRootOneIterator();
			FunctionIterator<Integer, List<Integer>> pathsIterator =
				Expressions.makeSingleIndexPathsIteratorFromTo(
						-1, expression.getSyntaxTree().numberOfImmediateSubTrees() - 1);
			
			List<Expression> emptyList = Collections.emptyList();
			Iterator<ExpressionAndContext> result =
				Expressions.makeIteratorOfSubExpressionsAndContextsFromIteratorsOnSubTreesAndPathsWithGivenQuantifiedVariables(
						subTreesAndRootTreeIterator, pathsIterator, emptyList /* no index expressions */, emptyList, process); // by default, no quantified variables
		
			return result;
		}
		return null;
	}
	
	public static Expression getFunctor(Expression expression) {
		if (knowledgeApplies(expression)) {
			return Expressions.makeFromSyntaxTree(expression.getSyntaxTree().getRootTree());
		}		
		return null;
	}

	public static Expression getFunctorOrSymbol(Expression expression) {
		if (expression.getSyntaxTree() instanceof CompoundSyntaxTree) {
			return expression.getFunctor();
		}
		else {
			return expression;
		}
	}

	private static boolean knowledgeApplies(Expression expression) {
		return expression != null && expression.getSyntaxTree() instanceof CompoundSyntaxTree;
	}

	@Override
	public Object getSyntacticFormType(Expression expression, RewritingProcess process) {
		if (knowledgeApplies(expression)) {
			return "Function application";
		}
		return null;
	}

	@Override
	public void rewritingProcessInitiated(RewritingProcess process) {
		ExpressionKnowledgeModule.register(this, process);
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		// Note: is a NoOpRewriter
		return expression; // will be removed eventually, not a real rewriter, just a module.
	}

	/**
	 * This is a temporary placeholder (to be used as we abandon the notion of representing functors in a root tree)
	 * to make it uniform to get all sub-syntax tree including
	 * the functor's one.
	 * Eventually, when there is no root tree anymore, it will be replaced by getImmediateSubTrees.
	 */
	public static Iterator<SyntaxTree> getImmediateSubTreesIncludingFunctorOne(SyntaxTree syntaxTree) {
		return syntaxTree.getImmediateSubTreesIncludingRootOneIterator();
	}

	/**
	 * This is a temporary placeholder (to be used as we abandon the notion of representing functors in a root tree)
	 * to make it uniform to get the number of sub-syntax tree including
	 * the functor's one.
	 * Eventually, when there is no root tree anymore, it will be replaced by numberOfImmediateSubTrees.
	 */
	public static int getNumberOfImmediateSubTreesIncludingFunctorOne(SyntaxTree syntaxTree) {
		return syntaxTree.numberOfImmediateSubTreesIncludingRootOneIterator();
	}

	public static Iterator<ExpressionAndContext> getImmediateSubExpressionsAndContextsIteratorFromFunctionApplication(
			Expression expression, RewritingProcess process) {
		return Expressions.getSubExpressionsAndContextsIteratorFromImmediateSubTreesIncludingRootOne(expression, process);
	}
}
