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

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.sri.ai.expresso.api.CompoundSyntaxTree;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.api.SyntaxTree;
import com.sri.ai.expresso.core.AbstractReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IsInstanceOf;
import com.sri.ai.util.base.NotContainedBy;

/**
 * 
 * @author braz
 *
 */
@Beta
public class StandardizedApartFrom {

	/**
	 * Standardizes apart the scoped variables of expression1 so that they do not collide with any variables in expression2.
	 */
	public static Expression standardizedApartFrom(Expression expression1, Expression expression2, RewritingProcess process) {		
		Collection<Expression> variablesThatCannotBeScopingInExpression = new LinkedHashSet<Expression>();
		variablesThatCannotBeScopingInExpression.addAll(process.getContextualVariables());
		variablesThatCannotBeScopingInExpression.addAll(Expressions.getVariables(expression2, process));
		
		ReplacementFunctionWithContextuallyUpdatedProcess standardizer = new Standardizer(variablesThatCannotBeScopingInExpression);
		
		Expression result = standardizer.apply(expression1, process);
		
		return result;
	}
	
	/**
	 * A function returning a version of an input expression that has its top scoped variables
	 * replaced so as not to collide with any variables in a given collection of forbidden variables,
	 * adding any newly created variables to it.
	 */
	private static class Standardizer extends AbstractReplacementFunctionWithContextuallyUpdatedProcess {
		private Collection<Expression> variablesThatCannotBeScopingInExpression;

		public Standardizer(Collection<Expression> variablesThatCannotBeScopingInExpression) {
			super();
			this.variablesThatCannotBeScopingInExpression = variablesThatCannotBeScopingInExpression;
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			// Replace sub-expressions first because if top expressions is replaced, then sub-expressions are not checked by Expression.replace
			// (it would make sense to add an option that does that to Expression.replace).
			Expression result = expression.replace(this, false /* not just the first one */, null, true /* ignore top expression - this will be done in the next line! */, null, process);
			result = standardizeTopExpressionScopedVariablesApartFrom(result, variablesThatCannotBeScopingInExpression, process);
			return result;
		}
	}

	/**
	 * Returns expression or a version of it with the <i>scoped</i> variables of the <i>top</i> expression
	 * renamed so they don't collide with the ones in variablesThatCannotBeScopingInExpression.
	 */
	private static Expression standardizeTopExpressionScopedVariablesApartFrom(
			Expression expression, Collection<Expression> variablesThatCannotBeScopingInExpression, RewritingProcess process) {
		List<Expression> scopedVariables1 = ScopedVariables.get(expression, process);
		List<SyntaxTree> scopedVariables1SyntaxTrees = Util.mapIntoArrayList(scopedVariables1, Expressions.GET_SYNTAX_TREE);
		if (Util.thereExists(scopedVariables1SyntaxTrees, new IsInstanceOf<SyntaxTree>(CompoundSyntaxTree.class))) {
			throw new StandardizingApartOnScopingFunctionApplicationsNotSupported(expression);
		}
		Collection<Expression> variablesToBeRenamed  = Util.intersection(scopedVariables1, variablesThatCannotBeScopingInExpression);
		Collection<Expression> variablesInExpression = Expressions.getVariables(expression, process);
		Collection<Expression> forbiddenVariables    = new LinkedHashSet<Expression>();
		forbiddenVariables.addAll(variablesInExpression);
		forbiddenVariables.addAll(variablesThatCannotBeScopingInExpression);
		Predicate<Expression>  isNovel               = new NotContainedBy<Expression>(forbiddenVariables);
		Expression result = expression;
		for (Expression variableToBeRenamed : variablesToBeRenamed) {
			Expression replacement = Expressions.primedUntilUnique(variableToBeRenamed, isNovel);
			result = Expressions.make(result.getSyntaxTree().replaceSubTreesAllOccurrences(variableToBeRenamed.getSyntaxTree(), replacement.getSyntaxTree()));
			// needs to be syntax tree because the symbol needs to be replaced even where it is not a part of a sub-expression, as in index expressions.
			// note that scoping does not matter. The semantics is not changed from "for X : for X : f(X)" to "for X' : for X' : f(X)".
			forbiddenVariables.add(replacement); // note that this affects the isNovel predicate!
		}
		return result;
	}
	
	public static class StandardizingApartOnScopingFunctionApplicationsNotSupported extends Error {
		private static final long serialVersionUID = 1L;
		//
		public Expression expression;

		public StandardizingApartOnScopingFunctionApplicationsNotSupported(Expression expression) {
			super("Standardizing apart only defined for simple scoping variables, not scoping function applications as in " + expression);
			this.expression = expression;
		}
	}
}
