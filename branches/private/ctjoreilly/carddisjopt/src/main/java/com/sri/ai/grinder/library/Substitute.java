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

import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.ReplacementFunctionWithContextuallyUpdatedProcess;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.PruningPredicate;
import com.sri.ai.grinder.core.ReplacementFunctionMaker;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;

/**
 * A class providing a static method for substituting symbols or function applications in an expression
 * by another expression (not subject itself to the same substitution).
 * 
 * Replacing a symbol works as one would expect:
 * 
 * Replacing <code>X</code> by 99 in <code>foo</code> returns <code>foo</code>
 * Replacing <code>X</code> by 99 in <code>X</code> returns <code>99</code>
 * 
 * However, quantifications need to be taken into account:
 * replacing <code>X</code> by 99 in <code>X + if there exists X : X = 9 then X else 0</code> returns <code>99 + if there exists X : X = 9 then 99 else 0</code>
 * because the quantified X works as a distinct variable. Its scope is limited to the if's condition,
 * so the X in the then branch corresponds to the original X.
 * 
 * When replacing a function substitution, the function's corresponding "cell" has its value replaced.
 * Here are some examples:
 * 
 * Replacing <code>f(10)</code> by 99 in <code>f(9)</code> returns <code>f(9)</code>
 * Replacing <code>f(10)</code> by 99 in <code>f(X)</code> returns <code>if X = 10 then 99 else f(X)</code>
 * Replacing <code>f(Y)</code> by 99 in <code>f(X)</code> returns <code>if X = Y then 99 else f(X)</code>
 * Replacing <code>f(Y)</code> by 99 in <code>f(Y)</code> returns <code>99</code>
 * 
 * Quantification for function applications extends the symbol case, but may seem more confusing:
 * 
 * Replacing <code>age(bob)</code> by 99 in <code>age(bob) + if there exists age(Y) : age(Y) = age(bob) then 1 else 0</code>
 * returns <code>99 + if Y = bob then (if there exists age(Y) : age(Y) = age(bob) then 1 else 0) else (if there exists age(Y) : age(Y) = 99 then 1 else 0)</code>
 * 
 * The above is justified in the following way: we know that the age of bob is 99, so we replace the first <code>age(bob)</code> by 99.
 * The there exists quantification is asking the question of whether there is a possible age for a person Y (which may or may not be bob) satisfying a condition.
 * If Y = bob, then the quantification is essentially defining a "local", distinct age(bob) (which is also age(Y)), so we do <i>not</i> substitute age(bob) by anything in that context
 * (because age(bob) inside that context is a distinct, new, local age(bob), much in the same way we have locally quantified symbols).
 * If Y != bob, however, "local" age(Y) is not the same as age(bob) and we do proceed with the substitution of age(bob) under that context.
 * 
 * IMPLEMENTATION NOTE: this implementation assumes that symbols or functions modified by quantification are not used as an argument to another quantified function application in the same scoping level, such as in
 * (on X, f(X)), or (on f(a), g(f(X))).
 * This is ultimately due to the fact that the current implementation does not consider the subsequent quantifications to be sub-expressions of the first, even though it should.
 * When the framework is modified to do that, this code should work correctly without any changes.
 * The current code works correctly if one re-structures such expressions as multiple separate quantifications, as in (on X) {{ (on f(X)) ..., or (on f(a) {{ (on g(f(X)) ... and so on.
 * 
 * Here are a few more examples:
 * <pre>
 *      Replacing    Replacement                                    Expression                                                              Result
 *      p(a)                   2                              {(on q(a)) p(a)}                                                       {(on q(a)) 2}
 *      p(a)                   2                              {(on p(a)) p(a)}                                                    {(on p(a)) p(a)}
 *      p(a)                   2                              {(on p(X)) p(a)}                              {(on p(X)) if X != a then 2 else p(a)}
 *      p(X)                   2                              {(on p(a)) p(X)}                              {(on p(a)) if X != a then 2 else p(X)}
 *      p(Y)                   2                                          p(X)                                           if X = Y then 2 else p(X)
 *      p(X,Y)                 2                          {(on p(Y,X)) p(X,Y)}                          {(on p(Y,X)) if X != Y then 2 else p(X,Y)} 
 *      p(X,Y)                 2                          {(on p(W,Z)) p(Y,X)}    {(on p(W,Z)) if (W != X or Z != Y) and X = Y then 2 else p(Y,X)}
 *      p(X,Y)                 2                          {(on p(Y,X)) p(Y,X)}                                                {(on p(Y,X)) p(Y,X)}
 *      p(X,Y)                 2  if W != X and Z != Y then p(W,Z) else p(a,Y)   if W != X and Z != Y then p(W,Z) else if X = a then 2 else p(a,Y)
 * </pre>
 * @author braz
 *
 */
@Beta
public class Substitute {

	public static Expression replaceAll(Expression expression,
			Map<? extends Expression, ? extends Expression> replacements,
			RewritingProcess process) {
		for (Map.Entry<? extends Expression, ? extends Expression> entry : replacements.entrySet()) {
			expression = substitute(expression, entry.getKey(), Expressions.TRUE, entry.getValue(), process);
		}
		return expression;
	}
	
	public static Expression replace(Expression expression,
			Expression replaced, Expression replacement,
			RewritingProcess process) {
		Expression result =  substitute(expression, replaced, Expressions.TRUE, replacement, process);
		
		return result;
	}

	private static Expression substitute(Expression expression, Expression replaced, Expression constraintOnReplaced, Expression replacement, RewritingProcess process) {	
		Expression result =
				expression.replaceAllOccurrences(
						new SubstituteReplacementFunction(replaced, constraintOnReplaced, replacement), new SubstituteReplacementFunctionMaker(),
						new SubstitutePruningPredicate(), null,
						process);
		return result;
	}
	
	private static class SubstituteReplacementFunction implements ReplacementFunctionWithContextuallyUpdatedProcess {

		public Expression replaced;
		public Expression constraintOnReplaced;
		public Expression replacement;
		
		public SubstituteReplacementFunction(Expression replaced, Expression constraintOnReplaced, Expression replacement) {
			this.replaced = replaced;
			this.constraintOnReplaced = constraintOnReplaced;
			this.replacement = replacement;
		}
		
		@Override
		public Expression apply(Expression arg0) {
			throw new Error(SubstituteReplacementFunction.class + ".apply(Expression) must not be invoked");
		}

		@Override
		public Expression apply(Expression expression, RewritingProcess process) {
			Expression result = expression;
			if (expression.getFunctorOrSymbol().equals(replaced.getFunctorOrSymbol())) {
				Expression argumentsAreTheSame = Equality.makePairwiseEquality(expression.getArguments(), replaced.getArguments());
				Expression argumentsAreTheSameAndReplacedIsConstrained = And.make(constraintOnReplaced, argumentsAreTheSame);
				Expression conditionForExpressionToMatchReplaced = process.rewrite(CardinalityRewriter.R_complete_simplify, argumentsAreTheSameAndReplacedIsConstrained);
				RewritingProcess newProcess = GrinderUtil.extendContextualConstraint(conditionForExpressionToMatchReplaced, process);
				Expression replacementIfConditionHolds = replacement;
				if (!replacement.equals(replaced)) {
					replacementIfConditionHolds = substitute(replacement, replaced, constraintOnReplaced, replacement, newProcess);
				}
				result = IfThenElse.make(conditionForExpressionToMatchReplaced, replacementIfConditionHolds, expression);
			}
			return result;
		}
	}

	private static class SubstitutePruningPredicate implements PruningPredicate {
		@Override
		public boolean apply(Expression expression, Function<Expression, Expression> replacementFunctionFunction, RewritingProcess process) {
			SubstituteReplacementFunction replacementFunction = (SubstituteReplacementFunction) replacementFunctionFunction;
			boolean result = replacementFunction.constraintOnReplaced.equals(Expressions.FALSE);
			return result;
		}
	}
	
	private static class SubstituteReplacementFunctionMaker implements ReplacementFunctionMaker {
		@Override
		public Function<Expression, Expression> apply(Expression expression, Function<Expression, Expression> replacementFunctionFunction, ExpressionAndContext expressionAndContext, RewritingProcess process) {
			SubstituteReplacementFunction replacementFunction = (SubstituteReplacementFunction) replacementFunctionFunction;
			Expression constraintOnReplaced = replacementFunction.constraintOnReplaced;
			for (Expression quantifiedVariable : expressionAndContext.getQuantifiedVariables()) {
				if (quantifiedVariable.getFunctorOrSymbol().equals(replacementFunction.replaced.getFunctorOrSymbol())) {
					Expression argumentsAreDistinct = Not.make(Equality.makePairwiseEquality(quantifiedVariable.getArguments(), replacementFunction.replaced.getArguments()));
					Expression argumentsAreDistinctAndReplacedIsConstrained = And.make(constraintOnReplaced, argumentsAreDistinct);
					constraintOnReplaced = process.rewrite(CardinalityRewriter.R_complete_simplify, argumentsAreDistinctAndReplacedIsConstrained);
				}
			}
			SubstituteReplacementFunction result = new SubstituteReplacementFunction(replacementFunction.replaced, constraintOnReplaced, replacementFunction.replacement);
			return result;
		}
	}
}
