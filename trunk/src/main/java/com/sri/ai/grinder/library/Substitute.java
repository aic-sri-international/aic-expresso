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

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractExpression;
import com.sri.ai.grinder.core.PruningPredicate;
import com.sri.ai.grinder.core.PruningPredicateMaker;
import com.sri.ai.grinder.core.ReplacementFunctionMaker;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.IdentityWrapper;
import com.sri.ai.util.collect.FunctionIterator;

/**
 * A class providing a set of static methods for substitution of sub-expressions
 * matching a given 'replaced' expression by a 'replacement'. This substitution
 * is semantic rather than syntactic, aiming at keeping an expression's
 * interpretation unchanged when the interpretation of 'replaced' is the same as
 * the interpretation of 'replacement'.
 * 
 * @author braz
 */
@Beta
public class Substitute {

	private static final List<Integer> _pathMinusOne = Collections
			.unmodifiableList(Arrays.asList(new Integer(-1)));

	public static Expression replaceAll(Expression expression,
			Map<? extends Expression, ? extends Expression> replacements,
			RewritingProcess process) {
		for (Map.Entry<? extends Expression, ? extends Expression> entry : replacements
				.entrySet()) {
			expression = replace(expression, entry.getKey(), entry.getValue(),
					true, process);
		}
		return expression;
	}

	/**
	 * Replaces all occurrences of an expression by another, taking into account
	 * that expressions can unify; for example, replacing <code>p(a)</code> by
	 * <code>true</code> will make <code>p(X)</code> be replaced by
	 * <code>if X = a then true else p(X)</code>.
	 * 
	 * @param needTotalReplacement
	 *            a flag indicating that replacements should be done only if the
	 *            resulting expression is not dependent on the interpretation of
	 *            expression anymore, or a {@link #NoSubstitution} exception be
	 *            thrown.
	 */
	public static Expression replace(Expression expression,
			Expression replaced, Expression replacement,
			boolean needTotalReplacement, RewritingProcess process) {
		return replace(expression, replaced, replacement, null,
				needTotalReplacement, process);
	}

	private static Expression replace(Expression expression,
			Expression replaced, Expression replacement,
			PruningPredicate prunePredicate,
			boolean needTotalReplacement, RewritingProcess process) {
		Expression result = expression.replace(new Substitution(replaced,
				replacement, needTotalReplacement, process),
				new MakeReplacementFunctionForSpecificSubExpression(),
				prunePredicate,
				new MakePruningPredicateForSpecificSubExpression(replaced),
				false, // all occurrences
				false, // do not ignore top expression
				null, // no listener
				process);
		return result;
	}

	/**
	 * NoSubstitution indicates that the function in a replaced function
	 * application has been found in a form other than an applications of the
	 * same arity. For example, we are replacing f(x) and find (f + g)(y). In
	 * this case, we abstain from substitution altogether because NOT
	 * substituting that f but substituting other applications of f would be
	 * incorrect. So we just return the original expression in this case.
	 * Another interesting case is replacing p or q and finding, say, p or r or
	 * q; again, just not replacing it may lead to inconsistencies. In the
	 * future, once we have stronger manipulation of lambda, perhaps we can do
	 * better than that by dealing with replacement of function applications
	 * being translated into the replacement of the function by a lambda
	 * expression: that is, replacing f(x) with 'replacement' is the same as
	 * replacing f with lambda z : if z = x then replacement else f(z)
	 */
	public static class NoSubstitution extends Error {
		private static final long serialVersionUID = 1L;
		
		public NoSubstitution(String message) {
			super(message);
		}
	}

	private static class Substitution implements Function<Expression, Expression> {
		private static Lock failedUnificationMapLock = new ReentrantLock();
		
		private Expression replaced;
		private Expression replacedFunctorOrSymbol;
		private Expression replacement;
		private boolean needTotalReplacement;
		private List<Expression> scopedVariables;
		private RewritingProcess process;
		private Map<IdentityWrapper, Set<Expression>> failedPreviousUnificationsMap = null;

		@SuppressWarnings("unchecked")
		public Substitution(Expression replaced, Expression replacement, List<Expression> scopedVariables,
				boolean needTotalReplacement, RewritingProcess process) {
			super();
			this.replaced = replaced;
			this.replacedFunctorOrSymbol = replaced.getFunctorOrSymbol();
			this.replacement = replacement;
			this.scopedVariables = scopedVariables;
			this.needTotalReplacement = needTotalReplacement;
			this.process = process;
			failedPreviousUnificationsMap = (Map<IdentityWrapper, Set<Expression>>) process.getGlobalObject("Substitute unification failed");
			if (failedPreviousUnificationsMap == null) {
				failedUnificationMapLock.lock();
				try {
					// Note: Ensure still null when acquire the lock
					if (failedPreviousUnificationsMap == null) {
						failedPreviousUnificationsMap = new ConcurrentHashMap<IdentityWrapper, Set<Expression>>();
						process.putGlobalObject("Substitute unification failed", failedPreviousUnificationsMap);
					}
				} finally {
					failedUnificationMapLock.unlock();
				}
			}
		}
		
		public Substitution(Expression replaced, Expression replacement, boolean needTotalReplacement, RewritingProcess process) {
			this(replaced, replacement, new LinkedList<Expression>(), needTotalReplacement, process);
		}
		
		public Substitution copyButForNewScopedVariables(List<Expression> newScopedVariables) {
			Substitution result = new Substitution(replaced, replacement, newScopedVariables, needTotalReplacement, process);
			return result;
		}
		
		@Override
		public Expression apply(Expression expression) {

			Expression expressionFunctorOrSymbol = expression
					.getFunctorOrSymbol();
			
			Set<Expression> failedPreviousUnifications = failedPreviousUnificationsMap.get(new IdentityWrapper(expression));
			
			if (failedPreviousUnifications == null || ! failedPreviousUnifications.contains(replaced)) {
				// See explanation of this condition below, when the result is annotated with the same kind of thing.
				if (Util.equals(expressionFunctorOrSymbol, replacedFunctorOrSymbol)) {
					// We perform two types of substitution: of a functor (for
					// example, replacing f by g in f(x)), and of entire
					// function applications
					// (for example, f(x) by g(x) in h(f(x))).
					// If we don't have one of those two cases, we don't know
					// how to safely make a substitution.
					// For example, it is not clear how to replace f(x) in
					// h(f(x,y)) or in h(f).
					// In that situation, we simply ignore that occurrence.
					// However, if the flag needTotalReplacement is true, we can
					// not ignore any occurrences.
					// For example, we may have a 'replaced' equal to f(x) and
					// an expression f(x) + (f + g)(x).
					// Ignoring f (in f + g) when needTotalReplacement is true
					// would be incorrect because replacing the value of f(x)
					// does affect f as a whole.
					// This would lead to a situation in which an expression has
					// been "half-replaced".
					// We therefore check whether the flag needTotalReplacement
					// is true, and raise an exception NoSubstitution (an error,
					// in fact, so that method signatures don't need to be
					// changed everywhere)
					// that can be caught by whatever code is using this
					// UnaryFunction.
					if (replaced.numberOfArguments() > 0
							&& replaced.numberOfArguments() != expression
									.numberOfArguments()) {
						if (needTotalReplacement) {
							throw new NoSubstitution(
									"Cannot substitute "
											+ replaced
											+ " in an expression containing "
											+ expression
											+ " because current methods do not deal with a function application being replaced in an expression containing that same function applied to a distinct number of arguments, or by itself.");
						} 
						else {
							return expression; // we cannot do the replacement,
												// but we can move on and leave
												// things partially replaced.
						}
					}
	
					Expression conditionForNotBeingShadowed = getConditionForNotBeingShadowedByScopedVariables(expression);			
					if (conditionForNotBeingShadowed.equals(Expressions.FALSE)) {
						// it is unconditionally shadowed by scoped variables,
						// so no replacement
						return expression;
					}
	
					Expression conditionForCoincidingWithReplaced =
							Equality.conditionForSubExpressionsEquality(expression, replaced);
					if (conditionForCoincidingWithReplaced.equals(Expressions.FALSE)) {
						// expression and replaced are unconditionally not
						// coinciding, so no replacement
						return expression;
					}
	
					Expression conditionForNotBeingShadowedAndCoincideWithReplaced =
							And.make(conditionForNotBeingShadowed, conditionForCoincidingWithReplaced);
					if (conditionForNotBeingShadowedAndCoincideWithReplaced.equals(Expressions.TRUE)) {
						// It's unconditionally replaced.
						return replacement;
					}
	
					// At this point, the result will be an expression depending
					// on a non-trivial unification.
					// Because substitution is justified (from a rewriting
					// system point of view) by the fact that dependencies
					// on 'replaced' are eliminated,
					// the non-trivial unification condition prevents the
					// simplification from holding in the cases in which
					// 'replaced'
					// is an equality application,
					// since it might *introduce* equalities, possibly ones
					// depending on 'replaced'.
					// For this reason, we test for 'replaced' being an equality
					// application and, if so, return the expression unchanged.
					if (replaced.hasFunctor(FunctorConstants.EQUAL)
							|| replaced.hasFunctor(FunctorConstants.INEQUALITY)) {
						return expression;
					}
	
					// Similarly, if the expression being replaced is an if then
					// else, it may unify again with the if then else that is
					// introduced
					// because of the non-trivial unification.
					// So we abstain from replacing in these cases.
					// Obs.: these types of heuristics are not guarantees of no
					// infinite substitutions -- the problem of deciding when
					// that happens is probably very
					// hard and we have not solved it.
					// Instead, users will have to know what substitution does,
					// and prove by themselves that, in the context of their
					// applications and with
					// the type of expressions they use, there will be no
					// infinite substitutions.
					if (IfThenElse.isIfThenElse(replaced)) {
						return expression;
					}
	
					// At this point, the result will be a conditional
					// expression with 'expression' in the else branch.
					// This will lead to a situation in which it might be
					// rewritten again in the same way if we attempt further
					// substitutions.
					// For example, we may want to replace true conditions by
					// 'true' and have
					// if p(X) then f(p(Y)) else q
					// which gets rewritten to
					// if p(X) then if X = Y then f(true) else f(p(Y))
					// If the same operation is attempted again, we get
					// if p(X) then if X = Y then f(true) else f(if X = Y then
					// f(true) else f(p(Y)))
					// to infinity...
					// To avoid that, we annotate the expression in the else branch (here, f(p(Y))) with the fact that it's already been checked for substitution.
					// However, annotating the 'expression' instance is not correct because it may be present somewhere else
					// where it still needs to be replaced.
					// Therefore we make a clone of it to be used and annotated instead.
					// This assumes that wherever that instance is moved to later, it will be in a context in which this substitution does
					// not need to be done.
					// This naturally depends on what other rewriters do, but it seems reasonable to assume that it will be true.
					Expression newExpressionInstance = null;
					try {
						newExpressionInstance = (Expression) expression.clone();
					} catch (CloneNotSupportedException e) {
						e.printStackTrace();
					}
					Set<Expression> newFailedPreviousUnifications = failedPreviousUnifications == null? new HashSet<Expression>() :  new HashSet<Expression>(failedPreviousUnifications);
					newFailedPreviousUnifications.add(replaced);
					failedPreviousUnificationsMap.put(new IdentityWrapper(newExpressionInstance), newFailedPreviousUnifications);

					Expression result =
						IfThenElse.make(conditionForNotBeingShadowedAndCoincideWithReplaced,
								replacement,
								newExpressionInstance);
	
					return result;
				}
			}

			return expression;
		}

		private Expression getConditionForNotBeingShadowedByScopedVariables(Expression expression) {
			Iterator<Expression> conditionsForNotBeingShadowedByEachScopedVariableIterator = new FunctionIterator<Expression, Expression>(
					scopedVariables,
					new GetConditionForNotBeingShadowedByScopedVariable(
							expression));
			Expression result = And
					.make(conditionsForNotBeingShadowedByEachScopedVariableIterator);
			return result;
		}

		public class GetConditionForNotBeingShadowedByScopedVariable implements
				Function<Expression, Expression> {

			private Expression expression;

			public GetConditionForNotBeingShadowedByScopedVariable(
					Expression expression) {
				super();
				this.expression = expression;
			}

			@Override
			public Expression apply(Expression scopedVariable) {
				Expression result = getConditionForNotBeingShadowedByScopedVariable(
						expression, scopedVariable);
				return result;
			}
		}

		public Expression getConditionForNotBeingShadowedByScopedVariable(
				Expression expression, Expression scopedVariable) throws Error {
			if (!expression.getFunctorOrSymbol().equals(
					scopedVariable.getFunctorOrSymbol())) {
				return Expressions.TRUE;
			}
			Expression condition = Disequality
					.conditionForSubExpressionsDisequality(expression,
							scopedVariable);
			return condition;
		}
	}
	
	private static class MakeReplacementFunctionForSpecificSubExpression implements ReplacementFunctionMaker {
		@Override
		public Function<Expression, Expression> apply(Expression expression, Function<Expression, Expression> substitutionFunctionObject, ExpressionAndContext subExpressionAndContext, RewritingProcess process) {
			Substitution substitutionFunction = (Substitution) substitutionFunctionObject;
			Collection<Expression> subExpressionScopedVariables = subExpressionAndContext.getQuantifiedVariables();
			List<Expression> newScopedVariables = new LinkedList<Expression>(substitutionFunction.scopedVariables);
			newScopedVariables.addAll(subExpressionScopedVariables);
			Substitution result = substitutionFunction.copyButForNewScopedVariables(newScopedVariables);
			return result;
		}
	}

	private static class MakePruningPredicateForSpecificSubExpression implements PruningPredicateMaker {
		private Expression replaced;

		public MakePruningPredicateForSpecificSubExpression(Expression replaced) {
			super();
			this.replaced = replaced;
		}

		@Override
		public PruningPredicate apply(Expression expression, PruningPredicate prunePredicate,
				ExpressionAndContext subExpressionAndContext) {
			if (replaced.getSyntacticFormType().equals("Function application")
					&& expression.getSyntacticFormType().equals("Function application")
					&& replaced.hasFunctor(expression.getFunctor())
					&& replaced.numberOfArguments() == expression.numberOfArguments()
					&& subExpressionAndContext.getPath().equals(_pathMinusOne)) { // sub-expression
																					// is
																					// the
																					// functor
																					// of
																					// expression
				return AbstractExpression.TRUE_PRUNING_PREDICATE;
				// We should not recurse into the functor of the function
				// application, because the
				// substitution will already be taken care of at the level of
				// the function application itself.
				// If we let it recurse, we would get a {@link #NoSubstitution}
				// error, which would cancel the entire current substitution.
				// See the description of that situation at the point in which
				// it is thrown.
			}
			return prunePredicate; // otherwise, proceed with the regular prune
									// predicate
		}
	}
}
