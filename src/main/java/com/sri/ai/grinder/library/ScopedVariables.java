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
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.AbstractModuleAndPossibleActiveRewriter;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.GetFunctorOrSymbol;
import com.sri.ai.expresso.helper.IsApplicationOf;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.Module;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.KindAttribute;
import com.sri.ai.util.Util;

/**
 * 
 * @author braz
 *
 */
@Beta
public class ScopedVariables extends AbstractModuleAndPossibleActiveRewriter {

	private static final Expression _emptyScope = Expressions.apply("list");
	//
	public ScopedVariables() {
		this.setReifiedTests(new HasKind(KindAttribute.VALUE_SCOPED_VARIABLES));
	}
	
	/**
	 * An interface for objects that know how to determine the set of variables scoped
	 * by certain types of expressions.
	 * For example, a rewriter that evaluates "for all" statements
	 * can be a provider, returning the set of variables being scoped by "for all" statements.
	 * <p>
	 * Providers must notify {@link ScopedVariables} of their existence so it can invoke them
	 * as necessary.
	 */
	public static interface Provider extends Module.Provider {
		/**
		 * Returns an expression representing a collection of variables being scoped by
		 * given expression, according to this provider's knowledge of that type of expression,
		 * or <code>null</code> if the provider does not know about it.
		 */
		Expression getScopedVariablesAsExpression(Expression expression, RewritingProcess process);
	}

	/**
	 * Registers a {@link Provider} in the {@link ScopedVariables} module of the given process,
	 * or throw an error if there is not one.
	 */
	public static void register(Provider provider, RewritingProcess process) throws Error {
		register(ScopedVariables.class, provider, process);
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		throw new Error("'scoped variables' discontinued as a syntactic function.");
	}

	public static boolean isScopedVariables(Expression expression) {
		return
		expression.getSyntaxTree().getLabel().equals("scoped variables") &&
		expression.getSyntaxTree().numberOfImmediateSubTrees() == 1; // does need to be sub-tree because 'scoped variables' is a syntactic form.
	}

	public Expression getScopedVariables(Expression expression, RewritingProcess process) {
		for (Module.Provider moduleProvider : providers.keySet()) {
			Provider provider = (Provider) moduleProvider;
			Expression scopedVariablesAccordingToThisProvider =
				provider.getScopedVariablesAsExpression(expression, process);
			if (scopedVariablesAccordingToThisProvider != null) {
				return scopedVariablesAccordingToThisProvider;
			}
		}
		return _emptyScope;
	}

	/**
	 * A static method finding a ScopedVariables rewriter in a process,
	 * and using it to return a list of scoped variables in a given expression.
	 */
	public static List<Expression> get(Expression expression, RewritingProcess process) {
		ScopedVariables scopedVariables = (ScopedVariables) process.findModule(ScopedVariables.class);
		if (scopedVariables == null) {
			throw new Error("ScopedVariables module not found");
		}
		Expression scopedVariablesExpression = scopedVariables.getScopedVariables(expression, process);
		List<Expression> result = scopedVariablesExpression.getArguments();
		return result;
	}
	
	/**
	 * Returns a list of symbols locally scoped by this expression.
	 * This is different from the scoped variables in that the arguments of scoped function applications are
	 * not locally scoped. For example, <code>for all p(X) : ...</code>
	 * has scoped variable <code>p(X)</code> but only <code>p</code> is a locally scoped symbol.
	 */
	public static List<Expression> getLocallyScopedSymbols(Expression expression, RewritingProcess process) {
		List<Expression> scopedVariables = get(expression, process);
		List<Expression> result = Util.mapIntoList(scopedVariables, new GetFunctorOrSymbol());
		return result;
	}
	
	public static boolean scopingVariablesAreDefined(Expression scopingExpression, RewritingProcess process) {
		List<Expression> variablesScopedByExpression = ScopedVariables.get(scopingExpression, process);
		boolean result = ! Util.thereExists(variablesScopedByExpression, new IsApplicationOf("value of"));
		return result;
	}
	
	/**
	 * Indicates whether a expression is <i>known</i> (from a syntactic point of view)
	 * to be independent from all indices of a scoping expression around it.
	 * Therefore it can detect that <code>j</code> is independent of <code>i</code> in <code>{ (on i) j }</code>,
	 * for example, and that <code>g(X)</code> is independent of <code>f(X)</code> in <code>{ (on f(X)) g(X) }</code>,
	 * but will not detect that
	 * <code>f(a)</code> is independent of <code>f(X)</code> in <code>{ (on f(X)) f(a) | X != a }</code>.
	 * <p>
	 * Note that <b>not known</b> to be independent is not the same as dependent!
	 * Therefore, a return value of <code>false</code> does not imply dependence.
	 */
	public static boolean isKnownToBeIndependentOfScopeIn(Expression expression, Expression scopingExpression, RewritingProcess process) {
		List<Expression> indices = ScopedVariables.get(scopingExpression, process);
		boolean result = isKnownToBeIndependentOfIndices(expression, indices, process);
		return result;
	}

	public static boolean isKnownToBeIndependentOfIndices(Expression expression, Collection<Expression> indices, RewritingProcess process) {
		for (Expression index : indices) {
			boolean isKnownToBeIndependentOfIndex = isKnownToBeIndependentOfIndex(expression, index, process);
			if ( ! isKnownToBeIndependentOfIndex) {
				return false;
			}
		}
		return true;
	}

	private static boolean isKnownToBeIndependentOfIndex(Expression expression, Expression index, RewritingProcess process) {
		if (index.hasFunctor("value of")) {
			return false;
		}
		Iterator<Expression> subExpressionsIterator = new SubExpressionsDepthFirstIterator(expression);
		while (subExpressionsIterator.hasNext()) {
			Expression subExpression = subExpressionsIterator.next();
			boolean subExpressionIsKnownToBeIndependentOfIndex =
				topExpressionIsKnownToBeIndependentOfIndex(subExpression, index, process);
			if ( ! subExpressionIsKnownToBeIndependentOfIndex) {
				return false;
			}
		}
		return true;
	}

	private static boolean topExpressionIsKnownToBeIndependentOfIndex(Expression expression, Expression index, RewritingProcess process) {
		// As the index changes its value, the only way an expression can always coincide with it
		// is by being the index itself; all others will remain with the same value as the index changes,
		// and will therefore be independent from that change.
		Expression expressionFunctorOrSymbol = expression.getFunctorOrSymbol();
		Expression indexFunctorOrSymbol = index.getFunctorOrSymbol();
		if ( ! expressionFunctorOrSymbol.equals(indexFunctorOrSymbol)) {
			return true; 
		}
		
		if (expression.numberOfArguments() != index.numberOfArguments()) {
			// if they have the same root and one of them is a function application and the other a symbol, then they are dependent.
			// If the index is the symbol, it means the entire function is changing, including every application of it (the expression), and there is a dependence.
			// If the index is a function application, it is changing and as a result the entire function (the expression) is changing as well, and there is a dependence.
//			if (
//					(expression instanceof Symbol && index instanceof CompoundSyntaxTree)
//					||
//					(expression instanceof CompoundSyntaxTree && index instanceof Symbol)
//			) {
			return false; // this case actually means that they are dependent for sure, but the method's interface does not allow us to state this, only to state that they are not known to be independent.
		}
		
		// At this point we know the roots are the same and will unify,
		// so this is really about unifying the arguments.
		// Two function applications with the same functors will be independent if it is impossible for them to unify.
		// If it is possible for them to unify (with condition below being TRUE or something not false, and thus possibly satisfiable)
		// then it is possible that they are dependent.
		Expression condition = Unification.unificationCondition(expression, index, process);
		boolean result = condition.equals(Expressions.FALSE);
		return result;
	}
}
