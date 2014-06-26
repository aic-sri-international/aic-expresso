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
package com.sri.ai.grinder.library.function;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.AbstractModuleNoOpRewriter;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Module;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.PruningPredicate;
import com.sri.ai.grinder.library.SyntacticSubstitute;
import com.sri.ai.grinder.library.indexexpression.IndexExpressions;
import com.sri.ai.grinder.library.lambda.Lambda;
import com.sri.ai.grinder.library.lambda.LambdaApplication;
import com.sri.ai.util.Util;


/**
 * This module concentrates the functionality for registering and using pieces
 * of knowledge that can tell whether an expression's interpretation is an
 * injective function of the interpretation of its sub-expressions.
 * 
 * @author rodrigo
 */
@Beta
public class InjectiveModule extends AbstractModuleNoOpRewriter {

	/**
	 * An interface for objects that know how to determine
	 * whether the function from interpretation of sub-expressions to interpretation of expression
	 * is injective.
	 * Providers must notify {@link InjectiveModule} of their existence
	 * with the method {@link InjectiveModule#register(Provider)} so it can invoke them.
	 * as necessary.
	 * {@link InjectiveModule#register(Provider, RewritingProcess)} is provided as a convenience for finding the module in the rewriting process.
	 */
	public static interface Provider extends Module.Provider {
		/**
		 * Returns a representation of the injective function, if any (<code>null</code> otherwise)
		 * between the subexpression interpretations and the expression representation.
		 * This object should be used only for direct comparison with other objects; we just need to know if such function in
		 * one expression is the same as in another.
		 * Note that, in the case of function applications, the functor is a subexpression itself, therefore
		 * this function we are talking about here is <i>not</i> the functor, but a function that can be described as
		 * "the function mapping a functor and n arguments to the value of that application".
		 * This is a constant function for all injective function applications with n arguments.
		 */
		Object getInjectiveFunctionToken(Expression expression, RewritingProcess process);
	}

	/**
	 * Registers a {@link Provider} in the {@link InjectiveModule} module of the given process,
	 * or throw an error if there is not one.
	 */
	public static void register(Provider provider, RewritingProcess process) throws Error {
		register(InjectiveModule.class, provider, process);
	}

	public Object getInjectiveFunctionToken(Expression expression, RewritingProcess process) {
		for (Module.Provider moduleProvider : providers.keySet()) {
			Provider provider = (Provider) moduleProvider;
			Object result = provider.getInjectiveFunctionToken(expression, process);
			if (result != null) {
				return result;
			}
		}
		return null;
	}

	public boolean injectiveFunctionTokensAreEqual(Expression expression1, Expression expression2, RewritingProcess process) {
		Object messageValueSetDestinationInjectiveFunctionToken      = getInjectiveFunctionToken(expression1, process);
		Object previousMessageValueDestinationInjectiveFunctionToken = getInjectiveFunctionToken(expression2, process);
		if ( ! messageValueSetDestinationInjectiveFunctionToken.equals(previousMessageValueDestinationInjectiveFunctionToken)) {
			return false;
		}
		return true;
	}
	
	/**
	 * Given an expression E, returns its injective application, which is an expression equivalent to itself defined as:
	 * <pre>
	 * - (lambda X1 ... Xn : E[Y1...Yn/X1...Xn])(Y1, ... Yn), if E is an expression composed of possibly nested injective expressions
	 * where Y1...Yn are the constants and free variables of E (once for each occurrence).
	 * - null otherwise.
	 * </pre>
	 * This is useful because for any two injective applications E1 and E2 with the same non-null injective application functor
	 * E1 = E2 <=> Y1 = Z1 and ... and Yn = Zn,
	 * for Y1...Yn and Z1...Zn the constants and free variables of E1 and E2 respectively.
	 * <p>
	 * Note: ideally, the index expression of the lambda expression should contain the type,
	 * but at this point it is not doing that.
	 */
	public static Expression getInjectiveApplication(Expression expression, boolean expressionsAlreadyKnownToBeInjective, RewritingProcess process) {
		
		Expression result;
		
		Map<Expression, Expression> fromValuesToParameters = new LinkedHashMap<Expression, Expression>();
		UpdatableInteger counter = new UpdatableInteger(0);

		PruningPredicate pruneAtNonInjectiveExpression = expressionsAlreadyKnownToBeInjective? Expressions.TRUE_PRUNING_PREDICATE : new PruneAtNonInjectiveExpression(process);
		ReplaceSymbolByParameter replaceSymbolByParameter = new ReplaceSymbolByParameter(fromValuesToParameters, counter, process);
		Expression parameterizedVersion = expression.replaceAllOccurrences(replaceSymbolByParameter,pruneAtNonInjectiveExpression,process);
		
		if (expressionsAlreadyKnownToBeInjective || ! ((PruneAtNonInjectiveExpression) pruneAtNonInjectiveExpression).pruned) {
			ArrayList<Expression> indexExpressions = new ArrayList<Expression>(fromValuesToParameters.values());
			Expression functor = Lambda.make(indexExpressions, parameterizedVersion);
			List<Expression> arguments = new ArrayList<Expression>(fromValuesToParameters.keySet());
			result = Expressions.apply(functor, arguments);
		}
		else {
			result = null;
		}
		
		return result;
	}
	
	private static class UpdatableInteger {
		int value;

		public UpdatableInteger(int value) {
			super();
			this.value = value;
		}
	};

	private static class ReplaceSymbolByParameter implements Function<Expression, Expression> {
		public Map<Expression, Expression> fromValuesToParameters;
		private UpdatableInteger counter;
		private RewritingProcess process;
		
		public ReplaceSymbolByParameter(Map<Expression, Expression> fromValuesToParameters, UpdatableInteger counter, RewritingProcess process) {
			super();
			this.fromValuesToParameters = fromValuesToParameters;
			this.counter = counter;
			this.process = process;
		}

		public Expression apply(Expression expression) {
			if (process.isVariable(expression) || process.isConstant(expression)) {
				Expression parameter = Expressions.makeSymbol("X" + counter.value++);
				fromValuesToParameters.put(expression, parameter);
				expression = parameter;
			}
			return expression;
		}
	}
	
	private static class PruneAtNonInjectiveExpression implements PruningPredicate {
		public boolean pruned = false;
		private InjectiveModule injectiveModule;
		
		public PruneAtNonInjectiveExpression(RewritingProcess process) {
			super();
			this.injectiveModule = (InjectiveModule) process.findModule(InjectiveModule.class);
		}

		@Override
		public boolean apply(Expression expression, Function<Expression, Expression> replacementFunction, RewritingProcess process) {
			if (process.isVariable(expression) || process.isConstant(expression)) {
				pruned = false; // symbols are considered injective expressions, so don't prune
			}
			else {
				Object injectiveToken = injectiveModule.getInjectiveFunctionToken(expression, process);
				if (injectiveToken == null) {
					pruned = true;
				}
			}
			return pruned;
		}
	}
}
