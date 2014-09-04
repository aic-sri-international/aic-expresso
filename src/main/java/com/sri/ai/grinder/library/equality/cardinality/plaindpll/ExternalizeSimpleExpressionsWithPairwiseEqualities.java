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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;

@Beta
/** 
 * Class with methods for externalization of pairwise equalities in simple expressions completely simplified by themselves.
 * This was originally developed for {@link PlainCardinalityDPLLWithFreeVariables} but a more specialized externalization routine in the class itself
 * was slightly faster.
 */
public class ExternalizeSimpleExpressionsWithPairwiseEqualities {
	
	/**
	 * Externalizes conditions of simple expressions (symbols and function applications) where the only conditions are two-argument equalities.
	 */
	public static Expression externalize(Expression expression, RewritingProcess process) {
		Expression result = externalize(expression, false, process);
		return result;
	}

	private static Expression externalize(Expression expression, boolean argumentsAlreadyExternalized, RewritingProcess process) {
		Expression result;
		
		if ( ! argumentsAlreadyExternalized) {
			expression = externalizeArguments(expression, process);
		}
		
		if (IfThenElse.isIfThenElse(expression)) {
			result = externalizeIfThenElse(expression, process);
		}
		else if (expression.getSyntacticFormType().equals("Function application")) {
			result = externalizeFunctionApplication(expression, process);
		}
		else {
			result = expression;
		}
		
		return result;
	}

	/**
	 * Assumes expression's arguments are already externalized.
	 */
	private static Expression externalizeIfThenElse(Expression expression, RewritingProcess process) {
		Expression result;
		
		Expression condition = IfThenElse.getCondition(expression);
		
		if (IfThenElse.isIfThenElse(condition)) {
			// if (if C then A else B) then D else E  --->   if C then if A then D else E else if B then D else E
			Expression conditionCondition  = IfThenElse.getCondition(condition);
			Expression conditionThenBranch = IfThenElse.getThenBranch(condition);
			Expression conditionElseBranch = IfThenElse.getElseBranch(condition);
			
			Expression thenBranch = IfThenElse.getThenBranch(expression);
			Expression elseBranch = IfThenElse.getElseBranch(expression);

			Expression thenIfThenElse = IfThenElse.make(conditionThenBranch, thenBranch, elseBranch);
			Expression elseIfThenElse = IfThenElse.make(conditionElseBranch, thenBranch, elseBranch);
			result = IfThenElse.make(conditionCondition, thenIfThenElse, elseIfThenElse); 
		}
		else {
			result = expression;
		}

		return result;
	}

	/**
	 * Assumes expression's arguments are already externalized.
	 */
	private static Expression externalizeFunctionApplication(Expression expression, RewritingProcess process) {
		Expression result;
		
		int firstConditionalArgumentIndex = getFirstConditionalArgument(expression);
		if (firstConditionalArgumentIndex != -1) {
			Expression firstConditionalArgument = expression.get(firstConditionalArgumentIndex);
			Expression conditionalArgumentCondition  = IfThenElse.getCondition(firstConditionalArgument);
			Expression conditionalArgumentThenBranch = IfThenElse.getThenBranch(firstConditionalArgument);
			Expression conditionalArgumentElseBranch = IfThenElse.getElseBranch(firstConditionalArgument);
			
			Expression resultThenBranch = expression;
			resultThenBranch = resultThenBranch.set(firstConditionalArgumentIndex, conditionalArgumentThenBranch);
			for (int i = 0; i != expression.numberOfArguments(); i++) {
				if (i != firstConditionalArgumentIndex) {
					Expression simplifiedIthArgument = SimplifyFormula.applyEqualityTo(resultThenBranch.get(i), conditionalArgumentCondition, process);
					resultThenBranch = resultThenBranch.set(i, simplifiedIthArgument);
				}
			}
			// other arguments in resultThenBranch may also be conditional, in which case it is will not yet externalized.
			// For example, assume expression is 'f(if C1 then A else B,   D,   if C2 then E else G)'
			// Then resultThenBranch is 'f(A,   D,   if C2 then E else G)'
			// We therefore externalize it, but first note that its arguments are already externalized (they are the same arguments as expression's),
			// so we indicate that fact in the 'argumentsAlreadyExternalized' flag.
			resultThenBranch = externalize(resultThenBranch, true /* argumentsAlreadyExternalized */, process);
			
			Expression resultElseBranch = expression;
			resultElseBranch = resultElseBranch.set(firstConditionalArgumentIndex, conditionalArgumentElseBranch);
			for (int i = 0; i != expression.numberOfArguments(); i++) {
				if (i != firstConditionalArgumentIndex) {
					Expression simplifiedIthArgument = SimplifyFormula.applyDisequalityTo(resultElseBranch.get(i), conditionalArgumentCondition, process);
					resultElseBranch = resultElseBranch.set(i, simplifiedIthArgument);
				}
			}
			// see comments above regarding resultThenBranch
			resultElseBranch = externalize(resultElseBranch, true /* argumentsAlreadyExternalized */, process);

			result = IfThenElse.make(conditionalArgumentCondition, resultThenBranch, resultElseBranch);
		}
		else {
			result = expression;
		}
		
		return result;
	}

	/**
	 * Returns index of first argument which is an if then else expression, or -1 if there is none.
	 */
	private static int getFirstConditionalArgument(Expression expression) {
		for (int i = 0; i != expression.numberOfArguments(); i++) {
			if (IfThenElse.isIfThenElse(expression.get(i))) {
				return i;
			}
		}
		return -1;
	}

	private static Expression externalizeArguments(Expression expression, RewritingProcess process) {
		Expression result = Expressions.replaceImmediateSubexpressions(expression, new Externalize(process));
		return result;
	}
	
	public static class Externalize implements Function<Expression, Expression> {

		private RewritingProcess process;

		public Externalize(RewritingProcess process) {
			this.process = process;
		}

		@Override
		public Expression apply(Expression input) {
			Expression result = externalize(input, process);
			return result;
		}
		
	}
}
