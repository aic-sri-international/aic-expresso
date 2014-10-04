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
package com.sri.ai.grinder.library.equality.cardinality.direct.core;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.core.HasNumberOfArguments;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.SyntacticFunctionsSubExpressionsProvider;

/**
 * A rewriter for replacing cardinality expressions on the type of a logical
 * variable with the size of that type.
 * For e.g. if the type size of X is 100 it would do the following:
 * 
 * <pre>
 * | type(X) | -> 100
 * </pre>
 * 
 * or if a Logical Variable representing a type is used:
 * 
 * <pre>
 * | People | -> 100
 * </pre>
 * 
 * This rewriter does not maintain the sizes associated with individual logical
 * variables. Instead it delegates to a TypeSizeOfLogicalVariable interface,
 * which it looks for on the current process in order to determine the size of
 * the type the logical variable belongs to.
 * 
 * @author oreilly
 * 
 */
@Beta
public class CardinalityTypeOfLogicalVariable extends AbstractRewriter {
	// The key for looking up the type size of logical variables interface
	// on the process's global objects map.
	public final static String PROCESS_GLOBAL_OBJECT_KEY_DOMAIN_SIZE_OF_LOGICAL_VARIABLE = "type size of logical variable";

	public final static String TYPE_LABEL = "type";

	/**
	 * Interface to be implemented by a source that is able to determine the
	 * type sizes of the logical variables passed to it.
	 * 
	 * @author oreilly
	 * 
	 */
	public interface TypeSizeOfLogicalVariable {
		/**
		 * Get the size of the logical variable's type.
		 * 
		 * @param logicalVariable
		 *            the logical variable whose type size is to be looked up.
		 * @param process
		 *            the rewriting process in which the look up is being
		 *            performed.
		 * @return null if the type size is unknown, otherwise its size.
		 */
		Integer size(Expression logicalVariable, RewritingProcess process);
	}

	public static void registerTypeSizeOfLogicalVariableWithProcess(
			TypeSizeOfLogicalVariable typeSizeOfLogicalVariable,
			RewritingProcess process) {
		process.putGlobalObject(
				PROCESS_GLOBAL_OBJECT_KEY_DOMAIN_SIZE_OF_LOGICAL_VARIABLE,
				typeSizeOfLogicalVariable);
	}
	
	public CardinalityTypeOfLogicalVariable() {
		this.setReifiedTests(new HasKind(FunctorConstants.CARDINALITY), new HasNumberOfArguments(1));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result = expression;

		Expression cardinalityArgument = expression.get(0);
		Expression logicalVariable = null;
		
		if (process.isVariable(cardinalityArgument)) {
			// | DomainNameLogicalVariable | 
			logicalVariable = cardinalityArgument;
		} 
		else {
			// | type(LogicalVariableName) |
			// Note: type(...) expressions are marked as being syntactic functions
			// and their arguments are not sub-expressions
			// (if they were, we would be able to rewrite type(X) to type(10) when X = 10,
			// and this would be incorrect.
			// In order to access their "argument", we must use their syntax tree.
			if (isTypeSyntacticFunctionApplication(cardinalityArgument)
					&& cardinalityArgument.getSyntaxTree().numberOfImmediateSubTrees() == 1
					&& process.isVariable(Expressions.makeFromSyntaxTree(cardinalityArgument.getSyntaxTree().getImmediateSubTrees().get(0)))) {
				
				logicalVariable = Expressions.makeFromSyntaxTree(cardinalityArgument.getSyntaxTree().getImmediateSubTrees().get(0));
			}
		}
		
		if (logicalVariable != null) {
			TypeSizeOfLogicalVariable typeSizeOfLogicalVariable = (TypeSizeOfLogicalVariable) process
					.getGlobalObject(PROCESS_GLOBAL_OBJECT_KEY_DOMAIN_SIZE_OF_LOGICAL_VARIABLE);
			if (typeSizeOfLogicalVariable != null) {
				Integer size = typeSizeOfLogicalVariable.size(logicalVariable, process);
				if (size != null) {
					result = Expressions.makeSymbol(size);
				}
			}
		} 

		return result;
	}

	public static boolean isTypeSyntacticFunctionApplication(Expression expression) {
		boolean result =
				expression.getSyntacticFormType().equals("Syntactic function") &&
				SyntacticFunctionsSubExpressionsProvider.getSyntacticFunctor(expression).equals(CardinalityTypeOfLogicalVariable.TYPE_LABEL);
		return result;
	}
}
