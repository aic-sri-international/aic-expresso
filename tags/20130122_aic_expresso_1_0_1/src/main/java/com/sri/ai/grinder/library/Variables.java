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

import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Predicate;
import com.google.common.collect.Sets;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.ExpressionAndContext;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * 
 * @author braz
 *
 */
@Beta
public class Variables {

	/**
	 * A static method returning the variables
	 * in a given expression, for a certain predicate indicating constants.
	 */
	public static LinkedHashSet<Expression> get(Expression argument, Predicate<Expression> isConstantPredicate) {
		return SubExpressionSelection.get(argument, new IsVariablePredicate(isConstantPredicate));
	}

	/**
	 * A static method returning the variables
	 * in a given expression, in a certain process.
	 */
	public static LinkedHashSet<Expression> get(Expression argument, RewritingProcess process) {
		return get(argument, process.getIsConstantPredicate());
	}
	
	/** Returns the set of free variables in an expression, according to a given process. */
	public static Set<Expression> freeVariables(Expression expression, RewritingProcess process) {
		Set<Expression> freeVariables       = new HashSet<Expression>(); 
		Set<Expression> quantifiedVariables = new HashSet<Expression>();
		
		freeVariables(expression, freeVariables, quantifiedVariables, process);
		
		return freeVariables;
	}

	/**
	 * Returns the set of free symbols (variables and constants, including function symbols)
	 * in an expression, according to a given process.
	 */
	public static Set<Expression> freeSymbols(Expression expression, RewritingProcess process) {
		Set<Expression> freeSymbols = new LinkedHashSet<Expression>(); 
		Iterator<ExpressionAndContext> subExpressionAndContextsIterator = expression.getImmediateSubExpressionsAndContextsIterator(process);
		while (subExpressionAndContextsIterator.hasNext()) {
			ExpressionAndContext subExpressionAndContext = subExpressionAndContextsIterator.next();
			Set<Expression> freeVariablesInSubExpression = freeVariables(subExpressionAndContext.getExpression().getSyntaxTree(), process);
			freeVariablesInSubExpression = Sets.difference(freeVariablesInSubExpression, subExpressionAndContext.getQuantifiedVariables()); 
			freeSymbols.addAll(freeVariablesInSubExpression);
		}
		if (expression instanceof Symbol) {
			freeSymbols.add(expression);
		}
		return freeSymbols;
	}
	
	//
	// PRIVATE METHODS
	//
	private static void freeVariables(Expression expression, Set<Expression> freeVariables, Set<Expression> quantifiedVariables, RewritingProcess process) {
		if (expression instanceof Symbol) {
			if (process.isVariable(expression)) {
				if (!quantifiedVariables.contains(expression)) {
					freeVariables.add(expression);
				}
			}
		} 
		else {
			Iterator<ExpressionAndContext> subExpressionAndContextsIterator = expression.getImmediateSubExpressionsAndContextsIterator(process);
			Set<Expression> newLocalQuantifiedVariables = null;
			while (subExpressionAndContextsIterator.hasNext()) {
				ExpressionAndContext subExpressionAndContext = subExpressionAndContextsIterator.next();
				
				// Only add newly quantified variables in this context
				if (newLocalQuantifiedVariables == null) {
					// For efficiency, only instantiate once
					newLocalQuantifiedVariables = new HashSet<Expression>();
				}
				else {
					newLocalQuantifiedVariables.clear();
				}
				for (Expression localVariable : subExpressionAndContext.getQuantifiedVariables()) {
					if (quantifiedVariables.add(localVariable)) {
						newLocalQuantifiedVariables.add(localVariable);
					}
				}

				freeVariables(subExpressionAndContext.getExpression().getSyntaxTree(), freeVariables, quantifiedVariables, process);
				
				// Only remove the newly quantified variables in this context
				quantifiedVariables.removeAll(newLocalQuantifiedVariables);
			}
		}

		return;
	}
}
