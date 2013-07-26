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

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.IncompleteLinearImpliedFormulasExtractor;
import com.sri.ai.grinder.GrinderConfiguration;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.util.base.Pair;

/**
 * A version of R_implied_certainty that is incomplete and linear time for use by R_simplify.
 * It additionally replaces variables bound to a constant by that constant.
 * 
 * @author braz
 *
 */
@Beta
public class IncompleteLinearImpliedCertainty extends AbstractRewriter {

	private static final boolean replaceVariableByConstantItIsBoundTo = GrinderConfiguration.isReplaceVariableWithConstantItIsBoundTo();

	public IncompleteLinearImpliedCertainty() {
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		Expression result  = expression;
		boolean isFormula  = FormulaUtil.isFormula(expression, process);
		boolean isVariable = process.isVariable(expression); 
		if (isFormula || isVariable) {
			// These conditions are tested again later. They are tested here to avoid unnecessary computation of implied facts.
			
			// deal with trivial implications from context
			Expression context = process.getContextualConstraint();
			List<Expression> impliedFactsByContext = IncompleteLinearImpliedFormulasExtractor.get(context);
			for (Expression impliedFactByContext : impliedFactsByContext) {
				if (!impliedFactByContext.equals(Expressions.FALSE)) {
					
					if (isFormula) {
						if (IncompleteLinearImplies.implies(impliedFactByContext, expression, process)) {
							result = Expressions.TRUE;
							if (result.equals(expression)) {
								// Ensure behaves correctly
								result = expression;
							}
							break;
						} 
						else if (IncompleteLinearImplies.implies(impliedFactByContext, Not.make(expression), process)) {
							result = Expressions.FALSE;
							if (result.equals(expression)) {
								// Ensure behaves correctly
								result = expression;
							}
							break;
						}
					}

					if (replaceVariableByConstantItIsBoundTo) {
						if (isVariable && Equality.isEquality(impliedFactByContext) && impliedFactByContext.getArguments().contains(expression)) {
							Pair<List<Expression>, Expression> variablesListAndConstant = Equality.getVariablesListAndConstantOrNullIfNoConstant(impliedFactByContext, process);
							if (variablesListAndConstant != null && variablesListAndConstant.first.contains(expression)) {
								result = variablesListAndConstant.second;
							}
						}
					}
				}
			}
		}
		
		return result;
	}
}
