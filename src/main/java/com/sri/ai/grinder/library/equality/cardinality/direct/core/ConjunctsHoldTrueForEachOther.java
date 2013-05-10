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

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.equality.cardinality.direct.CardinalityRewriter;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;

/**
 * Rewriter simplifying one conjunct by using the fact that the others must be true.
 * For example, X = a and (X != a or Y = a) must simplify to X = a and Y = a.
 * 
 * This corresponds to the theorem
 * <pre>
 * (p => (q <=> q'))  =>  (p and q) <=> (p and q')
 * </pre>
 * 
 * @author braz
 */
@Beta
public class ConjunctsHoldTrueForEachOther extends AbstractRewriter {
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		if (And.isConjunction(expression) && expression.numberOfArguments() > 1) {
			for (int i = 0; i != expression.numberOfArguments(); i++) {
				Expression iThConjunct = expression.get(i);
				Expression remainingOfConjunction = Expressions.removeIthArgument(expression, i);
				// Note: Only attempt this if the remaining conjunction can actually be used to
				// extend the context (i.e. is a formula).
				if (FormulaUtil.isFormula(remainingOfConjunction, process)) {
					RewritingProcess processAssumingRemainingOfConjunction = GrinderUtil.extendContextualConstraint(remainingOfConjunction, process);
					Expression newIThConjunct = processAssumingRemainingOfConjunction.rewrite(CardinalityRewriter.R_simplify, iThConjunct);
					if (newIThConjunct != iThConjunct) {
						Expression result;
						// Short circuit to 'false' straight away.
						if (newIThConjunct.equals(Expressions.FALSE)) {
							result = Expressions.FALSE;
						}
						else {
							List<Expression> newConjuncts = new ArrayList<Expression>(expression.getArguments());
							newConjuncts.set(i, newIThConjunct);
							result = And.make(newConjuncts);
						}
						return result;
					}
				}
			}
		}
		return expression;
	}
}
