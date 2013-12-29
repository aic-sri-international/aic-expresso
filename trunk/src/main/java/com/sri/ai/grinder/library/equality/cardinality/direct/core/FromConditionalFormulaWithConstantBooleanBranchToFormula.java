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
import com.sri.ai.grinder.core.HasFunctor;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;

/**
 * A rewriter rewriting conditional formulas in the following way:
 * 
 * if C then true  else F       ---->    C     or  F
 * if C then F     else true    ---->    not C or  F
 * if C then false else F       ---->    not C and F
 * if C then F     else false   ---->    C     and F
 * 
 * This is a simpler version of {@link FromConditionalFormulaToFormula} that
 * guarantees not to expand the expression.
 * This allows it to be used safely in more situations.
 * 
 * @author braz
 * 
 */
@Beta
public class FromConditionalFormulaWithConstantBooleanBranchToFormula extends AbstractRewriter {
	
	public FromConditionalFormulaWithConstantBooleanBranchToFormula() {
		this.setReifiedTests(new HasFunctor(FunctorConstants.IF_THEN_ELSE));
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		Expression result = expression;
		
		Expression condition = IfThenElse.getCondition(expression);
		Expression thenBranch = IfThenElse.getThenBranch(expression);
		Expression elseBranch = IfThenElse.getElseBranch(expression);
		
		if (thenBranch.equals(Expressions.TRUE) && FormulaUtil.isFormula(elseBranch, process)) {
			result = Or.make(condition, elseBranch);
		}
		else if (elseBranch.equals(Expressions.TRUE) && FormulaUtil.isFormula(thenBranch, process)) {
			result = Or.make(Not.make(condition), thenBranch);  
		}
		else if (thenBranch.equals(Expressions.FALSE) && FormulaUtil.isFormula(elseBranch, process)) {
			result = And.make(Not.make(condition), elseBranch);
		}
		else if (elseBranch.equals(Expressions.FALSE) && FormulaUtil.isFormula(thenBranch, process)) {
			result = And.make(condition, thenBranch);  
		}

		return result;
	}
}
