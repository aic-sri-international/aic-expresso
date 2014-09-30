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
package com.sri.ai.grinder.library.controlflow;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractRewriter;
import com.sri.ai.grinder.core.HasKind;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.SyntacticSubstitute;

/**
 * An atomic rewriter for conditional expressions.
 * 
 * @author braz
 *
 */
@Beta
public class IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch extends AbstractRewriter {

	private boolean simplifyToConditionIfPossible = false;
	
	/**
	 * Constructor setting a flag authorizing simplifications of the type "if C then true else false -> C" to occur.
	 */
	public IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch() {
		this.setReifiedTests(new HasKind(FunctorConstants.IF_THEN_ELSE));
	}
	
	/**
	 * Constructor taking a flag authorizing simplifications of the type "if C then true else false -> C" to occur.
	 */
	public IfThenElseConditionIsTrueInThenBranchAndFalseInElseBranch(boolean simplifyToConditionIfPossible) {
		this.setReifiedTests(new HasKind(FunctorConstants.IF_THEN_ELSE));
		this.simplifyToConditionIfPossible = simplifyToConditionIfPossible;
	}
	
	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		
		Expression condition  = expression.get(0);
		Expression thenBranch = expression.get(1);
		Expression elseBranch = expression.get(2);
		
		Expression thenBranchReplacement =
			replaceCondition(thenBranch, condition, Expressions.TRUE, process);
		
		Expression elseBranchReplacement =
			replaceCondition(elseBranch, condition, Expressions.FALSE, process);

		if (thenBranchReplacement != thenBranch || elseBranchReplacement != elseBranch) {				
			Expression result = IfThenElse.make(condition, thenBranchReplacement, elseBranchReplacement, simplifyToConditionIfPossible);			
			return result;
		}

		return expression;
	}

	public Expression replaceCondition(Expression expression, Expression condition, Expression booleanConstant, RewritingProcess process) {
		Expression result;
		if (condition.hasFunctor(FunctorConstants.NOT)) {
			result = replaceCondition(expression, condition.get(0), Expressions.opposite(booleanConstant), process);
		}
		else {
			result = replaceNonNegatedCondition(expression, condition, booleanConstant, process);
		}
		return result;
	}

	public Expression replaceNonNegatedCondition(Expression expression, Expression condition, Expression booleanConstant, RewritingProcess process) {
		Expression result = null;

		if (booleanConstant.equals(Expressions.TRUE)){
			result = replaceTrueCondition(expression, condition, process);
		}
		else if (booleanConstant.equals(Expressions.FALSE)){
			result = replaceFalseCondition(expression, condition, process);
		}
		else {
			throw new Error("replaceNonNegatedCondition should take a boolean constant but got " + booleanConstant);
		}

		return result;
	}
	
	public Expression replaceTrueCondition(Expression expression, Expression condition, RewritingProcess process) {
		if (condition.hasFunctor(FunctorConstants.AND)) {
			for (Expression conjunct : condition.getArguments()) {
				expression = replaceTrueCondition(expression, conjunct, process);
			}
			return expression;
		}
		// Note: When this if condition is false, we leave the then branch alone 
		// because either it does not make sense (in the case of 'true' and 'false'),
		// or we don't support replacement in these cases yet 
		// (boolean formulas other than conjunctions).
		else if (!(Expressions.isBooleanOperatorApplication(condition) ||
				   Expressions.isEqualityFormulaOnAtomicSymbols(condition) ||
				   condition.equals(FunctorConstants.TRUE) ||
				   condition.equals(FunctorConstants.FALSE) ||
				   IfThenElse.isIfThenElse(condition))) {
//			expression = SemanticSubstitute.replace(expression, condition, Expressions.TRUE, process);
			expression = SyntacticSubstitute.replace(expression, condition, Expressions.TRUE, process);
		}

		return expression;
	}

	public Expression replaceFalseCondition(Expression expression, Expression condition, RewritingProcess process) {
		if (condition.hasFunctor(FunctorConstants.OR)) {
			for (Expression disjunct : condition.getArguments()) {
				expression = replaceFalseCondition(expression, disjunct, process);
			}
			return expression;
		}
		// Note: When this if condition is false, we leave the else branch alone 
		// because either it does not make sense (in the case of 'true' and 'false'),
		// or we don't support replacement in these cases yet 
		// (boolean formulas other than conjunctions).
		else if (!(Expressions.isBooleanOperatorApplication(condition) ||
				   Expressions.isEqualityFormulaOnAtomicSymbols(condition) ||
				   condition.equals(FunctorConstants.TRUE) ||
				   condition.equals(FunctorConstants.FALSE) ||
				   IfThenElse.isIfThenElse(condition))) {	
//			expression = SemanticSubstitute.replace(expression, condition, Expressions.FALSE, process);
			expression = SyntacticSubstitute.replace(expression, condition, Expressions.FALSE, process);			
		}
		
		return expression;
	}
}
