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
package com.sri.ai.grinder.sgdpllt.theory.help;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.QuantifierEliminationProblem;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;

/** 
 * A {@link Theory} that passes all methods through to a base theory.
 * It is useful as a basis for other classes that modify some behaviors of a base theory.
 */
@Beta
public class TheoryWrapper implements Theory {

	private Theory base;
	
	public TheoryWrapper(Theory base) {
		this.base = base;
	}
	
	public Theory getBase() {
		return base;
	}
	
	@Override
	public Expression simplify(Expression expression, Context context) {
		return base.simplify(expression, context);
	}

	@Override
	public TopRewriter getTopRewriter() {
		return base.getTopRewriter();
	}

	@Override
	public ExpressionLiteralSplitterStepSolver makeEvaluatorStepSolver(Expression expression) {
		return base.makeEvaluatorStepSolver(expression);
	}

	@Override
	public boolean isSuitableFor(Type type) {
		return base.isSuitableFor(type);
	}

	@Override
	public boolean isAtom(Expression expression, Context context) {
		return base.isAtom(expression, context);
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraintAfterBookkeeping(Expression variable, Context context) {
		return base.makeSingleVariableConstraintAfterBookkeeping(variable, context);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return base.singleVariableConstraintIsCompleteWithRespectToItsVariable();
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression) {
		return base.isInterpretedInThisTheoryBesidesBooleanConnectives(expression);
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context) {
		return base.getSingleVariableConstraintSatisfiabilityStepSolver(constraint, context);
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context) {
		return base.getSingleVariableConstraintModelCountingStepSolver(constraint, context);
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getQuantifierEliminatorStepSolver(QuantifierEliminationProblem problem, Context context) {
		return base.getQuantifierEliminatorStepSolver(problem, context);
	}

	@Override
	public Collection<Type> getNativeTypes() {
		return base.getNativeTypes();
	}

	@Override
	public TheoryWrapper clone() {
		TheoryWrapper result = null;
		try {
			result = (TheoryWrapper) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}
		return result;
	}
}