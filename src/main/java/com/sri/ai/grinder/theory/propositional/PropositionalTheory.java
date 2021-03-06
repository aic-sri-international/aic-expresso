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
package com.sri.ai.grinder.theory.propositional;

import static com.sri.ai.grinder.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter.toExpressionLiteralSplitterStepSolver;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.grinder.library.boole.Not.not;
import static com.sri.ai.grinder.library.commonrewriters.CommonSimplifiersAndSymbolicQuantifierEliminationRewritersTopRewriter.INSTANCE;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Symbol;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.ExpressionStepSolver;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.core.constraint.AbstractTheory;
import com.sri.ai.grinder.core.solver.QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.rewriter.api.TopRewriter;

@Beta
/** 
 * A {@link Theory} for propositional logic.
 */
public class PropositionalTheory extends AbstractTheory {

	public PropositionalTheory() {
		super();
	}

	@Override
	public TopRewriter makeTopRewriter() {
		return TopRewriter.merge(INSTANCE, new BooleanSimplifier());
	}

	@Override
	public boolean isSuitableFor(Type type) {
		boolean result = type.getName().equals("Boolean");
		return result;
	}
	
	@Override
	public boolean isAtom(Expression expression, Context context) {
		Object syntacticFormType = expression.getSyntacticFormType();
		boolean result = 
				syntacticFormType.equals(Symbol.SYNTACTIC_FORM_TYPE) &&
				GrinderUtil.isBooleanTyped(expression, context);
		return result;
	}
	
	@Override
	public Expression getAtomNegation(Expression atom, Context context) {
		return not(atom);
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraintAfterBookkeeping(Expression variable, Context context) {
		return new SingleVariablePropositionalConstraint(variable, context.getTheory());
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression) {
		return false; // nothing else is interpreted
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context) {
		ExpressionLiteralSplitterStepSolver result = new SatisfiabilityOfSingleVariablePropositionalConstraintStepSolver((SingleVariablePropositionalConstraint) constraint);
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context) {
		ExpressionLiteralSplitterStepSolver result = new ModelCountingOfSingleVariablePropositionalConstraintStepSolver((SingleVariablePropositionalConstraint) constraint);
		return result;
	}

	@Override
	public 	ExpressionLiteralSplitterStepSolver getSingleQuantifierEliminatorStepSolver(SingleQuantifierEliminationProblem problem, Context context) {
		ExpressionStepSolver formulaSplitterStepSolver = new QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver(problem);
		ExpressionLiteralSplitterStepSolver result = toExpressionLiteralSplitterStepSolver(formulaSplitterStepSolver);
		return result;
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return true;
	}

	/**
	 * An overriding more efficient implementation.
	 */
	@Override
	public Expression getLiteralNegation(Expression literal, Context context) {
		Expression result;
		if (literal.hasFunctor(NOT)) {
			result = literal.get(0);
		}
		else {
			result = Not.make(literal);
		}
		return result;
	}
}
