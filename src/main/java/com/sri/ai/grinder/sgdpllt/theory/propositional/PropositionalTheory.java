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
package com.sri.ai.grinder.sgdpllt.theory.propositional;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.helper.GrinderUtil.BOOLEAN_TYPE;
import static com.sri.ai.grinder.sgdpllt.library.FormulaUtil.functorIsALogicalConnectiveIncludingConditionals;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.NOT;
import static com.sri.ai.util.Util.map;

import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionFormulaSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractTheory;
import com.sri.ai.grinder.sgdpllt.core.solver.ExpressionFormulaToLiteralSplitterStepSolverAdapter;
import com.sri.ai.grinder.sgdpllt.core.solver.QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.FormulaUtil;
import com.sri.ai.grinder.sgdpllt.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.sgdpllt.library.boole.Not;

@Beta
/** 
 * A {@link Theory} for propositional logic.
 */
public class PropositionalTheory extends AbstractTheory {

	@Override
	public boolean isSuitableFor(Expression variable, Type type) {
		boolean result =
				type.getName().equals("Boolean") &&
				!functorIsALogicalConnectiveIncludingConditionals(variable);
		return result;
	}
	
	/**
	 * Constructor overrides default constructor in order to define
	 * testing symbols <code>P, Q, R</code> of type <code>Boolean</code>,
	 * and main testing variable as <code>P</code>.
	 */
	public PropositionalTheory() {
		super(new BooleanSimplifier());
		setVariableNamesAndTypesForTesting(map("P", BOOLEAN_TYPE, "Q", BOOLEAN_TYPE, "R", BOOLEAN_TYPE));
	}

	@Override
	public boolean isNonTrivialAtom(Expression expression, Context context) {
		Object syntacticFormType = expression.getSyntacticFormType();
		boolean result = 
				(syntacticFormType.equals("Symbol") || syntacticFormType.equals("Function application")) &&
				GrinderUtil.isBooleanTyped(expression, context) && 
				!FormulaUtil.functorIsALogicalConnectiveIncludingConditionals(expression);
		return result;
	}
	
	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, Theory theory, Context context) {
		return new SingleVariablePropositionalConstraint(variable, theory);
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, Context context) {
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
	public 	ExpressionLiteralSplitterStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraintForThisIndex, Expression currentBody, Context context) {
		ExpressionFormulaSplitterStepSolver formulaSplitterStepSolver = new QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver(group, constraintForThisIndex, currentBody);
		ExpressionLiteralSplitterStepSolver result = new ExpressionFormulaToLiteralSplitterStepSolverAdapter(formulaSplitterStepSolver);
		return result;
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return true;
	}

	@Override
	public Expression makeRandomAtomOn(String variable, Random random, Context context) {
		return makeSymbol(variable);
	}

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
