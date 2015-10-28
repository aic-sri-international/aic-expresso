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
package com.sri.ai.grinder.sgdpll2.theory.propositional;

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.map;
import static com.sri.ai.util.Util.mapIntoArrayList;

import java.util.ArrayList;
import java.util.Random;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.equality.formula.FormulaUtil;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.AbstractConstraintTheory;

@Beta
/** 
 * A {@link ConstraintTheory} for propositional logic.
 */
public class PropositionalConstraintTheory extends AbstractConstraintTheory {

	@Override
	public boolean isSuitableFor(Expression variable, RewritingProcess process) {
		boolean result = isNonTrivialAtom(variable, process);
		return result;
	}
	
	/**
	 * Constructor overrides default constructor in order to define
	 * testing symbols <code>P, Q, R</code> of type <code>Boolean</code>,
	 * and main testing variable as <code>P</code>.
	 */
	public PropositionalConstraintTheory() {
		super();
		ArrayList<Expression> knownConstants = mapIntoArrayList(list("true", "false"), s -> makeSymbol(s));
		setTypesForTesting(list(new Categorical("Boolean", 2, knownConstants)));
		setVariableNamesAndTypeNamesForTesting(map("P", "Boolean", "Q", "Boolean", "R", "Boolean"));
		setTestingVariable("P");
	}

	private Simplifier simplifier = new BooleanSimplifier();
	
	@Override
	public Expression simplify(Expression expression, RewritingProcess process) {
		Expression result = simplifier.apply(expression, process);
		return result;
	}

	@Override
	public boolean isNonTrivialLiteral(Expression expression, RewritingProcess process) {
		boolean result = isNonTrivialAtom(expression, process) || isNonTrivialNegativeLiteral(expression, process);
		return result;
	}

	private static boolean isNonTrivialAtom(Expression expression, RewritingProcess process) {
		boolean result = GrinderUtil.isBooleanTyped(expression, process) && !FormulaUtil.functorIsALogicalConnectiveIncludingConditionals(expression);
		return result;
	}
	
	private static boolean isNonTrivialNegativeLiteral(Expression expression, RewritingProcess process) {
		boolean result = expression.hasFunctor(NOT) && isNonTrivialAtom(expression.get(0), process);
		return result;
	}
	
	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, RewritingProcess process) {
		return new SingleVariablePropositionalConstraint(variable, this);
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, RewritingProcess process) {
		return false; // nothing else is interpreted
	}

	@Override
	public ContextDependentProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return new SatisfiabilityOfSingleVariablePropositionalConstraintStepSolver((SingleVariablePropositionalConstraint) constraint);
	}

	@Override
	public ContextDependentProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return new ModelCountingOfSingleVariablePropositionalConstraintStepSolver((SingleVariablePropositionalConstraint) constraint);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return true;
	}

	@Override
	public Expression makeRandomAtomOn(String variable, Random random, RewritingProcess process) {
		return makeSymbol(variable);
	}

	@Override
	public Expression getLiteralNegation(Expression literal, RewritingProcess process) {
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
