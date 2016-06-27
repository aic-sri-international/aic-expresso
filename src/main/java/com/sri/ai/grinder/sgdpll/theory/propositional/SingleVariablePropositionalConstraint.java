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
package com.sri.ai.grinder.sgdpll.theory.propositional;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;

import java.util.ArrayList;
import java.util.List;

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.core.constraint.AbstractSingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.constraint.AbstractSingleVariableConstraintWithIndependentNormalizedAtoms;
import com.sri.ai.util.base.Pair;

/**
 * A single-variable propositional constraint.
 * 
 * @author braz
 *
 */
public class SingleVariablePropositionalConstraint extends AbstractSingleVariableConstraintWithIndependentNormalizedAtoms {

	private static final long serialVersionUID = 1L;

	public SingleVariablePropositionalConstraint(Expression variable, ConstraintTheory constraintTheory) {
		super(variable, constraintTheory);
	}

	public SingleVariablePropositionalConstraint(
			Expression variable,
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals,
			ConstraintTheory constraintTheory) {
		
		super(variable, positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, constraintTheory);
	}

	public SingleVariablePropositionalConstraint(SingleVariablePropositionalConstraint other) {
		super(other);
	}

	@Override
	protected SingleVariablePropositionalConstraint makeSimplification(ArrayList<Expression> positiveNormalizedAtoms, ArrayList<Expression> negativeNormalizedAtoms, List<Expression> externalLiterals) {
		// no special bookkeeping to be retained in simplifications, so we just make a new constraint.
		SingleVariablePropositionalConstraint result = new SingleVariablePropositionalConstraint(getVariable(), positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, getConstraintTheory());
		return result;
	}

	@Override
	public SingleVariablePropositionalConstraint clone() {
		SingleVariablePropositionalConstraint result = new SingleVariablePropositionalConstraint(this);
		return result;
	}

	@Override
	public AbstractSingleVariableConstraint destructiveUpdateOrNullAfterConjoiningNewNormalizedAtom(boolean sign, Expression atom, Context context) {
		return this;
	}

	@Override
	public Expression fromNormalizedAtomToItsNegationAsLiteral(Expression negativeAtom) {
		return apply(NOT, negativeAtom);
	}

	@Override
	public Pair<Boolean, Expression> fromLiteralOnVariableToSignAndNormalizedAtom(Expression variable, Expression literal, Context context) {
		Pair<Boolean, Expression> result;
		if (literal.hasFunctor(NOT)) {
			result = Pair.make(false, literal.get(0));
		}
		else {
			result = Pair.make(true, literal);
		}
		return result;
	}

	@Override
	public SingleVariablePropositionalConstraint conjoin(Expression formula, Context context) {
		return (SingleVariablePropositionalConstraint) super.conjoin(formula, context);
	}

	@Override
	public Expression binding() {
		if (getPositiveNormalizedAtoms().contains(getVariable())) {
			return TRUE;
		}
		else if (getNegativeNormalizedAtoms().contains(getVariable())) {
			return FALSE;
		}
		else {
			return null;
		}
	}
}