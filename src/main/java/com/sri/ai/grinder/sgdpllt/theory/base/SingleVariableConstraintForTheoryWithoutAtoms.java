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
package com.sri.ai.grinder.sgdpllt.theory.base;

import java.util.ArrayList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.constraint.AbstractSingleVariableConstraint;
import com.sri.ai.util.base.Pair;

/**
 * An implementation of {@link SingleVariableConstraint} for
 * theories that define no atoms.
 * <p>
 * Theories may have no atoms if they rely on translating their problems into some other theory.
 * In those cases, there is no need to split on atoms or to keep single-variable constraints.
 * 
 * @author braz
 *
 */
@Beta
public class SingleVariableConstraintForTheoryWithoutAtoms extends AbstractSingleVariableConstraint {

	private static final long serialVersionUID = 1L;

	public SingleVariableConstraintForTheoryWithoutAtoms(Expression variable, Theory theory) {
		super(variable, theory);
	}

	public SingleVariableConstraintForTheoryWithoutAtoms(
			Expression variable,
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals,
			Theory theory) {
		super(variable, positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, theory);
	}

	@Override
	public Expression binding() {
		return null;
	}

	@Override
	public SingleVariableConstraintForTheoryWithoutAtoms clone() {
		return (SingleVariableConstraintForTheoryWithoutAtoms) super.clone();
	}

	@Override
	protected AbstractSingleVariableConstraint makeSimplification(ArrayList<Expression> positiveNormalizedAtoms, ArrayList<Expression> negativeNormalizedAtoms, List<Expression> externalLiterals) {
		// does nothing special, just a plain new copy
		return new SingleVariableConstraintForTheoryWithoutAtoms(getVariable(), positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, getTheory());
	}

	@Override
	protected Pair<Boolean, Expression> fromLiteralOnVariableToSignAndNormalizedAtom(Expression variable, Expression literal, Context context) {
		throw new Error(getClass() + " has no atoms, so this method should not have been invoked.");
	}

	@Override
	public Expression fromNormalizedAtomToItsNegationAsLiteral(Expression normalizedAtom) {
		throw new Error(getClass() + " has no atoms, so this method should not have been invoked.");
	}

	@Override
	public AbstractSingleVariableConstraint destructiveUpdateOrNullAfterConjoiningNewNormalizedAtom(boolean sign, Expression atom, Context context) {
		throw new Error(getClass() + " has no atoms, so this method should not have been invoked.");
	}

	@Override
	protected boolean conjoiningRedundantSignAndNormalizedAtomNeverChangesConstraintInstance() {
		throw new Error(getClass() + " has no atoms, so this method should not have been invoked.");
	}

	@Override
	protected AbstractSingleVariableConstraint conjoinNonTrivialSignAndNormalizedAtom(boolean sign, Expression normalizedAtom, Context context) {
		throw new Error(getClass() + " has no atoms, so this method should not have been invoked.");
	}
}