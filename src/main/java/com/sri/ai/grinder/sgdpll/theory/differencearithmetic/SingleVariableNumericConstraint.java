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
package com.sri.ai.grinder.sgdpll.theory.differencearithmetic;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.core.constraint.AbstractSingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.theory.base.AbstractSingleVariableConstraintWithBinaryAtomsIncludingEquality;
import com.sri.ai.grinder.sgdpll.theory.numeric.AbstractNumericConstraintTheory;
import com.sri.ai.util.Util;

/**
 * An abstract numeric single-variable constraint solver.
 *
 * @author braz
 *
 */
@Beta
public abstract class SingleVariableNumericConstraint extends AbstractSingleVariableConstraintWithBinaryAtomsIncludingEquality {

	private static final long serialVersionUID = 1L;

	private static final Collection<String> normalFunctors = Util.set(EQUALITY, LESS_THAN, GREATER_THAN);

	private static final Map<String, String> flipFunctor =
	Util.map(
			EQUALITY,                 EQUALITY,
			DISEQUALITY,              DISEQUALITY,
			LESS_THAN,                GREATER_THAN,
			LESS_THAN_OR_EQUAL_TO,    GREATER_THAN_OR_EQUAL_TO,
			GREATER_THAN,             LESS_THAN,
			GREATER_THAN_OR_EQUAL_TO, LESS_THAN_OR_EQUAL_TO
			);

	public SingleVariableNumericConstraint(
			Expression variable,
			boolean propagateAllLiteralsWhenVariableIsBound,
			ConstraintTheory constraintTheory) {
		
		super(variable, propagateAllLiteralsWhenVariableIsBound, constraintTheory);
	}

	protected SingleVariableNumericConstraint(
			Expression variable,
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals,
			boolean propagateAllLiteralsWhenVariableIsBound,
			ConstraintTheory constraintTheory) {
		
		super(variable, positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, propagateAllLiteralsWhenVariableIsBound, constraintTheory);
	}

	public SingleVariableNumericConstraint(SingleVariableNumericConstraint other) {
		super(other);
	}

	@Override
	protected Collection<String> getNormalFunctors() {
		return normalFunctors;
	}
	
	@Override
	protected String getNegationFunctor(String functor) {
		String result = AbstractNumericConstraintTheory.getNegationFunctor(functor);
		return result;
	}
	
	@Override
	protected String getFlipFunctor(String functor) {
		String result = flipFunctor.get(functor);
		return result;
	}
	
	@Override
	abstract protected Expression isolateVariable(Expression atom, Context context);

	@Override
	protected boolean conjoiningRedundantSignAndNormalizedAtomNeverChangesConstraintInstance() {
		boolean result = ! getPropagateAllLiteralsWhenVariableIsBound();
		return result;
		// Explanation: once we propagate incoming literals, 
		// they are stored as external literals and are no further analysed at this constraint's level
		// (other than being eventually provided as splitters).
		// Therefore, under propagation we may produce multiple external literals that may be redundant between themselves
		// but whose redundancies will only be detected when they are themselves analysed
		// in their own constraints.
	}

	@Override
	public Expression getVariableFreeLiteralEquivalentToSign1Atom1ImpliesSign2Atom2(boolean sign1, Expression atom1, boolean sign2, Expression atom2, Context context) {
		Expression result;
		
		if (sign1) {
			result = getVariableFreeFormulaEquivalentToSign1Atom1ImpliesSign2Atom2PositiveAtom1Cases(atom1, sign2, atom2, context);
		}
		else {
			result = getVariableFreeFormulaEquivalentToSign1Atom1ImpliesSign2Atom2NegativeAtom1Cases(atom1, sign2, atom2, context);
		}
		
		return result;
	}

	private Expression getVariableFreeFormulaEquivalentToSign1Atom1ImpliesSign2Atom2PositiveAtom1Cases(Expression atom1, boolean sign2, Expression atom2, Context context) throws Error {

		Expression result;
		
		Expression a = atom1.get(1);
		Expression b = atom2.get(1);

		switch (atom1.getFunctor().toString()) {
	
		case EQUALITY:
			// X = a implies:
			// X = b, iff a = b
			// X < b, iff a < b
			// X > b, iff a > b
			// not (X = b), iff not (a = b)
			// not (X < b), iff not (a < b)
			// not (X > b), iff not (a > b)
			// That is, X = a implies sign2 (X op b) iff sign2 (a op b)
			Expression atom = apply(atom2.getFunctor(), a, b);
			result = sign2? atom : Not.make(atom);
			break;
			
		case LESS_THAN:
			// X < a implies:
			// X = b, iff false (never implies a =)
			// X < b, iff a < b
			// X > b, iff false (never implies a >)
			// not (X = b), iff b >= a
			// not (X < b), that is, X >= b, iff false (never implies >=)
			// not (X > b), that is, X <= b, iff b >= a - 1
			if (sign2) {
				if (atom2.hasFunctor(LESS_THAN)) {
					result = apply(LESS_THAN, a, b);
				}
				else {
					result = FALSE;
				}
			}
			else {
				switch (atom2.getFunctor().toString()) {
				case EQUALITY:
					result = apply(GREATER_THAN_OR_EQUAL_TO, b, a);
					break;
				case LESS_THAN:
					result = FALSE;
					break;
				case GREATER_THAN:
					result = apply(GREATER_THAN_OR_EQUAL_TO, b, apply(MINUS, a, ONE));
					break;
				default:
					throw notNormalized(atom2);
				}
			}
			
			break;
			
		case GREATER_THAN:
			// Mirror image of LESS_THAN:
			if (sign2) {
				if (atom2.hasFunctor(GREATER_THAN)) {
					result = apply(GREATER_THAN, a, b);
				}
				else {
					result = FALSE;
				}
			}
			else {
				switch (atom2.getFunctor().toString()) {
				case EQUALITY:
					result = apply(LESS_THAN_OR_EQUAL_TO, b, a);
					break;
				case GREATER_THAN:
					result = FALSE;
					break;
				case LESS_THAN:
					result = apply(LESS_THAN_OR_EQUAL_TO, b, apply(PLUS, a, ONE));
					break;
				default:
					throw notNormalized(atom2);
				}
			}
			
			break;
			
		default:
			throw notNormalized(atom1);
		}

		return result;
	}

	private Expression getVariableFreeFormulaEquivalentToSign1Atom1ImpliesSign2Atom2NegativeAtom1Cases(Expression atom1, boolean sign2, Expression atom2, Context context) {

		Expression result;
		
		Expression a = atom1.get(1);
		Expression b = atom2.get(1);

		switch (atom1.getFunctor().toString()) {
	
		case EQUALITY: // sign1 is false, therefore sign1 atom1 is a DISEQUALITY
			// X != a implies:
			// X = b, iff false (does not imply)
			// X < b, iff false (does not imply)
			// X > b, iff false (does not imply)
			// not (X = b), iff a = b
			// not (X < b), iff false (does not imply)
			// not (X > b), iff false (does not imply)
			// That is, X != a implies (X = b) iff a = b
			Expression aEqualsB = apply(atom1.getFunctor(), a, b);
			result = !sign2 && atom2.hasFunctor(EQUALITY)? aEqualsB : FALSE;
			break;
			
		case LESS_THAN: // sign1 is false, therefore sign1 atom1 is GREATER_THAN_OR_EQUAL_TO
			// X >= a implies:
			// X = b, iff false
			// X < b, iff false
			// X > b, iff b < a
			// not (X = b), iff b < a
			// not (X < b), that is, X >= b, iff b <= a
			// not (X > b), that is, X <= b, iff false
			if (sign2) {
				if (atom2.hasFunctor(GREATER_THAN)) {
					result = apply(LESS_THAN, b, a);
				}
				else {
					result = FALSE;
				}
			}
			else {
				switch (atom2.getFunctor().toString()) {
				case EQUALITY:
					result = apply(LESS_THAN, b, a);
					break;
				case LESS_THAN:
					result = apply(LESS_THAN_OR_EQUAL_TO, b, a);
					break;
				case GREATER_THAN:
					result = FALSE;
					break;
				default:
					throw notNormalized(atom2);
				}
			}
			
			break;
			
		case GREATER_THAN:
			// Mirror image of LESS_THAN:
			if (sign2) {
				if (atom2.hasFunctor(LESS_THAN)) {
					result = apply(GREATER_THAN, b, a);
				}
				else {
					result = FALSE;
				}
			}
			else {
				switch (atom2.getFunctor().toString()) {
				case EQUALITY:
					result = apply(GREATER_THAN, b, a);
					break;
				case GREATER_THAN:
					result = apply(GREATER_THAN_OR_EQUAL_TO, b, a);
					break;
				case LESS_THAN:
					result = FALSE;
					break;
				default:
					throw notNormalized(atom2);
				}
			}
			
			break;
			
		default:
			throw notNormalized(atom1);
		}
	
		return result;
	}

	private Error notNormalized(Expression atom) {
		return new Error(getClass().getSimpleName() + ": got atom that is not normalized: " + atom);
	}

	@Override
	/**
	 * Implementation simply returns <code>this</code>,
	 * since typically implementations will have nothing to do besides pairwise atom contradiction checks.
	 */
	public AbstractSingleVariableConstraint destructiveUpdateOrNullAfterConjoiningNewNormalizedAtom(boolean sign, Expression atom, Context context) {
		return this;
	}
}