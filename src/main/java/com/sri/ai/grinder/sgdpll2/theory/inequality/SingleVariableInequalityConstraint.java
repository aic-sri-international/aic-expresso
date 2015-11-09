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
package com.sri.ai.grinder.sgdpll2.theory.inequality;

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

import java.util.Collection;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.core.constraint.AbstractSingleVariableConstraintWithDependentNormalizedAtoms;
import com.sri.ai.grinder.sgdpll2.theory.equality.SingleVariableEqualityConstraint;
import com.sri.ai.util.Util;
import com.sri.ai.util.math.Rational;

/**
 * An inequalities on integers constraint solver.
 * <p>
 * This extends {@link SingleVariableEqualityConstraint} so that it inherits the
 * functionality of that class of counting disequalities to unique constraints
 * and detecting inconsistency when that number becomes equal to the variable's type size.
 * However, while that functionality ensures completeness for {@link SingleVariableEqualityConstraint}s,
 * it does not suffice in the presence of inequalities.
 * <p>
 * For example, <code>X > 10 and X != 11 and X != 12 and X < 13</code> is unsatisfiable,
 * but this will not be detected by this class because the number of disequalities
 * to unique constants is only 2 (while the type of X could be much larger).
 * Trying to detect such inconsistencies would make the time complexity of conjoining a literal
 * greater than linear, so we choose to leave this type of detection for when it is
 * absolutely required.
 * <p>
 * 
 * @author braz
 *
 */
@Beta
public class SingleVariableInequalityConstraint extends SingleVariableEqualityConstraint {

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

	public SingleVariableInequalityConstraint(Expression variable, ConstraintTheory constraintTheory) {
		super(variable, constraintTheory);
	}

	public SingleVariableInequalityConstraint(SingleVariableInequalityConstraint other) {
		super(other);
	}

	@Override
	public SingleVariableInequalityConstraint clone() {
		SingleVariableInequalityConstraint result = new SingleVariableInequalityConstraint(this);
		return result;
	}

	@Override
	protected Collection<String> getNormalFunctors() {
		return normalFunctors;
	}
	
	@Override
	protected String getNegationFunctor(String functor) {
		String result = InequalityConstraintTheory.negationFunctor.get(functor);
		return result;
	}
	
	@Override
	protected String getFlipFunctor(String functor) {
		String result = flipFunctor.get(functor);
		return result;
	}
	
	private Error notNormalized(Expression atom) {
		return new Error(getClass().getSimpleName() + ": got atom that is not normalized: " + atom);
	}

	/**
	 * We override this method to check whether normalized atom violates the bounds of the constraint variable type,
	 * before checking it against all the atoms already in the constraint.
	 */
	@Override
	protected AbstractSingleVariableConstraintWithDependentNormalizedAtoms conjoinNonTrivialSignAndNormalizedAtom(boolean sign, Expression normalizedAtom, RewritingProcess process) {
		if (normalizedAtom.get(1).getValue() instanceof Number) {
			Rational value = (Rational) normalizedAtom.get(1).getValue();
			if (
					(   sign && normalizedAtom.hasFunctor(LESS_THAN)    &&  value.compareTo(0) <= 0)
					||
					(   sign && normalizedAtom.hasFunctor(GREATER_THAN) &&  value.compareTo(9) >= 0)
					||
					(   sign && normalizedAtom.hasFunctor(EQUALITY)     && (value.compareTo(0) < 0 || value.compareTo(0) > 9) )
					||
					( ! sign && normalizedAtom.hasFunctor(LESS_THAN)    &&  value.compareTo(9) > 0)
					||
					( ! sign && normalizedAtom.hasFunctor(GREATER_THAN) &&  value.compareTo(0) < 0)
					) {
				return null;
			}
		}
	
		return super.conjoinNonTrivialSignAndNormalizedAtom(sign, normalizedAtom, process);
	}

	public Expression getVariableFreeLiteralEquivalentToSign1Atom1ImpliesSign2Atom2(boolean sign1, Expression atom1, boolean sign2, Expression atom2, RewritingProcess process) {
		Expression result;
		
		if (sign1) {
			result = getVariableFreeFormulaEquivalentToSign1Atom1ImpliesSign2Atom2PositiveAtom1Cases(atom1, sign2, atom2, process);
		}
		else {
			result = getVariableFreeFormulaEquivalentToSign1Atom1ImpliesSign2Atom2NegativeAtom1Cases(atom1, sign2, atom2, process);
		}
		
		return result;
	}

	private Expression getVariableFreeFormulaEquivalentToSign1Atom1ImpliesSign2Atom2PositiveAtom1Cases(Expression atom1, boolean sign2, Expression atom2, RewritingProcess process) throws Error {

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

	private Expression getVariableFreeFormulaEquivalentToSign1Atom1ImpliesSign2Atom2NegativeAtom1Cases(Expression atom1, boolean sign2, Expression atom2, RewritingProcess process) {

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
}