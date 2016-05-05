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
import static com.sri.ai.expresso.helper.Expressions.INFINITY;
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
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.IntegerExpressoType;
import com.sri.ai.expresso.type.IntegerInterval;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.number.UnaryMinus;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.core.constraint.AbstractSingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.theory.base.AbstractSingleVariableConstraintWithBinaryAtomsIncludingEquality;
import com.sri.ai.util.Util;

/**
 * A difference arithmetic constraint solver.
 *
 * @author braz
 *
 */
@Beta
public class SingleVariableDifferenceArithmeticConstraint extends AbstractSingleVariableConstraintWithBinaryAtomsIncludingEquality {

	// these two methods are kept first in the class because they heavily depend
	// on which super class we are using, so it is good to keep them near the class declaration
	
	@Override
	protected boolean conjoiningRedundantSignAndNormalizedAtomNeverChangesConstraintInstance() {
		boolean result = ! propagateAllLiteralsWhenVariableIsBound();
		return result;
		// Explanation: once we propagate incoming literals, we analyse them less and don't
		// necessarily detect redundancies between them.
		// We may produce multiple external literals that may be redundant between themselves
		// but whose redundancies will only be detected when they are themselves analysed
		// in their own constraints.
	}
	
	private static boolean propagateAllLiteralsWhenVariableIsBound() {
		boolean result = SingleVariableDifferenceArithmeticConstraint.class
		.getGenericSuperclass()
		.equals(AbstractSingleVariableConstraintWithBinaryAtomsIncludingEquality.class);
		return result;
	}

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

	public SingleVariableDifferenceArithmeticConstraint(
			Expression variable,
			boolean propagateAllLiteralsWhenVariableIsBound,
			ConstraintTheory constraintTheory) {
		
		super(variable, propagateAllLiteralsWhenVariableIsBound, constraintTheory);
	}

	private SingleVariableDifferenceArithmeticConstraint(
			Expression variable,
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals,
			boolean propagateAllLiteralsWhenVariableIsBound,
			ConstraintTheory constraintTheory) {
		
		super(variable, positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, propagateAllLiteralsWhenVariableIsBound, constraintTheory);
	}

	public SingleVariableDifferenceArithmeticConstraint(SingleVariableDifferenceArithmeticConstraint other) {
		super(other);
	}

	@Override
	protected SingleVariableDifferenceArithmeticConstraint makeSimplification(ArrayList<Expression> positiveNormalizedAtoms, ArrayList<Expression> negativeNormalizedAtoms, List<Expression> externalLiterals) {
		SingleVariableDifferenceArithmeticConstraint result = new SingleVariableDifferenceArithmeticConstraint(getVariable(), positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, getPropagateAllLiteralsWhenVariableIsBound(), getConstraintTheory());
		return result;
	}

	@Override
	public SingleVariableDifferenceArithmeticConstraint clone() {
		SingleVariableDifferenceArithmeticConstraint result = new SingleVariableDifferenceArithmeticConstraint(this);
		return result;
	}

	@Override
	protected Collection<String> getNormalFunctors() {
		return normalFunctors;
	}
	
	@Override
	protected String getNegationFunctor(String functor) {
		String result = DifferenceArithmeticConstraintTheory.getNegationFunctor(functor);
		return result;
	}
	
	@Override
	protected String getFlipFunctor(String functor) {
		String result = flipFunctor.get(functor);
		return result;
	}
	
	@Override
	protected Expression isolateVariable(Expression atom, Context context) {
		Expression result = DifferenceArithmeticSimplifier.isolateVariable(getVariable(), atom);
//		System.out.println("\nAtom: " + atom);	
//		System.out.println("Result: " + result);
		return result;
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
	public AbstractSingleVariableConstraint destructiveUpdateOrNullAfterInsertingNewNormalizedAtom(boolean sign, Expression atom, Context context) {
		return this;
	}

	@Override
	protected Iterator<Expression> getImplicitPositiveNormalizedAtomsIterator(Context context) {
		return iterator();
	}

	List<Expression> cachedImplicitNegativeNormalizedAtoms;
	@Override
	protected Iterator<Expression> getImplicitNegativeNormalizedAtomsIterator(Context context) {
		if (cachedImplicitNegativeNormalizedAtoms == null) {
			IntegerInterval interval = getType(context);
			Expression nonStrictLowerBound = interval.getNonStrictLowerBound();
			Expression nonStrictUpperBound = interval.getNonStrictUpperBound();
			cachedImplicitNegativeNormalizedAtoms = list();
			if (!nonStrictLowerBound.equals("unknown") && !nonStrictLowerBound.equals(UnaryMinus.make(INFINITY))) {
				cachedImplicitNegativeNormalizedAtoms.add(apply(LESS_THAN, getVariable(), nonStrictLowerBound));
				// this is the negation of variable >= nonStrictLowerBound. We need to use a negative normalized atom because applications of >= are not considered normalized atoms
			}
			if (!nonStrictUpperBound.equals("unknown") && !nonStrictUpperBound.equals(INFINITY)) {
				cachedImplicitNegativeNormalizedAtoms.add(apply(GREATER_THAN, getVariable(), nonStrictUpperBound));
				// this is the negation of variable <= nonStrictUpperBound. We need to use a negative normalized atom because applications of <= are not considered normalized atoms
			}
		}
		return cachedImplicitNegativeNormalizedAtoms.iterator();
	}

	private IntegerInterval cachedType;
	
	/**
	 * Returns the {@link IntegerInterval} type of the constraint's variable.
	 * @param context
	 * @return
	 */
	public IntegerInterval getType(Context context) {
		if (cachedType == null) {
			Type type = context.getType(getVariableTypeExpression(context));
			if (type instanceof IntegerExpressoType) {
				cachedType = new IntegerInterval("-infinity..infinity");
			}
			else {
				cachedType = (IntegerInterval) type;
			}
		}
		return cachedType ;
	}
}