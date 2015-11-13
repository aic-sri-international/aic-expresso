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
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;
import static com.sri.ai.util.Util.getFirstSatisfyingPredicateOrNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.google.common.base.Function;
import com.google.common.collect.Multiset;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.number.Plus;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.core.constraint.AbstractSingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.AbstractSingleVariableConstraintWithDependentNormalizedAtoms;
import com.sri.ai.grinder.sgdpll2.theory.base.AbstractSingleVariableConstraintWithBinaryAtoms;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Triple;
import com.sri.ai.util.math.Rational;

/**
 * An inequalities on integers constraint solver.
 *
 * @author braz
 *
 */
@Beta
public class SingleVariableInequalityConstraint extends AbstractSingleVariableConstraintWithBinaryAtoms {

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
	
	@Override
	protected Expression isolateVariable(Expression atom, RewritingProcess process) {
		
		Function<Expression, Error> makeDuplicateError = duplicate -> new Error(atom + " is not a difference arithmetic atom because " + duplicate + " sums with itself, but no multiples are allowed in difference arithmetic");
		
		Triple<Multiset<Expression>, Multiset<Expression>, Integer>
		items0 = Plus.gatherPositiveAndNegativeTermsAndConstantInteger(atom.get(0), makeDuplicateError);
		
		Triple<Multiset<Expression>, Multiset<Expression>, Integer>
		items1 = Plus.gatherPositiveAndNegativeTermsAndConstantInteger(atom.get(1), makeDuplicateError);
		
		Triple<Set<Expression>, Set<Expression>, Integer> result1 = subtractDifferenceLogicTriples(items0, items1, makeDuplicateError);
		
		Set<Expression> positiveVariables = result1.first;
		Set<Expression> negativeVariables = result1.second;
		int constant = result1.third.intValue();
		
		// now isolate variable:
		
		// create array for all the arguments of the sum that is going to be on the side opposite to the variable
		ArrayList<Expression> sumArguments = new ArrayList<Expression>(positiveVariables.size() + negativeVariables.size() -1 + 1); // minus variable, plus constant
		Expression result;
		if (positiveVariables.contains(getVariable())) {
			for (Expression positiveVariable : positiveVariables) { // variable will be on left-hand side: invert signs of everybody else (negative variables become positive)
				if ( ! positiveVariable.equals(getVariable())) {
					sumArguments.add(apply(MINUS, positiveVariable));
				}
			}
			for (Expression negativeVariable : negativeVariables) {
				sumArguments.add(negativeVariable);
			}
			sumArguments.add(makeSymbol(-constant));

			Expression oppositeSide = Plus.make(sumArguments);
			result = apply(atom.getFunctor(), getVariable(), oppositeSide);
		}
		else {
			for (Expression positiveVariable : positiveVariables) { // variable will be on right-hand side: everybody else stays on left-hand side and keeps their sign (negative variables get the negative sign in their representation)
				sumArguments.add(positiveVariable);
			}
			for (Expression negativeVariable : negativeVariables) {
				if ( ! negativeVariable.equals(getVariable())) {
					sumArguments.add(apply(MINUS, negativeVariable));
				}
			}
			sumArguments.add(makeSymbol(constant));
			
			Expression oppositeSide = Plus.make(sumArguments);
			result = apply(atom.getFunctor(), oppositeSide, getVariable());
		}
		
//		System.out.println("\nAtom: " + atom);	
//		System.out.println("Result: " + result);
		
		return result;
	}

	/**
	 * Given two difference arithmetic tuples, each containing positive and negative terms and a numeric constant in a summation,
	 * returns another tuple of the same form representing their subtraction,
	 * or throws an Error if any of the terms appears with the same final sign multiple times
	 * (which would require representing a multiple of it), such as in ({X}, {}, 1) - ({}, {X}, 2)
	 * which would result in 2*X - 1.
	 * @param positiveAndNegativeTermsAndConstant1
	 * @param positiveAndNegativeTermsAndConstant2
	 * @param makeDuplicateError a function getting the offending duplicate term and returning an Error to be thrown.
	 * @return
	 * @throws Error
	 */
	public static Triple<Set<Expression>, Set<Expression>, Integer> subtractDifferenceLogicTriples(Triple<Multiset<Expression>, Multiset<Expression>, Integer> positiveAndNegativeTermsAndConstant1, Triple<Multiset<Expression>, Multiset<Expression>, Integer> positiveAndNegativeTermsAndConstant2, Function<Expression, Error> makeDuplicateError) throws Error {
		// get positiveAndNegativeTermsAndConstant2 on left-hand-side with opposite sign:
		Triple<Multiset<Expression>, Multiset<Expression>, Integer>
		invertedItems1 = Triple.make(positiveAndNegativeTermsAndConstant2.second, positiveAndNegativeTermsAndConstant2.first, - positiveAndNegativeTermsAndConstant2.third.intValue());
		
		Set<Expression> positiveVariables = new LinkedHashSet<>();
		positiveVariables.addAll(positiveAndNegativeTermsAndConstant1.first);
		positiveVariables.addAll(invertedItems1.first);
		if (positiveVariables.size() != positiveAndNegativeTermsAndConstant1.first.size() + invertedItems1.first.size()) { // some variable appears twice
			Expression intersection = getFirstSatisfyingPredicateOrNull(positiveAndNegativeTermsAndConstant1.first, e -> invertedItems1.first.contains(e));
			throw makeDuplicateError.apply(intersection);
		}
		
		Set<Expression> negativeVariables = new LinkedHashSet<>();
		negativeVariables.addAll(positiveAndNegativeTermsAndConstant1.second);
		negativeVariables.addAll(invertedItems1.second);
		if (negativeVariables.size() != positiveAndNegativeTermsAndConstant1.second.size() + invertedItems1.second.size()) { // some variable appears twice
			Expression intersection = getFirstSatisfyingPredicateOrNull(positiveAndNegativeTermsAndConstant1.second, e -> invertedItems1.second.contains(e));
			throw makeDuplicateError.apply(intersection);
		}
		
		int constant = positiveAndNegativeTermsAndConstant1.third.intValue() + invertedItems1.third.intValue();

		Triple<Set<Expression>, Set<Expression>, Integer> result = Triple.make(positiveVariables, negativeVariables, constant);
		return result;
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

	private Error notNormalized(Expression atom) {
		return new Error(getClass().getSimpleName() + ": got atom that is not normalized: " + atom);
	}

	@Override
	public AbstractSingleVariableConstraint destructiveUpdateOrNullAfterInsertingNewNormalizedAtom(boolean sign, Expression atom, RewritingProcess process) {
		return this;
	}
}