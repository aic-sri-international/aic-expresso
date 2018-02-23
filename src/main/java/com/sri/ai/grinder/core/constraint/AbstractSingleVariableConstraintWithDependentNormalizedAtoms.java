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
package com.sri.ai.grinder.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.removeFromArrayListNonDestructively;
import static com.sri.ai.util.Util.thereExists;
import static com.sri.ai.util.collect.NestedIterator.nestedIterator;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.util.Util;

/**
 * An implementation of {@link AbstractSingleVariableConstraint} for theories
 * in which there is interaction between normalized atoms.
 * It provides functionality to check contradiction and redundancies among them, 
 * based on {@link #getVariableFreeLiteralEquivalentToSign1Atom1ImpliesSign2Atom2(boolean, Expression, boolean, Expression, Context)},
 * to be provided by extensions,
 * which must produce a literal not involving the constraint variable
 * that is equivalent to whether a sign and normalized atom pair implies another.
 * For example, in equality theory, <code>X = Y</code> implies <code>X != Z</code> if and only if <code>Y != Z<code>.
 * These formula is then stored as an external literal and <code>X != Z</code> does not need to be stored.
 * The condition <code>Y != Z</code> may then be used later by other code to decide the satisfiability of this constraint.
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleVariableConstraintWithDependentNormalizedAtoms extends AbstractSingleVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	/**
	 * Returns an iterator to positive normalized atoms, including implicit ones
	 * (for example, for bounded integers, the type bounds on the variable).
	 * Default implementation is <code>nestedIterator(getPositiveNormalizedAtoms().iterator(), getImplicitPositiveNormalizedAtomsIterator(context))</code>.
	 * @return
	 */
	protected Iterator<Expression> getPositiveNormalizedAtomsIncludingImplicitOnes(Context context) {
		return nestedIterator(getPositiveNormalizedAtoms().iterator(), getImplicitPositiveNormalizedAtomsIterator(context));
	}

	/**
	 * Returns an iterator to negative normalized atoms, including implicit ones
	 * (for example, for bounded integers, the type bounds on the variable).
	 * Default implementation is <code>nestedIterator(getNegativeNormalizedAtoms().iterator(), getImplicitNegativeNormalizedAtomsIterator(context))</code>.
	 * @return
	 */
	protected Iterator<Expression> getNegativeNormalizedAtomsIncludingImplicitOnes(Context context) {
		return nestedIterator(getNegativeNormalizedAtoms().iterator(), getImplicitNegativeNormalizedAtomsIterator(context));
	}

	/**
	 * Returns an iterator over positive normalized atoms not actually represented in constraint,
	 * but assumed for purposes of determining redundancy or contradiction of new normalized atoms.
	 * For example, if an extension defines a difference arithmetic theory,
	 * and the constraint's variable X is in an integer interval ]n, model[,
	 * then having this method return X > n and X < model will cause the solver to
	 * act as if these atoms where actually part of the constraint,
	 * which will result in contradicting normalized atoms
	 * stating that X is out of these bounds, and making redundant normalized atoms saying X is within these bounds.
	 * @param context
	 * @return an iterator over implicit normalized atoms.
	 * @see #getImplicitNegativeNormalizedAtomsIterator(Context)
	 */
	abstract protected Iterator<Expression> getImplicitPositiveNormalizedAtomsIterator(Context context);

	/**
	 * Analogous to {@link #getImplicitPositiveNormalizedAtomsIterator(Context)}.
	 * @param context
	 * @return
	 */
	abstract protected Iterator<Expression> getImplicitNegativeNormalizedAtomsIterator(Context context);

	public AbstractSingleVariableConstraintWithDependentNormalizedAtoms(Expression variable, Theory theory) {
		this(variable, Util.arrayList(), Util.arrayList(), Util.arrayList(), theory);
	}
	
	public AbstractSingleVariableConstraintWithDependentNormalizedAtoms(
			Expression variable,
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals,
			Theory theory) {
		
		super(variable, positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, theory);
	}

	protected AbstractSingleVariableConstraintWithDependentNormalizedAtoms(AbstractSingleVariableConstraintWithDependentNormalizedAtoms other) {
		super(other);
	}
	
	//////////// THEORY RULES
	
	/**
	 * Conjoin a literal described by sign and normalized atom;
	 * this may be a useful hook for extending classes to intercept in order to perform theory-specific processing,
	 * while having access to the literal already broken down into sign and normalized atom.
	 * @param sign
	 * @param normalizedAtom
	 * @param context
	 * @return
	 */
	@Override
	protected AbstractSingleVariableConstraintWithDependentNormalizedAtoms conjoinNonTrivialSignAndNormalizedAtom(boolean sign, Expression normalizedAtom, Context context) {
		
		AbstractSingleVariableConstraintWithDependentNormalizedAtoms result;
		// OPTIMIZATION
		// Here it would pay to have the database of atoms to be indexed by theory-specific properties
		// such that only relevant atoms are checked, depending on properties of the new literal.
		// For example, in equality theory,
		// X = a can only make redundant literals X != T1 for T1 some distinct constant,
		// and can only be contradictory with X = T2 for T2 some distinct constant,
		// while equalities between two variables never affect literals based on distinct atoms.
		
		boolean oppositeSign = sign? false : true;
		if (
				thereExists(
						getPositiveNormalizedAtomsIncludingImplicitOnes(context),
						p -> impliesLiteralWithDifferentNormalizedAtom(true,  p, sign, normalizedAtom, context))
						||
						thereExists(
								getNegativeNormalizedAtomsIncludingImplicitOnes(context),
								p -> impliesLiteralWithDifferentNormalizedAtom(false, p, sign, normalizedAtom, context))) {
			
			result = this; // redundant
		}
		else if (
				thereExists(
						getPositiveNormalizedAtomsIncludingImplicitOnes(context),
						p -> impliesLiteralWithDifferentNormalizedAtom(true,  p, oppositeSign, normalizedAtom, context))
						||
						thereExists(
								getNegativeNormalizedAtomsIncludingImplicitOnes(context),
								p -> impliesLiteralWithDifferentNormalizedAtom(false, p, oppositeSign, normalizedAtom, context))) {
			
			result = makeContradiction(); // contradiction
		}
		else {
			// remove redundant literals and add new one
			ArrayList<Expression> newPositiveNormalizedAtoms = 
					removeFromArrayListNonDestructively(
							getPositiveNormalizedAtoms(),
							p -> impliesLiteralWithDifferentNormalizedAtom(sign, normalizedAtom, true,  p, context));
			ArrayList<Expression> newNegativeNormalizedAtoms = 
					removeFromArrayListNonDestructively(
							getNegativeNormalizedAtoms(),
							p -> impliesLiteralWithDifferentNormalizedAtom(sign, normalizedAtom, false, p, context));
			
			if (sign) {
				newPositiveNormalizedAtoms.add(normalizedAtom);
			}
			else {
				newNegativeNormalizedAtoms.add(normalizedAtom);
			}
			result =
					(AbstractSingleVariableConstraintWithDependentNormalizedAtoms)
					setPositiveAndNegativeNormalizedAtoms(newPositiveNormalizedAtoms, newNegativeNormalizedAtoms);
		}
		return result;
	}

	/**
	 * @return
	 */
	@Override
	public AbstractSingleVariableConstraintWithDependentNormalizedAtoms makeContradiction() {
		return (AbstractSingleVariableConstraintWithDependentNormalizedAtoms) super.makeContradiction();
	}

	/**
	 * Indicates whether, according to the current theory, sign1 atom1 implies sign2 atom2,
	 * where atom1 and atom2 can be assumed distinct (the result is not defined otherwise).
	 * Remember that the notion of "atom" is specific to this constraint and variable.
	 * @param sign1
	 * @param atom1
	 * @param sign2
	 * @param atom2
	 * @param context
	 * @return
	 */
	private boolean impliesLiteralWithDifferentNormalizedAtom(boolean sign1, Expression atom1, boolean sign2, Expression atom2, Context context) {
		Expression sign1Atom1ImpliesSign2Atom2 = getVariableFreeLiteralEquivalentToSign1Atom1ImpliesSign2Atom2(sign1, atom1, sign2, atom2, context);
		boolean result = sign1Atom1ImpliesSign2Atom2.equals(TRUE);
		return result;
		
	}

	/**
	 * Must return a simplified literal, in which the constraint variable does not occur,
	 * equivalent to whether sign1 atom1 implies sign2 atom2,
	 * according to the constraint's theory.
	 * This method provides the basis for simplifications in the constraint as well as
	 * literal propagation.
	 *  
	 * @param sign1
	 * @param atom1
	 * @param sign2
	 * @param atom2
	 * @param context
	 * @return
	 */
	abstract protected Expression getVariableFreeLiteralEquivalentToSign1Atom1ImpliesSign2Atom2(boolean sign1, Expression atom1, boolean sign2, Expression atom2, Context context);
}