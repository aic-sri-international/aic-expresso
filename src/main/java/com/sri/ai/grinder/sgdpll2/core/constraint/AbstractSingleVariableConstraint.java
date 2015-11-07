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
package com.sri.ai.grinder.sgdpll2.core.constraint;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.contains;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.removeFromArrayListNonDestructively;
import static com.sri.ai.util.Util.thereExists;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.Pair;

/**
 * An abstract {@link Expression} with efficient internal representation for a conjunction in a given theory.
 * It does the basic work of weeding out redundant literals and detecting syntactic contradictions
 * (based simply on atom identity).
 * Furthermore, it can use implication between literals of different atoms,
 * defined by extensions knowledgeable of specific theories
 * in order to detect further redundancies and inconsistencies,
 * thus decreasing the effort to define theory solvers. 
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleVariableConstraint extends AbstractExpressionWrapper implements SingleVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private Expression variable;
	private ArrayList<Expression> positiveNormalizedAtoms;
	private ArrayList<Expression> negativeNormalizedAtoms;
	private ArrayList<Expression> externalLiterals; // literals not on variable
	private ConstraintTheory constraintTheory;
	
	public AbstractSingleVariableConstraint(Expression variable, ConstraintTheory constraintTheory) {
		this(variable, Util.arrayList(), Util.arrayList(), Util.arrayList(), constraintTheory);
	}
	
	public AbstractSingleVariableConstraint(Expression variable, ArrayList<Expression> positiveNormalizedAtoms, ArrayList<Expression> negativeNormalizedAtoms,
			ArrayList<Expression> externalLiterals, ConstraintTheory constraintTheory) {
		this.variable = variable;
		this.positiveNormalizedAtoms = positiveNormalizedAtoms;
		this.negativeNormalizedAtoms = negativeNormalizedAtoms;
		this.externalLiterals = externalLiterals;
		this.constraintTheory = constraintTheory;
	}

	/**
	 * A copy constructor, defined for the benefit of having a clone method defined.
	 * The reason we clone is that we want to create a copy of a constraint in other methods
	 * in this class, and it has to be a copy of whatever extending class is using those methods,
	 * so the copy constructor would not be enough (it cannot be used in that manner because this class
	 * is abstract). To define clone in extending classes, however, it does not work to use
	 * Object.clone implementation because not all fields are cloneable and an exception is thrown.
	 * Instead, the extending class needs to define its own clone method and use a copy constructor
	 * (of its own, since the clone has to be an instance of the extending class).
	 * But a copy constructor of the extending class does not have access to the private fields
	 * of this class, so we need to define this copy constructor to be used by the extending class's
	 * copy constructor.
	 * @param other
	 */
	protected AbstractSingleVariableConstraint (AbstractSingleVariableConstraint other) {
		this.variable = other.variable;
		this.positiveNormalizedAtoms = other.positiveNormalizedAtoms;
		this.negativeNormalizedAtoms = other.negativeNormalizedAtoms;
		this.externalLiterals = other.externalLiterals;
		this.constraintTheory = other.constraintTheory;
	}
	
	@Override
	abstract public AbstractSingleVariableConstraint clone();
	
	//////////// GETTERS
	
	public List<Expression> getPositiveAtoms() {
		return Collections.unmodifiableList(positiveNormalizedAtoms);
	}

	public List<Expression> getNegativeAtoms() {
		return Collections.unmodifiableList(negativeNormalizedAtoms);
	}

	public List<Expression> getExternalLiterals() {
		return Collections.unmodifiableList(externalLiterals);
	}

	@Override
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}

	//////////// THEORY RULES
	
	/**
	 * Defines how a literal is decomposed into sign and atom.
	 * Atom representations should be normalized (equivalent atoms should always be represented by the same expressions).
	 * For example, <code>X != a</code> could be decomposed into <code>false</code> and <code>X = a</code>.
	 * Note that the notion of "atom" is internal to a single-variable constraint with a specific variable.
	 * Other constraints may produce different atoms from the same literals;
	 * for example, <code>X = Y</code> may be normalized into <code>X = Y</code> for a constraint
	 * on <code>X</code>, and to <code>Y = X</code> for a constraint on <code>Y</code>.
	 * @param variable
	 * @param literal
	 * @param process TODO
	 * @return
	 */
	protected abstract Pair<Boolean, Expression> fromLiteralOnVariableToSignAndNormalizedAtom(Expression variable, Expression literal, RewritingProcess process);

	/**
	 * Returns the literal corresponding to the negation of the given normalized atom.
	 * Typically this will just return the application of NOT to the atom,
	 * but some languages may have more conventional (and more pleasing to read) ways of representing these
	 * negations (for example, not(X = a) -> X != a).
	 * @param normalizedAtom
	 * @return
	 */
	abstract public Expression fromNormalizedAtomToItsNegationAsLiteral(Expression normalizedAtom);

	/** Indicates whether there are interactions between distinct atoms (for this constraint and variable). */
	abstract public boolean normalizedAtomMayImplyLiteralsOnDifferentAtoms();

	/**
	 * Indicates whether, according to the current theory, sign1 atom1 implies sign2 atom2,
	 * where atom1 and atom2 can be assumed distinct (the result is not defined otherwise).
	 * Remember that the notion of "atom" is specific to this constraint and variable.
	 * @param sign1
	 * @param atom1
	 * @param sign2
	 * @param atom2
	 * @param process
	 * @return
	 */
	abstract public boolean impliesLiteralWithDifferentNormalizedAtom(boolean sign1, Expression atom1, boolean sign2, Expression atom2, RewritingProcess process);

	public AbstractSingleVariableConstraint copyWithNewPositiveNormalizedAtom(Expression atom) {
		AbstractSingleVariableConstraint result = clone();
		ArrayList<Expression> newPositiveNormalizedAtoms = new ArrayList<Expression>(positiveNormalizedAtoms);
		newPositiveNormalizedAtoms.add(atom);
		result.positiveNormalizedAtoms = newPositiveNormalizedAtoms;
		return result;
	}

	public AbstractSingleVariableConstraint copyWithNewNegativeNormalizedAtom(Expression atom) {
		AbstractSingleVariableConstraint result = clone();
		ArrayList<Expression> newNegativeNormalizedAtoms = new ArrayList<Expression>(negativeNormalizedAtoms);
		newNegativeNormalizedAtoms.add(atom);
		result.negativeNormalizedAtoms = newNegativeNormalizedAtoms;
		return result;
	}

	public AbstractSingleVariableConstraint copyWithNewExternalLiteral(Expression newExternalLiteral) {
		AbstractSingleVariableConstraint result = clone();
		ArrayList<Expression> newExternalLiterals = new ArrayList<Expression>(externalLiterals);
		newExternalLiterals.add(newExternalLiteral);
		result.externalLiterals = newExternalLiterals;
		return result;
	}

	public AbstractSingleVariableConstraint copyWithNewPositiveAndNegativeNormalizedAtoms(ArrayList<Expression> newPositiveNormalizedAtoms, ArrayList<Expression> newNegativeNormalizedAtoms) {
		AbstractSingleVariableConstraint result = clone();
		result.positiveNormalizedAtoms = newPositiveNormalizedAtoms;
		result.negativeNormalizedAtoms = newNegativeNormalizedAtoms;
		return result;
	}
	
	@Override
	public Expression getVariable() {
		return variable;
	}

	/**
	 * Extending classes define this method to perform whatever checks are needed
	 * after a new atom has been inserted, returning null in case of a detected contradiction
	 * or this object, or a simplified version of it, otherwise.
	 * <p>
	 * In general, this will contain whatever theory-specific checks and simplifications that go beyond
	 * simple contradiction between normalized atoms.
	 * <p>
	 * One example of this method being useful is in equality theory with finite domains.
	 * If a variable is constrained to be disequal to <code>n</code> distinct values
	 * and its type size is equal or less than <code>n</code>, we know the constraint
	 * is unsatisfiable.
	 * Because {@link AbstractSingleVariableConstraint} knows nothing about equalities,
	 * it will not detect that.
	 * An extension defining equality constraints can define this method to perform such check.
	 * 
	 * @param sign
	 * @param atom
	 * @param process 
	 * @return 
	 */
	abstract public AbstractSingleVariableConstraint destructiveUpdateOrNullAfterInsertingNewNormalizedAtom(boolean sign, Expression atom, RewritingProcess process);

	@Override
	public SingleVariableConstraint conjoinWithLiteral(Expression formula, RewritingProcess process) {
		AbstractSingleVariableConstraint result;
		if (formula.equals(TRUE)) {
			result = this;
		}
		else if (formula.equals(FALSE)) {
			result = null;
		}
		else if (!contains(formula, getVariable(), process)) {
			result = copyWithNewExternalLiteral(formula);
		}
		else {
			Pair<Boolean, Expression> signAndNormalizedAtom = fromLiteralOnVariableToSignAndNormalizedAtom(getVariable(), formula, process);
			boolean    sign = signAndNormalizedAtom.first;
			Expression normalizedAtom = signAndNormalizedAtom.second;
			ArrayList<Expression>     sameSignNormalizedAtoms = sign? positiveNormalizedAtoms : negativeNormalizedAtoms;
			ArrayList<Expression> oppositeSignNormalizedAtoms = sign? negativeNormalizedAtoms : positiveNormalizedAtoms;
			if (sameSignNormalizedAtoms.contains(normalizedAtom)) {
				result = this; // redundant
			}
			else if (oppositeSignNormalizedAtoms.contains(normalizedAtom)) {
				result = null; // contradiction
			}
			else {
				result = conjoinNonTrivialSignAndNormalizedAtom(sign, normalizedAtom, process);
			}
		}
		return result;
	}

	/**
	 * Conjoin a literal described by sign and normalized atom;
	 * this may be a useful hook for extending classes to intercept in order to perform theory-specific processing,
	 * while having access to the literal already broken down into sign and normalized atom.
	 * @param sign
	 * @param normalizedAtom
	 * @param process
	 * @return
	 */
	protected AbstractSingleVariableConstraint conjoinNonTrivialSignAndNormalizedAtom(boolean sign, Expression normalizedAtom, RewritingProcess process) {
		AbstractSingleVariableConstraint result;
		if (normalizedAtomMayImplyLiteralsOnDifferentAtoms()) {
			result = conjoinNonTrivialPossiblyDependentSignAndNormalizedAtom(sign, normalizedAtom, process);
		}
		else {
			result = conjoinNonTrivialIndependentSignAndNormalizedAtom(sign, normalizedAtom, process);
		}
		return result;
	}

	/**
	 * @param sign
	 * @param normalizedAtom
	 * @param process
	 * @return
	 */
	private AbstractSingleVariableConstraint conjoinNonTrivialPossiblyDependentSignAndNormalizedAtom(boolean sign, Expression normalizedAtom, RewritingProcess process) {
		AbstractSingleVariableConstraint result;
		// OPTIMIZATION
		// Here it would pay to have the database of atoms to be indexed by theory-specific properties
		// such that only relevant atoms are checked, depending on properties of the new literal.
		// For example, in equality theory,
		// X = a can only make redundant literals X != T1 for T1 some distinct constant,
		// and can only be contradictory with X = T2 for T2 some distinct constant,
		// while equalities between two variables never affect literals based on distinct atoms.
		
		boolean oppositeSign = sign? false : true;
		if (    thereExists(positiveNormalizedAtoms, p -> impliesLiteralWithDifferentNormalizedAtom(true,  p, sign, normalizedAtom, process)) ||
				thereExists(negativeNormalizedAtoms, p -> impliesLiteralWithDifferentNormalizedAtom(false, p, sign, normalizedAtom, process))) {
			result = this; // redundant
		}
		else if (thereExists(positiveNormalizedAtoms, p -> impliesLiteralWithDifferentNormalizedAtom(true,  p, oppositeSign, normalizedAtom, process)) ||
				 thereExists(negativeNormalizedAtoms, p -> impliesLiteralWithDifferentNormalizedAtom(false, p, oppositeSign, normalizedAtom, process))) {
			result = null; // contradiction
		}
		else {
			// remove redundant literals and add new one
			ArrayList<Expression> newPositiveAtoms = 
					removeFromArrayListNonDestructively(positiveNormalizedAtoms, p -> impliesLiteralWithDifferentNormalizedAtom(sign, normalizedAtom, true,  p, process));
			ArrayList<Expression> newNegativeAtoms = 
					removeFromArrayListNonDestructively(negativeNormalizedAtoms, p -> impliesLiteralWithDifferentNormalizedAtom(sign, normalizedAtom, false, p, process));
			if (sign) {
				newPositiveAtoms.add(normalizedAtom);
			}
			else {
				newNegativeAtoms.add(normalizedAtom);
			}
			result = copyWithNewPositiveAndNegativeNormalizedAtoms(newPositiveAtoms, newNegativeAtoms);
			result = result.destructiveUpdateOrNullAfterInsertingNewNormalizedAtom(sign, normalizedAtom, process);
		}
		return result;
	}

	/**
	 * @param sign
	 * @param normalizedAtom
	 * @param process
	 * @return
	 */
	private AbstractSingleVariableConstraint conjoinNonTrivialIndependentSignAndNormalizedAtom(boolean sign, Expression normalizedAtom, RewritingProcess process) {
		AbstractSingleVariableConstraint result;
		if (sign) {
			result = copyWithNewPositiveNormalizedAtom(normalizedAtom);
			result = result.destructiveUpdateOrNullAfterInsertingNewNormalizedAtom(sign, normalizedAtom, process);
		}
		else {
			result = copyWithNewNegativeNormalizedAtom(normalizedAtom);
			result = result.destructiveUpdateOrNullAfterInsertingNewNormalizedAtom(sign, normalizedAtom, process);
		}
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> conjuncts = list();
		conjuncts.addAll(positiveNormalizedAtoms);
		Util.mapIntoList(negativeNormalizedAtoms, n -> fromNormalizedAtomToItsNegationAsLiteral(n), conjuncts);
		conjuncts.addAll(externalLiterals);
		Expression result = And.make(conjuncts);
		return result;
	}
}