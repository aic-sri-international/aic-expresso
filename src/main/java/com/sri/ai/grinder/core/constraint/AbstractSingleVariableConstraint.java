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

import static com.sri.ai.expresso.helper.Expressions.contains;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.library.boole.And;
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
public abstract class AbstractSingleVariableConstraint extends AbstractConstraint implements SingleVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private Expression variable;
	private ArrayList<Expression> positiveNormalizedAtoms;
	private ArrayList<Expression> negativeNormalizedAtoms;
	private List<Expression> externalLiterals; // literals not on variable
	
	public AbstractSingleVariableConstraint(Expression variable, Theory theory) {
		this(variable, Util.arrayList(), Util.arrayList(), Util.arrayList(), theory);
	}
	
	public AbstractSingleVariableConstraint(
			Expression variable,
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals,
			Theory theory) {
		super(theory);
		this.variable = variable;
		this.positiveNormalizedAtoms = positiveNormalizedAtoms;
		this.negativeNormalizedAtoms = negativeNormalizedAtoms;
		this.externalLiterals = externalLiterals;
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
		super(other.getTheory());
		this.variable = other.variable;
		this.positiveNormalizedAtoms = other.positiveNormalizedAtoms;
		this.negativeNormalizedAtoms = other.negativeNormalizedAtoms;
		this.externalLiterals = other.externalLiterals;
		this.isContradiction = other.isContradiction;
	}
	
	@Override
	public AbstractSingleVariableConstraint clone() {
		return (AbstractSingleVariableConstraint) super.clone();
	}
	
	/**
	 * Creates a simplification of this constraint out of the
	 * given normalized atoms and external literals.
	 * The result is only allowed to be used in contexts in which the original
	 * constraint holds.
	 * That is to say, implementations are allowed to rely
	 * on this fact to keep whatever internal bookkeeping they may
	 * have from the original constraint,
	 * thus avoiding the need to re-compute it.
	 * <p>
	 * For example, equality constraints keep track of how many
	 * disequalities against uniquely named constants have been observed
	 * on the constraint's variable so far (even the disequalities that
	 * have been discarded since then).
	 * Copying this information to the new constraint is only justified
	 * if we know it is going to be used as refinement of the original
	 * constraint, since it may not follow from the normalized atoms and external literals
	 * alone.
	 * @param positiveNormalizedAtoms
	 * @param negativeNormalizedAtoms
	 * @param externalLiterals
	 * @return
	 */
	abstract protected AbstractSingleVariableConstraint makeSimplification(
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals);
	
	@Override
	public AbstractSingleVariableConstraint makeSimplificationWithoutExternalLiterals() {
		AbstractSingleVariableConstraint result =
				makeSimplification(getPositiveNormalizedAtoms(), getNegativeNormalizedAtoms(), list());
		return result;
	}

	
	//////////// GETTERS
	
	@Override
	public Expression getVariable() {
		return variable;
	}

	public ArrayList<Expression> getPositiveNormalizedAtoms() {
		return positiveNormalizedAtoms;
	}

	public ArrayList<Expression> getNegativeNormalizedAtoms() {
		return negativeNormalizedAtoms;
	}

	@Override
	public List<Expression> getExternalLiterals() {
		return Collections.unmodifiableList(externalLiterals);
	}

	//////////// THEORY RULES
	
	public AbstractSingleVariableConstraint addPositiveNormalizedAtom(Expression atom) {
		AbstractSingleVariableConstraint result = clone();
		ArrayList<Expression> newPositiveNormalizedAtoms = new ArrayList<Expression>(positiveNormalizedAtoms);
		newPositiveNormalizedAtoms.add(atom);
		result.positiveNormalizedAtoms = newPositiveNormalizedAtoms;
		return result;
	}

	public AbstractSingleVariableConstraint addNegativeNormalizedAtom(Expression atom) {
		AbstractSingleVariableConstraint result = clone();
		ArrayList<Expression> newNegativeNormalizedAtoms = new ArrayList<Expression>(negativeNormalizedAtoms);
		newNegativeNormalizedAtoms.add(atom);
		result.negativeNormalizedAtoms = newNegativeNormalizedAtoms;
		return result;
	}

	public AbstractSingleVariableConstraint addExternalLiteral(Expression newExternalLiteral) {
		AbstractSingleVariableConstraint result = clone(); // TODO: need to use a stack here to avoid copying everything every time
		ArrayList<Expression> newExternalLiterals = new ArrayList<Expression>(externalLiterals);
		newExternalLiterals.add(newExternalLiteral);
		result.externalLiterals = newExternalLiterals;
		return result;
	}

	public AbstractSingleVariableConstraint setPositiveAndNegativeNormalizedAtoms(ArrayList<Expression> newPositiveNormalizedAtoms, ArrayList<Expression> newNegativeNormalizedAtoms) {
		AbstractSingleVariableConstraint result = clone();
		result.positiveNormalizedAtoms = newPositiveNormalizedAtoms;
		result.negativeNormalizedAtoms = newNegativeNormalizedAtoms;
		return result;
	}

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
	 * @param context
	 * @return
	 */
	protected abstract Pair<Boolean, Expression> fromLiteralOnVariableToSignAndNormalizedAtom(Expression variable, Expression literal, Context context);

	/**
	 * Returns the literal corresponding to the negation of the given normalized atom.
	 * Typically this will just return the application of NOT to the atom,
	 * but some languages may have more conventional (and more pleasing to read) ways of representing these
	 * negations (for example, not(X = a) -> X != a).
	 * @param normalizedAtom
	 * @return
	 */
	abstract public Expression fromNormalizedAtomToItsNegationAsLiteral(Expression normalizedAtom);

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
	 * @param context 
	 * @return 
	 */
	abstract public AbstractSingleVariableConstraint destructiveUpdateOrNullAfterConjoiningNewNormalizedAtom(boolean sign, Expression atom, Context context);

	
	
	
	
	@Override
	public SingleVariableConstraint conjoinWithLiteral(Expression formula, Context context) {
		AbstractSingleVariableConstraint result;
		if (!contains(formula, getVariable())) {
			result = addExternalLiteral(formula);
		}
		else {
			Pair<Boolean, Expression> signAndNormalizedAtom = fromLiteralOnVariableToSignAndNormalizedAtom(getVariable(), formula, context);
			boolean    sign = signAndNormalizedAtom.first;
			Expression normalizedAtom = signAndNormalizedAtom.second;
			ArrayList<Expression>     sameSignNormalizedAtoms = sign? positiveNormalizedAtoms : negativeNormalizedAtoms;
			ArrayList<Expression> oppositeSignNormalizedAtoms = sign? negativeNormalizedAtoms : positiveNormalizedAtoms;
			if (sameSignNormalizedAtoms.contains(normalizedAtom)) {
				result = this; // redundant
			}
			else if (oppositeSignNormalizedAtoms.contains(normalizedAtom)) {
				result = makeContradiction(); // contradiction
			}
			else {
				result = conjoinNonTrivialSignAndNormalizedAtom(sign, normalizedAtom, context);
				if (conjoiningRedundantSignAndNormalizedAtomNeverChangesConstraintInstance() && result != this && result != null) {
					// the condition above guarantees that result is not null and that the "new atom" is indeed new;
					// if the instance is a new one (not 'this'), then the atom is not redundant.
					result = result.destructiveUpdateOrNullAfterConjoiningNewNormalizedAtom(sign, normalizedAtom, context);
				}
			}
		}
		return result;
	}

	/**
	 * @return
	 */
	@Override
	public AbstractSingleVariableConstraint makeContradiction() {
		return (AbstractSingleVariableConstraint) super.makeContradiction();
	}

	/**
	 * Indicates whether implementation guarantees that conjoining redundant atom always results in the same object instance.
	 * @return
	 */
	abstract protected boolean conjoiningRedundantSignAndNormalizedAtomNeverChangesConstraintInstance();

	/**
	 * Conjoin a literal described by sign and normalized atom.
	 * This method is only invoked if the atom is known to not be present
	 * in the constraint already, in either positive or negative form.
	 * <p>
	 * Extensions should do whatever theory-specific processing here,
	 * decide whether to add a positive or negative atom,
	 * or perhaps even set them all to a new collection,
	 * and generate the resulting constraint by using
	 * {@link AbstractSingleVariableConstraint#addPositiveNormalizedAtom(Expression)},
	 * {@link AbstractSingleVariableConstraint#addPositiveNormalizedAtom(Expression)},
	 * and
	 * {@link AbstractSingleVariableConstraint#setPositiveAndNegativeNormalizedAtoms(ArrayList, ArrayList)}
	 * 
	 * @param sign
	 * @param normalizedAtom
	 * @param context
	 * @return
	 */
	abstract protected AbstractSingleVariableConstraint conjoinNonTrivialSignAndNormalizedAtom(boolean sign, Expression normalizedAtom, Context context);

	
	
	@Override
	protected Expression computeInnerExpressionIfNotContradiction() {
		List<Expression> conjuncts = list();
		conjuncts.addAll(positiveNormalizedAtoms);
		Util.mapIntoList(negativeNormalizedAtoms, n -> fromNormalizedAtomToItsNegationAsLiteral(n), conjuncts);
		conjuncts.addAll(externalLiterals);
		Expression result = And.make(conjuncts);
		return result;
	}
}