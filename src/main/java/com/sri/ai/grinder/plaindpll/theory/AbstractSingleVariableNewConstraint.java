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
package com.sri.ai.grinder.plaindpll.theory;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.contains;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.removeFromSetNonDestructively;
import static com.sri.ai.util.Util.thereExists;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.NewConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.SingleVariableNewConstraint;
import com.sri.ai.grinder.plaindpll.core.SGDPLLT;
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
 * This interface is defined for use primarily by {@link SGDPLLT}.
 * 
 * @author braz
 *
 */
@Beta
public abstract class AbstractSingleVariableNewConstraint extends AbstractExpressionWrapper implements SingleVariableNewConstraint {

	private static final long serialVersionUID = 1L;
	
	private Expression variable;
	private Set<Expression> positiveAtoms;
	private Set<Expression> negativeAtoms;
	private Collection<Expression> externalLiterals; // literals not on variable
	private NewConstraintTheory constraintTheory;
	
	public AbstractSingleVariableNewConstraint(Expression variable, NewConstraintTheory constraintTheory) {
		this(variable, Util.set(), Util.set(), Util.set(), constraintTheory);
	}
	
	public AbstractSingleVariableNewConstraint(Expression variable, Set<Expression> positiveAtoms, Set<Expression> negativeAtoms,
			Collection<Expression> externalLiterals, NewConstraintTheory constraintTheory) {
		this.variable = variable;
		this.positiveAtoms = positiveAtoms;
		this.negativeAtoms = negativeAtoms;
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
	protected AbstractSingleVariableNewConstraint (AbstractSingleVariableNewConstraint other) {
		this.variable = other.variable;
		this.positiveAtoms = other.positiveAtoms;
		this.negativeAtoms = other.negativeAtoms;
		this.externalLiterals = other.externalLiterals;
		this.constraintTheory = other.constraintTheory;
	}
	
	@Override
	abstract public AbstractSingleVariableNewConstraint clone();
	
	//////////// THEORY RULES
	
	/**
	 * Returns the literal corresponding to the negation of the given atom
	 * (which is known to be false).
	 * Typically this will just return the application of NOT to the atom,
	 * but some languages may have more conventional ways of representing these
	 * negations (for example, not(X = a) -> X != a).
	 * @param negativeAtom
	 * @return
	 */
	abstract public Expression fromNegativeAtomToLiteral(Expression negativeAtom);

	/**
	 * Defines how a literal is decomposed into sign and atom.
	 * Atom representations should be normalized (equivalent atoms should always be represented by the same expressions).
	 * For example, <code>X != a</code> could be decomposed into <code>false</code> and <code>X = a</code>.
	 * @param variable
	 * @param literal
	 * @return
	 */
	abstract public Pair<Boolean, Expression> fromLiteralOnVariableToSignAndAtom(Expression variable, Expression literal);

	/** Indicates whether there are interactions between distinct atoms in current theory. */
	abstract public boolean atomMayImplyLiteralsOnDifferentAtoms();

	/**
	 * Indicates whether, according to the current theory, sign1 atom1 implies sign2 atom2,
	 * where atom1 and atom2 can be assumed distinct (the result is not defined otherwise).
	 * @param sign1
	 * @param atom1
	 * @param sign2
	 * @param atom2
	 * @param process
	 * @return
	 */
	abstract public boolean impliesLiteralWithDifferentAtom(boolean sign1, Expression atom1, boolean sign2, Expression atom2, RewritingProcess process);

	public AbstractSingleVariableNewConstraint copyWithNewPositiveAtom(Expression atom) {
		AbstractSingleVariableNewConstraint result = clone();
		Set<Expression> newPositiveAtoms = new LinkedHashSet<Expression>(positiveAtoms);
		newPositiveAtoms.add(atom);
		result.positiveAtoms = newPositiveAtoms;
		return result;
	}

	public AbstractSingleVariableNewConstraint copyWithNewNegativeAtom(Expression atom) {
		AbstractSingleVariableNewConstraint result = clone();
		Set<Expression> newNegativeAtoms = new LinkedHashSet<Expression>(negativeAtoms);
		newNegativeAtoms.add(atom);
		result.negativeAtoms = newNegativeAtoms;
		return result;
	}

	public AbstractSingleVariableNewConstraint copyWithNewExternalLiteral(Expression newExternalLiteral) {
		AbstractSingleVariableNewConstraint result = clone();
		Collection<Expression> newExternalLiterals = new LinkedHashSet<Expression>(externalLiterals);
		newExternalLiterals.add(newExternalLiteral);
		result.externalLiterals = newExternalLiterals;
		return result;
	}

	public AbstractSingleVariableNewConstraint copyWithNewPositiveAndNegativeAtoms(Set<Expression> newPositiveAtoms, Set<Expression> newNegativeAtoms) {
		AbstractSingleVariableNewConstraint result = clone();
		result.positiveAtoms = newPositiveAtoms;
		result.negativeAtoms = newNegativeAtoms;
		return result;
	}
	
	@Override
	public Expression getVariable() {
		return variable;
	}

	/**
	 * Extending classes define this method to perform whatever bookkeeping is needed
	 * after a new atom has been inserted, returning null in case of a detected contradiction
	 * or this object otherwise.
	 * @param sign
	 * @param atom
	 * @param process 
	 * @return 
	 */
	abstract public AbstractSingleVariableNewConstraint afterInsertingNewAtom(boolean sign, Expression atom, RewritingProcess process);

	@Override
	public SingleVariableNewConstraint conjoin(Expression literal, RewritingProcess process) {
		AbstractSingleVariableNewConstraint result;
		if (literal.equals(TRUE)) {
			result = this;
		}
		else if (literal.equals(FALSE)) {
			result = null;
		}
		else if (!contains(literal, getVariable(), process)) {
			result = copyWithNewExternalLiteral(literal);
		}
		else {
			Pair<Boolean, Expression> signAndAtom = fromLiteralOnVariableToSignAndAtom(getVariable(), literal);
			boolean    sign = signAndAtom.first;
			Expression atom = signAndAtom.second;
			Set<Expression>     sameSignAtoms = sign? positiveAtoms : negativeAtoms;
			Set<Expression> oppositeSignAtoms = sign? negativeAtoms : positiveAtoms;
			if (sameSignAtoms.contains(atom)) {
				result = this; // redundant
			}
			else if (oppositeSignAtoms.contains(atom)) {
				result = null; // contradiction
			}
			else if (atomMayImplyLiteralsOnDifferentAtoms()) {
				// OPTIMIZATION
				// Here it would pay to have the database of atoms to be indexed by theory-specific properties
				// such that only relevant atoms are checked, depending on properties of the new literal.
				// For example, in equality theory,
				// X = a can only make redundant literals X != T1 for T1 some distinct constant,
				// and can only be contradictory with X = T2 for T2 some distinct constant,
				// while equalities between two variables never affect literals based on distinct atoms.
				
				boolean oppositeSign = sign? false : true;
				if (    thereExists(positiveAtoms, p -> impliesLiteralWithDifferentAtom(true,  p, sign, atom, process)) ||
						thereExists(negativeAtoms, p -> impliesLiteralWithDifferentAtom(false, p, sign, atom, process))) {
					result = this; // redundant
				}
				else if (thereExists(positiveAtoms, p -> impliesLiteralWithDifferentAtom(true,  p, oppositeSign, atom, process)) ||
						thereExists(negativeAtoms, p -> impliesLiteralWithDifferentAtom(false, p, oppositeSign, atom, process))) {
					result = null; // contradiction
				}
				else {
					// remove redundant literals and add new one
					Set<Expression> newPositiveAtoms = removeFromSetNonDestructively(positiveAtoms, p -> impliesLiteralWithDifferentAtom(sign, atom, true,  p, process));
					Set<Expression> newNegativeAtoms = removeFromSetNonDestructively(negativeAtoms, p -> impliesLiteralWithDifferentAtom(sign, atom, false, p, process));
					if (sign) {
						newPositiveAtoms.add(atom);
					}
					else {
						newNegativeAtoms.add(atom);
					}
					result = copyWithNewPositiveAndNegativeAtoms(newPositiveAtoms, newNegativeAtoms);
					result = result.afterInsertingNewAtom(sign, atom, process);
				}
			}
			else {
				if (sign) {
					result = copyWithNewPositiveAtom(atom);
				}
				else {
					result = copyWithNewNegativeAtom(atom);
				}
			}
		}
		return result;
	}

	@Override
	protected Expression computeInnerExpression() {
		List<Expression> conjuncts = list();
		conjuncts.addAll(positiveAtoms);
		Util.mapIntoList(negativeAtoms, n -> fromNegativeAtomToLiteral(n), conjuncts);
		conjuncts.addAll(externalLiterals);
		Expression result = And.make(conjuncts);
		return result;
	}
}