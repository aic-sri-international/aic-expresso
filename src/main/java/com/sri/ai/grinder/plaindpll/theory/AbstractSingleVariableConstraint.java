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

import static com.sri.ai.grinder.helper.GrinderUtil.getType;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.list;
import static com.sri.ai.util.Util.removeNonDestructively;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.expresso.helper.AbstractExpressionWrapper;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;
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
public abstract class AbstractSingleVariableConstraint extends AbstractExpressionWrapper implements SingleVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private Expression variable;
	private Set<Expression> positiveAtoms;
	private Set<Expression> negativeAtoms;
	private Collection<Expression> externalLiterals; // literals not on variable
	private ConstraintTheory constraintTheory;
	
	public AbstractSingleVariableConstraint(Expression variable, ConstraintTheory constraintTheory) {
		this(variable, Util.set(), Util.set(), Util.set(), constraintTheory);
	}
	
	public AbstractSingleVariableConstraint(Expression variable, Set<Expression> positiveAtoms, Set<Expression> negativeAtoms, Collection<Expression> externalLiterals, ConstraintTheory constraintTheory) {
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
	protected AbstractSingleVariableConstraint (AbstractSingleVariableConstraint other) {
		this.variable = other.variable;
		this.positiveAtoms = other.positiveAtoms;
		this.negativeAtoms = other.negativeAtoms;
		this.externalLiterals = other.externalLiterals;
		this.constraintTheory = other.constraintTheory;
	}
	
	abstract public AbstractSingleVariableConstraint clone();
	
	public AbstractSingleVariableConstraint copyWithNewPositiveAtom(Expression atom) {
		AbstractSingleVariableConstraint result = clone();
		Set<Expression> newPositiveAtoms = new LinkedHashSet<Expression>(positiveAtoms);
		newPositiveAtoms.add(atom);
		result.positiveAtoms = newPositiveAtoms;
		return result;
	}

	public AbstractSingleVariableConstraint copyWithNewNegativeAtom(Expression atom) {
		AbstractSingleVariableConstraint result = clone();
		Set<Expression> newNegativeAtoms = new LinkedHashSet<Expression>(negativeAtoms);
		newNegativeAtoms.add(atom);
		result.negativeAtoms = newNegativeAtoms;
		return result;
	}

	public AbstractSingleVariableConstraint copyWithNewExternalLiteral(Expression newExternalLiteral) {
		AbstractSingleVariableConstraint result = clone();
		Collection<Expression> newExternalLiterals = new LinkedHashSet<Expression>(externalLiterals);
		newExternalLiterals.add(newExternalLiteral);
		result.externalLiterals = newExternalLiterals;
		return result;
	}

	public AbstractSingleVariableConstraint copyWithNewPositiveAndNegativeAtoms(Set<Expression> newPositiveAtoms, Set<Expression> newNegativeAtoms) {
		AbstractSingleVariableConstraint result = clone();
		result.positiveAtoms = newPositiveAtoms;
		result.negativeAtoms = newNegativeAtoms;
		return result;
	}
	
	/**
	 * Extending classes define this method to perform whatever bookkeeping is needed
	 * after a new atom has been inserted.
	 * @param sign
	 * @param atom
	 */
	abstract public void afterInsertingNewAtom(boolean sign, Expression atom);

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

	/** Indicates whether a literal is external to this constraint or not, that is, whether it involves the main variable. */
	abstract public boolean isExternalLiteral(Expression literal);

	/**
	 * Defines how a literal is decomposed into sign and atom.
	 * Atom representations should be normalized (equivalent atoms should always be represented by the same expressions).
	 * For example, <code>X != a</code> could be decomposed into <code>false</code> and <code>X = a</code>.
	 * @param literal
	 * @return
	 */
	abstract public Pair<Boolean, Expression> fromLiteralToSignAndAtom(Expression literal);

	/** Indicates whether there are interactions between distinct atoms in current theory. */
	abstract public boolean thereAreImplicationsBetweenDifferentAtoms();

	/**
	 * Indicates whether, according to the current theory, sign1 atom1 implies sign2 atom2,
	 * where atom1 and atom2 can be assumed distinct (the result is not defined otherwise).
	 * @param sign1
	 * @param atom1
	 * @param sign2
	 * @param atom2
	 * @return
	 */
	abstract public boolean impliesLiteralWithDifferentAtom(boolean sign1, Expression atom1, boolean sign2, Expression atom2);

	@Override
	public ConstraintTheory getConstraintTheory() {
		return constraintTheory;
	}

	@Override
	public Expression getVariable() {
		return variable;
	}

	@Override
	public Collection<Expression> getExternalLiterals() {
		return Collections.unmodifiableCollection(externalLiterals);
	}

	@Override
	public SingleVariableConstraint conjoin(Expression literal, RewritingProcess process) {
		AbstractSingleVariableConstraint result;
		if (isExternalLiteral(literal)) {
			result = copyWithNewExternalLiteral(literal);
		}
		else {
			Pair<Boolean, Expression> signAndAtom = fromLiteralToSignAndAtom(literal);
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
			else if (thereAreImplicationsBetweenDifferentAtoms()) {
				// OPTIMIZATION
				// Here it would pay to have the database of atoms to be indexed by theory-specific properties
				// such that only relevant atoms are checked, depending on properties of the new literal.
				// For example, in equality theory,
				// X = a can only make redundant literals X != T1 for T1 some distinct constant,
				// and can only be contradictory with X = T2 for T2 some distinct constant,
				// while equalities between two variables never affect literals based on distinct atoms.
				
				boolean oppositeSign = sign? false : true;
				if (    Util.thereExists(positiveAtoms, p -> impliesLiteralWithDifferentAtom(true,  p, sign, atom)) ||
						Util.thereExists(negativeAtoms, p -> impliesLiteralWithDifferentAtom(false, p, sign, atom))) {
					result = this; // redundant
				}
				else if (Util.thereExists(positiveAtoms, p -> impliesLiteralWithDifferentAtom(true,  p, oppositeSign, atom)) ||
						Util. thereExists(negativeAtoms, p -> impliesLiteralWithDifferentAtom(false, p, oppositeSign, atom))) {
					result = null; // contradiction
				}
				else {
					// remove redundant literals and add new one
					Set<Expression> newPositiveAtoms = removeNonDestructively(positiveAtoms, p -> impliesLiteralWithDifferentAtom(sign, atom, true,  p));
					Set<Expression> newNegativeAtoms = removeNonDestructively(negativeAtoms, p -> impliesLiteralWithDifferentAtom(sign, atom, false, p));
					if (sign) {
						newPositiveAtoms.add(atom);
					}
					else {
						newNegativeAtoms.add(atom);
					}
					result = copyWithNewPositiveAndNegativeAtoms(newPositiveAtoms, newNegativeAtoms);
					result.afterInsertingNewAtom(sign, atom);
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
	public SingleVariableConstraint simplifyGiven(Expression externalLiteral, RewritingProcess process) {
		throw new Error("Not implemented");
	}

	@Override
	public Expression pickSplitter(RewritingProcess process) {
		throw new Error("Not implemented");
	}

	@Override
	public Expression modelCount(RewritingProcess process) {
		throw new Error("Not implemented");
	}

	@Override
	public String debuggingDescription(RewritingProcess process) {
		return toString();
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

	@Override
	public Expression getVariableDomain(RewritingProcess process) {
		Expression variableType = getType(variable, process);
		if (variableType == null) {
			variableType = new DefaultSyntacticFunctionApplication(TYPE, variable);
		}
		return variableType;
	}

	protected long cachedIndexDomainSize = -1;

	@Override
	public long getVariableDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(variable, process);
		}
		return cachedIndexDomainSize;
	}
}