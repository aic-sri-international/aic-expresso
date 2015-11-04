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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.helper.GrinderUtil.getTypeCardinality;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.grinder.library.FunctorConstants.TYPE;
import static com.sri.ai.util.Util.list;

import java.util.Collection;
import java.util.Iterator;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.core.DefaultSyntacticFunctionApplication;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.core.constraint.AbstractSingleVariableConstraint;
import com.sri.ai.util.base.Pair;

/**
 * An equality constraint solver.
 * 
 * @author braz
 *
 */
@Beta
public class SingleVariableInequalityConstraint extends AbstractSingleVariableConstraint {

	private static final long serialVersionUID = 1L;
	
	private int numberOfDisequalitiesFromConstantsSeenSoFar;

	public SingleVariableInequalityConstraint(Expression variable, ConstraintTheory constraintTheory) {
		super(variable, constraintTheory);
		this.numberOfDisequalitiesFromConstantsSeenSoFar = 0;
	}

	public SingleVariableInequalityConstraint(SingleVariableInequalityConstraint other) {
		super(other);
		this.numberOfDisequalitiesFromConstantsSeenSoFar = other.numberOfDisequalitiesFromConstantsSeenSoFar;
	}

	@Override
	public SingleVariableInequalityConstraint clone() {
		SingleVariableInequalityConstraint result = new SingleVariableInequalityConstraint(this);
		result.numberOfDisequalitiesFromConstantsSeenSoFar = numberOfDisequalitiesFromConstantsSeenSoFar;
		return result;
	}

	@Override
	public SingleVariableInequalityConstraint destructiveUpdateOrNullAfterInsertingNewAtom(boolean sign, Expression atom, RewritingProcess process) {
		SingleVariableInequalityConstraint result = this;
		if (!sign && process.isUniquelyNamedConstant(atom.get(1))) {
			numberOfDisequalitiesFromConstantsSeenSoFar++;
			long variableDomainSize = getVariableDomainSize(process);
			if (variableDomainSize >= 0 && numberOfDisequalitiesFromConstantsSeenSoFar == variableDomainSize) {
				result = null;
			}
		}
		return result;
	}
	
	/**
	 * @return the number of disequalities between the variable and uniquely named constants that have been conjoined to this constraint so far
	 * (some of these disequalities may have been eliminated by a conjunction with an equality that implies them).
	 */
	public int getNumberOfDisequalitiesFromConstantsSeenSoFar() {
		return numberOfDisequalitiesFromConstantsSeenSoFar;
	}
	
	@Override
	public SingleVariableInequalityConstraint conjoinWithLiteral(Expression literal, RewritingProcess process) {
		Collection<Expression> binaryEqualities = breakMultiTermEquality(literal, process);
		SingleVariableInequalityConstraint result = null; // initial value never used, but compiler does not realize it
		for (Expression binaryEquality : binaryEqualities) {
			result = (SingleVariableInequalityConstraint) super.conjoinWithLiteral(binaryEquality, process);
			if (result == null) {
				break;
			}
		}
		return result;
	}

	private Collection<Expression> breakMultiTermEquality(Expression literal, RewritingProcess process) {
		if (literal.hasFunctor(EQUALITY) && literal.numberOfArguments() > 2) {
			Collection<Expression> result = list();
			for (int i = 0; i != literal.numberOfArguments() - 2; i++) {
				Expression binaryLiteral = Equality.make(literal.get(i), literal.get(i + 1));
				result.add(binaryLiteral);
			}
			return result;
		}
		else {
			return list(literal);
		}
	}

	@Override
	public Expression fromNegativeAtomToLiteral(Expression negativeAtom) {
		Expression result = Disequality.make(negativeAtom.get(0), negativeAtom.get(1));
		return result;
	}

	@Override
	public Pair<Boolean, Expression> fromLiteralOnVariableToSignAndAtom(Expression variable, Expression literal) {
		Pair<Boolean, Expression> result;
		
		literal = moveNotIn(literal);
		
		Expression other = termToWhichVariableIsEqualedToOrNull(variable, literal);
		if (other == null) {
			throw new Error("Invalid literal for equality theory received: " + literal);
		}
		Expression atom = apply(EQUALITY, variable, other);
		result = Pair.make(literal.hasFunctor(EQUALITY), atom);
		return result;
	}

	private Expression moveNotIn(Expression literal) throws Error {
		if (literal.hasFunctor(NOT)) {
			if (literal.get(0).hasFunctor(EQUALITY)) {
				literal = Expressions.apply(DISEQUALITY, literal.get(0).get(0), literal.get(0).get(1));
			}
			else if (literal.get(0).hasFunctor(DISEQUALITY)) {
				literal = Expressions.apply(EQUALITY, literal.get(0).get(0), literal.get(0).get(1));
			}
			else {
				throw new Error("Invalid literal for equality theory received: " + literal);
			}
		}
		return literal;
	}

	private Expression termToWhichVariableIsEqualedToOrNull(Expression variable, Expression equalityLiteral) {
		Expression result;
		if (equalityLiteral.get(0).equals(variable)) {
			result = equalityLiteral.get(1);
		}
		else if (equalityLiteral.get(1).equals(variable)) {
			result = equalityLiteral.get(0);
		}
		else {
			result = null;
		}
		return result;
	}

	@Override
	public boolean atomMayImplyLiteralsOnDifferentAtoms() {
		return true;
	}

	@Override
	public boolean impliesLiteralWithDifferentAtom(boolean sign1, Expression atom1, boolean sign2, Expression atom2, RewritingProcess process) {
		// X = c1 implies X != c2 for every other constant c2
		boolean result = sign1 && !sign2 && process.isUniquelyNamedConstant(atom1.get(1)) && process.isUniquelyNamedConstant(atom2.get(1));
		return result;
	}

	/**
	 * Returns an iterator to terms constrained to be equal to variable.
	 * @return
	 */
	public Iterator<Expression> getEqualsIterator() {
		return getPositiveAtoms().stream().
		map(e -> e.get(1)) // second arguments of Variable = Term
		.iterator();
	}
	
	/** Returns one of the expressions to which the variable is bound to, or null if there aren't any. */
	public Expression getABoundValueOrNull() {
		Expression result;
		Iterator<Expression> equalsIterator = getEqualsIterator();
		if (equalsIterator.hasNext()) {
			result = equalsIterator.next();
		}
		else {
			result = null;
		}
		return result;
	}

	/**
	 * Returns an iterator to terms constrained to be disequal to variable.
	 * @return
	 */
	public Iterator<Expression> getDisequalsIterator() {
		return getNegativeAtoms().stream().
		map(e -> e.get(1)) // second arguments of Variable != Term
		.iterator();
	}

	protected long cachedIndexDomainSize = -1;

	public long getVariableDomainSize(RewritingProcess process) {
		if (cachedIndexDomainSize == -1) {
			cachedIndexDomainSize = getTypeCardinality(getVariable(), process);
		}
		return cachedIndexDomainSize;
	}
	
	public Expression getVariableDomain(RewritingProcess process) {
		Expression variableType = process.getContextualSymbolType(getVariable());
		if (variableType == null) {
			variableType = new DefaultSyntacticFunctionApplication(TYPE, getVariable());
		}
		return variableType;
	}

	@Override
	public SingleVariableInequalityConstraint conjoin(Expression formula, RewritingProcess process) {
		return (SingleVariableInequalityConstraint) super.conjoin(formula, process);
	}
}