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
package com.sri.ai.grinder.sgdpll.theory.equality;

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.addAll;
import static com.sri.ai.util.Util.arrayList;
import static com.sri.ai.util.Util.iterator;
import static com.sri.ai.util.Util.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.theory.base.AbstractSingleVariableConstraintWithBinaryAtomsIncludingEquality;
import com.sri.ai.util.Util;

/**
 * An equality constraint solver.
 * 
 * @author braz
 *
 */
@Beta
public class SingleVariableEqualityConstraint extends AbstractSingleVariableConstraintWithBinaryAtomsIncludingEquality {

	// these methods are kept first in the class because they heavily depend
	// on which super class we are using, so it is good to keep them near the class declaration
	
	/**
	 * Indicates whether this implementation is complete with respect to the constraint's variable.
	 * @return
	 */
	public boolean isCompleteWithRespectToVariable() {
		boolean result = ! getPropagateAllLiteralsWhenVariableIsBound();
		return result;
		// Explanation: if we are implementing SingleVariableEqualityConstraint from
		// AbstractSingleVariableConstraintWithBinaryAtomsIncludingEquality,
		// when the variable is bound to a value, incoming literals are immediately
		// propagated as external literals.
		// This prevents detection of contradictions between them,
		// because these contradictions no longer involve the constraint variable.
		// For example, whereas X = Y and X != Z and X = Z contains a contradiction
		// that would normally be detected,
		// propagating literals once X is bound to Y would propagate the two last literals as
		// external literals Y != Z and Y = Z, which are not analysed at this point
		// to allow the contradiction detection.
	}

	@Override
	protected boolean conjoiningRedundantSignAndNormalizedAtomNeverChangesConstraintInstance() {
		boolean result = ! getPropagateAllLiteralsWhenVariableIsBound();
		return result;
		// Explanation: once we propagate incoming literals, we analyse them less and don't
		// necessarily detect redundancies between them.
		// We may produce multiple external literals that may be redundant between themselves
		// but whose redundancies will only be detected when they are themselves analysed
		// in their own constraints.
	}
	
	private static final long serialVersionUID = 1L;
	
	/**
	 * The number of disequalities from uniquely named constants;
	 * this field is only accurate if {@link #conjoiningRedundantSignAndNormalizedAtomNeverChangesConstraintInstance()}
	 * returns true.
	 * This is because this field is maintained by {@link #destructiveUpdateOrNullAfterInsertingNewNormalizedAtom(boolean, Expression, Context)},
	 * which is invoked only in that case.
	 */
	private int numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable;

	public SingleVariableEqualityConstraint(Expression variable, boolean propagateAllLiteralsWhenVariableIsBound, ConstraintTheory constraintTheory) {
		super(variable, propagateAllLiteralsWhenVariableIsBound, constraintTheory);
		this.numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable = 0;
	}

	private SingleVariableEqualityConstraint(
			Expression variable,
			ArrayList<Expression> positiveNormalizedAtoms,
			ArrayList<Expression> negativeNormalizedAtoms,
			List<Expression> externalLiterals,
			int numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable,
			boolean propagateAllLiteralsWhenVariableIsBound,
			ConstraintTheory constraintTheory) {
		
		super(variable, positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, propagateAllLiteralsWhenVariableIsBound, constraintTheory);
		this.numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable = numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable;
	}

	public SingleVariableEqualityConstraint(SingleVariableEqualityConstraint other) {
		super(other);
		this.numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable = other.numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable;
	}

	@Override
	protected SingleVariableEqualityConstraint makeSimplification(ArrayList<Expression> positiveNormalizedAtoms, ArrayList<Expression> negativeNormalizedAtoms, List<Expression> externalLiterals) {
		SingleVariableEqualityConstraint result = new SingleVariableEqualityConstraint(getVariable(), positiveNormalizedAtoms, negativeNormalizedAtoms, externalLiterals, numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable, getPropagateAllLiteralsWhenVariableIsBound(), getConstraintTheory());
		return result;
	}

	@Override
	public SingleVariableEqualityConstraint clone() {
		SingleVariableEqualityConstraint result = new SingleVariableEqualityConstraint(this);
		return result;
	}

	@Override
	public SingleVariableEqualityConstraint destructiveUpdateOrNullAfterInsertingNewNormalizedAtom(boolean sign, Expression atom, Context context) {
		SingleVariableEqualityConstraint result = this;
		if (!sign && context.isUniquelyNamedConstant(atom.get(1))) {
			numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable++;
			long variableDomainSize = getVariableTypeSize(context);
			if (variableDomainSize >= 0 && numberOfDisequalitiesFromUniquelyNamedConstantsSeenSoFarForThisVariable == variableDomainSize) {
				result = null;
			}
		}
		return result;
		// this control is performed after the conjoining of literals has been performed,
		// and one may ask why not do it as soon as the literal is received, in order to save time.
		// the reason it is done here is so that we know for sure it is the first disequality
		// we see against this constant, because those already seen are not re-inserted.
	}
	
	/**
	 * Overridden in order to break equalities of the type <code>X = Y = Z</code>
	 * into binary literals, then taken by the super class implementation.
	 */
	@Override
	public SingleVariableEqualityConstraint conjoinWithLiteral(Expression literal, Context context) {
		Collection<Expression> binaryLiterals = breakMultiTermEquality(literal, context);
		SingleVariableEqualityConstraint result = null; // initial value never used, but compiler does not realize it
		for (Expression binaryEquality : binaryLiterals) {
			result = (SingleVariableEqualityConstraint) super.conjoinWithLiteral(binaryEquality, context);
			if (result == null) {
				break;
			}
		}
		return result;
	}

	private Collection<Expression> breakMultiTermEquality(Expression literal, Context context) {
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
	public Expression fromNormalizedAtomToItsNegationAsLiteral(Expression negativeAtom) {
		Expression result = Disequality.make(negativeAtom.get(0), negativeAtom.get(1));
		return result;
	}

	private static final Collection<String> normalFunctors =
			Util.set(EQUALITY);
	
	private static final Map<String, String> negationFunctor =
			Util.map(
					EQUALITY,                 DISEQUALITY,
					DISEQUALITY,              EQUALITY
					);

	@Override
	protected Collection<String> getNormalFunctors() {
		return normalFunctors;
	}
	
	@Override
	protected String getNegationFunctor(String functor) {
		String result = negationFunctor.get(functor);
		return result;
	}
	
	@Override
	protected String getFlipFunctor(String functor) {
		return functor; // both equality and disequality are symmetrical
	}

	@Override
	protected Expression isolateVariable(Expression atom, Context context) {
		// do not need to do anything, as variable is supposed to be isolated already for this theory
		return atom;
	}

	@Override
	public Expression getVariableFreeLiteralEquivalentToSign1Atom1ImpliesSign2Atom2(boolean sign1, Expression atom1, boolean sign2, Expression atom2, Context context) {
		Expression result;
		if (sign1) {
			if (sign2) {
				// X = Y => X = Z iff Y = Z
				result = Equality.makeWithConstantSimplification(atom1.get(1), atom2.get(1), context);
			}
			else {
				// X = Y => X != Z iff Y != Z
				result = Disequality.makeWithConstantSimplification(atom1.get(1), atom2.get(1), context);
			}
		}
		else {
			// X != Y => X = Z => false
			// X != Y and X != Z => false
			result = FALSE;
		}
		return result;
	}

	/**
	 * Returns an iterator to terms constrained to be equal to variable.
	 * @return
	 */
	public Iterator<Expression> getEqualsIterator() {
		return getPositiveNormalizedAtoms().stream().
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
		return getNegativeNormalizedAtoms().stream().
		map(e -> e.get(1)) // second arguments of Variable != Term
		.iterator();
	}

	public ArrayList<Expression> getDisequals() {
		return addAll(arrayList(), getDisequalsIterator());
	}

	public int numberOfDisequals() {
		return getNegativeNormalizedAtoms().size();
	}

	@Override
	public SingleVariableEqualityConstraint conjoin(Expression formula, Context context) {
		return (SingleVariableEqualityConstraint) super.conjoin(formula, context);
	}

	@Override
	protected Iterator<Expression> getImplicitPositiveNormalizedAtomsIterator(Context context) {
		return iterator();
	}

	@Override
	protected Iterator<Expression> getImplicitNegativeNormalizedAtomsIterator(Context context) {
		return iterator();
	}

	@Override
	public Expression binding(Expression variable) {
		// TODO Auto-generated method stub
		return null;
	}
}