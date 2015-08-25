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

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.pickUniformly;

import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.plaindpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.plaindpll.api.TermTheory;
import com.sri.ai.util.base.Pair;
import com.sri.ai.util.collect.PredicateIterator;
@Beta
/** 
 * A {@link ConstraintTheory} for equality literals.
 */
public class EqualityNewConstraintTheory extends AbstractEqualityNewConstraintTheory {

	public EqualityNewConstraintTheory(TermTheory termTheory) {
		super(termTheory);
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable) {
		return new SingleVariableEqualityConstraint(variable, this);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return true; // SingleVariableEqualityConstraint is complete
	}

	@Override
	public Expression fromNegativeAtomToLiteral(Expression negativeAtom) {
		Expression result = Disequality.make(negativeAtom.get(0), negativeAtom.get(1));
		return result;
	}

	@Override
	public Pair<Boolean, Expression> fromLiteralOnVariableToSignAndAtom(Expression variable, Expression literal) {
		Pair<Boolean, Expression> result;
		Expression other = termToWhichVariableIsEqualedToOrNull(variable, literal);
		if (other == null) {
			throw new Error("Invalid literal for equality theory received: " + literal);
		}
		Expression atom = apply(EQUALITY, variable, other);
		result = Pair.make(literal.hasFunctor(EQUALITY), atom);
		return result;
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

	@Override
	public Expression makeRandomAtomOn(Random random, RewritingProcess process) {
		Map<String, String> variablesAndTypes = getVariableNamesAndTypeNamesForTesting();
		String typeName = variablesAndTypes.get(getTestingVariable());
		Set<String> allVariables = variablesAndTypes.keySet();
		PredicateIterator<String> isNameOfVariableOfSameType = PredicateIterator.make(allVariables, s -> variablesAndTypes.get(s).equals(typeName));
		Expression otherTerm;
		if (random.nextBoolean()) {
			otherTerm = makeSymbol(pickUniformly(isNameOfVariableOfSameType, random));
		}
		else {
			otherTerm = process.getType(typeName).sampleConstant(random);
		}
		Expression result =
				random.nextBoolean()?
				Equality.make(getTestingVariable(), otherTerm) : Equality.make(otherTerm, getTestingVariable());
		return result;
	}
}