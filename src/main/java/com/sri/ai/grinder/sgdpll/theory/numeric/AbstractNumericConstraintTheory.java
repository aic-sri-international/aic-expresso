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
package com.sri.ai.grinder.sgdpll.theory.numeric;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.library.FunctorConstants.PLUS;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.equality.EqualitySimplifier;
import com.sri.ai.grinder.library.inequality.InequalitySimplifier;
import com.sri.ai.grinder.library.number.NumericSimplifier;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.simplifier.api.MapBasedSimplifier;
import com.sri.ai.grinder.sgdpll.simplifier.core.RecursiveExhaustiveSeriallyMergedMapBasedSimplifier;
import com.sri.ai.grinder.sgdpll.theory.base.AbstractConstraintTheoryWithBinaryAtomsIncludingEquality;
import com.sri.ai.grinder.sgdpll.theory.compound.CompoundConstraintTheory;
import com.sri.ai.util.Util;

/** 
 * A {@link ConstraintTheory} for numeric literals using equalities and inequalities.
 */
@Beta
public abstract class AbstractNumericConstraintTheory extends AbstractConstraintTheoryWithBinaryAtomsIncludingEquality {

	/**
	 * Creates a numeric constraint theory.
	 * It takes an argument indicating whether all equalities and disequalities are literals in this theory;
	 * this may not be the case if a {@link CompoundConstraintTheory} mixing multiple theories involving
	 * equalities is being used.
	 * @param assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory
	 * whether all equalities and disequalities can be safely assumed to belong to this theory
	 * (if you know all such expressions are literals in this theory, invoke this constructor with a <code>true</code> argument).
	 * @param propagateAllLiteralsWhenVariableIsBound whether literals on a variable bound to a term should be immediately replaced by a literal on that term instead.
	 * @param extraSimplifier an extra {@link MapBasedSimplifier} containing extra elementary simplifiers besides the basic numeric ones.
	 */
	public AbstractNumericConstraintTheory(boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, boolean propagateAllLiteralsWhenVariableIsBound, MapBasedSimplifier extraSimplifier) {
		super(
				negationFunctor.keySet(),
				assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory,
				new RecursiveExhaustiveSeriallyMergedMapBasedSimplifier(
						// basic simplification of involved interpreted functions in this theory:
						new EqualitySimplifier(),
						new InequalitySimplifier(),
						new BooleanSimplifier(),
						new NumericSimplifier(),
						extraSimplifier),
				propagateAllLiteralsWhenVariableIsBound);

		initializeTestingInformation();
	}
	
	abstract protected void initializeTestingInformation();
	
	private static final Map<String, String> negationFunctor =
	Util.map(
			EQUALITY,                 DISEQUALITY,
			DISEQUALITY,              EQUALITY,
			LESS_THAN,                GREATER_THAN_OR_EQUAL_TO,
			LESS_THAN_OR_EQUAL_TO,    GREATER_THAN,
			GREATER_THAN,             LESS_THAN_OR_EQUAL_TO,
			GREATER_THAN_OR_EQUAL_TO, LESS_THAN
			);

	public static String getNegationFunctor(String functor) {
		return negationFunctor.get(functor);
	}

	@Override
	protected Expression getNonTrivialAtomNegation(Expression atom) {
		String functorString = atom.getFunctor().toString();
		String negatedFunctor = negationFunctor.get(functorString);
		Expression result = apply(negatedFunctor, atom.get(0), atom.get(1));
		return result;
	}

	@Override
	/**
	 * Extends super implementation by considering + and - applications as interpreted in this theory.
	 */
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, Context context) {
		boolean result = super.isInterpretedInThisTheoryBesidesBooleanConnectives(expression, context)
				|| expression.equals(PLUS) || expression.equals(MINUS)
				|| expression.hasFunctor(PLUS) || expression.hasFunctor(MINUS); 
		return result;
	}
}