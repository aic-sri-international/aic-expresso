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
package com.sri.ai.grinder.sgdpllt.theory.numeric;

import static com.sri.ai.expresso.helper.Expressions.apply;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.GREATER_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.LESS_THAN_OR_EQUAL_TO;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.MINUS;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.PLUS;
import static com.sri.ai.grinder.sgdpllt.library.commonrewriters.CommonTopRewriter.COMMON_TOP_REWRITER;
import static com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter.merge;

import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.library.BindingTopSimplifier;
import com.sri.ai.grinder.sgdpllt.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.sgdpllt.library.equality.EqualitySimplifier;
import com.sri.ai.grinder.sgdpllt.library.inequality.InequalitySimplifier;
import com.sri.ai.grinder.sgdpllt.library.number.NumericSimplifier;
import com.sri.ai.grinder.sgdpllt.library.set.CardinalityOfSetConstantSimplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.base.AbstractTheoryWithBinaryAtomsIncludingEquality;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;
import com.sri.ai.util.Util;

/** 
 * A {@link Theory} for numeric literals using equalities and inequalities.
 */
@Beta
public abstract class AbstractNumericTheory extends AbstractTheoryWithBinaryAtomsIncludingEquality {

	/**
	 * Creates a numeric theory.
	 * It takes an argument indicating whether all equalities and disequalities are literals in this theory;
	 * this may not be the case if a {@link CompoundTheory} mixing multiple theories involving
	 * equalities is being used.
	 * @param atomFunctorsAreUniqueToThisTheory
	 * whether all equalities and disequalities can be safely assumed to belong to this theory
	 * (if you know all such expressions are literals in this theory, invoke this constructor with a <code>true</code> argument).
	 * @param propagateAllLiteralsWhenVariableIsBound whether literals on a variable bound to a term should be immediately replaced by a literal on that term instead.
	 * @param extraTopRewriter an extra {@link TopRewriter} containing extra elementary operations besides the basic numeric ones.
	 */
	public AbstractNumericTheory(
			boolean atomFunctorsAreUniqueToThisTheory,
			boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				negationFunctor.keySet(),
				atomFunctorsAreUniqueToThisTheory,
				propagateAllLiteralsWhenVariableIsBound);
	}

	/**
	 * Make simplifiers from numeric simplifiers plus elementary simplifiers in given extra simplifier.
	 * @param extraSimplifier
	 * @return
	 */
	@Override
	public TopRewriter makeTopRewriter() {
		return merge(
				COMMON_TOP_REWRITER,
				// basic simplification of involved interpreted functions in this theory:
				new BindingTopSimplifier(),
				new EqualitySimplifier(),
				new InequalitySimplifier(),
				new BooleanSimplifier(),
				new NumericSimplifier(),
				new CardinalityOfSetConstantSimplifier());
	}
	
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

	/**
	 * Overrides super method due to knowledge about specific relational operators <code>>, <, <=, >=</code>
	 */
	@Override
	public Expression getAtomNegation(Expression atom, Context context) {
		String functorString = atom.getFunctor().toString();
		String negatedFunctor = negationFunctor.get(functorString);
		Expression result = apply(negatedFunctor, atom.get(0), atom.get(1));
		return result;
	}

	@Override
	/**
	 * Extends super implementation by considering + and - applications as interpreted in this theory.
	 */
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression) {
		boolean result = super.isInterpretedInThisTheoryBesidesBooleanConnectives(expression)
				|| expression.equals(PLUS) || expression.equals(MINUS)
				|| expression.hasFunctor(PLUS) || expression.hasFunctor(MINUS); 
		return result;
	}
}