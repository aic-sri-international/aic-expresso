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

import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
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
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Equivalence;
import com.sri.ai.grinder.library.boole.Implication;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.plaindpll.api.SingleVariableNewConstraint;
import com.sri.ai.grinder.plaindpll.core.AbstractNewConstraintTheory;
import com.sri.ai.util.Util;
import com.sri.ai.util.base.BinaryFunction;
import com.sri.ai.util.collect.PredicateIterator;

/** 
 * A {@link NewConstraintTheory} for equality literals.
 */
@Beta
public class EqualityNewConstraintTheory extends AbstractNewConstraintTheory {

	@Override
	protected boolean usesDefaultImplementationOfSimplifyByOverridingGetFunctionApplicationSimplifiersAndGetSyntacticFormTypeSimplifiers() {
		return true;
	}

	private static Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> functionApplicationSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map(
					FunctorConstants.EQUALITY,        (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equality.simplify(f, process),

					FunctorConstants.DISEQUALITY,     (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Disequality.simplify(f, process),

					FunctorConstants.NOT,             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Not.simplify(f),

					FunctorConstants.AND,             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					And.simplify(f),

					FunctorConstants.OR,              (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Or.simplify(f),

					FunctorConstants.NOT,             (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Not.simplify(f),

					FunctorConstants.IF_THEN_ELSE,    (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					IfThenElse.simplify(f),

					FunctorConstants.EQUIVALENCE,     (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Equivalence.simplify(f),

					FunctorConstants.IMPLICATION,     (BinaryFunction<Expression, RewritingProcess, Expression>) (f, process) ->
					Implication.simplify(f)
					);

	private Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> syntacticFormTypeSimplifiers =
			Util.<String, BinaryFunction<Expression, RewritingProcess, Expression>>map();

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers() {
		return functionApplicationSimplifiers;
	}

	@Override
	public Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers() {
		return syntacticFormTypeSimplifiers;
	}

	@Override
	public SingleVariableNewConstraint makeSingleVariableConstraint(Expression variable) {
		return new SingleVariableEqualityNewConstraint(variable, this);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return true; // SingleVariableEqualityNewConstraint is complete
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression, RewritingProcess process) {
		boolean result = 
				expression.hasFunctor(EQUALITY) || expression.hasFunctor(DISEQUALITY) ||
				expression.equals(EQUALITY) || expression.equals(DISEQUALITY);
		return result;
	}

	@Override
	public Expression makeRandomAtomOn(String variable, Random random, RewritingProcess process) {
		Map<String, String> variablesAndTypes = getVariableNamesAndTypeNamesForTesting();
		String typeName = variablesAndTypes.get(variable);
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
				Equality.make(variable, otherTerm) : Equality.make(otherTerm, variable);
		return result;
	}

	/**
	 * We override the default version because disequality literals must be represented as <code>T1 != T2</code> in this theory,
	 * and the default version of random literal generation would only negate atoms, representing disequalities as <code>not (T1 = T2)</code>. 	
	 */
	@Override
	public Expression makeRandomLiteralOn(String variable, Random random, RewritingProcess process) {
		Expression atom = makeRandomAtomOn(variable, random, process);
		Expression literal = atom.hasFunctor(EQUALITY)? random.nextBoolean()? atom : Disequality.make(atom.get(0), atom.get(1)) : atom;
		return literal;
	}
}