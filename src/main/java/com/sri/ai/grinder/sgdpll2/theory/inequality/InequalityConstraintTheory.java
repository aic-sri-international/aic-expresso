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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.expresso.helper.Expressions.makeSymbol;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.NOT;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.pickUniformly;

import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.api.Simplifier;
import com.sri.ai.grinder.core.simplifier.RecursiveExhaustiveMergedMapBasedSimplifier;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.Disequality;
import com.sri.ai.grinder.library.Equality;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.equality.EqualitySimplifier;
import com.sri.ai.grinder.library.inequality.InequalitySimplifier;
import com.sri.ai.grinder.sgdpll2.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll2.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll2.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll2.core.constraint.AbstractConstraintTheory;
import com.sri.ai.util.collect.PredicateIterator;


/** 
 * A {@link ConstraintTheory} for integer inequality literals.
 */
@Beta
public class InequalityConstraintTheory extends AbstractConstraintTheory {

	private Simplifier simplifier = new RecursiveExhaustiveMergedMapBasedSimplifier(new EqualitySimplifier(), new InequalitySimplifier(), new BooleanSimplifier());
	
	@Override
	public boolean isSuitableFor(Expression variable, RewritingProcess process) {
		Expression type = GrinderUtil.getType(variable, process);
		boolean result = type.equals("Integer");
		return result;
	}

	@Override
	public Expression simplify(Expression expression, RewritingProcess process) {
		Expression result = simplifier.apply(expression, process);
		return result;
	}

	private boolean assumeEqualitiesAreAlwaysLiteralsOfThisTheory = true;
	
	@Override
	public boolean isNonTrivialLiteral(Expression expression, RewritingProcess process) {
		boolean hasEqualityFunctor = expression.hasFunctor(EQUALITY) || expression.hasFunctor(DISEQUALITY);
		boolean result = hasEqualityFunctor;
		
		if (assumeEqualitiesAreAlwaysLiteralsOfThisTheory) {
			result = hasEqualityFunctor;
		}
		else {
			// the following is good, but expensive
			result = hasEqualityFunctor
					&&
					forAll(
							expression.getArguments(),
							e -> {
								Expression type = GrinderUtil.getType(e, process);
								return type.equals("Integer") || process.getType(type.toString()) instanceof Categorical;
							});
		}
		
		return result;
	}
	
	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, ConstraintTheory constraintTheory, RewritingProcess process) {
		return new SingleVariableInequalityConstraint(variable, constraintTheory);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return true; // SingleVariableInequalityConstraint is complete
	}

	@Override
	public ContextDependentProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return new SatisfiabilityOfSingleVariableInequalityConstraintStepSolver((SingleVariableInequalityConstraint) constraint);
	}

	@Override
	public ContextDependentProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return new ModelCountingOfSingleVariableInequalityConstraintStepSolver((SingleVariableInequalityConstraint) constraint);
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

	@Override
	public Expression getLiteralNegation(Expression literal, RewritingProcess process) {
		Expression result;
		if (literal.hasFunctor(NOT) && (literal.get(0).hasFunctor(EQUALITY) || literal.get(0).hasFunctor(DISEQUALITY))) {
			result = literal;
		}
		else if (literal.hasFunctor(EQUALITY)) {
			result = Expressions.apply(DISEQUALITY, literal.get(0), literal.get(1));
		}
		else if (literal.hasFunctor(DISEQUALITY)) {
			result = Expressions.apply(EQUALITY, literal.get(0), literal.get(1));
		} 
		else if (literal.equals(TRUE)) {
			result = FALSE;
		} 
		else if (literal.equals(FALSE)) {
			result = TRUE;
		} 
		else {
			throw new Error("Invalid literal: " + literal);
		}
		return result;
	}
}