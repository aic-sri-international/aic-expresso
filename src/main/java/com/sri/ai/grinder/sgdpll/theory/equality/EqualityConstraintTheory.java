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

import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.simplifier.api.Simplifier;
import com.sri.ai.grinder.core.simplifier.core.RecursiveExhaustiveSeriallyMergedMapBasedSimplifier;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.equality.EqualitySimplifier;
import com.sri.ai.grinder.sgdpll.api.ConstraintTheory;
import com.sri.ai.grinder.sgdpll.api.ContextDependentExpressionProblemStepSolver;
import com.sri.ai.grinder.sgdpll.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpll.core.solver.QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver;
import com.sri.ai.grinder.sgdpll.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpll.theory.base.AbstractConstraintTheoryWithBinaryAtomsIncludingEquality;
import com.sri.ai.grinder.sgdpll.theory.compound.CompoundConstraintTheory;


/** 
 * A {@link ConstraintTheory} for equality literals.
 */
@Beta
public class EqualityConstraintTheory extends AbstractConstraintTheoryWithBinaryAtomsIncludingEquality {

//	/**
//	 * Creates an equality theory that does <i>not</i> assume equality literals are literal of this theory
//	 * (this is more expensive -- use for a more efficiency setting if all equalities belong to this theory).
//	 */
//	public EqualityConstraintTheory(boolean propagateAllLiteralsWhenVariableIsBound) {
//		this(false, propagateAllLiteralsWhenVariableIsBound);
//	}
	
	/**
	 * Creates an equality theory.
	 * It takes an argument indicating whether all equalities and disequalities are literals in this theory;
	 * this may not be the case if a {@link CompoundConstraintTheory} mixing multiple theories involving
	 * equalities is being used.
	 * @param assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory
	 * whether all equalities and disequalities can be safely assumed to belong to this theory
	 * (if you know all such expressions are literals in this theory, invoke this constructor with a <code>true</code> argument).
	 * @param propagateAllLiteralsWhenVariableIsBound TODO
	 */
	public EqualityConstraintTheory(boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				set(EQUALITY, DISEQUALITY),
				assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory,
				new RecursiveExhaustiveSeriallyMergedMapBasedSimplifier(new EqualitySimplifier(), new BooleanSimplifier()), propagateAllLiteralsWhenVariableIsBound);
	}
	
	@Override
	protected boolean isValidArgument(Expression expression, Type type) {
		boolean result = isNonBooleanCategoricalType(type);
		return result;
	}
	
	private boolean isNonBooleanCategoricalType(Type type) {
		boolean result = !type.getName().equals("Boolean") && type instanceof Categorical;
		return result;
	}

	@Override
	protected Expression getNonTrivialAtomNegation(Expression atom) {
		Expression result;
		if (atom.hasFunctor(EQUALITY)) {
			result = Expressions.apply(DISEQUALITY, atom.get(0), atom.get(1));
		}
		else if (atom.hasFunctor(DISEQUALITY)) {
			result = Expressions.apply(EQUALITY, atom.get(0), atom.get(1));
		}
		else {
			result = null;
		}
		return result;
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, ConstraintTheory constraintTheory, RewritingProcess process) {
		return new SingleVariableEqualityConstraint(variable, getPropagateAllLiteralsWhenVariableIsBound(), constraintTheory);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return ! getPropagateAllLiteralsWhenVariableIsBound();
	}

	@Override
	public ContextDependentExpressionProblemStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return new SatisfiabilityOfSingleVariableEqualityConstraintStepSolver((SingleVariableEqualityConstraint) constraint);
	}

	@Override
	public ContextDependentExpressionProblemStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, RewritingProcess process) {
		return new ModelCountingOfSingleVariableEqualityConstraintStepSolver((SingleVariableEqualityConstraint) constraint);
	}

	@Override
	public 	ContextDependentExpressionProblemStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraint, Expression currentBody, Simplifier simplifier, RewritingProcess process) {
		return new QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver
		(group, simplifier, constraint, currentBody);
	}
}