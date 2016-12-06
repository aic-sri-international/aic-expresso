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
package com.sri.ai.grinder.sgdpllt.theory.equality;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUALITY;
import static com.sri.ai.util.Util.set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter;
import com.sri.ai.grinder.sgdpllt.core.solver.QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.boole.BooleanSimplifier2;
import com.sri.ai.grinder.sgdpllt.library.equality.EqualitySimplifier2;
import com.sri.ai.grinder.sgdpllt.rewriter.core.FirstOfSwitchMerge;
import com.sri.ai.grinder.sgdpllt.theory.base.AbstractTheoryWithBinaryAtomsIncludingEquality;
import com.sri.ai.grinder.sgdpllt.theory.compound.CompoundTheory;


/** 
 * A {@link Theory} for equality literals.
 */
@Beta
public class EqualityTheory extends AbstractTheoryWithBinaryAtomsIncludingEquality {

	/**
	 * Creates an equality theory.
	 * It takes an argument indicating whether all equalities and disequalities are literals in this theory;
	 * this may not be the case if a {@link CompoundTheory} mixing multiple theories involving
	 * equalities is being used.
	 * @param assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory
	 * whether all equalities and disequalities can be safely assumed to belong to this theory
	 * (if you know all such expressions are literals in this theory, invoke this constructor with a <code>true</code> argument).
	 * @param propagateAllLiteralsWhenVariableIsBound TODO
	 */
	public EqualityTheory(boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				set(EQUALITY, DISEQUALITY),
				assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory,
				propagateAllLiteralsWhenVariableIsBound,
				FirstOfSwitchMerge.merge(
						new EqualitySimplifier2(),
						new BooleanSimplifier2()));
	}
	
	@Override
	public boolean isSuitableFor(Expression variable, Type type) {
		boolean result = isNonBooleanCategoricalType(type);
		return result;
	}

	@Override
	protected boolean isValidArgument(Expression expression, Type type, Context context) {
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
	public SingleVariableConstraint makeSingleVariableConstraint(Expression variable, Theory theory, Context context) {
		return new SingleVariableEqualityConstraint(variable, getPropagateAllLiteralsWhenVariableIsBound(), theory);
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {
		return ! getPropagateAllLiteralsWhenVariableIsBound();
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context) {
		ExpressionLiteralSplitterStepSolver result  = new SatisfiabilityOfSingleVariableEqualityConstraintStepSolver((SingleVariableEqualityConstraint) constraint);
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context) {
		ExpressionLiteralSplitterStepSolver result = new ModelCountingOfSingleVariableEqualityConstraintStepSolver((SingleVariableEqualityConstraint) constraint);
		return result;
	}

	@Override
	public 	ExpressionLiteralSplitterStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraint, Expression currentBody, Context context) {
		ExpressionStepSolver formulaSplitterStepSolver = new QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver(group, constraint, currentBody);
		ExpressionLiteralSplitterStepSolver result = new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(formulaSplitterStepSolver);
		return result;
	}
}