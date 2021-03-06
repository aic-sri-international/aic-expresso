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
package com.sri.ai.grinder.theory.equality;

import static com.sri.ai.grinder.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter.toExpressionLiteralSplitterStepSolver;
import static com.sri.ai.grinder.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.library.commonrewriters.CommonSimplifiersAndSymbolicQuantifierEliminationRewritersTopRewriter.INSTANCE;
import static com.sri.ai.grinder.rewriter.api.TopRewriter.merge;
import static com.sri.ai.util.Util.forAll;
import static com.sri.ai.util.Util.set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.type.Categorical;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.ExpressionStepSolver;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.api.Theory;
import com.sri.ai.grinder.core.solver.QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver;
import com.sri.ai.grinder.helper.GrinderUtil;
import com.sri.ai.grinder.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.library.equality.EqualitySimplifier;
import com.sri.ai.grinder.rewriter.api.TopRewriter;
import com.sri.ai.grinder.theory.base.AbstractTheoryWithBinaryAtomsIncludingEquality;
import com.sri.ai.grinder.theory.compound.CompoundTheory;


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
	 * @param atomFunctorsAreUniqueToThisTheory
	 * whether all equalities and disequalities can be safely assumed to belong to this theory
	 * (if you know all such expressions are literals in this theory, invoke this constructor with a <code>true</code> argument).
	 * @param propagateAllLiteralsWhenVariableIsBound TODO
	 */
	public EqualityTheory(boolean atomFunctorsAreUniqueToThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				set(EQUALITY, DISEQUALITY),
				atomFunctorsAreUniqueToThisTheory,
				propagateAllLiteralsWhenVariableIsBound);
	}
	
	@Override
	public TopRewriter makeTopRewriter() {
		return merge(INSTANCE, new EqualitySimplifier(), new BooleanSimplifier());
	}

	@Override
	public boolean isSuitableFor(Type type) {
		boolean result = isNonBooleanCategoricalType(type);
		return result;
	}

	@Override
	public boolean applicationOfAtomFunctorIsIndeedAtom(Expression applicationOfAtomFunctor, Context context) {
		boolean result = forAll(applicationOfAtomFunctor.getArguments(), e -> argumentIsValid(e, context));
		return result;
	}

	private boolean argumentIsValid(Expression argumentOfAtomFunctor, Context context) {
		boolean result;
		if (argumentOfAtomFunctor.getSyntacticFormType().equals("Symbol")) {
			Type eType = GrinderUtil.getTypeOfExpression(argumentOfAtomFunctor, context);
			result = isNonBooleanCategoricalType(eType);
		}
		else {
			result = false;
		}
		return result;
	}

	private boolean isNonBooleanCategoricalType(Type type) {
		boolean result = !type.getName().equals("Boolean") && type instanceof Categorical;
		return result;
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraintAfterBookkeeping(Expression variable, Context context) {
		return new SingleVariableEqualityConstraint(variable, getPropagateAllLiteralsWhenVariableIsBound(), context.getTheory());
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
	public 	ExpressionLiteralSplitterStepSolver getSingleQuantifierEliminatorStepSolver(SingleQuantifierEliminationProblem problem, Context context) {
		ExpressionStepSolver formulaSplitterStepSolver = new QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver(problem);
		ExpressionLiteralSplitterStepSolver result = toExpressionLiteralSplitterStepSolver(formulaSplitterStepSolver);
		return result;
	}
}