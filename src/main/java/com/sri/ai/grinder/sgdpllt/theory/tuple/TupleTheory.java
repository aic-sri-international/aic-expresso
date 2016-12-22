/*
 * Copyright (c) 2016, SRI International
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
package com.sri.ai.grinder.sgdpllt.theory.tuple;

import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.DISEQUALITY;
import static com.sri.ai.grinder.sgdpllt.library.FunctorConstants.EQUALITY;
import static com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter.merge;
import static com.sri.ai.util.Util.set;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.Type;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.type.TupleType;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.sgdpllt.api.ExpressionStepSolver;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.api.Theory;
import com.sri.ai.grinder.sgdpllt.core.solver.ExpressionStepSolverToLiteralSplitterStepSolverAdapter;
import com.sri.ai.grinder.sgdpllt.core.solver.QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.library.boole.BooleanSimplifier;
import com.sri.ai.grinder.sgdpllt.rewriter.api.TopRewriter;
import com.sri.ai.grinder.sgdpllt.theory.base.AbstractTheoryWithBinaryAtomsIncludingEquality;
import com.sri.ai.grinder.sgdpllt.theory.equality.ModelCountingOfSingleVariableEqualityConstraintStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.equality.SatisfiabilityOfSingleVariableEqualityConstraintStepSolver;
import com.sri.ai.grinder.sgdpllt.theory.equality.SingleVariableEqualityConstraint;
import com.sri.ai.grinder.sgdpllt.theory.tuple.rewriter.TupleEqualityTopRewriter;

/**
 * A {@link Theory) for Tuples.
 * 
 * @author oreilly
 *
 */
@Beta
public class TupleTheory extends AbstractTheoryWithBinaryAtomsIncludingEquality {
	
	public TupleTheory(boolean assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory, boolean propagateAllLiteralsWhenVariableIsBound) {
		super(
				set(EQUALITY, DISEQUALITY),
				assumeAllTheoryFunctorApplicationsAreAtomsInThisTheory,
				propagateAllLiteralsWhenVariableIsBound);
	}
	
	@Override
	public TopRewriter makeDefaultTopRewriter() {
		// TODO - need to be able to rewrite quantifiers with tuple types.
		// TODO - do we need to include the BooleanSimplifier?				
		return merge(super.makeDefaultTopRewriter(), new TupleEqualityTopRewriter(), new BooleanSimplifier());
	}

	@Override
	public boolean isSuitableFor(Expression variable, Type type) {
		boolean result = isTupleType(type);
		return result;
	}
	
	@Override
	protected boolean isValidArgument(Expression expression, Type type, Context context) {
		boolean result = isTupleType(type);
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
// TODO - in the case of tuples should we always return false?		
		return !getPropagateAllLiteralsWhenVariableIsBound();
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context) {
// TODO - can we use SatisfiabilityOfSingleVariableEqualityConstraintStepSolver here or do we need our Tuple specific version?		
		ExpressionLiteralSplitterStepSolver result  = new SatisfiabilityOfSingleVariableEqualityConstraintStepSolver((SingleVariableEqualityConstraint) constraint);
		return result;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context) {
// TODO - can we use ModelCountingOfSingleVariableEqualityConstraintStepSolver here or do we need our own Tuple specific version?		
		ExpressionLiteralSplitterStepSolver result = new ModelCountingOfSingleVariableEqualityConstraintStepSolver((SingleVariableEqualityConstraint) constraint);
		return result;
	}

	@Override
	public 	ExpressionLiteralSplitterStepSolver getSingleVariableConstraintQuantifierEliminatorStepSolver(AssociativeCommutativeGroup group, SingleVariableConstraint constraint, Expression currentBody, Context context) {
// TODO - can we use QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver here or do we need our own Tuple specific version		
		ExpressionStepSolver formulaSplitterStepSolver = new QuantifierEliminationOnBodyInWhichIndexOnlyOccursInsideLiteralsStepSolver(group, constraint, currentBody);
		ExpressionLiteralSplitterStepSolver result = new ExpressionStepSolverToLiteralSplitterStepSolverAdapter(formulaSplitterStepSolver);
		return result;
	}
	
	private boolean isTupleType(Type type) {
		boolean result = type instanceof TupleType;
		return result;
	}
}
