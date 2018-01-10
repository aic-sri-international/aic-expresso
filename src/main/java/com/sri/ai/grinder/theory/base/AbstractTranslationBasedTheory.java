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
package com.sri.ai.grinder.theory.base;

import static com.sri.ai.expresso.helper.Expressions.ONE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.grinder.library.boole.Not.not;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.api.ExpressionLiteralSplitterStepSolver;
import com.sri.ai.grinder.api.SingleQuantifierEliminationProblem;
import com.sri.ai.grinder.api.SingleVariableConstraint;
import com.sri.ai.grinder.core.constraint.AbstractTheory;
import com.sri.ai.grinder.core.solver.DefaultSingleQuantifierEliminationProblem;
import com.sri.ai.grinder.group.Disjunction;
import com.sri.ai.grinder.group.Sum;

/**
 * An abstract implementation of {@link Theory) for theories that rely on
 * translating its core problems to another theory's.
 * It therefore has no atoms or interpreted symbols.
 * It may add top rewriters to the default top rewriter.
 * 
 * @author oreilly
 *
 */
@Beta
public abstract class AbstractTranslationBasedTheory extends AbstractTheory {
	
	@Override
	public boolean isAtom(Expression expression, Context context) {
		return false;
	}
	
	@Override
	public Expression getAtomNegation(Expression atom, Context context) {
		return not(atom);
		// even though this theory does not have atoms, it may generate atoms for testing.
		// This probably needs to change, and theory testing support classes should be able to indicate that a theory does not have atoms.
	}

	@Override
	public boolean isInterpretedInThisTheoryBesidesBooleanConnectives(Expression expression) {
		return false;
	}

	@Override
	public SingleVariableConstraint makeSingleVariableConstraintAfterBookkeeping(Expression variable, Context context) {
		return new SingleVariableConstraintForTheoryWithoutAtoms(variable, context.getTheory());
	}

	@Override
	public boolean singleVariableConstraintIsCompleteWithRespectToItsVariable() {	
		// We return always 'true' (because if all other theories present are complete, then this one will be, too)
		return true;
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintSatisfiabilityStepSolver(SingleVariableConstraint constraint, Context context) {
		Expression index = constraint.getVariable();
		Expression indexType = context.getTypeExpressionOfRegisteredSymbol(index);
		DefaultSingleQuantifierEliminationProblem problem = new DefaultSingleQuantifierEliminationProblem(new Disjunction(), index, indexType, constraint, TRUE);
		return getSingleQuantifierEliminatorStepSolver(problem, context);
	}

	@Override
	public ExpressionLiteralSplitterStepSolver getSingleVariableConstraintModelCountingStepSolver(SingleVariableConstraint constraint, Context context) {
		Expression index = constraint.getVariable();
		Expression indexType = context.getTypeExpressionOfRegisteredSymbol(index);
		DefaultSingleQuantifierEliminationProblem problem = new DefaultSingleQuantifierEliminationProblem(new Sum(), index, indexType, constraint, ONE);
		return getSingleQuantifierEliminatorStepSolver(problem, context);
	}

	@Override
	public abstract	ExpressionLiteralSplitterStepSolver getSingleQuantifierEliminatorStepSolver(SingleQuantifierEliminationProblem problem, Context context);
}
