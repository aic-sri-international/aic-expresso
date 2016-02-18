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
package com.sri.ai.grinder.sgdpll.theory.base;

import static com.sri.ai.util.Util.myAssert;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll.core.constraint.ContextualConstraintSplitting;

/**
 * An abstract context-dependent problem step solver
 * going over a sequence of integers up to a value n.
 * <p>
 * Implementations must provide methods for
 * creating a literal per visited integer,
 * for creating sub-step solvers depending on the value of the literal,
 * and providing a final solution once they have all been visited.
 *
 * @author braz
 *
 */
@Beta
public abstract class AbstractLinearStepSolver<T> implements ContextDependentProblemStepSolver<T> {

	private int n;
	protected int current;

	/**
	 * Provides the next literal to be checked to make decision.
	 * @return
	 */
	abstract protected Expression makeLiteral();
	
	/**
	 * Provides sub-step solver to be used when literal is (or enforced to be) true.
	 * @return
	 */
	abstract protected ContextDependentProblemStepSolver<T> makeSubStepSolverWhenLiteralIsTrue();
	
	/**
	 * Provides sub-step solver to be used when literal is (or enforced to be) false.
	 * @return
	 */
	abstract protected ContextDependentProblemStepSolver<T> makeSubStepSolverWhenLiteralIsFalse();

	/**
	 * Provides solution if all elements have already been checked.
	 * @return
	 */
	abstract protected SolutionStep<T> makeSolutionWhenAllElementsHaveBeenChecked();

	/**
	 * Makes a step solver in this class for the sequence of integers
	 * <code>next, next + 1, ..., n</code>.
	 * @param n
	 * @param next
	 */
	public AbstractLinearStepSolver(int n, int next) {
		myAssert(() -> next <= n, () -> "'next' should be less than or equal to 'n', but it is " + next + " whereas 'n' " + n);
		this.n = n;
		this.current = next;
	}

	@Override
	public AbstractLinearStepSolver clone() {
		try {
			return (AbstractLinearStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error(e);
		}
	}

	/** Returns current integer to be used in sequence. */
	protected int getCurrent() {
		return current;
	}
	
	@Override
	public SolutionStep<T> step(Context contextualConstraint, Context context) {
		SolutionStep<T> result;
		if (current != n) {
			Expression unsimplifiedLiteral = makeLiteral();
			Expression literal = contextualConstraint.getConstraintTheory().simplify(unsimplifiedLiteral, context);
			ContextualConstraintSplitting split = new ContextualConstraintSplitting(literal, contextualConstraint, context);
			switch (split.getResult()) {
			case CONSTRAINT_IS_CONTRADICTORY:
				result = null;
				break;
			case LITERAL_IS_TRUE:
				result = makeSubStepSolverWhenLiteralIsTrue().step(contextualConstraint, context);
				break;
			case LITERAL_IS_FALSE:
				result = makeSubStepSolverWhenLiteralIsFalse().step(contextualConstraint, context);
				break;
			case LITERAL_IS_UNDEFINED:
				result = new ItDependsOn<T>(
						literal,
						split,
						makeSubStepSolverWhenLiteralIsTrue(),
						makeSubStepSolverWhenLiteralIsFalse());
				break;
			default: throw new Error("Unexpected ConstraintSplitting result.");
			}
		}
		else {
			result = makeSolutionWhenAllElementsHaveBeenChecked();
		}
		
		return result;
	}
}