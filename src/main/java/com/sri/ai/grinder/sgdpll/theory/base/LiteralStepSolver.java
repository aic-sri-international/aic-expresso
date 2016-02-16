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

import static com.sri.ai.grinder.sgdpll.theory.base.ConstantStepSolver.constantStepSolver;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.Context;
import com.sri.ai.grinder.sgdpll.api.Constraint;
import com.sri.ai.grinder.sgdpll.api.ContextDependentProblemStepSolver;
import com.sri.ai.grinder.sgdpll.core.constraint.ConstraintSplitting;

/**
 * A context-dependent problem step solver
 * that simply decides whether a literal is true or not.
 * <p>
 *
 * @author braz
 *
 */
@Beta
public class LiteralStepSolver implements ContextDependentProblemStepSolver<Boolean> {

	protected Expression literal;

	public LiteralStepSolver(Expression literal) {
		super();
		this.literal = literal;
	}

	@Override
	public LiteralStepSolver clone() {
		try {
			return (LiteralStepSolver) super.clone();
		} catch (CloneNotSupportedException e) {
			throw new Error("Trying to clone " + getClass() + " but cloning is not supported for this class.");
		}
	}

	@Override
	public ContextDependentProblemStepSolver.SolutionStep<Boolean> step(Constraint contextualConstraint, Context context) {
		ConstraintSplitting split = new ConstraintSplitting(contextualConstraint, literal, context);
		switch (split.getResult()) {
		case CONSTRAINT_IS_CONTRADICTORY:
			return null;
		case LITERAL_IS_TRUE:
			return new Solution<Boolean>(true);
		case LITERAL_IS_FALSE:
			return new Solution<Boolean>(false);
		case LITERAL_IS_UNDEFINED:
			ContextDependentProblemStepSolver<Boolean> ifTrue  = constantStepSolver(true);
			ContextDependentProblemStepSolver<Boolean> ifFalse = constantStepSolver(false);
			return new ItDependsOn<Boolean>(literal, split, ifTrue, ifFalse);
		default:
			throw new Error("Unrecognized splitting result.");
		}
	}
}