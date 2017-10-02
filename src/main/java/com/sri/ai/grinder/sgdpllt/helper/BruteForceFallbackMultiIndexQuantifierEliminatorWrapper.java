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
package com.sri.ai.grinder.sgdpllt.helper;

import static com.sri.ai.util.Util.list;

import java.util.LinkedList;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpllt.api.Context;
import com.sri.ai.grinder.sgdpllt.api.MultiIndexQuantifierEliminator;
import com.sri.ai.grinder.sgdpllt.api.SingleVariableConstraint;
import com.sri.ai.grinder.sgdpllt.core.solver.QuantifierEliminationStepSolver;
import com.sri.ai.grinder.sgdpllt.group.AssociativeCommutativeGroup;
import com.sri.ai.grinder.sgdpllt.interpreter.BruteForceMultiIndexQuantifierEliminator;

/**
 * Wraps a base {@link QuantifierEliminationStepSolver} into another
 * one that solves the problem by brute force when the base one cannot.
 *
 * @author braz
 *
 */
@Beta
public class BruteForceFallbackMultiIndexQuantifierEliminatorWrapper implements QuantifierEliminationStepSolver {
	
	private QuantifierEliminationStepSolver base;
	
	public BruteForceFallbackMultiIndexQuantifierEliminatorWrapper(QuantifierEliminationStepSolver base) {
		this.base = base;
	}

	@Override
	public Step step(Context context) {
		Step result;
		try {
			result = base.step(context);
		}
		catch (IllegalArgumentException exception) {
			MultiIndexQuantifierEliminator bruteForceMultiIndexQuantifierEliminator = makeBruteForceQuantifierEliminator(context);
			Expression resultExpression = solveByBruteForce(bruteForceMultiIndexQuantifierEliminator, context);
			result = new Solution(resultExpression);
		}
		return result;
	}

	private MultiIndexQuantifierEliminator makeBruteForceQuantifierEliminator(Context context) {
		MultiIndexQuantifierEliminator result = new BruteForceMultiIndexQuantifierEliminator(context.getTheory().getTopRewriter());
		return result;
	}

	private Expression solveByBruteForce(MultiIndexQuantifierEliminator multiIndexQuantifierEliminator, Context context) {
		
		AssociativeCommutativeGroup group = getGroup();
		LinkedList<Expression> indices = list(getIndex());
		SingleVariableConstraint indicesCondition = getIndexConstraint();
		Expression body = getBody();
		
		Expression resultExpression = multiIndexQuantifierEliminator.solve(group, indices, indicesCondition, body, context);
		return resultExpression;
	}

	@Override
	public AssociativeCommutativeGroup getGroup() {
		return base.getGroup();
	}

	@Override
	public SingleVariableConstraint getIndexConstraint() {
		return base.getIndexConstraint();
	}

	@Override
	public Expression getIndex() {
		return base.getIndex();
	}

	@Override
	public Expression getBody() {
		return base.getBody();
	}

	@Override
	public BruteForceFallbackMultiIndexQuantifierEliminatorWrapper clone() {
		BruteForceFallbackMultiIndexQuantifierEliminatorWrapper result;
		try {
			result = (BruteForceFallbackMultiIndexQuantifierEliminatorWrapper) super.clone();
		}
		catch (CloneNotSupportedException exception) {
			throw new RuntimeException(exception);
		}
		return result;
	}
}