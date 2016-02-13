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

import java.util.LinkedList;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.sgdpll.theory.inequality.AbstractExpressionsSequenceStepSolver;
import com.sri.ai.util.collect.ImmutableStackedLinkedList;

/**
 * A context-dependent problem step solver deciding which in a set of expressions
 * satisfy a literal specified by {@link #makeLiteral()}.
 *
 * @author braz
 *
 */
@Beta
public abstract class SelectionStepSolver extends AbstractExpressionsSequenceStepSolver<List<Expression>> {

	private List<Expression> selection;

	/**
	 * Makes step solver
	 * @param expressions the expressions being examined
	 */
	public SelectionStepSolver(List<Expression> expressions) {
		this(expressions, 0, new LinkedList<Expression>());
	}

	/**
	 * Makes step solver
	 * @param expressions the expressions being examined
	 * @param current the index of the current expression
	 * @param selection the expressions that have already been selected
	 */
	private SelectionStepSolver(List<Expression> expressions, int current, List<Expression> selection) {
		super(expressions, current);
		this.selection = selection;
	}
	
	@Override
	protected SelectionStepSolver makeSubStepSolverWhenLiteralIsTrue() {
		SelectionStepSolver result = (SelectionStepSolver) clone();
		result.current = getCurrent() + 1;
		result.selection = new ImmutableStackedLinkedList<Expression>(getCurrentExpression(), selection);
		return result;
	}

	@Override
	protected SelectionStepSolver makeSubStepSolverWhenLiteralIsFalse() {
		SelectionStepSolver result = (SelectionStepSolver) clone();
		result.current = getCurrent() + 1;
		// selection remains the same
		return result;
	}

	@Override
	protected SolutionStep<List<Expression>> makeSolutionWhenAllElementsHaveBeenChecked() {
		Solution<List<Expression>> result = new Solution<List<Expression>>(selection);
		return result;
	}
}