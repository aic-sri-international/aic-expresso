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
package com.sri.ai.grinder.plaindpll.core;

import java.util.Collection;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.api.Constraint;
import com.sri.ai.grinder.plaindpll.api.ConstraintTheory;
import com.sri.ai.grinder.plaindpll.api.GroupProblemType;
import com.sri.ai.grinder.plaindpll.api.Solver;
import com.sri.ai.util.base.Wrapper;

/**
 * A skeleton for a parallel version of {@link SGDPLLT},
 * using {@link SGDPLLTParallelizer} to break a problem into multiple sub-problems (map)
 * and {@link #addSymbolicResults(Expression, Expression)} to combine them (reduce).
 * 
 * @author braz
 *
 */
@Beta
public class SGDPLLTMapReduce extends AbstractSolver {

	public SGDPLLTMapReduce(ConstraintTheory theory, GroupProblemType problemType) {
		super(theory, problemType);
	}

	@Override
	protected Expression solveAfterBookkeeping(Expression expression, Collection<Expression> indices, Constraint constraint, RewritingProcess process) {
		
		int depth = 3;
		
		final Wrapper<Expression> accumulatedSolution = new Wrapper<>(getProblemType().getGroup().additiveIdentityElement()); // starts with "zero"
		// Wrapper is used because one cannot use a non-final object inside a closure as seen below.
		
		Solver solver = new SGDPLLT(getConstraintTheory(), getProblemType());
		
		SGDPLLTParallelizer.Collector collector =
				(e, i, c, p) -> {
					// System.out.println("Received sum_{" + i + " : " + c + "} " + e);	

					Expression solution = solver.solve(e, i, c, p);

					// System.out.println("Solution is " + solution);	
					// System.out.println("Accumulated solution was   : " + accumulatedSolution.value);

					accumulatedSolution.value = addSymbolicResults(accumulatedSolution.value, solution);

					// System.out.println("Accumulated solution now is: " + accumulatedSolution.value);	
				};

		SGDPLLTParallelizer parallelizer = new SGDPLLTParallelizer(getConstraintTheory(), getProblemType(), collector, depth);
		
		parallelizer.solve(expression, indices, constraint, process);
		
		Expression result = accumulatedSolution.value;
		
		return result;
	}
}