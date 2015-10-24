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

import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.plaindpll.api.Constraint1;
import com.sri.ai.grinder.plaindpll.api.InputTheory;
import com.sri.ai.grinder.plaindpll.api.SemiRingProblemType;

/**
 * A Variable Elimination algorithm generalized in the same manner
 * {@link SGDPLLT} is generalized from DPLL,
 * that is, it can produce symbolic answers and it does not need
 * to only solve problems with the operations from its classic version
 * (for the case of VE, sum and product, or max and product).
 * <p>
 * It relies on a partial decomposition of the problem, producing instances
 * to be solved by a {@link SGDPLLT}, in the following manner:
 * <pre>
 * sum_{i1,...,i_n} prod_j f_j(args_j)
 * =
 * sum_{i1,...,i_{n-1}} prod_{j : args_j does *not* contain i_n} f_j(args_j) sum_{i_n} prod_{j : args_j contains i_n} f_j(args_j)
 * </pre>
 * and then solving
 * <pre>
 * sum_{i_n} prod_{j : args_j contains i_n} f_j(args_j)
 * </pre>
 * with {@link SGDPLLT}.
 * Note that the symbolic capability of {@link SGDPLLT} is crucial here, as
 * args_j for the various functions f_j will typically involve other indices which,
 * at the level of the sub-problem, are free variables.
 * 
 * @author braz
 *
 */
public class SGVET extends AbstractPlainDPLLSolver {

	private PlainDPLLSGVETFunctionality veFunctionality;
	
	public SGVET(InputTheory inputTheory, SemiRingProblemType problemType) {
		this(inputTheory, problemType, null);
	}

	public SGVET(InputTheory inputTheory, SemiRingProblemType problemType, CountsDeclaration countsDeclaration) {
		super(inputTheory, problemType, countsDeclaration);
		this.veFunctionality = new PlainDPLLSGVETFunctionality(inputTheory, problemType, countsDeclaration);
	}

	@Override
	protected Expression solveAfterBookkeepingAndBodyConstraintCheck(
			Expression expression, Collection<Expression> indices, Constraint1 constraint, RewritingProcess process) {

		return veFunctionality.solve(expression, indices, constraint, process);
	}

	@Override
	public boolean getDebug() {
		return veFunctionality.getDebug();
	}
	
	@Override
	public void setDebug(boolean newValue) {
		veFunctionality.setDebug(newValue);
	}

	@Override 
	public void interrupt() {
		super.interrupt();
		veFunctionality.interrupt();
	}

	@Override
	public String toString() {
		return "SGVE(T)";
	}
}