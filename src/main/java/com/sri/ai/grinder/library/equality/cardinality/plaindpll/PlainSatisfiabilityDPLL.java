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
package com.sri.ai.grinder.library.equality.cardinality.plaindpll;

import java.util.Collection;
import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.ExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.boole.Or;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.util.base.Pair;

@Beta
/** 
 * A DPLL specialization for satisfiability.
 */
public class PlainSatisfiabilityDPLL extends AbstractPlainDPLLForEqualityLogic {
	
	/**
	 * Builds a rewriter for cardinality computation.
	 */
	public PlainSatisfiabilityDPLL() {
	}

	/**
	 * Builds a rewriter for cardinality computation.
	 */
	public PlainSatisfiabilityDPLL(CountsDeclaration countsDeclaration) {
		super(countsDeclaration);
	}

	@Override
	protected Pair<Expression, List<Expression>> getFormulaAndIndexExpressionsFromRewriterProblemArgument(Expression expression, RewritingProcess process) {
		ExistentiallyQuantifiedFormula existential = (ExistentiallyQuantifiedFormula) expression;
		Pair<Expression, List<Expression>> formulaAndIndices = Pair.make(existential.getBody(), existential.getIndexExpressions());
		return formulaAndIndices;
	}

	@Override
	protected Expression bottomSolution() {
		return Expressions.FALSE;
	}

	@Override
	protected boolean isTopSolution(Expression solutionForSubProblem) {
		boolean result = solutionForSubProblem.equals(Expressions.TRUE);
		return result;
	}

	@Override
	protected Expression combineUnconditionalSolutions(Expression solution1, Expression solution2) {
		return Or.make(solution1, solution2);
	}

	@Override
	protected TheoryConstraint makeConstraint(Expression disequalitiesConjunction, Collection<Expression> indices, RewritingProcess process) {
		SymbolEqualitySatisfiabilityConstraint result = new SymbolEqualitySatisfiabilityConstraint(disequalitiesConjunction, indices, process);
		return result;
	}
}
