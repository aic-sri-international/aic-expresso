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
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.DefaultRewritingProcess;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.grinder.library.equality.cardinality.direct.core.Simplify;
import com.sri.ai.grinder.library.set.intensional.IntensionalSet;
import com.sri.ai.util.base.Pair;

@Beta
/** 
 * A DPLL specialization for model counting.
 */
public class PlainCardinalityDPLLWithFreeVariables extends PlainGenericDPLLWithFreeVariables {
	
	protected CountsDeclaration countsDeclaration;

	/**
	 * Builds a rewriter for cardinality computation.
	 */
	public PlainCardinalityDPLLWithFreeVariables(CountsDeclaration countsDeclaration) {
		this.countsDeclaration = countsDeclaration;
	}

	@Override
	protected Pair<Expression, List<Expression>> getFormulaAndIndicesFromRewriterProblemArgument(Expression problem, RewritingProcess process) {
		Expression set = problem.get(0);
		List<Expression> indices = IntensionalSet.getIndices(set);
		Expression formula = SimplifyFormula.simplify(IntensionalSet.getCondition(set), process);
		Pair<Expression, List<Expression>> formulaAndIndices = Pair.make(formula, indices);
		return formulaAndIndices;
	}

	@Override
	protected Expression neutralSolution() {
		return Expressions.ZERO;
	}

	@Override
	protected boolean isShortCircuitingSolution(Expression solutionForSubProblem) {
		return false;
	}

	@Override
	protected Expression combineUnconditionalSolutions(Expression solution1, Expression solution2) {
		return Expressions.makeSymbol(solution1.rationalValue().add(solution2.rationalValue()));
	}

	@Override
	public RewritingProcess makeRewritingProcess(Expression expression) {
		Rewriter rewriterWithModules = new Simplify();
		RewritingProcess result = new DefaultRewritingProcess(expression, rewriterWithModules);
		result.notifyReadinessOfRewritingProcess();
		if (countsDeclaration != null) {
			countsDeclaration.setup(result);
		}
		return result;
	}

	@Override
	protected TheoryConstraint makeConstraint(Expression atomsConjunction, Collection<Expression> indices, RewritingProcess process) {
		SymbolEqualityConstraint result = new SymbolEqualityConstraint(atomsConjunction, indices, process);
		return result;
	}
}
