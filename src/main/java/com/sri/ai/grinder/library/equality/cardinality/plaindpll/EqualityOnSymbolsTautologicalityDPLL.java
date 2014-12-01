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

import java.util.List;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.api.UniversallyQuantifiedFormula;
import com.sri.ai.expresso.core.DefaultExistentiallyQuantifiedFormula;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.grinder.api.Rewriter;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.core.AbstractHierarchicalRewriter;
import com.sri.ai.grinder.library.FunctorConstants;
import com.sri.ai.grinder.library.boole.And;
import com.sri.ai.grinder.library.boole.Not;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.grinder.library.equality.cardinality.core.CountsDeclaration;
import com.sri.ai.util.Util;

@Beta
/** 
 * A DPLL specialization for tautologicality.
 */
public class EqualityOnSymbolsTautologicalityDPLL extends AbstractHierarchicalRewriter {
	
	private Rewriter satisfiabilitySolver;
	
	/**
	 * Builds a rewriter for cardinality computation.
	 */
	public EqualityOnSymbolsTautologicalityDPLL() {
		satisfiabilitySolver = new DPLLGeneralizedAndSymbolic(new EqualityOnSymbolsTheory(), new Satisfiability());
	}

	/**
	 * Builds a rewriter for cardinality computation.
	 */
	public EqualityOnSymbolsTautologicalityDPLL(CountsDeclaration countsDeclaration) {
		satisfiabilitySolver = new DPLLGeneralizedAndSymbolic(new EqualityOnSymbolsTheory(), new Satisfiability(), countsDeclaration);
	}

	@Override
	public Expression rewriteAfterBookkeeping(Expression expression, RewritingProcess process) {
		UniversallyQuantifiedFormula universal = (UniversallyQuantifiedFormula) expression;
		Expression satisfiabilityFormula = Not.make(universal.getBody());
		Expression existential = new DefaultExistentiallyQuantifiedFormula(universal.getIndexExpressions(), satisfiabilityFormula);
		Expression satisfiable = satisfiabilitySolver.rewrite(existential, process);
		Expression result = negateBooleanSolution(satisfiable, process);
		return result;
	}

	private static Expression negateBooleanSolution(Expression solution, RewritingProcess process) {
		Expression result;
		if (IfThenElse.isIfThenElse(solution)) {
			Expression negatedThenBranch = negateBooleanSolution(IfThenElse.getThenBranch(solution), process);
			Expression negatedElseBranch = negateBooleanSolution(IfThenElse.getElseBranch(solution), process);
			result = IfThenElse.make(IfThenElse.getCondition(solution), negatedThenBranch, negatedElseBranch, false /* do not simplify to condition */);
		}
		else if (solution.equals(Expressions.TRUE)) {
			result = Expressions.FALSE;
		}
		else if (solution.equals(Expressions.FALSE)) {
			result = Expressions.TRUE;
		}
		else if (solution.hasFunctor(FunctorConstants.GREATER_THAN)) {
			result = Expressions.apply(FunctorConstants.LESS_THAN_OR_EQUAL_TO, solution.get(0), solution.get(1));
		}
		else if (solution.hasFunctor(FunctorConstants.OR)) {
			List<Expression> newArguments = Util.mapIntoArrayList(solution.getArguments(), e -> negateBooleanSolution(e, process));
			result = And.make(newArguments);
		}
		else {
			throw new Error("Should be boolean solution but it is not: " + solution);
		}
		return result;
	}
}
