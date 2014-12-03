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

import java.util.Iterator;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.base.BinaryFunction;

@Beta
/** 
 * Basic implementation of some methods of {@link Theory}.
 */
abstract public class AbstractTheory implements Theory {

	/**
	 * Provides a map from functors's getValue() values (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified formula
	 * according to this theory.
	 * DPLL will use these simplifiers when a new decision is made and literals are replaced by boolean constants. 
	 * @return
	 */
	abstract protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getFunctionApplicationSimplifiers();


	/**
	 * Provides a map from syntactic form types (Strings) to a function mapping a
	 * function application of that functor and a rewriting process to an equivalent, simplified formula
	 * according to this theory.
	 * DPLL will use these simplifiers when a new decision is made and literals are replaced by boolean constants. 
	 * @return
	 */
	abstract protected Map<String, BinaryFunction<Expression, RewritingProcess, Expression>> getSyntacticFormTypeSimplifiers();


	/**
	 * Simplifies an expression by exhaustively simplifying its top expression with basic boolean operators in equality logic (including quantifier elimination),
	 * then simplifying its sub-expressions,
	 * and again exhaustively simplifying its top expression.
	 * @param expression
	 * @param topSimplifier
	 * @param process
	 * @return
	 */
	@Override
	public Expression simplify(Expression expression, RewritingProcess process) {
		return DPLLUtil.simplify(expression, getFunctionApplicationSimplifiers(), getSyntacticFormTypeSimplifiers(), process);
	}

	@Override
	public Expression pickSplitterInExpression(Expression expression, Constraint constraint, RewritingProcess process) {
		Expression result = null;
		
		Iterator<Expression> subExpressionIterator = new SubExpressionsDepthFirstIterator(expression);
		while (result == null && subExpressionIterator.hasNext()) {
			Expression subExpression = subExpressionIterator.next();
			Expression splitterCandidate = makeSplitterIfPossible(subExpression, constraint.getIndices(), process);
			result = splitterCandidate;
		}
	
		return result;
	}

	@Override
	public Expression applyConstraintToSolution(Constraint constraint, Expression solution, RewritingProcess process) {
		Expression result;
		
		if (DPLLUtil.isConditionalSolution(solution, this, process)) {
			Expression solutionSplitter = IfThenElse.getCondition(solution);
			Constraint constraintUnderSolutionSplitter = constraint.applySplitter(true, solutionSplitter, process);
			if (constraintUnderSolutionSplitter != null) {
				Constraint constraintUnderSolutionSplitterNegation = constraint.applySplitter(false, solutionSplitter, process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression newSolutionSplitter = DPLLUtil.normalizeSplitter(solutionSplitter, constraint, this, process);
					Expression thenBranch = IfThenElse.getThenBranch(solution);
					Expression elseBranch = IfThenElse.getElseBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter, thenBranch, process);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					result = IfThenElse.makeIfDistinctFrom(solution, newSolutionSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
				}
				else {
					Expression thenBranch = IfThenElse.getThenBranch(solution);
					Expression newThenBranch = applyConstraintToSolution(constraintUnderSolutionSplitter, thenBranch, process);
					result = newThenBranch;
				}
			}
			else {
				Constraint constraintUnderSolutionSplitterNegation = constraint.applySplitter(false, solutionSplitter, process);
				if (constraintUnderSolutionSplitterNegation != null) {
					Expression elseBranch = IfThenElse.getElseBranch(solution);
					Expression newElseBranch = applyConstraintToSolution(constraintUnderSolutionSplitterNegation, elseBranch, process);
					result = newElseBranch;
				}
				else {
					throw new Error("Constraint applied to solution should be compatible with the solution splitter or its negation (otherwise either the constraint is unsatisfiable, or the sub-solution is, and in this case we should not have gotten here).");
				}
			}
		}
		else {
			result = constraint.normalize(solution, process);
		}
		
		return result;
	}
}