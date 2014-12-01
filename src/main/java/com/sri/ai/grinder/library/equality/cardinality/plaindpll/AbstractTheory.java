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

import static com.sri.ai.expresso.helper.Expressions.FALSE;
import static com.sri.ai.expresso.helper.Expressions.TRUE;
import static com.sri.ai.util.Util.list;

import java.util.Collections;
import java.util.Iterator;
import java.util.Map;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.expresso.helper.Expressions;
import com.sri.ai.expresso.helper.SubExpressionsDepthFirstIterator;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.library.controlflow.IfThenElse;
import com.sri.ai.util.Util;
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
	public Expression pickSplitterInExpression(Expression expression, TheoryConstraint constraint, RewritingProcess process) {
		Expression result = null;
		
		Iterator<Expression> subExpressionIterator = new SubExpressionsDepthFirstIterator(expression);
		while (result == null && subExpressionIterator.hasNext()) {
			Expression subExpression = subExpressionIterator.next();
			Expression splitterCandidate = makeSplitterIfPossible(subExpression, constraint.getIndices(), process);
			if (splitterCandidate != null) {
				result = constraint.getMostRequiredSplitter(splitterCandidate, process); // should be generalized to any theory
			}
		}
	
		return result;
	}

	@Override
	public Expression applySplitterToSolution(boolean splitterSign, Expression splitter, Expression solution, RewritingProcess process) {
		Expression result;
		if (IfThenElse.isIfThenElse(solution)) {
			TheoryConstraint constraint = makeConstraint(Collections.emptyList()); // no indices in solutions
			constraint = constraint.applySplitter(splitterSign, splitter, process);
			result = applySplitterToSolution(splitterSign, splitter, constraint, solution, process);
		}
		else {
			result = applySplitterToExpression(splitterSign, splitter, solution, process);
			//result = solution;
		}
		return result;
	}
	
//	private Expression applySplitterToSolution(boolean splitterSign, Expression splitter, TheoryConstraint constraintUnderSplitter, Expression solution, RewritingProcess process) {
//		Expression result;
//		
//		if (DPLLUtil.isConditionalSolution(solution, this, process)) {
//			Expression solutionSplitter = IfThenElse.getCondition(solution);
//			TheoryConstraint constraintUnderSolutionSplitter = constraintUnderSplitter.applySplitter(true, solutionSplitter, process);
//			if (constraintUnderSolutionSplitter != null) {
//				TheoryConstraint constraintUnderSolutionSplitterNegation = constraintUnderSplitter.applySplitter(false, solutionSplitter, process);
//				if (constraintUnderSolutionSplitterNegation != null) {
//					Expression thenBranch = IfThenElse.getThenBranch(solution);
//					Expression elseBranch = IfThenElse.getElseBranch(solution);
//					Expression newThenBranch = applySplitterToSolution(splitterSign, splitter, constraintUnderSolutionSplitter, thenBranch, process);
//					Expression newElseBranch = applySplitterToSolution(splitterSign, splitter, constraintUnderSolutionSplitterNegation, elseBranch, process);
//					result = IfThenElse.makeIfDistinctFrom(solution, solutionSplitter, newThenBranch, newElseBranch, false /* no simplification to condition */);
//				}
//				else {
//					Expression thenBranch = IfThenElse.getThenBranch(solution);
//					Expression newThenBranch = applySplitterToSolution(splitterSign, splitter, constraintUnderSolutionSplitter, thenBranch, process);
//					result = newThenBranch;
//				}
//			}
//			else {
//				TheoryConstraint constraintUnderSolutionSplitterNegation = constraintUnderSplitter.applySplitter(false, solutionSplitter, process);
//				if (constraintUnderSolutionSplitterNegation != null) {
//					Expression elseBranch = IfThenElse.getElseBranch(solution);
//					Expression newElseBranch = applySplitterToSolution(splitterSign, splitter, constraintUnderSolutionSplitterNegation, elseBranch, process);
//					result = newElseBranch;
//				}
//				else {
//					throw new Error("Constraint applied to solution should be compatible with the solution splitter or its negation (otherwise either the constraint is unsatisfiable, or the sub-solution is, and in this case we should not have gotten here).");
//				}
//			}
//		}
//		else {
//			result = applySplitterToExpression(splitterSign, splitter, solution, process);
//			//result = solution;
//		}
//		
//		return result;
//	}

	private Expression applySplitterToSolution(boolean splitterSign, Expression splitter, TheoryConstraint constraintUnderSplitter, Expression solution, RewritingProcess process) {
		Expression result;
		
		if (DPLLUtil.isConditionalSolution(solution, this, process)) {
			Expression solutionSplitter = IfThenElse.getCondition(solution);
			TheoryConstraint constraintUnderSolutionSplitter         = constraintUnderSplitter.applySplitter(true,  solutionSplitter, process);
			TheoryConstraint constraintUnderSolutionSplitterNegation = constraintUnderSplitter.applySplitter(false, solutionSplitter, process);

			Expression thenBranch = IfThenElse.getThenBranch(solution);
			Expression elseBranch = IfThenElse.getElseBranch(solution);
			Expression newCondition;
			Expression newThenBranch;
			Expression newElseBranch;

			if (constraintUnderSolutionSplitter == null) {
				newCondition = FALSE;
				newThenBranch = Expressions.makeSymbol("whatever");
				newElseBranch = elseBranch;
			}
			else if (constraintUnderSolutionSplitterNegation == null) {
				newCondition = TRUE;
				newThenBranch = thenBranch;
				newElseBranch = Expressions.makeSymbol("whatever");
			}
			else {
				newCondition  = applySplitterOrTrueToSplitter(splitterSign, splitter, true, solutionSplitter, process);
				newThenBranch = applySplitterToSolution(true,  newCondition, thenBranch, process);
				newElseBranch = applySplitterToSolution(false, newCondition, elseBranch, process);
			}
			
			if ( ! newCondition.equals(FALSE)) {
				Expression splitterForThenBranch = applySplitterOrTrueToSplitter(true, newCondition, splitterSign, splitter, process);
				assert ! signedSplitterIsFalse(splitterSign, splitterForThenBranch); // otherwise newCondition would be FALSE
				if ( ! signedSplitterIsTrue(splitterSign, splitterForThenBranch)) {
					newThenBranch = applySplitterToSolution(splitterSign, splitterForThenBranch, constraintUnderSolutionSplitter, thenBranch, process);
				}
			}
			if ( ! newCondition.equals(TRUE)) {
				Expression splitterForElseBranch = applySplitterOrTrueToSplitter(false, newCondition, splitterSign, splitter, process);
				assert ! signedSplitterIsTrue(splitterSign, splitterForElseBranch); // otherwise newCondition would be FALSE
				if ( ! signedSplitterIsFalse(splitterSign, splitterForElseBranch)) {
					newElseBranch = applySplitterToSolution(splitterSign, splitterForElseBranch, constraintUnderSolutionSplitterNegation, elseBranch, process);
				}
			}
			// we assume here that whatever normalizations were re-imposed by newCondition on branches is preserved by the application of splitter to them.

			result = IfThenElse.makeIfDistinctFrom(solution, newCondition, newThenBranch, newElseBranch, false /* no simplification to condition */);
		}
		else {
			result = applySplitterToExpression(splitterSign, splitter, solution, process);
			//result = solution;
		}
		
		return result;
	}


	/**
	 * @param splitterSign
	 * @param splitter
	 * @return
	 */
	protected boolean signedSplitterIsTrue(boolean splitterSign, Expression splitter) {
		return splitterSign == true && splitter.equals(TRUE) || splitterSign == false && splitter.equals(FALSE);
	}

	/**
	 * @param splitterSign
	 * @param splitter
	 * @return
	 */
	protected boolean signedSplitterIsFalse(boolean splitterSign, Expression splitter) {
		return splitterSign == true && splitter.equals(FALSE) || splitterSign == false && splitter.equals(TRUE);
	}

	/**
	 * Applies a splitter to another assuming that they do not trivialize each other.
	 * This ensures the second one follows normalization properties imposed by the first one.
	 * @param splitterSign
	 * @param splitter
	 * @param anotherSplitter
	 * @param process
	 * @return
	 */
	private Expression applySplitterOrTrueToSplitter(boolean splitterSign, Expression splitter, boolean anotherSplitterSign, Expression anotherSplitter, RewritingProcess process) {
		Expression result;
		if ((splitterSign == true && splitter.equals(TRUE)) || (splitterSign == false && splitter.equals(FALSE))) {
			result = anotherSplitter;
		}
		else {
			result = applySplitterToExpression(splitterSign, splitter, anotherSplitter, process);
			if ( ! result.equals(Expressions.TRUE) && ! result.equals(Expressions.FALSE)) {
				result = makeSplitterIfPossible(result, list(), process);
			}
		}
		return result;
	}
}