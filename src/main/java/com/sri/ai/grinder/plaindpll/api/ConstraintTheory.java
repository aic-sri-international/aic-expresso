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
package com.sri.ai.grinder.plaindpll.api;

import java.util.Collection;
import java.util.Iterator;

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;
import com.sri.ai.grinder.plaindpll.core.PlainSGDPLLT;
import com.sri.ai.grinder.plaindpll.core.SignedSplitter;

/**
 * An interface for constraint theories to be plugged into quantifier problems.
 * <p>
 * One of its tasks is to select and manipulate <i>splitters</i>.
 * A splitter is a literal on which DPLL splits the possible interpretations of an expression (see {@link PlainSGDPLLT}).
 * The constraintTheoryWithEquality needs to know how to simplify expressions based on the fact that a splitter is true or false,
 * as well as how to simplify a <i>solution</i> based on a splitter's being true or false into a simpler solution.
 * A solution is an if-then-else expression in which all conditions are splitters.
 * 
 * @author braz
 *
 */
@Beta
public interface ConstraintTheory extends Theory {
	
	/**
	 * Indicates whether an expression is to be considered a variable term in this constraintTheory.
	 * @param term
	 * @param process
	 * @return
	 */
	boolean isVariable(Expression term, RewritingProcess process);

	/**
	 * Simplifies an expression given the assumption that a splitter is true or false, depending on given sign.
	 * @param splitterSign
	 * @param splitter
	 * @param expression
	 * @param process
	 * @return
	 */
	public abstract Expression applySplitterToExpression(boolean splitterSign, Expression splitter, Expression expression, RewritingProcess process);

	/** Same as {@link #applySplitterToExpression(boolean, Expression, Expression, RewritingProcess)} but using {@link SignedSplitter}. */
	default Expression applySplitterToExpression(SignedSplitter signedSplitter, Expression expression, RewritingProcess process) {
		return applySplitterToExpression(signedSplitter.getSplitterSign(), signedSplitter.getSplitter(), expression, process);
	}

	/**
	 * Makes splitter equivalent to given expression if such exists, or null otherwise.
	 * Every decision as to whether an expression is a splitter or equivalent to a splitter must go through this method.
	 * @param expression
	 * @param indices
	 * @param process
	 * @return
	 */
	Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process);

	/**
	 * Returns an iterator that ranges over splitters from a given input expression,
	 * based on one of the constraintTheoryWithEquality's literals in the given expression under given constraint.
	 * The returned splitter must be a splitter that will help solve the current expression;
	 * it does not need to be equivalent to the given expression.
	 * See {@link ConstraintTheory} documentation for the definition of a splitter.
	 * @param expression
	 * @param constraint
	 * @param process
	 * @return
	 */
	Iterator<Expression> pickSplitterInExpressionIterator(Expression expression, Constraint1 constraint, RewritingProcess process);

	/**
	 * Indicates whether a splitter interpretation depends on the interpretation of some index.
	 */
	boolean splitterDependsOnIndex(Expression splitter, Collection<Expression> indices);

	/**
	 * Simplifies solution under constraint, by eliminating trivialized splitters
	 * and normalizing remaining splitters and leaf expressions according to constraintTheoryWithEquality normalization properties.
	 * If the constraint is found to be inconsistent during this operation (this may happen if the solver is incomplete),
	 * returns null.
	 * @param constraint
	 * @param solution
	 * @param process
	 * @return
	 */
	Expression applyConstraintToSolution(Constraint1 constraint, Expression solution, RewritingProcess process);

	/**
	 * Indicates whether the application of a constraint on a splitter can result in true, false, or the splitter itself;
	 * this is used to optimize applications of constraints on solutions.
	 * @return whether the application of a constraint on a splitter can result in true, false, or the splitter itself.
	 */
	boolean applicationOfConstraintOnSplitterAlwaysEitherTrivializesItOrEffectsNoChangeAtAll();

	/**
	 * Make a new constraint for this constraintTheoryWithEquality over a set of indices (equivalent to all assignments to those indices).
	 * @return
	 */
	Constraint1 makeConstraint(Collection<Expression> indices);
}