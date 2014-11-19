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

import com.google.common.annotations.Beta;
import com.sri.ai.expresso.api.Expression;
import com.sri.ai.grinder.api.RewritingProcess;

/**
 * An interface for theories to be plugged into DPLL.
 * <p>
 * A theory represents a subset of all possible interpretations of a let of literals in its language.
 * It provides all services to DPLL specific to the theory, that is, that require knowledge about the specific subset of interpretations.
 * <p>
 * One of its tasks is to select and manipulate <i>splitters</i>.
 * A splitter is a literal on which DPLL splits the possible interpretations of an expression (see {@link DPLLGeneralizedAndSymbolic}).
 * The theory needs to know how to simplify expressions based on the fact that a splitter is true or false,
 * as well as how to simplify a <i>solution</i> based on a splitter's being true or false into a simpler solution.
 * A solution is an if-then-else expression in which all conditions are splitters.
 * 
 * @author braz
 *
 */
@Beta
public interface Theory {
	
	/**
	 * Simplifies expression given theory.
	 * @param expression
	 * @param process
	 * @return
	 */
	Expression simplify(Expression expression, RewritingProcess process);

	/**
	 * Makes splitter equivalent to given expression if such exists, or null otherwise.
	 * @param expression
	 * @param indices
	 * @param process
	 * @return
	 */
	Expression makeSplitterIfPossible(Expression expression, Collection<Expression> indices, RewritingProcess process);

	/**
	 * Picks a splitter based on one of the theory's literals in the given expression under given constraint.
	 * The returned splitter must be a splitter that will help solve the current expression;
	 * it does not need to be equivalent to the given expression.
	 * Also receives the indices as arguments, in case they are helpful.
	 * See {@link Theory} documentation for the definition of a splitter.
	 * @param expression
	 * @param constraint
	 * @param process
	 * @return
	 */
	Expression pickSplitterInExpression(Expression expression, TheoryConstraint constraint, RewritingProcess process);

	/**
	 * Indicates whether a splitter interpretation depends on the interpretation of some index.
	 */
	boolean splitterInvolvesIndex(Expression splitter, Collection<Expression> indices);

	/**
	 * Simplifies an expression given the assumption that a splitter is true.
	 * @param splitter
	 * @param expression
	 * @param process
	 * @return
	 */
	Expression applySplitterToExpression(Expression splitter, Expression expression, RewritingProcess process);

	/**
	 * Simplifies an expression given the assumption that a splitter is false.
	 * @param splitter
	 * @param expression
	 * @param process
	 * @return
	 */
	Expression applySplitterNegationToExpression(Expression splitter, Expression expression, RewritingProcess process);

	/**
	 * Simplifies a solution given the assumption that a splitter is true,
	 * returning a solution that is equivalent to the given one under that assumption. 
	 * @param splitter
	 * @param solution
	 * @param process
	 * @return an equivalent solution
	 */
	Expression applySplitterToSolution(Expression splitter, Expression solution, RewritingProcess process);

	/**
	 * Simplifies a solution given the assumption that a splitter is false,
	 * returning a solution that is equivalent to the given one under that assumption. 
	 * @param splitter
	 * @param solution
	 * @param process
	 * @return an equivalent solution
	 */
	Expression applySplitterNegationToSolution(Expression splitter, Expression solution, RewritingProcess process);

	/**
	 * Make a new constraint for this theory over a set of indices (equivalent to all assignments to those indices).
	 * @return
	 */
	TheoryConstraint makeConstraint(Collection<Expression> indices);
}