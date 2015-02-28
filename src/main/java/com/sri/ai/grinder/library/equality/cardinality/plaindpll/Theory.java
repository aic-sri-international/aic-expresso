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
 * An interface for theories to be plugged into SGDPLL(T).
 * <p>
 * A theoryWithEquality represents a subset of all possible interpretations of a let of literals in its language.
 * It provides all services to DPLL specific to the theoryWithEquality, that is, that require knowledge about the specific subset of interpretations.
 * <p>
 * One of its tasks is to select and manipulate <i>splitters</i>.
 * A splitter is a literal on which DPLL splits the possible interpretations of an expression (see {@link SGDPLLT}).
 * The theoryWithEquality needs to know how to simplify expressions based on the fact that a splitter is true or false,
 * as well as how to simplify a <i>solution</i> based on a splitter's being true or false into a simpler solution.
 * A solution is an if-then-else expression in which all conditions are splitters.
 * 
 * @author braz
 *
 */
@Beta
public interface Theory {
	
	/**
	 * Indicates whether an expression is to be considered a variable term in this theory.
	 * @param term
	 * @param process
	 * @return
	 */
	boolean isVariableTerm(Expression term, RewritingProcess process);

	/**
	 * Simplifies expression given theoryWithEquality.
	 * @param expression
	 * @param process
	 * @return
	 */
	Expression simplify(Expression expression, RewritingProcess process);

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
	 * Picks a splitter based on one of the theoryWithEquality's literals in the given expression under given constraint.
	 * The returned splitter must be a splitter that will help solve the current expression;
	 * it does not need to be equivalent to the given expression.
	 * See {@link Theory} documentation for the definition of a splitter.
	 * @param expression
	 * @param constraint
	 * @param process
	 * @return
	 */
	Expression pickSplitterInExpression(Expression expression, Constraint constraint, RewritingProcess process);

	/**
	 * Indicates whether a splitter interpretation depends on the interpretation of some index.
	 */
	boolean splitterDependsOnIndex(Expression splitter, Collection<Expression> indices);

	/**
	 * Simplifies an expression given the assumption that a splitter is true or false, depending on given sign.
	 * @param splitterSign
	 * @param splitter
	 * @param expression
	 * @param process
	 * @return
	 */
	Expression applySplitterToExpression(boolean splitterSign, Expression splitter, Expression expression, RewritingProcess process);
	
	/** Same as {@link #applySplitterToExpression(boolean, Expression, Expression, RewritingProcess)} but using {@link SignedSplitter}. */
	default Expression applySplitterToExpression(SignedSplitter signedSplitter, Expression expression, RewritingProcess process) {
		return applySplitterToExpression(signedSplitter.getSplitterSign(), signedSplitter.getSplitter(), expression, process);
	}
	
	/**
	 * Simplifies solution under constraint, by eliminating trivialized splitters
	 * and normalizing remaining splitters and leaf expressions according to theoryWithEquality normalization properties.
	 * @param constraint
	 * @param solution
	 * @param process
	 * @return
	 */
	Expression applyConstraintToSolution(Constraint constraint, Expression solution, RewritingProcess process);

	/**
	 * Indicates whether the application of a constraint on a splitter can result in true, false, or the splitter itself;
	 * this is used to optimize applications of constraints on solutions.
	 * @return whether the application of a constraint on a splitter can result in true, false, or the splitter itself.
	 */
	boolean applicationOfConstraintOnSplitterAlwaysEitherTrivializesItOrEffectsNoChangeAtAll();

	/**
	 * Make a new constraint for this theoryWithEquality over a set of indices (equivalent to all assignments to those indices).
	 * @return
	 */
	Constraint makeConstraint(Collection<Expression> indices);

	/**
	 * An interface for theoryWithEquality-specific representations of the current constraint in DPLL.
	 * 
	 * @author braz
	 *
	 */
	@Beta
	public interface Constraint extends Expression {
		
		/**
		 * The set of variables on subsets of which one can count models of this constraint.
		 */
		Collection<Expression> getSupportedIndices();
		
		/**
		 * Simplifies a given splitter to true if implied by constraint, false if its negation is implied by constraint,
		 * or a version of itself with terms replaced by representatives.
		 * Note that {@link #normalize(Expression, RewritingProcess)} cannot be used instead of this method
		 * because, for certain theories, the result of normalizing an splitter by treating it as an expression
		 * may produce a non-boolean-constant expression that is not a splitter either.
		 * For example, in equality theory, {@link #normalize(Expression, RewritingProcess)}
		 * transforms 'Term = true' into 'Term' and 'Term = false' into 'not Term',
		 * and these are not splitters for that theory anymore.
		 */
		Expression normalizeSplitterGivenConstraint(Expression splitter, RewritingProcess process);

		/**
		 * Generates new constraint representing conjunction of this constraint and given splitter
		 * (or its negation, depending on the sign).
		 * @param splitterSign the splitter's sign (true for splitter itself, false for its negation)
		 * @param splitter the splitter according to this theoryWithEquality's choice
		 * @param process the rewriting process
		 */
		Constraint applySplitter(boolean splitterSign, Expression splitter, RewritingProcess process);

		/** Same as {@link #applySplitter(boolean, Expression, RewritingProcess)} but using {@link SignedSplitter}. */
		default Constraint applySplitter(SignedSplitter signedSplitter, RewritingProcess process) {
			return applySplitter(signedSplitter.getSplitterSign(), signedSplitter.getSplitter(), process);
		}
		
		/**
		 * Provides a splitter, not already explicitly represented by the constraint,
		 * toward a state in which the model count for the given subset of indices
		 * can be computed (the model count may be condition on the other variables),
		 * or returns null if it already is in such a state.
		 */
		Expression pickSplitter(Collection<Expression> indicesSubSet, RewritingProcess process);
		
		/**
		 * Same as {@link #pickSplitter(Collection, RewritingProcess)} invoked on getIndices().
		 */
		default Expression pickSplitter(RewritingProcess process) {
			return pickSplitter(getSupportedIndices(), process);
		}
		
		/**
		 * Computes model count for constraint, given a sub-set of indices, in polynomial time.
		 * Assumes that {@link #pickSplitter(Collection, RewritingProcess)} returns <code>null</code>,
		 * that is, the constraint is in such a state and context that allows the determination of a unique model count.
		 * The model count is required to contain no literals implied by the contextual constraint.
		 */
		Expression modelCount(Collection<Expression> indicesSubSet, RewritingProcess process);
		
		/**
		 * Same as {@link #modelCount(Collection, RewritingProcess)} invoked on getIndices().
		 */
		default Expression modelCount(RewritingProcess process) {
			return modelCount(getSupportedIndices(), process);
		}
		
		/**
		 * Receives an expression and returns an equivalent one according to some normalization property
		 * For example, an implementation involving equality may choose to always represent all symbols in an equality cluster
		 * by the same symbol. 
		 * This method is not required to perform complete inference (that is, to return some minimal representation
		 * of the expression).
		 * @param expression
		 * @param process
		 * @return
		 */
		Expression normalize(Expression expression, RewritingProcess process);
	}
}